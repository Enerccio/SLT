(cl:in-package #:eclector.reader)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro WITH-FORBIDDEN-QUASIQUOTATION.
;;;
;;; This macro controls whether quasiquote and/or unquote should be
;;; allowed in a given context.

(defmacro with-forbidden-quasiquotation
    ((context &optional (quasiquote-forbidden-p t)
                        (unquote-forbidden-p t))
     &body body)
  (alexandria:with-unique-names (context*)
    (let ((context-used-p nil))
      (flet ((make-binding (variable value-form)
               (cond ((constantp value-form)
                      (case (eval value-form)
                        (:keep
                         '())
                        ((nil)
                         `((,variable nil)))
                        (t
                         (setf context-used-p t)
                         `((,variable ,context*)))))
                     (t
                      (setf context-used-p t)
                      `((,variable (case ,value-form
                                     (:keep ,variable)
                                     ((nil) nil)
                                     (t ,context*))))))))
        `(let* ((,context* ,context)
                ,@(make-binding '*quasiquote-forbidden* quasiquote-forbidden-p)
                ,@(make-binding '*unquote-forbidden* unquote-forbidden-p))
           ,@(unless context-used-p
               `((declare (ignore ,context*))))
           ,@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for semicolon.
;;;
;;; We read characters until end-of-file or until we have read a
;;; newline character.  Since reading a comment does not generate an
;;; object, the semicolon reader must indicate that fact by returning
;;; zero values.

(defun semicolon (stream char)
  (declare (ignore char))
  (loop with state = :semicolon
        for char = (read-char stream nil nil t)
        until (or (null char) (eql char #\Newline))
        if (and (eq state :semicolon) (char= char #\;))
        count 1 into semicolons
        else
        do (setf state nil)
        finally (setf *skip-reason* (cons :line-comment (1+ semicolons))))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for single quote.
;;;
;;; They HyperSpec says that the reader signals an error if
;;; end-of-file is encountered before an object has been entirely
;;; parsed, independently of whether EOF-ERROR-P is true or not.  For
;;; that reason, we call the reader recursively with the value of
;;; EOF-ERROR-P being T.

(defun single-quote (stream char)
  (declare (ignore char))
  (let ((material (handler-case
                      (read stream t nil t)
                    ((and end-of-file (not incomplete-construct)) (condition)
                      (%recoverable-reader-error
                       stream 'end-of-input-after-quote
                       :stream-position (stream-position condition)
                       :report 'inject-nil)
                      nil)
                    (end-of-list (condition)
                      (%recoverable-reader-error
                       stream 'object-must-follow-quote
                       :position-offset -1 :report 'inject-nil)
                      (unread-char (%character condition) stream)
                      nil))))
    (wrap-in-quote *client* material)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for double quote.
;;;
;;; We identify a single escape character by its syntax type, so that
;;; if a user wants a different escape chacacter, we can handle that.
;;;
;;; Furthermore, They HyperSpec says that the reader signals an error
;;; if end-of-file is encountered before an object has been entirely
;;; parsed, independently of whether EOF-ERROR-P is true or not.  For
;;; that reason, we call READ-CHAR with the value of EOF-ERROR-P being
;;; T.
;;;
;;; We accumulate characters in an adjustable vector.  However, the
;;; HyperSpec says that we must return a SIMPLE-STRING.  For that
;;; reason, we call COPY-SEQ in the end.  COPY-SEQ is guaranteed to
;;; return a simple vector.

(defun double-quote (stream char)
  (let ((result (make-array 100 :element-type 'character
                                :adjustable t
                                :fill-pointer 0)))
    (loop with readtable = *readtable*
          for char2 = (read-char-or-recoverable-error
                       stream char 'unterminated-string
                       :delimiter char :report 'use-partial-string)
          until (eql char2 char)
          when (eq (eclector.readtable:syntax-type readtable char2) :single-escape)
            do (setf char2 (read-char-or-recoverable-error
                            stream nil 'unterminated-single-escape-in-string
                            :position-offset -1
                            :escape-char char2 :report 'use-partial-string))
          when char2
            do (vector-push-extend char2 result)
          finally (return (copy-seq result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macros for backquote and comma.
;;;
;;;
;;; The control structure we use for backquote requires some
;;; explanation.
;;;
;;; The HyperSpec says (see section 2.4.6) that backquote and comma
;;; are allowed only inside lists and vectors.  Since READ can be
;;; called recursively from other functions as well (such as the
;;; reader for arrays, or user-defined readers), we somehow need to
;;; track whether backquote and comma are allowed in the current
;;; context.
;;;
;;; We could (and previously did) forbid backquote and comma except
;;; inside lists and vectors, but in practice, clients expect control
;;; over this behavior in order to implement reader macros such as
;;;
;;;   #L`(,!1 ,!1) => (lambda (g1) `(,g1 ,g1))
;;;   `#{,key ,value} => (let ((g1 (make-hash-table ...))) ...)
;;;
;;; We use the flags *QUASIQUOTE-FORBIDDEN-P* and
;;; *UNQUOTE-FORBIDDEN-P* to control whether backquote and comma are
;;; allowed.  Initially, both variables are bound to T, allowing
;;; backquote and comma (*QUASIQUOTE-DEPTH* ensures that backquote and
;;; comma are nested properly).  Reader macros such as #C, #A,
;;; etc. bind the variables to a true value that also indicates the
;;; context (usually the symbol naming the reader macro function).
;;; The only way these variables can be re-bound to NIL (in the
;;; standard readtable) is the SHARPSIGN-DOT reader macro.
;;;
;;;
;;; Representation of quasiquoted forms
;;;
;;; The HyperSpec explicitly encourages us (see section 2.4.6.1) to
;;; follow the example of Scheme for representing backquote
;;; expression.  We see no reason for choosing a different
;;; representation, so we use (QUASIQUOTE <form>), (UNQUOTE <form>),
;;; and (UNQUOTE-SPLICING <form>).  Then we define QUASIQUOTE as a
;;; macro that expands to a CL form that will build the final data
;;; structure.

(defun backquote (stream char)
  (declare (ignore char))
  (alexandria:when-let ((context *quasiquote-forbidden*))
    (unless *read-suppress*
      (%recoverable-reader-error
       stream 'backquote-in-invalid-context
       :position-offset -1 :context context :report 'ignore-quasiquote)
      (return-from backquote
        (let ((*backquote-depth* 0))
          (read stream t nil t)))))
  (let ((material (let ((*backquote-depth* (1+ *backquote-depth*))
                        (*unquote-forbidden* nil))
                    (handler-case
                        (read stream t nil t)
                      ((and end-of-file (not incomplete-construct)) (condition)
                        (%recoverable-reader-error
                         stream 'end-of-input-after-backquote
                         :stream-position (stream-position condition)
                         :report 'inject-nil)
                        nil)
                      (end-of-list (condition)
                        (%recoverable-reader-error
                         stream 'object-must-follow-backquote
                         :position-offset -1 :report 'inject-nil)
                        (unread-char (%character condition) stream)
                        nil)))))
    (wrap-in-quasiquote *client* material)))

(defun comma (stream char)
  (declare (ignore char))
  (let* ((depth *backquote-depth*)
         (char2 (read-char stream nil nil t))
         (splicing-p (case char2
                       ((#\@ #\.) t)
                       ((nil) nil) ; end-of-input, but we may recover
                       (t (unread-char char2 stream)))))
    (flet ((read-material ()
             (handler-case
                 (read stream t nil t)
               ((and end-of-file (not incomplete-construct)) (condition)
                 (%recoverable-reader-error
                  stream 'end-of-input-after-unquote
                  :stream-position (stream-position condition)
                  :splicing-p splicing-p :report 'inject-nil)
                 nil)
               (end-of-list (condition)
                 (%recoverable-reader-error
                  stream 'object-must-follow-unquote
                  :position-offset -1
                  :splicing-p splicing-p :report 'inject-nil)
                 (unread-char (%character condition) stream)
                 nil))))
      (unless (plusp depth)
        (%recoverable-reader-error
         stream 'unquote-not-inside-backquote
         :position-offset (if splicing-p -2 -1)
         :splicing-p splicing-p :report 'ignore-unquote)
        (return-from comma (read-material)))
      (alexandria:when-let ((context *unquote-forbidden*))
        (unless *read-suppress*
          (%recoverable-reader-error
           stream 'unquote-in-invalid-context
           :position-offset (if splicing-p -2 -1)
           :splicing-p splicing-p :context context :report 'ignore-unquote)
          (return-from comma (read-material))))
      (let* ((*backquote-depth* (1- depth))
             (form (read-material)))
        (if splicing-p
            (wrap-in-unquote-splicing *client* form)
            (wrap-in-unquote *client* form))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macros for left-parenthesis and right-parenthesis.
;;;
;;; The HyperSpec says that right-parenthesis is a macro character.
;;; In the reader macro for left-parenthesis, we can not just read
;;; until we find a right parenthesis, because it is possible that
;;; some other character has been assigned the same meaning, and we
;;; need to handle that situation too.
;;;
;;; Another problem we need to solve is that of the CONSING-DOT.  The
;;; HyperSpec says that it is a token.  For that reason, we can not
;;; just read characters and look for a single period, because it is
;;; possible that the single dot has a different syntax type in this
;;; particular readtable.  Furthermore, we must handle error
;;; situations such as an attempt to use more than one dot in a list,
;;; or having zero or strictly more than one expression following a
;;; dot.
;;;
;;; We solve these problems as follows: the reader macro for a right
;;; parenthesis calls SIGNAL with a particular condition (of type
;;; END-OF-LIST).  In situations where the right parenthesis is
;;; allowed, there will be a handler for this condition type.
;;; Therefore, in that situation, the call to SIGNAL will not return.
;;; If the call to SIGNAL returns, we signal and ERROR, because then
;;; the right parenthesis was read in a context where it is not
;;; allowed.
;;;
;;; The reader macro for left parenthesis manages two local variables,
;;; REVERSED-RESULT and TAIL.  The variable REVERSED-RESULT is used to
;;; accumulate elements of the list (preceding a possible consing dot)
;;; being read, in reverse order.  A handler for END-OF-LIST is
;;; established around the recursive calls to READ inside the reader
;;; macro function.  When this handler is invoked, it calls NRECONC to
;;; reverse the value of REVERSED-RESULT and attach the value of TAIL
;;; to the end.  Normally, the value of TAIL is NIL, so the handler
;;; will create and return a proper list containing the accumulated
;;; elements.
;;;
;;; We use a special variable name *CONSING-DOT-ALLOWED-P* to
;;; determine the contexts in which a consing dot is allowed.
;;; Whenever the token parser detects a consing dot, it examines this
;;; variable, and if it is true it returns the unique CONSING-DOT
;;; token, and if it is false, signals an error.  Initially, this
;;; variable has the value FALSE.  Whenever the reader macro for left
;;; parenthesis is called, it binds this variable to TRUE.  When a
;;; recursive call to READ returns with the consing dot as a value,
;;; the reader macro for left parenthesis does three things.  First it
;;; SETS (as opposed to BINDS) *CONSING-DOT-ALLOWED-P* to FALSE, so
;;; that if a second consing dot should occur, then the token reader
;;; signals an error.  Second, it establishes a nested handler for
;;; END-OF-LIST, so that if a right parenthesis should occur
;;; immediately after the consing dot, then an error is signaled.
;;; With this handler established, READ is called.  If it returns
;;; normally, then the return value becomes the value of the variable
;;; TAIL.  Third, it calls READ again without any nested handler
;;; established.  This call had better result in a right parenthesis,
;;; so that END-OF-LIST is signaled, which is caught by the outermost
;;; handler and the correct list is built and returned.  If this call
;;; should return normally, we have a problem, because this means that
;;; there was a second subform after the consing dot in the list, so
;;; we signal an ERROR.

(defun left-parenthesis (stream char)
  (declare (ignore char))
  (%read-delimited-list stream #\)))

(defun right-parenthesis (stream char)
  ;; If the call to SIGNAL returns, then there is no handler for this
  ;; condition, which means that the right parenthesis was found in a
  ;; context where it is not allowed.
  (signal-end-of-list char)
  (%recoverable-reader-error
   stream 'invalid-context-for-right-parenthesis
   :position-offset -1
   :found-character char :report 'ignore-trailing-right-paren))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for sharpsign single quote.

(defun %sharpsign-single-quote (stream char parameter allow-unquote)
  (declare (ignore char))
  (unless (null parameter)
    (numeric-parameter-ignored stream 'sharpsign-single-quote parameter))
  (let ((name (with-forbidden-quasiquotation
                  ('sharpsign-single-quote :keep (if allow-unquote :keep t))
                (handler-case
                    (read stream t nil t)
                  ((and end-of-file (not incomplete-construct)) (condition)
                    (%recoverable-reader-error
                     stream 'end-of-input-after-sharpsign-single-quote
                     :stream-position (stream-position condition)
                     :report 'inject-nil)
                    nil)
                  (end-of-list (condition)
                    (%recoverable-reader-error
                     stream 'object-must-follow-sharpsign-single-quote
                     :position-offset -1 :report 'inject-nil)
                    (unread-char (%character condition) stream)
                    nil)))))
    (cond (*read-suppress*
           nil)
          ((null name)
           nil)
          (t
           (wrap-in-function *client* name)))))

;;; This variation of SHARPSIGN-SINGLE-QUOTE allows unquote within #',
;;; that is `#',(foo) is read as
;;;
;;;   (quasiquote (function (unquote (foo))))
;;;
;;; .  It is not clear that this behavior is supported by
;;; specification, but it is widely relied upon and thus the default
;;; behavior.
(defun sharpsign-single-quote (stream char parameter)
  (%sharpsign-single-quote stream char parameter t))

;;; This variation of SHARPSIGN-SINGLE-QUOTE does not allow unquote
;;; within #'.
(defun strict-sharpsign-single-quote (stream char parameter)
  (%sharpsign-single-quote stream char parameter nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for sharpsign left parenthesis.

(defun sharpsign-left-parenthesis (stream char parameter)
  (declare (ignore char))
  (flet ((next-element ()
           (handler-case
               (values (read stream t nil t) t)
             (end-of-list ()
               (values nil nil))
             ((and end-of-file (not incomplete-construct)) (condition)
               (%recoverable-reader-error
                stream 'unterminated-vector
                :stream-position (stream-position condition)
                :delimiter #\) :report 'use-partial-vector)
               (values nil nil)))))
    (cond (*read-suppress*
           (loop for elementp = (nth-value 1 (next-element))
                 while elementp))
          ((null parameter)
           (loop with result = (make-array 10 :adjustable t :fill-pointer 0)
                 for (element elementp) = (multiple-value-list (next-element))
                 while elementp
                 do (vector-push-extend element result)
                 finally (return (coerce result 'simple-vector))))
          (t
           (loop with result = (make-array parameter)
                 with excess-position = nil
                 for index from 0
                 for (element elementp) = (multiple-value-list
                                           (next-element))
                 while elementp
                 if (< index parameter)
                   do (setf (aref result index) element)
                 else
                   do (setf excess-position (eclector.base:source-position
                                             *client* stream))
                 finally (cond ((and (zerop index) (plusp parameter))
                                (%recoverable-reader-error
                                 stream 'no-elements-found
                                 :position-offset -1
                                 :array-type 'vector :expected-number parameter
                                 :report 'use-empty-vector)
                                (setf result (make-array 0)
                                      index parameter))
                               ((> index parameter)
                                (%recoverable-reader-error
                                 stream 'too-many-elements
                                 :stream-position excess-position ; inaccurate
                                 :position-offset -1
                                 :array-type 'vector
                                 :expected-number parameter
                                 :number-found index
                                 :report 'ignore-excess-elements)))
                         (return
                           (if (< index parameter)
                               (fill result (aref result (1- index))
                                     :start index)
                               result)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for sharpsign dot.

(defun sharpsign-dot (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (numeric-parameter-ignored stream 'sharpsign-dot parameter))
  (cond ((not *read-eval*)
         (%reader-error stream 'read-time-evaluation-inhibited))
        (*read-suppress*
         (read stream t nil t))
        (t
         (let ((expression (with-forbidden-quasiquotation (nil nil nil)
                             (let ((*list-reader* nil))
                               (handler-case
                                   (read stream t nil t)
                                 ((and end-of-file (not incomplete-construct)) (condition)
                                   (%recoverable-reader-error
                                    stream 'end-of-input-after-sharpsign-dot
                                    :stream-position (stream-position condition)
                                    :report 'inject-nil)
                                   nil)
                                 (end-of-list (condition)
                                   (%recoverable-reader-error
                                    stream 'object-must-follow-sharpsign-dot
                                    :position-offset -1 :report 'inject-nil)
                                   (unread-char (%character condition) stream)
                                   nil))))))
           (handler-case
               (evaluate-expression *client* expression)
             (error (condition)
               (%recoverable-reader-error
                stream 'read-time-evaluation-error
                :expression expression :original-condition condition
                :report 'inject-nil)
               nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for sharpsign backslash.

;;; Mandatory character names according to 13.1.7 Character Names.
(defparameter *character-names*
  (alexandria:alist-hash-table '(("NEWLINE"   . #.(code-char 10))
                                 ("SPACE"     . #.(code-char 32))
                                 ("RUBOUT"    . #.(code-char 127))
                                 ("PAGE"      . #.(code-char 12))
                                 ("TAB"       . #.(code-char 9))
                                 ("BACKSPACE" . #.(code-char 8))
                                 ("RETURN"    . #.(code-char 13))
                                 ("LINEFEED"  . #.(code-char 10)))
                               :test 'equalp))

(defun find-standard-character (name)
  (gethash name *character-names*))

(defun sharpsign-backslash (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (numeric-parameter-ignored stream 'sharpsign-backslash parameter))
  (let ((char1 (read-char-or-recoverable-error
                stream nil 'end-of-input-after-backslash
                :report '(use-replacement-character #1=#\?))))
    (when (null char1) ; can happen when recovering
      (return-from sharpsign-backslash #1#))
    (with-token-info (push-char () finalize :lazy t)
      (labels ((handle-char (char escapep)
                 (declare (ignore escapep))
                 (when (not (null char1))
                   (push-char char1)
                   (setf char1 nil))
                 (push-char char))
               (unterminated-single-escape (escape-char)
                 (%recoverable-reader-error
                  stream 'unterminated-single-escape-in-character-name
                  :escape-char escape-char :report 'use-partial-character-name))
               (unterminated-multiple-escape (delimiter)
                 (%recoverable-reader-error
                  stream 'unterminated-multiple-escape-in-character-name
                  :delimiter delimiter :report 'use-partial-character-name))
               (lookup (name)
                 (let ((character (find-character *client* name)))
                   (cond ((null character)
                          (%recoverable-reader-error
                           stream 'unknown-character-name
                           :position-offset (- (if (characterp name)
                                                   1
                                                   (length name)))
                           :name name
                           :report '(use-replacement-character #2=#\?))
                          #2#)
                         (t
                          character))))
               (terminate-character ()
                 (return-from sharpsign-backslash
                   (cond (*read-suppress* nil)
                         ((not (null char1)) ; no additional characters pushed (same as (null token))
                          (lookup char1))
                         (t
                          (lookup (finalize)))))))
        (token-state-machine
         stream *readtable* handle-char nil nil
         unterminated-single-escape unterminated-multiple-escape
         terminate-character)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for sharpsign B, X, O and R.

(defun read-rational (stream base)
  (let ((readtable *readtable*)
        (read-suppress *read-suppress*))
    (labels ((next-char (eof-error-p)
               (let ((char (read-char stream nil nil t)))
                 (cond ((not (null char))
                        (values char (eclector.readtable:syntax-type
                                      readtable char)))
                       ((and eof-error-p (not read-suppress))
                        (%recoverable-reader-error
                         stream 'end-of-input-before-digit
                         :base base :report 'replace-invalid-digit)
                        (values #\1 :constituent))
                       (t
                        (values nil nil)))))
             (digit-expected (char type recover-value)
               (%recoverable-reader-error
                stream 'digit-expected
                :position-offset -1
                :character-found char :base base
                :report 'replace-invalid-digit)
               (unless (eq type :constituent)
                 (unread-char char stream))
               recover-value)
             (ensure-digit (char type)
               (let ((value (digit-char-p char base)))
                 (if (null value)
                     (digit-expected char type 1)
                     value)))
             (maybe-sign ()
               (multiple-value-bind (char type) (next-char t)
                 (cond (read-suppress
                        (values 1 0))
                       ((not (eq type :constituent))
                        (digit-expected char type nil))
                       ((char= char #\-)
                        (values -1 0))
                       (t
                        (values 1 (ensure-digit char type))))))
             (integer (empty-allowed /-allowed initial-value)
               (let ((value initial-value))
                 (tagbody
                    (when empty-allowed (go rest)) ; also when READ-SUPPRESS
                    (multiple-value-bind (char type) (next-char t)
                      (case type
                        (:constituent
                         (setf value (ensure-digit char type)))
                        (t
                         (digit-expected char type nil)
                         (return-from integer value))))
                  rest
                    (multiple-value-bind (char type) (next-char nil)
                      (ecase type
                        ((nil)
                         (return-from integer value))
                        (:whitespace
                         (unread-char char stream)
                         (return-from integer value))
                        (:terminating-macro
                         (unread-char char stream)
                         (return-from integer value))
                        ((:non-terminating-macro
                          :single-escape :multiple-escape)
                         (cond (read-suppress
                                (go rest))
                               (t
                                (digit-expected char type nil)
                                (return-from integer value))))
                        (:constituent
                         (cond (read-suppress
                                (go rest))
                               ((and /-allowed (eql char #\/))
                                (return-from integer (values value t)))
                               (t
                                (setf value (+ (* base (or value 0))
                                               (ensure-digit char type)))
                                (go rest)))))))))
             (read-denominator ()
               (let ((value (integer nil nil nil)))
                 (cond ((eql value 0)
                        (%recoverable-reader-error
                         stream 'zero-denominator
                         :position-offset -1 :report 'replace-invalid-digit)
                        nil)
                       (t
                        value)))))
      (multiple-value-bind (sign numerator) (maybe-sign)
        (if (null sign)
            0
            (multiple-value-bind (numerator slashp)
                (integer (= sign 1) t numerator)
              (unless read-suppress ; When READ-SUPPRESS, / has been consumed
                (let ((denominator (when slashp (read-denominator))))
                  (* sign (if denominator
                              (/ numerator denominator)
                              numerator))))))))))

(defun sharpsign-b (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (numeric-parameter-ignored stream 'sharpsign-b parameter))
  (read-rational stream 2.))

(defun sharpsign-x (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (numeric-parameter-ignored stream 'sharpsign-x parameter))
  (read-rational stream 16.))

(defun sharpsign-o (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (numeric-parameter-ignored stream 'sharpsign-o parameter))
  (read-rational stream 8.))

(defun sharpsign-r (stream char parameter)
  (declare (ignore char))
  (let ((radix (cond ((not parameter)
                      (numeric-parameter-not-supplied stream 'sharpsign-r)
                      36)
                     ((not (<= 2 parameter 36))
                      (unless *read-suppress*
                        (%recoverable-reader-error
                         stream 'invalid-radix
                         :position-offset (- (+ (parameter-length parameter) 1))
                         :radix parameter :report 'use-replacement-radix))
                      36)
                     (t
                      parameter))))
    (read-rational stream radix)))

(defun sharpsign-asterisk (stream char parameter)
  (declare (ignore char))
  (let ((read-suppress *read-suppress*)
        (readtable *readtable*))
    (flet ((next-bit ()
             (let ((char (read-char stream nil nil t)))
               (multiple-value-bind (syntax-type value)
                   (unless (null char)
                     (values (eclector.readtable:syntax-type
                              readtable char)
                             (digit-char-p char 2)))
                 (when (eq syntax-type :terminating-macro)
                   (unread-char char stream))
                 (cond ((member syntax-type '(nil :whitespace :terminating-macro))
                        nil)
                       (read-suppress
                        t)
                       ((null value)
                        (%recoverable-reader-error
                         stream 'digit-expected
                         :position-offset -1
                         :character-found char :base 2.
                         :report 'replace-invalid-digit)
                        0)
                       (t
                        value))))))
      (cond (read-suppress
             (loop for value = (next-bit) while value))
            ((null parameter)
             (loop with bits = (make-array 10 :element-type 'bit
                                              :adjustable t :fill-pointer 0)
                   for value = (next-bit)
                   while value
                   do (vector-push-extend value bits)
                   finally (return (coerce bits 'simple-bit-vector))))
            (t
             (loop with result = (make-array parameter :element-type 'bit)
                   for index from 0
                   for value = (next-bit)
                   while value
                   when (< index parameter)
                   do (setf (sbit result index) value)
                   finally (cond ((and (zerop index) (plusp parameter))
                                  (%recoverable-reader-error
                                   stream 'no-elements-found
                                   :array-type 'bit-vector
                                   :expected-number parameter
                                   :report 'use-empty-vector)
                                  (setf result (make-array 0 :element-type 'bit)
                                        index parameter))
                                 ((> index parameter)
                                  (%recoverable-reader-error
                                   stream 'too-many-elements
                                   :position-offset (- (- index parameter))
                                   :array-type 'bit-vector
                                   :expected-number parameter
                                   :number-found index
                                   :report 'ignore-excess-elements)))
                           (return
                             (if (< index parameter)
                                 (fill result (sbit result (1- index))
                                       :start index)
                                 result))))))))

(defun sharpsign-vertical-bar (stream sub-char parameter)
  (unless (null parameter)
    (numeric-parameter-ignored stream 'sharpsign-vertical-bar parameter))
  (handler-case
      (loop for char = (read-char stream t nil t)
            do (cond ((eql char #\#)
                      (let ((char2 (read-char stream t nil t)))
                        (if (eql char2 sub-char)
                            (sharpsign-vertical-bar stream sub-char nil)
                            (unread-char char2 stream))))
                     ((eql char sub-char)
                      (let ((char2 (read-char stream t nil t)))
                        (if (eql char2 #\#)
                            (progn
                              (setf *skip-reason* :block-comment)
                              (return-from sharpsign-vertical-bar (values)))
                            (unread-char char2 stream))))
                     (t
                      nil)))
    ((and end-of-file (not incomplete-construct)) (condition)
      (%recoverable-reader-error
       stream 'unterminated-block-comment
       :stream-position (stream-position condition)
       :delimiter sub-char :report 'ignore-missing-delimiter))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for sharpsign A.

(labels ((check-sequence (stream object)
           (when (not (typep object 'alexandria:proper-sequence))
             (%recoverable-reader-error
              stream 'read-object-type-error
              :position-offset -1 ; inaccurate
              :expected-type 'sequence :datum object
              :report 'use-empty-array)
             (invoke-restart '%make-empty))
           nil)
         (make-empty-dimensions (rank)
           (make-list rank :initial-element 0))
         (determine-dimensions (stream rank initial-contents)
           (labels ((rec (rank initial-contents)
                      (cond ((zerop rank)
                             '())
                            ((check-sequence stream initial-contents))
                            (t
                             (let ((length (length initial-contents)))
                               (if (zerop length)
                                   (make-empty-dimensions rank)
                                   (list* length
                                          (rec (1- rank)
                                               (elt initial-contents 0)))))))))
             (rec rank initial-contents)))

         (check-dimensions (stream dimensions initial-contents)
           (labels ((rec (first rest axis initial-contents)
                      (cond ((not first))
                            ((check-sequence stream initial-contents))
                            ((not (eql (length initial-contents) (or first 0)))
                             (%recoverable-reader-error
                              stream 'incorrect-initialization-length
                              :array-type 'array :axis axis
                              :expected-length first :datum initial-contents
                              :report 'use-empty-array)
                             (invoke-restart '%make-empty))
                            (t
                             (every (lambda (subseq)
                                      (rec (first rest) (rest rest)
                                           (1+ axis) subseq))
                                    initial-contents)))))
             (rec (first dimensions) (rest dimensions) 0 initial-contents)))
         (read-init (stream)
           (with-forbidden-quasiquotation ('sharpsign-a :keep)
             (handler-case
                 (read stream t nil t)
               ((and end-of-file (not incomplete-construct)) (condition)
                 (%recoverable-reader-error
                  stream 'end-of-input-after-sharpsign-a
                  :stream-position (stream-position condition)
                  :report 'use-empty-array)
                 (invoke-restart '%make-empty))
               (end-of-list (condition)
                 (%recoverable-reader-error
                  stream 'object-must-follow-sharpsign-a
                  :position-offset -1 :report 'use-empty-array)
                 (unread-char (%character condition) stream)
                 (invoke-restart '%make-empty))))))

  (defun sharpsign-a (stream char parameter)
    (declare (ignore char))
    (when *read-suppress*
      (return-from sharpsign-a (read stream t nil t)))

    (let ((rank (cond ((null parameter)
                       (numeric-parameter-not-supplied stream 'sharpsign-a)
                       0)
                      (t
                       parameter))))
      (multiple-value-bind (dimensions init)
          (restart-case
              (let* ((init (read-init stream))
                     (dimensions (determine-dimensions
                                  stream rank init)))
                (check-dimensions stream dimensions init)
                (values dimensions init))
            (%make-empty ()
              (values (make-empty-dimensions rank) '())))
        (make-array dimensions :initial-contents init)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for sharpsign colon.

(defun symbol-from-token (stream token token-escapes package-marker)
  (when *read-suppress*
    (return-from symbol-from-token nil))
  (when package-marker
    (%recoverable-reader-error
     stream 'uninterned-symbol-must-not-contain-package-marker
     :stream-position (if (eq package-marker t)
                          nil
                          package-marker)
     :token token :report 'treat-as-escaped))
  (convert-according-to-readtable-case token token-escapes)
  (interpret-symbol *client* stream nil (copy-seq token) nil))

(defun sharpsign-colon (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (numeric-parameter-ignored stream 'sharpsign-colon parameter))
  (with-token-info (push-char (start-escape end-escape) finalize)
    (let ((package-marker nil))
      (labels ((handle-char (char escapep)
                 (when (and (not escapep)
                            (char= char #\:)
                            (not package-marker))
                   (setf package-marker (or (ignore-errors (file-position stream))
                                            t)))
                 (push-char char))
               (unterminated-single-escape (escape-char)
                 (%recoverable-reader-error
                  stream 'unterminated-single-escape-in-symbol
                  :escape-char escape-char :report 'use-partial-symbol))
               (unterminated-multiple-escape (delimiter)
                 (%recoverable-reader-error
                  stream 'unterminated-multiple-escape-in-symbol
                  :delimiter delimiter :report 'use-partial-symbol))
               (return-symbol ()
                 (return-from sharpsign-colon
                   (multiple-value-bind (token escape-ranges) (finalize)
                     (symbol-from-token stream token escape-ranges package-marker)))))
        (token-state-machine
         stream *readtable* handle-char start-escape end-escape
         unterminated-single-escape unterminated-multiple-escape
         return-symbol)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for sharpsign C.

(defun %sharpsign-c (stream char parameter allow-non-list)
  (declare (ignore char))
  (unless (null parameter)
    (numeric-parameter-ignored stream 'sharpsign-c parameter))
  (when *read-suppress*
    (read stream t nil t)
    (return-from %sharpsign-c nil))
  ;; When we get here, we have to read a list of the form
  ;; (REAL-PART-REAL-NUMBER-LITERAL IMAGINARY-PART-REAL-NUMBER) that
  ;; is, a list of exactly two elements of type REAL.
  ;;
  ;; We call %READ-LIST-ELEMENTS which calls the local function PART
  ;; for each list element as well as the events such as the end of
  ;; the list or the end of input. The variable PART keeps track of
  ;; the currently expected part which can be :REAL, :IMAGINARY, :END
  ;; or :PAST-END (the latter only comes into play when reading more
  ;; than two list elements due to error recovery).
  (let ((listp nil)
        (part :real)
        (real 1) (imaginary 1))
    (labels ((check-value (value)
               (typecase value
                 ((eql #1=#.(gensym "END-OF-LIST"))
                  (%recoverable-reader-error
                   stream 'complex-part-expected
                   :position-offset -1
                   :which part :report 'use-partial-complex)
                  1)
                 ((eql #2=#.(gensym "END-OF-INPUT"))
                  (%recoverable-reader-error
                   stream 'end-of-input-before-complex-part
                   :which part :report 'use-partial-complex)
                  1)
                 (real
                  value)
                 (t
                  (%recoverable-reader-error stream 'read-object-type-error
                                             :datum value :expected-type 'real
                                             :report 'use-replacement-part)
                  1)))
             (part (kind value)
               (declare (ignore kind))
               (case part
                 (:real
                  (setf real (check-value value)
                        part :imaginary)
                  t)
                 (:imaginary
                  (setf imaginary (check-value value)
                        part :end)
                  t)
                 ((:end :past-end)
                  (case value
                    (#1# t)
                    (#2# nil)
                    (t
                     (when (eq part :end)
                       (%recoverable-reader-error
                        stream 'too-many-complex-parts
                        :position-offset -1
                        :report 'ignore-excess-parts)
                       (setf part :past-end))
                     t)))))
             (read-parts (stream char)
               ;; If this is called, the input started with "#C(" (or,
               ;; generally, "#C" followed by any input resulting in a
               ;; LEFT-PARENTHESIS call).  We record that fact (for
               ;; error reporting) by setting LISTP.  We reset
               ;; *LIST-READER* so lists appearing in the complex
               ;; parts are processed normally instead of with
               ;; READ-PARTS.
               (setf listp t)
               (let ((*list-reader* nil))
                 (%read-list-elements stream #'part '#1# '#2# char nil))
               nil)) ; unused, but must not return (values)
      (handler-case
          ;; Depending on ALLOW-NON-LIST, we call either READ or
          ;; %READ-MAYBE-NOTHING.  Calling %READ-MAYBE-NOTHING will:
          ;; - not skip whitespace or comments (the spec is not clear
          ;;   about whether #C<skippable things>(...) is valid syntax)
          ;; - invoke reader macros, in particular LEFT-PARENTHESIS to
          ;;   initiate reading a list
          ;; - not behave like a full READ call in terms of e.g. parse
          ;;   result construction so (1 2) will not appear as a list
          ;;   result with two atom result children.
          ;; We bind *LIST-READER* to use READ-PARTS for reading lists.
          (with-forbidden-quasiquotation ('sharpsign-c)
            (let ((*list-reader* #'read-parts))
              (values (if allow-non-list
                          (read stream t nil t)
                          (%read-maybe-nothing *client* stream t nil)))))
        ((and end-of-file (not incomplete-construct)) (condition)
          (%recoverable-reader-error
           stream 'end-of-input-after-sharpsign-c
           :stream-position (stream-position condition)
           :report 'use-replacement-part))
        (end-of-list (condition) ; (... #C)
          (%recoverable-reader-error
           stream 'complex-parts-must-follow-sharpsign-c
           :position-offset -1 :report 'use-partial-complex)
          (unread-char (%character condition) stream))
        (:no-error (object)
          ;; If we got here, we managed to read an object.
          (cond (listp)
                ((or (not allow-non-list) (not (typep object 'cons)))
                 (%recoverable-reader-error
                  stream 'non-list-following-sharpsign-c
                  :position-offset -1 ; inaccurate
                  :report 'use-replacement-part))
                ((typep object #3='(cons real (cons real null)))
                 (setf real (first object)
                       imaginary (second object)))
                (t
                 (%recoverable-reader-error
                  stream 'read-object-type-error
                  :position-offset -1 ; inaccurate
                  :datum object :expected-type #3#
                  :report 'use-replacement-part)))))
      (complex real imaginary))))

(defun sharpsign-c (stream char parameter)
  (%sharpsign-c stream char parameter t))

(defun strict-sharpsign-c (stream char parameter)
  (%sharpsign-c stream char parameter nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for sharpsign S.
;;;
;;; In contrast to 2.4.8.11 Sharpsign C which says "#C reads a
;;; following object …" thus allowing whitespace preceding the object,
;;; 2.4.8.13 Sharpsign S spells out the syntax as "#s(…)". However,
;;; since a strict reading of this would also preclude "#S(…)" we
;;; assume that the intention is to allow whitespace after "#s".

(defun sharpsign-s (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (numeric-parameter-ignored stream 'sharpsign-s parameter))
  (when *read-suppress*
    (read stream t nil t)
    (return-from sharpsign-s nil))
  ;; When we get here, we have to read a list of the form
  ;; (STRUCTURE-TYPE-NAME SLOT-NAME SLOT-VALUE …). We call
  ;; %READ-LIST-ELEMENTS which calls the local function ELEMENT for
  ;; each list element as well as events such as the end of the list
  ;; or the end of input. The variable ELEMENT keeps track of the
  ;; currently expected ELEMENT which can be :TYPE, :SLOT-NAME, or
  ;; :SLOT-VALUE.
  (let ((old-quasiquote-forbidden *quasiquote-forbidden*)
        (listp nil)
        (element :type)
        (type)
        (slot-name)
        (initargs '()))
    (labels ((element (kind value)
               (declare (ignore kind))
               (case element
                 (:type
                  (typecase value
                    ((eql #1=#.(gensym "END-OF-LIST"))
                     (%recoverable-reader-error
                      stream 'no-structure-type-name-found
                      :position-offset -1 :report 'inject-nil))
                    ((eql #2=#.(gensym "END-OF-INPUT"))
                     (%recoverable-reader-error
                      stream 'end-of-input-before-structure-type-name
                      :report 'inject-nil))
                    (symbol
                     (setf type value))
                    (t
                     (%recoverable-reader-error
                      stream 'structure-type-name-is-not-a-symbol
                      :position-offset -1 :datum value :report 'inject-nil)))
                  (setf *quasiquote-forbidden* 'sharpsign-s-slot-name
                        *unquote-forbidden* 'sharpsign-s-slot-name
                        element :name))
                 (:name
                  (typecase value
                    ((eql #1#))
                    ((eql #2#)
                     (%recoverable-reader-error
                      stream 'end-of-input-before-slot-name
                      :report 'use-partial-initargs))
                    (alexandria:string-designator
                     (setf slot-name value))
                    (t
                     (%recoverable-reader-error
                      stream 'slot-name-is-not-a-string-designator
                      :position-offset -1 :datum value :report 'skip-slot)
                     (setf slot-name value)))
                  (setf *quasiquote-forbidden* old-quasiquote-forbidden
                        *unquote-forbidden* 'sharpsign-s-slot-value
                        element :object))
                 (:object
                  (typecase value
                    ((eql #1#)
                     (%recoverable-reader-error
                      stream 'no-slot-value-found
                      :position-offset -1
                      :slot-name slot-name :report 'skip-slot))
                    ((eql #2#)
                     (%recoverable-reader-error
                      stream 'end-of-input-before-slot-value
                      :slot-name slot-name :report 'skip-slot))
                    (t
                     (push slot-name initargs)
                     (push value initargs)))
                  (setf *quasiquote-forbidden* 'sharpsign-s-slot-name
                        *unquote-forbidden* 'sharpsign-s-slot-name
                        element :name))))
             (read-constructor (stream char)
               ;; If this is called, the input started with "#S(" (or,
               ;; generally, "#S" followed by any input resulting in a
               ;; LEFT-PARENTHESIS call).  We record that fact (for
               ;; error reporting) by setting LISTP.  We reset
               ;; *LIST-READER* so lists appearing in the constructor
               ;; parts are processed normally instead of with
               ;; READ-CONSTRUCTOR.
               (setf listp t)
               (setf *quasiquote-forbidden* 'sharpsign-s-type
                     *unquote-forbidden* 'sharpsign-s-type)
               (let ((*list-reader* nil))
                 (%read-list-elements stream #'element '#1# '#2# char nil))))
      (handler-case
          ;; Instead of READ we call %READ-MAYBE-NOTHING which will
          ;; - not skip whitespace or comments (the spec is not clear
          ;;   about whether #S<skippable things>(...) is valid syntax)
          ;; - invoke reader macros, in particular LEFT-PARENTHESIS to
          ;;   initiate reading a list
          ;; - not behave like a full READ call in terms of e.g. parse
          ;;   result construction so (foo :bar 2) will not appear as
          ;;   a list result with three atom result children.
          ;; We bind *LIST-READER* to use READ-CONSTRUCTOR for reading lists.
          (with-forbidden-quasiquotation ('sharpsign-s)
            (let ((*list-reader* #'read-constructor))
              (%read-maybe-nothing *client* stream t nil)))
        ((and end-of-file (not incomplete-construct)) (condition)
          (%recoverable-reader-error
           stream 'end-of-input-after-sharpsign-s
           :stream-position (stream-position condition)
           :report 'inject-nil))
        (end-of-list (condition)
          (%recoverable-reader-error
           stream 'structure-constructor-must-follow-sharpsign-s
           :position-offset -1 :report 'inject-nil)
          (unread-char (%character condition) stream))
        (:no-error (&rest values)
          (declare (ignore values))
          (unless listp
            (%recoverable-reader-error
             stream 'non-list-following-sharpsign-s
             :position-offset -1 :report 'inject-nil))))
      (if (not (null type))
          (make-structure-instance *client* type (nreverse initargs))
          nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for sharpsign P.

(defun sharpsign-p (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (numeric-parameter-ignored stream 'sharpsign-p parameter))
  (when *read-suppress*
    (read stream t nil t)
    (return-from sharpsign-p nil))
  (let ((expression
          (with-forbidden-quasiquotation ('sharpsign-p)
            (handler-case
                (read stream t nil t)
              ((and end-of-file (not incomplete-construct)) (condition)
                (%recoverable-reader-error
                 stream 'end-of-input-after-sharpsign-p
                 :stream-position (stream-position condition)
                 :report 'replace-namestring)
                ".")
              (end-of-list (condition)
                (%recoverable-reader-error
                 stream 'namestring-must-follow-sharpsign-p
                 :position-offset -1 :report 'replace-namestring)
                (unread-char (%character condition) stream)
                ".")))))
    (cond ((stringp expression)
           (values (parse-namestring expression)))
          (t
           (%recoverable-reader-error
            stream 'non-string-following-sharpsign-p
            :position-offset -1
            :expected-type 'string :datum expression
            :report 'replace-namestring)
           #P"."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macros for sharpsign + and sharpsign -.

;;; This variable is bound to the current input stream in
;;; SHARPSIGN-PLUS-MINUS to make the stream available for error
;;; reporting in CHECK-STANDARD-FEATURE-EXPRESSION.
(defvar *input-stream*)

(deftype feature-expression-operator ()
  '(member :not :or :and))

(defun check-standard-feature-expression (feature-expression)
  (flet ((lose (stream-condition no-stream-condition &rest arguments)
           (alexandria:if-let ((stream *input-stream*))
             (apply #'%reader-error stream stream-condition
                    :position-offset -1 arguments)
             (apply #'error no-stream-condition arguments))))
    (unless (or (symbolp feature-expression)
                (alexandria:proper-list-p feature-expression))
      (lose 'feature-expression-type-error/reader
            'feature-expression-type-error
            :datum feature-expression
            :expected-type '(or symbol cons)))
    (when (consp feature-expression)
      (destructuring-bind (operator &rest operands) feature-expression
        (unless (typep operator 'feature-expression-operator)
          (lose 'feature-expression-type-error/reader
                'feature-expression-type-error
                :datum operator
                :expected-type 'feature-expression-operator))
        (when (and (eq operator :not)
                   (not (alexandria:length= 1 operands)))
          (lose 'single-feature-expected/reader 'single-feature-expected
                :features (cdr feature-expression)))))))

(defun evaluate-standard-feature-expression
    (feature-expression
     &key
     (check 'check-standard-feature-expression)
     (recurse 'evaluate-standard-feature-expression))
  (funcall check feature-expression)
  (typecase feature-expression
    (symbol
     (member feature-expression *features* :test #'eq))
    ((cons (eql :not))
     (not (funcall recurse (second feature-expression))))
    ((cons (eql :or))
     (some recurse (rest feature-expression)))
    ((cons (eql :and))
     (every recurse (rest feature-expression)))))

(defun sharpsign-plus-minus (stream char parameter invertp)
  (declare (ignore char))
  (unless (null parameter)
    (numeric-parameter-ignored stream 'sharpsign-plus-minus parameter))
  (let ((context (if invertp
                     :sharpsign-minus
                     :sharpsign-plus)))
    (flet ((read-expression (end-of-file-condition end-of-list-condition
                             fallback-value)
             (handler-case
                 (read stream t nil t)
               ((and end-of-file (not incomplete-construct)) (condition)
                 (%recoverable-reader-error
                  stream end-of-file-condition
                  :stream-position (stream-position condition)
                  :context context :report 'inject-nil)
                 fallback-value)
               (end-of-list (condition)
                 (%recoverable-reader-error
                  stream end-of-list-condition
                  :position-offset -1 :context context :report 'inject-nil)
                 (unread-char (%character condition) stream)
                 fallback-value))))
      (let* ((client *client*)
             (feature-expression
               (call-with-current-package
                client (lambda ()
                         (let ((*read-suppress* nil))
                           (with-forbidden-quasiquotation (context)
                             (read-expression
                              'end-of-input-after-sharpsign-plus-minus
                              'feature-expression-must-follow-sharpsign-plus-minus
                              '(:and)))))
                '#:keyword)))
        (if (alexandria:xor
             (with-simple-restart
                 (recover (recovery-description 'treat-as-false))
               (let ((*input-stream* stream))
                 (evaluate-feature-expression client feature-expression)))
             invertp)
            (read-expression 'end-of-input-after-feature-expression
                             'object-must-follow-feature-expression
                             nil)
            (progn
              (setf *skip-reason* (cons context feature-expression))
              (let ((*read-suppress* t))
                (read-expression 'end-of-input-after-feature-expression
                                 'object-must-follow-feature-expression
                                 nil))
              (values)))))))

(defun sharpsign-plus (stream char parameter)
  (sharpsign-plus-minus stream char parameter nil))

(defun sharpsign-minus (stream char parameter)
  (sharpsign-plus-minus stream char parameter t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macros for sharpsign equals and sharpsign sharpsign.
;;;
;;; When the SHARPSIGN-EQUALS reader macro encounters #N=EXPRESSION,
;;; it associates a marker object with N in the hash-table bound to
;;; *LABELS*. The marker object is of the form
;;;
;;;   ((FINALP) . FINAL-OBJECT)
;;;
;;; where FINALP and FINAL-OBJECT are initially NIL. The cons cell
;;; (FINALP) is called the temporary object of the marker object.
;;;
;;; If #N# is encountered, the marker for N is looked up in *LABELS*
;;; and FINALP is examined. If FINALP is true, FINAL-OBJECT can be
;;; returned as the result of reading #N#. However, if FINALP is false
;;; (this can happen while READing EXPRESSION if #N=EXPRESSION is
;;; circular), the temporary object is returned as the result of
;;; reading #N# and a deferred fixup step will be necessary. This
;;; fixup happens in READ-AUX.
;;;
;;; After reading EXPRESSION, the resulting object is stored in the
;;; cdr as FINAL-OBJECT and FINALP within the temporary object is set
;;; to true. Subsequent #N# encounters can directly return
;;; FINAL-OBJECT as described above.

(declaim (inline make-fixup-marker
                 fixup-marker-temporary
                 fixup-marker-final-p (setf fixup-marker-final-p)
                 fixup-marker-final (setf fixup-marker-final)))

(defun make-fixup-marker ()
  (let ((temporary (list nil)))
    (cons temporary nil)))

(defun fixup-marker-temporary (marker)
  (car marker))

(defun fixup-marker-final-p (marker)
  (car (fixup-marker-temporary marker)))

(defun (setf fixup-marker-final-p) (new-value marker)
  (setf (car (fixup-marker-temporary marker)) new-value))

(defun fixup-marker-final (marker)
  (cdr marker))

(defun (setf fixup-marker-final) (new-value marker)
  (setf (cdr marker) new-value))

(defun sharpsign-equals (stream char parameter)
  (declare (ignore char))
  (flet ((read-object ()
           (handler-case
               (read stream t nil t)
             ((and end-of-file (not incomplete-construct)) (condition)
               (%recoverable-reader-error
                stream 'end-of-input-after-sharpsign-equals
                :stream-position (stream-position condition)
                :report 'inject-nil)
               nil)
             (end-of-list (condition)
               (%recoverable-reader-error
                stream 'object-must-follow-sharpsign-equals
                :position-offset -1 :report 'inject-nil)
               (unread-char (%character condition) stream)
               nil))))
    (when *read-suppress*
      (return-from sharpsign-equals (read-object)))
    (when (null parameter)
      (numeric-parameter-not-supplied stream 'sharpsign-equals)
      (return-from sharpsign-equals (read-object)))
    (let ((labels *labels*))
      (when (nth-value 1 (gethash parameter labels))
        (%recoverable-reader-error
         stream 'sharpsign-equals-label-defined-more-than-once
         :position-offset (- (+ 1 (parameter-length parameter) 1))
         :label parameter :report 'ignore-label)
        (return-from sharpsign-equals (read-object)))
      (let ((marker (make-fixup-marker)))
        (setf (gethash parameter labels) marker)
        (let ((result (read-object)))
          (when (eq result (fixup-marker-temporary marker))
            (%recoverable-reader-error
             stream 'sharpsign-equals-only-refers-to-self
             :position-offset -1 :label parameter :report 'inject-nil)
            (remhash parameter labels)
            (return-from sharpsign-equals nil))
          (setf (fixup-marker-final marker) result
                (fixup-marker-final-p marker) t)
          result)))))

(defun sharpsign-sharpsign (stream char parameter)
  (declare (ignore char))
  (when *read-suppress*
    (return-from sharpsign-sharpsign nil))
  (when (null parameter)
    (numeric-parameter-not-supplied stream 'sharpsign-equals)
    (return-from sharpsign-sharpsign nil))
  (multiple-value-bind (marker definedp) (gethash parameter *labels*)
    (cond ((not definedp)
           (%recoverable-reader-error
            stream 'sharpsign-sharpsign-undefined-label
            :position-offset (- (+ 1 (parameter-length parameter) 1))
            :label parameter :report 'inject-nil)
           nil)
          ;; If the final object has already been supplied, use it.
          ((fixup-marker-final-p marker)
           (fixup-marker-final marker))
          ;; Else, we must use the temporary object and it will be
          ;; fixed up later.
          (t
           (fixup-marker-temporary marker)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macros for sharpsign < and sharpsign )

(defun sharpsign-invalid (stream char parameter)
  (declare (ignore parameter))
  (%recoverable-reader-error
   stream 'sharpsign-invalid
   :position-offset -1 :character-found char :report 'inject-nil)
  nil)
