(cl:in-package #:eclector.parse-result)

;;; A list of sub-lists the form
;;;
;;;   (CHILDREN-OF-CURRENT-NODE CHILDREN-OF-PARENT ...)
;;;
(defvar *stack*)

(defvar *start*)

(defmethod eclector.reader:note-skipped-input
    ((client parse-result-client) input-stream reason)
  (let* ((start *start*)
         (end (eclector.base:source-position client input-stream))
         (range (eclector.base:make-source-range client start end))
         (parse-result (make-skipped-input-result
                        client input-stream reason range)))
    (when parse-result
      (push parse-result (second *stack*)))))

;;; Establishing context

(defmethod eclector.reader:call-as-top-level-read :around
    ((client parse-result-client) thunk input-stream
     eof-error-p eof-value preserve-whitespace-p)
  (declare (ignore thunk input-stream preserve-whitespace-p))
  ;; We bind *CLIENT* here (instead of in, say, READ-AUX) to allow
  ;; (call-as-top-level-read
  ;;  client (lambda () ... (read-maybe-nothing client ...) ...) ...)
  ;; to work without the user code explicitly binding the variable.
  (let* ((eclector.base:*client* client)
         (stack (list '()))
         (*stack* stack)
         (values (multiple-value-list (call-next-method)))
         (value (first values))
         (results (if (and (null eof-error-p) (eq value eof-value))
                      (first stack)
                      (rest (first stack))))
         (orphan-results (reverse results)))
    (if (null orphan-results)
        (values-list values)
        (multiple-value-call #'values (values-list values) orphan-results))))

(defmethod eclector.reader:read-common
    ((client parse-result-client) input-stream eof-error-p eof-value)
  (loop for (value what parse-result)
           = (multiple-value-list
              (eclector.reader:read-maybe-nothing
               client input-stream eof-error-p eof-value))
        do (ecase what
             (:eof
              (return (values value nil)))
             ((:suppress :object)
              (return (values value parse-result)))
             ((:whitespace :skip)))))

(defmethod eclector.reader:read-maybe-nothing
    ((client parse-result-client) input-stream eof-error-p eof-value)
  (declare (ignore eof-error-p eof-value))
  (let* ((stack (list* '() *stack*))
         (start (eclector.base:source-position client input-stream)))
    (multiple-value-bind (value what)
        (let ((*stack* stack)
              ;; *START* is used in NOTE-SKIPPED-INPUT to describe
              ;; skipped input (comments, reader macros,
              ;; *READ-SUPPRESS*).
              (*start* start))
          (call-next-method))
      (case what
        (:object
         (let* ((children (reverse (first stack)))
                (end (eclector.base:source-position client input-stream))
                (source (eclector.base:make-source-range client start end))
                (parse-result (make-expression-result
                               client value children source)))
           (push parse-result (second stack))
           (values value what parse-result)))
        ((:eof :whitespace)
         (values value what))
        (t
         (values value what (first (second stack))))))))

;;; Entry points

(defun read-aux (client input-stream eof-error-p eof-value preserve-whitespace-p)
  (multiple-value-bind (result parse-result orphan-results)
      (flet ((read-common ()
               (eclector.reader:read-common
                client input-stream eof-error-p eof-value)))
        (declare (dynamic-extent #'read-common))
        (eclector.reader:call-as-top-level-read
         client #'read-common input-stream
         eof-error-p eof-value preserve-whitespace-p))
    ;; If we come here, that means that either the call to READ-AUX
    ;; succeeded without encountering end-of-file, or that EOF-ERROR-P
    ;; is false, end-of-file was encountered, and EOF-VALUE was
    ;; returned.  In the latter case, we want READ to return
    ;; EOF-VALUE.
    (values (if (and (null eof-error-p) (eq eof-value result))
                eof-value
                parse-result)
            orphan-results)))

(defun read (client &optional (input-stream *standard-input*)
                              (eof-error-p t)
                              (eof-value nil))
  (read-aux client input-stream eof-error-p eof-value nil))

(defun read-preserving-whitespace (client &optional
                                          (input-stream *standard-input*)
                                          (eof-error-p t)
                                          (eof-value nil))
  (read-aux client input-stream eof-error-p eof-value t))

(locally (declare #+sbcl (sb-ext:muffle-conditions eclector.base:&optional-and-&key-style-warning))
  (defun read-from-string (client string &optional (eof-error-p t)
                                                   (eof-value nil)
                                         &key (start 0)
                                              (end nil)
                                              (preserve-whitespace nil))
    (let ((index))
      (multiple-value-bind (result orphan-results)
          (with-input-from-string (stream string :start start :end end
                                                 :index index)
            (read-aux client stream eof-error-p eof-value preserve-whitespace))
        (values result index orphan-results)))))
