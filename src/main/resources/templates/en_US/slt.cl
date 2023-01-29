; TODO: swank:defslimefunction

(defpackage :slt-core
    (:use :cl :swank)
    (:export analyze-symbol analyze-symbols read-fix-packages list-package-names
             initialize-or-get-debug-context debug-context debug-frame-variable register-variable
             install-breakpoint uninstall-breakpoint ; TODO install-method-breakpoint uninstall-method-breakpoint
             install-inner-breakpoint uninstall-inner-breakpoint
             ))

; swank/slime overrides

(in-package sb-debug)

(export 'frame-code-location)
(export 'frame-call)
(export 'ensure-printable-object)
(export 'code-location-source-form)

(in-package swank/backend)

(in-package :swank)

(export 'find-source-location)

(defslimefun slt-eval (string)
    (let ((*echo-area-prefix* ""))
        (with-buffer-syntax ()
            (with-retry-restart (:msg "Retry SLIME interactive evaluation request.")
                (let ((values (multiple-value-list (eval (from-string string)))))
                    (finish-output)
                    (format-values-for-echo-area values))))))

(defslimefun invoke-nth-restart-slt (sldb-level n args rest)
    (when (= sldb-level *sldb-level*)
        (let ((restart (nth-restart n))
              (parsed-args (from-string args))
              (parsed-rest (from-string rest)))
            (when restart
                (if (or parsed-args parsed-rest)
                    (apply #'invoke-restart (concatenate 'list (list restart) parsed-args parsed-rest))
                    (invoke-restart restart))))))

(defun format-restarts-for-emacs ()
  "Return a list of restarts for *swank-debugger-condition* in a
format suitable for Emacs."
  (let ((*print-right-margin* most-positive-fixnum))
    (loop for restart in *sldb-restarts* collect
          (list (format nil "~:[~;*~]~a"
                        (eq restart *sldb-quit-restart*)
                        (restart-name restart))
                (with-output-to-string (stream)
                  (without-printing-errors (:object restart
                                            :stream stream
                                            :msg "<<error printing restart>>")
                    (princ restart stream)))
                (swank-backend:arglist (slot-value restart 'function))))))

(defun print-frame-call-place (frame)
    (multiple-value-bind (name args info)
            (SB-DEBUG:frame-call frame)
        (declare (ignore args info))
        (let ((name (SB-DEBUG:ensure-printable-object name)))
            name)))

(defslimefun backtrace (start end)
  (loop for frame in (compute-backtrace start end)
        for i from start collect
        (list i (frame-to-string frame)
                (format NIL "~S" (print-frame-call-place frame))
                (frame-source-location i)
                (let ((pkg (frame-package i)))
                    (cond
                        (pkg (package-name pkg))
                        (T NIL))))))

(defslimefun compile-string-region-slt (string buffer offset filename package)
    (with-buffer-syntax ()
      (collect-notes
       (lambda ()
         (let ((*compile-print* t) (*compile-verbose* nil) (*package* (find-package package)))
           (swank-compile-string string
                                 :buffer buffer
                                 :position offset
                                 :filename filename))))))

(in-package swank)

(defun get-all-symbols-with-prefix (prefix)
  (let ((all (get-all-symbols)))
    (loop for data-pair in all
              when (prefix-match-p prefix (first data-pair))
                            collect data-pair)))

(defun get-all-symbols ()
  (let ((data '()))
    (do-symbols (s)
      (push (list (unparse-symbol s) s (symbol-package s)) data))
    (remove-duplicates data :key (lambda (e) (second e)))))

(defun find-reference-class-filter (symbol package)
    (find-class symbol NIL))

(defun get-reference-prefix (prefix type)
  (let ((apply-func (cond
                     ((eq type :class) #'find-reference-class-filter)
                     (T (lambda (symbol package) T)))))
    (let ((filtered-symbols (get-all-symbols-with-prefix prefix))
          (*source-snippet-size* 0))
      (loop for data-pair in filtered-symbols
                  when (funcall apply-func (second data-pair) (third data-pair))
                                   collect
        (let* ((symbol (second data-pair))
               (str (first data-pair))
               (package (third data-pair))
               (strpackage (format NIL "~S" package)))
          (cond
            ((not symbol) (list str strpackage NIL NIL))
            ((macro-function symbol) (list str strpackage :macro (swank:find-source-location (symbol-function symbol))))
            ((and (fboundp symbol)
                 (typep (symbol-function symbol) 'generic-function))
              (list str strpackage :method (swank:find-source-location (symbol-function symbol))))
            ((fboundp symbol) (list str strpackage :function (swank:find-source-location (symbol-function symbol))))
            ((find-class symbol NIL) (list str strpackage :class (swank:find-source-location (find-class symbol))))
            (T (list str strpackage NIL NIL))))))))

(defslimefun find-reference-prefix (prefix type)
  (let ((references (get-reference-prefix prefix type)))
    (when references
      (sort references #'string< :key (lambda (x) (first x))))))

(export 'slt-eval)
(export 'compile-string-region-slt)

(in-package swank/sbcl)

(in-package swank/source-file-cache)

(in-package :slt-core)

(defun specialp (test-sym)
    (eq (sb-cltl2:variable-information test-sym) :special))

(defun analyze-symbol (test-sym)
    (cons test-sym
        (let ((*standard-output* (make-string-output-stream))
              (swank::*source-snippet-size* 0))
            (cond
                ((not test-sym) (list NIL NIL NIL))
                ((and (fboundp test-sym)
                      (typep (symbol-function test-sym) 'generic-function))
                                        (progn
                                           (describe test-sym)
                                           (list :method (get-output-stream-string *standard-output*)
                                                 (swank:find-source-location (symbol-function test-sym)))))
                ((special-operator-p test-sym) (list :special-form NIL NIL))
                ((macro-function test-sym) (progn
                                             (describe test-sym)
                                             (list
                                                :macro
                                                (get-output-stream-string *standard-output*)
                                                (swank:find-source-location (symbol-function test-sym)))))
                ((fboundp test-sym) (progn
                                      (describe test-sym)
                                      (list
                                        :function
                                        (get-output-stream-string *standard-output*)
                                            (swank:find-source-location (symbol-function test-sym)))))
                ((specialp test-sym) (progn
                                       (describe test-sym)
                                       (list :special (get-output-stream-string *standard-output*) NIL)))
                ((keywordp test-sym) (progn
                                       (describe test-sym)
                                       (list :keyword (get-output-stream-string *standard-output*) NIL)))
                ((constantp test-sym) (progn
                                        (describe test-sym)
                                        (list :constant (get-output-stream-string *standard-output*) NIL)))
                ((find-class test-sym NIL) (progn
                                           (describe test-sym)
                                           (list :class (get-output-stream-string *standard-output*)
                                                 (swank:find-source-location (find-class test-sym)))))
                (T (list NIL NIL NIL))))))

(defun analyze-symbols (symbols)
   (map 'list #'analyze-symbol symbols))

(defun reader-recover (c)
    (declare (ignorable c))
       (let ((restart (find-restart 'eclector.reader:recover)))
           (when restart
               (invoke-restart restart))))

(defun reader-recover (c)
    (declare (ignorable c))
       (let ((restart (find-restart 'eclector.reader:recover)))
           (when restart
               (invoke-restart restart))))

(defun reader-user-anyway (c)
    (declare (ignorable c))
    (let ((restart (find-restart 'eclector.reader::use-anyway)))
        (when restart
            (invoke-restart restart))))

(defun read-fix-packages (str)
    (handler-bind
           ((ECLECTOR.READER:SYMBOL-NAME-MUST-NOT-END-WITH-PACKAGE-MARKER
                #'reader-recover)
            (ECLECTOR.READER:SYMBOL-IS-NOT-EXTERNAL
                #'reader-user-anyway)
            (ECLECTOR.READER:SYMBOL-DOES-NOT-EXIST
                #'reader-recover)
            (ECLECTOR.READER:PACKAGE-DOES-NOT-EXIST
                #'reader-recover)
            (error (lambda (c)
                   (format *error-output* "general error: ~S ~%" c))))
       (eclector.reader:read-from-string str)))

(defun list-package-names ()
    (let ((packages (list-all-packages)))
        (loop for package in packages collect
            (package-name package))))

(defun install-breakpoint (symbol)
  (ignore-errors (eval `(trace ,symbol :break T)) T))

(defun uninstall-breakpoint (symbol)
  (ignore-errors (eval `(untrace ,symbol)) T))

; for methods (method fname qualifiers* (specializers*)) denoting a method.

(defun install-inner-breakpoint (symbol parent-symbol type)
  (ignore-errors (eval `(trace (,type ,symbol :in ,parent-symbol) :break T)) T))

(defun uninstall-inner-breakpoint (symbol parent-symbol type)
  (ignore-errors (eval `(untrace (,type ,symbol :in ,parent-symbol))) T))

(in-package :cl-user)