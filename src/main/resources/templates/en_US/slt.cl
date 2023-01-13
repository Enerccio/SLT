; TODO: swank:defslimefunction

(defpackage :slt-core
    (:use :cl :swank)
    (:export analyze-symbol analyze-symbols read-fix-packages))

; swank/slime overrides

(in-package sb-debug)

(export 'frame-code-location)
(export 'frame-call)
(export 'ensure-printable-object)
(export 'code-location-source-form)

(in-package swank/backend)

(in-package :swank)

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
                (print-frame-call-place frame)
                (frame-source-location i)
                (let ((pkg (frame-package i)))
                    (cond
                        (pkg (package-name pkg))
                        (T NIL))))))

(defslimefun compile-string-region-slt (string buffer offset filename)
  "Compile STRING (exerpted from BUFFER at POSITION).
Record compiler notes signalled as `compiler-condition's."
    (with-buffer-syntax ()
      (collect-notes
       (lambda ()
         (let ((*compile-print* t) (*compile-verbose* nil))
           (swank-compile-string string
                                 :buffer buffer
                                 :position offset
                                 :filename filename))))))

(export 'slt-eval)
(export 'compile-string-region-slt)

(in-package swank/sbcl)

(in-package :slt-core)

(defun specialp (test-sym)
    (eq (sb-cltl2:variable-information test-sym) :special))

(defun analyze-symbol (test-sym)
    (cons test-sym (let ((*standard-output* (make-string-output-stream)))
        (cond
            ((special-operator-p test-sym) (list :special-form NIL))
            ((macro-function test-sym) (progn
                                         (describe test-sym)
                                         (list :macro (get-output-stream-string *standard-output*))))
            ((fboundp test-sym) (progn
                                  (describe test-sym)
                                  (list :function (get-output-stream-string *standard-output*))))
            ((specialp test-sym) (progn
                                   (describe test-sym)
                                   (list :special (get-output-stream-string *standard-output*))))
            ((keywordp test-sym) (progn
                                   (describe test-sym)
                                   (list :keyword (get-output-stream-string *standard-output*))))
            ((constantp test-sym) (progn
                                    (describe test-sym)
                                    (list :constant (get-output-stream-string *standard-output*))))
            (T (list NIL NIL))))))

(defun analyze-symbols (symbols)
   (map 'list #'analyze-symbol symbols))

(defun read-fix-packages (str)
    (remove-if-not #'identity
        (handler-bind (
               (sb-ext:package-locked-error
                (lambda (c)
                    (declare (ignore c))
                    (let ((restart (find-restart :ignore-all)))
                        (when restart
                            (invoke-restart restart)))))
               (T
                (lambda (c)
                    (declare (ignore c))
                    (let ((restart (find-restart 'use-value)))
                        (when restart
                            (invoke-restart restart NIL))))))
           (eclector.reader:read-from-string str))))

(in-package :cl-user)