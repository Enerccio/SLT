; TODO: swank:defslimefunction

(defpackage :slt-core
    (:use :cl :swank)
    (:export analyze-symbol compile-region))

; swank/slime overrides

(in-package swank/backend)

(definterface swank-slt-compile-region (string filename lineno charno)
    "SLT Region compilation")

(in-package :swank)

(defslimefun slt-eval (string)
    (let ((*echo-area-prefix* ""))
        (with-buffer-syntax ()
            (with-retry-restart (:msg "Retry SLIME interactive evaluation request.")
                (let ((values (multiple-value-list (eval (from-string string)))))
                    (finish-output)
                    (format-values-for-echo-area values))))))

(defslimefun slt-compile-region (string filename lineno charno)
    (with-buffer-syntax ()
        (collect-notes
            (lambda ()
                (let ((*compile-print* t) (*compile-verbose* nil))
                    (swank-slt-compile-region string filename lineno charno))))))

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

(in-package swank/sbcl)

(defimplementation swank-slt-compile-region (string filename lineno charno)
    (let ((*buffer-tmpfile* (temp-file-name)))
        (labels ((load-it (filename)
                    (cond (*trap-load-time-warnings*
                                (with-compilation-hooks () (load filename)))
                          (t (load filename))))
                  (cf ()
                    (with-compiler-policy NIL
                        (with-compilation-unit
                            (:source-plist (list :slt-source filename
                                                 :slt-lineno lineno
                                                 :slt-charno charno)
                             :source-namestring filename
                             :allow-other-keys t)
                            (compile-file *buffer-tmpfile* :external-format :utf-8)))))
            (with-open-file (s *buffer-tmpfile* :direction :output :if-exists :error
                                 :external-format :utf-8)
                (write-string string s))
            (unwind-protect
                (multiple-value-bind (output-file warningsp failurep)
                        (with-compilation-hooks () (cf))
                    (declare (ignore warningsp))
                    (when output-file
                        (load-it output-file))
                    (not failurep))
                (ignore-errors
                    (delete-file *buffer-tmpfile*)
                    (delete-file (compile-file-pathname *buffer-tmpfile*)))))))

(in-package :slt-core)

(defun specialp (test-sym)
    (eq (sb-cltl2:variable-information test-sym) :special))

(defun analyze-symbol (test-sym)
    (let ((*standard-output* (make-string-output-stream)))
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
            (T (list NIL NIL)))))

(in-package :cl-user)