(defpackage :slt-core
    (:use :slt :cl :swank)
    (:export analyze-symbol analyze-symbols read-fix-packages list-package-names
             initialize-or-get-debug-context debug-context debug-frame-variable register-variable
             ))

(when (eq slt:+slt-interpret+ :sbcl)
  (load (merge-pathnames "slt-sbcl.lisp" *load-truename*)))

(in-package :slt-core)

(defun analyze-symbol (test-sym)
    (cons test-sym
        (let ((*standard-output* (make-string-output-stream)))
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
            (ECLECTOR.READER:TWO-PACKAGE-MARKERS-MUST-BE-ADJACENT
                #'reader-recover)
            (error (lambda (c)
                   (format *error-output* "general error: ~S ~%" c))))
       (eclector.reader:read-from-string str)))

(defun list-package-names ()
    (let ((packages (list-all-packages)))
        (loop for package in packages collect
            (package-name package))))