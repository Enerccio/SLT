(load (merge-pathnames "swank-backend.lisp" *load-truename*))

(when (eq slt:+slt-interpret+ :sbcl)
  (load (merge-pathnames "swank-sbcl.lisp" *load-truename*))
  (in-package swank/source-file-cache)
  (setf *source-snippet-size* 0))
(when (eq slt:+slt-interpret+ :abcl)
  (load (merge-pathnames "swank-abcl.lisp" *load-truename*)))
(when (eq slt:+slt-interpret+ :ccl)
  (load (merge-pathnames "swank-ccl.lisp" *load-truename*)))
(when (eq slt:+slt-interpret+ :allegro)
  (load (merge-pathnames "swank-allegro.lisp" *load-truename*)))
(when (eq slt:+slt-interpret+ :cmucl)
  (load (merge-pathnames "swank-cmucl.lisp" *load-truename*))
  (in-package swank/source-file-cache)
  (setf *source-snippet-size* 0))

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
                (swank-backend::get-restart-function-args restart)))))

(defslimefun compile-string-region-slt (string buffer offset filename package)
    (with-buffer-syntax ()
      (collect-notes
       (lambda ()
         (let ((*compile-print* t) (*compile-verbose* nil) (*package* (find-package package)))
           (swank-compile-string string
                                 :buffer buffer
                                 :position offset
                                 :filename filename))))))

(defun get-all-symbols ()
  (let ((data '()))
    (do-symbols (s)
      (push (list (unparse-symbol s) s (symbol-package s)) data))
    (remove-duplicates data :key (lambda (e) (second e)))))

(defun get-all-symbols-with-prefix (prefix)
  (let ((all (get-all-symbols)))
    (loop for data-pair in all
              when (prefix-match-p prefix (first data-pair))
                            collect data-pair)))

(defun find-reference-class-filter (symbol package)
    (declare (ignorable package))
    (find-class symbol NIL))

(defun get-reference-prefix (prefix type)
  (let ((apply-func (cond
                     ((eq type :class) #'find-reference-class-filter)
                     (T (lambda (symbol package) T)))))
    (let ((filtered-symbols (get-all-symbols-with-prefix prefix)))
      (loop for data-pair in filtered-symbols
                  when (funcall apply-func (second data-pair) (third data-pair))
                                   collect
        (let* ((symbol (second data-pair))
               (str (first data-pair))
               (package (third data-pair))
               (strpackage (package-name package)))
          (cond
            ((not symbol) (list str strpackage NIL NIL))
            ((macro-function symbol) (list str strpackage :macro (find-source-location (symbol-function symbol))))
            ((and (fboundp symbol)
                 (typep (symbol-function symbol) 'generic-function))
              (list str strpackage :method (find-source-location (symbol-function symbol))))
            ((fboundp symbol) (list str strpackage :function (find-source-location (symbol-function symbol))))
            ((find-class symbol NIL) (list str strpackage :class (find-source-location (find-class symbol))))
            (T (list str strpackage NIL NIL))))))))

(defslimefun find-reference-prefix (prefix type)
  (let ((references (get-reference-prefix prefix type)))
    (when references
      (sort references #'string< :key (lambda (x) (first x))))))

(defslimefun operator-arglist-list (name package)
  (ignore-errors
    (let ((args (arglist (parse-symbol name (guess-buffer-package package)))))
      (cond ((eq args :not-available) nil)
	    (t args)))))

(export 'slt-eval)
(export 'compile-string-region-slt)
(export 'find-reference-prefix)
(export 'operator-arglist-list)
