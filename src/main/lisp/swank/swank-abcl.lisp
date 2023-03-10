(in-package :swank/backend)

(defun get-restart-function-args (restart)
  (swank-backend:arglist (system::restart-function restart)))

(in-package :swank/abcl)

(defimplementation find-source-location (obj)
  (handler-case
      (source-location obj)
    (error (c)
      (format t "Error: ~A~%" c) NIL)))

(defmethod source-location ((func T))
  NIL)

(defun slime-location-from-source-annotation (sym it)
  (destructuring-bind (what path pos) it
    (let* ((isfunction
             ;; all of these are (defxxx forms, which is what :function
             ;; locations look for in slime
             (and (consp what)
                  (member (car what)
                          '(:function :generic-function :macro :class :compiler-macro
                            :type :constant :variable :package :structure :condition))))
           (ismethod
             (and (consp what)
                  (eq (car what) :method)))
           ;;; <file:../slime/slime.el> docstring for
           ;;; slime-goto-source-location constains the position to a
           ;;; single clause.  We prioritize a :POSITION clause over
           ;;; others.
           (<position>
             (cond (isfunction
                    (if pos
                        `(:position ,(1+ (or pos 0)))
                        `(:function-name ,(princ-to-string (second what)))))
                   (ismethod
                    (if pos
                        `(:position ,(1+ (or pos 0)))
                        (stringify-method-specs what)))
                   (t ;; Are we ever called with a nil POS?
                    `(:position ,(1+ (or pos 0))))))
           (path2
             (if (eq path :top-level)
                 ;; this is bogus - figure out some way to guess which
                 ;; is the repl associated with :toplevel or get
                 ;; rid of this
                 "emacs-buffer:*slime-repl*"
                 (maybe-redirect-to-jar path))))
      (when (atom what)
        (setq what (list what sym)))
      (list (definition-specifier what)
            (if (ext:pathname-jar-p (pathname path2))
                `(:location
                  (:zip ,@(split-string (subseq path2 (length "jar:file:")) "!/"))
                  ;; pos never seems right. Use function name.
                  ,<position>
                  (:align t))
                ;; conspire with swank-compile-string to keep the
                ;; buffer name in a pathname whose device is
                ;; "emacs-buffer".
                (if (eql 0 (search "emacs-buffer:" path2))
                    `(:location
                      (:buffer ,(subseq path2  (load-time-value (length "emacs-buffer:"))))
                      ,<position>
                      (:align t))
                    `(:location
                      (:file ,path2)
                      ,<position>
                      (:align t))))))))

(defimplementation swank-compile-string (string &key buffer position filename
                                                line column policy)
  (declare (ignore filename line column policy))
  (let ((jvm::*resignal-compiler-warnings* t)
        (*abcl-signaled-conditions* nil))
    (handler-bind ((warning #'handle-compiler-warning))
      (let ((*buffer-name* buffer)
            (*buffer-start-position* position)
            (*buffer-string* string)
            (sys::*source* (pathname buffer))
            (sys::*source-position* position))
        (funcall (compile nil (read-from-string
                               (format nil "(~S () ~A)" 'lambda string))))
        t))))

(in-package :swank)

(defslimefun backtrace (start end)
  (loop for frame in (compute-backtrace start end)
        for i from start collect
        (list i (frame-to-string frame)
                (format NIL "~A" (print-frame-call-place frame))
                (handler-case
                    (frame-source-location i)
                  (error (c) (format t "Error: ~A~%" c)
                             NIL))
                NIL)))

(defun print-frame-call-place (frame)
    (handler-case
        (if (typep frame 'sys::lisp-stack-frame)
            (first (sys::frame-to-list frame))
            (sys::frame-to-string frame))
      (error (c)
        (format t "Error: ~A~%" c))))