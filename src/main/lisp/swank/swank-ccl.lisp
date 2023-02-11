(in-package :swank/backend)

(defun get-restart-function-args (restart)
  NIL)

(in-package :swank/ccl)

(in-package :swank)

(defslimefun backtrace (start end)
  (loop for frame in (compute-backtrace start end)
        for i from start collect
        (list i (frame-to-string frame)
                (format NIL "~A" (print-frame-call-place frame))
                (frame-source-location i)
                (let ((pkg (frame-package i)))
                    (cond
                        (pkg (package-name pkg))
                        (T NIL))))))

(defun print-frame-call-place (frame)
  (destructuring-bind (p context) (rest frame)
    (let ((lfun (ccl:frame-function p context)))
      (or (ccl:function-name lfun) lfun))))