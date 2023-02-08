(in-package :swank/backend)

(defun get-restart-function-args (restart)
  NIL)

(in-package :swank/allegro)

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
  (multiple-value-bind (x fun xx xxx pc) (debugger::dyn-fd-analyze frame)
    (declare (ignore x xx xxx))
    (cross-reference::object-to-function-name fun)))