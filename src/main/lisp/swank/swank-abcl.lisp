(in-package :swank/abcl)

(defmethod source-location ((func (eql :ANONYMOUS-INTERPRETED-FUNCTION)))
  NIL)

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