(in-package :swank/backend)

(defparameter *in-get-restart-function-args* NIL)
(defparameter *in-print-frame-call-place* NIL)

(defun get-restart-function-args (restart)
  (unless *in-get-restart-function-args*
    (let ((*in-get-restart-function-args* T))
      (handler-case
        (progn
          (let*
              ((fnc (slot-value restart 'function))
               (header (kernel:get-type fnc)))
            (cond ((= header vm:function-header-type)
                      (kernel:%function-arglist fnc))
                  ((= header vm:closure-header-type)
                       NIL)
                  ((eval::interpreted-function-p fnc)
                       (kernel:%function-type fnc))
                  (t NIL))))
      (error (e)
        (declare (ignore e))
        NIL)))))

(in-package :swank/cmucl)

(defun foreign-frame-source-location (frame)
  `(:error "no srcloc available"))

(in-package :swank)

(defun print-frame-call-place (frame)
  (if *in-print-frame-call-place* "Recursive frame"
    (let ((*in-print-frame-call-place* T))
      (handler-case
          (di:debug-function-name (di:frame-debug-function (di::frame-real-frame frame)))
        (error (e)
          (declare (ignore e))
          "Unknown frame")))))

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