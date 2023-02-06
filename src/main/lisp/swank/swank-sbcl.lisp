(in-package :swank/backend)

(defun get-restart-function-args (restart)
  (swank-backend:arglist (sb-kernel::restart-function restart)))

(in-package :swank/sbcl)

(defun form-number-position (definition-source stream)
  (let* ((tlf-number (car (sb-introspect:definition-source-form-path definition-source)))
         (form-number (sb-introspect:definition-source-form-number definition-source)))
    (multiple-value-bind (tlf pos-map) (read-source-form tlf-number stream)
      (let* ((path-table (sb-di::form-number-translations tlf 0))
             (path (cond ((<= (length path-table) form-number)
                          ; (warn "inconsistent form-number-translations") ; this fucks up stderr wtf
                          (list 0))
                         (t
                          (reverse (cdr (aref path-table form-number)))))))
        (source-path-source-position path tlf pos-map)))))

(defun stream-source-position (code-location stream)
  (let* ((cloc (sb-debug::maybe-block-start-location code-location))
         (tlf-number (sb-di::code-location-toplevel-form-offset cloc))
         (form-number (sb-di::code-location-form-number cloc)))
    (multiple-value-bind (tlf pos-map) (read-source-form tlf-number stream)
      (let* ((path-table (sb-di::form-number-translations tlf 0))
             (path (cond ((<= (length path-table) form-number)
                          ; (warn "inconsistent form-number-translations") ; same as above
                          (list 0))
                         (t
                          (reverse (cdr (aref path-table form-number)))))))
        (source-path-source-position path tlf pos-map)))))

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
    (multiple-value-bind (name args info)
            (sb-debug::frame-call frame)
        (declare (ignore args info))
        (let ((name (sb-debug::ensure-printable-object name)))
            name)))