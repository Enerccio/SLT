(in-package sb-debug)

(export 'frame-code-location)
(export 'frame-call)
(export 'ensure-printable-object)
(export 'code-location-source-form)

(in-package :swank)

(defun print-frame-call-place (frame)
    (multiple-value-bind (name args info)
            (SB-DEBUG:frame-call frame)
        (declare (ignore args info))
        (let ((name (SB-DEBUG:ensure-printable-object name)))
            name)))
