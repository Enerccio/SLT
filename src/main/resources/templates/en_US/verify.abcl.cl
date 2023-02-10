(ignore-errors
  (load "~qlpath~"))
(ignore-errors
  (ql:quickload :cl-utilities)
  (ql:quickload :swank)
  (ql:quickload :eclector)
  (format *error-output* "SltVerified~%"))