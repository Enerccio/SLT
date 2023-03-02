(cl:in-package #:eclector.base)

;;; Source location protocol

(defgeneric source-position (client stream)
  (:method (client stream)
    (declare (ignore client))
    (file-position stream)))

(defgeneric make-source-range (client start end)
  (:method (client start end)
    (declare (ignore client))
    (cons start end)))
