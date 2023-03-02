(cl:in-package #:eclector.parse-result)

;;; Source location protocol (has moved to base module)
;;;
;;; The default methods delegate to the default methods defined in the
;;; base module.
;;;
;;; This protocol will be removed from this module after a grace
;;; period.

(defgeneric source-position (client stream)
  (:method (client stream)
    (eclector.base:source-position nil stream)))

(defgeneric make-source-range (client start end)
  (:method (client start end)
    (eclector.base:make-source-range nil start end)))

;;; Parse result protocol

(defgeneric make-expression-result (client result children source))

(defgeneric make-skipped-input-result (client stream reason source)
  (:method (client stream reason source)
    (declare (ignore client stream reason source))
    nil))
