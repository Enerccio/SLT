(cl:in-package #:eclector.parse-result)

(defclass parse-result-client ()
  ())

;;; The following two methods are for backwards compatibility:
;;; Eclector code always calls the generic functions defined in the
;;; base module. The following two methods delegate such calls to
;;; client-defined methods on generic functions defined in the
;;; parse-result module. If there are no such client-defined methods,
;;; the default methods defined in the parse-result module delegate
;;; back to the default methods defined in the base module.
;;;
;;; This mechanism will be removed after a grace period.
(defmethod eclector.base:source-position ((client parse-result-client) stream)
  (source-position client stream))

(defmethod eclector.base:make-source-range ((client parse-result-client) start end)
  (make-source-range client start end))
