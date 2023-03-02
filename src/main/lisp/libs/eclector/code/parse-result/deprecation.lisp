(cl:in-package #:eclector.parse-result)

(declaim (sb-ext:deprecated
          :early ("Eclector" "0.7")
          (function source-position :replacement eclector.base:source-position)
          (function make-source-range :replacement eclector.base:make-source-range)))
