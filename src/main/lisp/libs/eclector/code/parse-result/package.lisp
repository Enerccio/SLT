(cl:defpackage #:eclector.parse-result
  (:use
   #:common-lisp
   #:alexandria)

  (:shadow
   #:read
   #:read-preserving-whitespace
   #:read-from-string)

  ;; Source location protocol (deprecated, moved to base module)
  (:export
   #:source-position
   #:make-source-range)

  ;; Parse result protocol
  (:export
   #:make-expression-result
   #:make-skipped-input-result)

  ;; Read protocol
  (:export
   #:read
   #:read-preserving-whitespace
   #:read-from-string)

  ;; Client protocol class (can be used as a superclass)
  (:export
   #:parse-result-client))
