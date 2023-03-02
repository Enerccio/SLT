(cl:defpackage #:eclector.concrete-syntax-tree
  (:use
   #:common-lisp)

  (:shadow
   #:read
   #:read-preserving-whitespace
   #:read-from-string)

  ;; Read protocol
  (:export
   #:read
   #:read-preserving-whitespace
   #:read-from-string)

  ;; Client class (can be used as a superclass)
  (:export
   #:cst-client))
