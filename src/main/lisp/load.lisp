(format T "SLT Interpret ~S~%" +slt-interpret+)

(case +slt-interpret+
  (:sbcl (unless (string= "SBCL"
             (lisp-implementation-type))
           (format *error-output* "Invalid lisp instance. Maybe a configuration error? This is not SBCL!~%")
           (exit 1)))
  (otherwise
   (format *error-output* "Unsupported lisp instance. Maybe a configuration error?~%")
   (exit 1)))

(load (merge-pathnames "swank/swank.lisp" *load-truename*))
(load (merge-pathnames "slt/slt.lisp" *load-truename*))

(in-package :cl-user)