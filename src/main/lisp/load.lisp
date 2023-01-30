(format T "SLT Interpret ~S~%" +slt-interpret+)

(load (merge-pathnames "swank/swank.lisp" *load-truename*))
(load (merge-pathnames "slt/slt.lisp" *load-truename*))

(in-package :cl-user)