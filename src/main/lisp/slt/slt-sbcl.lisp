(in-package :slt-core)

(defun specialp (test-sym)
    (eq (sb-cltl2:variable-information test-sym) :special))