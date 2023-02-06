(in-package :slt-core)

(defun specialp (test-sym)
    (eq (ccl:variable-information test-sym) :special))