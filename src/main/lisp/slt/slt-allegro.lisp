(in-package :slt-core)

(defun specialp (test-sym)
    (eq (system:variable-information test-sym) :special))