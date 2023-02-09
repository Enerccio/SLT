(in-package :slt-core)

(defun specialp (test-sym)
    (eq (extensions:variable-information test-sym) :special))