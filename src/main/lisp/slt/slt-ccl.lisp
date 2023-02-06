(in-package :slt-core)

(defun install-breakpoint (symbol)
  (ignore-errors (eval `(trace ,symbol :break-before T)) T))

(defun uninstall-breakpoint (symbol)
  (ignore-errors (eval `(untrace ,symbol)) T))

(defun specialp (test-sym)
    (eq (ccl:variable-information test-sym) :special))