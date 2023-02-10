(in-package :slt-core)

(defun uninstall-breakpoints () (symbol)
  NIL)

(defun uninstall-breakpoint (name)
  NIL)

(defun install-breakpoint (function-name)
  NIL)

(defun with-breakpoints (symbols)
  NIL)

(defun specialp (test-sym)
    (eq (ccl:variable-information test-sym) :special))