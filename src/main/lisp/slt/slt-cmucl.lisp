(in-package :slt-core)

(defun uninstall-breakpoints ()
  (untrace))

(defun uninstall-breakpoint (name)
  (eval `(untrace ,name)))

(defun install-breakpoint (function-name)
  (ignore-errors
    (swank::with-buffer-syntax ()
      (swank::sldb-break-at-start (swank::read-from-string function-name)))))

(defun with-breakpoints (function-names)
  (when function-names
    (uninstall-breakpoints)
    (loop for function-name in (cl-utilities:split-sequence #\Space function-names) do
      (install-breakpoint function-name))))

(defun specialp (test-sym)
    (eq (extensions:variable-information test-sym) :special))