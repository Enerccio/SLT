(cl:in-package #:eclector.reader)

;;; Establishing context

(defgeneric call-as-top-level-read (client thunk input-stream
                                    eof-error-p eof-value preserve-whitespace-p))

(defgeneric read-common (client input-stream eof-error-p eof-value))

(defgeneric read-maybe-nothing (client input-stream eof-error-p eof-value))

(defgeneric note-skipped-input (client input-stream reason)
  (:method ((client t) (input-stream t) (reason t))
    (declare (ignore client input-stream reason))))

;;; Reading tokens

(defgeneric read-token (client input-stream eof-error-p eof-value))

(defgeneric interpret-token (client input-stream token escape-ranges))

(defgeneric check-symbol-token (client input-stream
                                token escape-ranges
                                position-package-marker-1
                                position-package-marker-2))

(defgeneric interpret-symbol-token (client input-stream
                                    token
                                    position-package-marker-1
                                    position-package-marker-2))

(defgeneric interpret-symbol (client input-stream
                              package-indicator symbol-name internp))

;;; Calling reader macros and behavior of standard reader macros

(defgeneric call-reader-macro (client input-stream char readtable)
  (:method ((client t) (input-stream t) (char t) (readtable t))
    (let ((function (eclector.readtable:get-macro-character readtable char)))
      (funcall function input-stream char))))

(defgeneric find-character (client designator)
  (:method ((client t) (designator character))
    designator)
  (:method ((client t) (designator string))
    (find-standard-character designator)))

(defgeneric make-structure-instance (client name initargs))

(defgeneric call-with-current-package (client thunk package-designator)
  (:method ((client t) (thunk t) (package-designator t))
    (let ((*package* (find-package package-designator)))
      (funcall thunk))))

(defgeneric evaluate-expression (client expression)
  (:method ((client t) (expression t))
    (declare (ignore client))
    (eval expression)))

(defgeneric check-feature-expression (client feature-expression)
  (:method ((client t) (feature-expression t))
    (declare (ignore client))
    (check-standard-feature-expression feature-expression)))

(defgeneric evaluate-feature-expression (client feature-expression)
  (:method ((client t) (feature-expression t))
    (evaluate-standard-feature-expression
     feature-expression
     :check (alexandria:curry #'check-feature-expression client)
     :recurse (alexandria:curry #'evaluate-feature-expression client))))

(defgeneric fixup (client object seen-objects mapping))

;;; Creating s-expressions

(defgeneric wrap-in-quote (client material)
  (:method (client material)
    (declare (ignore client))
    (list 'quote material)))

(defgeneric wrap-in-quasiquote (client form)
  (:method (client form)
    (declare (ignore client))
    (list 'quasiquote form)))

(defgeneric wrap-in-unquote (client form)
  (:method (client form)
    (declare (ignore client))
    (list 'unquote form)))

(defgeneric wrap-in-unquote-splicing (client form)
  (:method (client form)
    (declare (ignore client))
    (list 'unquote-splicing form)))

(defgeneric wrap-in-function (client name)
  (:method (client name)
    (declare (ignore client))
    (list 'function name)))
