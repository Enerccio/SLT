(defpackage :slt
    (:use :cl)
    (:export +slt-interpret+))

(in-package :slt)
(defconstant +slt-interpret+ ~interpret~
  "Defines current slt interpret.")

(load "~qlpath~")

(ql:quickload :cl-utilities)

(load "~swankPath~/swank-loader.lisp")
(setq swank-loader::*source-directory* "~swankPath~/")
(setq swank-loader::*fasl-directory* "~swankPath~/../fasl/")
(swank-loader:init)

(let ((ql:*local-project-directories*
        (append (list #P"~eclectorPath~") ql:*local-project-directories*)))
  (ql:quickload :eclector))

; Does not work in allegro...
; (setf *default-pathname-defaults* #P"~cwd~")

(load "~corefile~")

(swank:create-server :port ~port~ :dont-close nil)