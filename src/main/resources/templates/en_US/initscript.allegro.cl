(defpackage :slt
    (:use :cl)
    (:export +slt-interpret+))

(in-package :slt)
(defconstant +slt-interpret+ ~interpret~
  "Defines current slt interpret.")

(load "~qlpath~")
(ql:quickload :cl-utilities)
(ql:quickload :swank)
(ql:quickload :eclector)

; Does not work in allegro...
; (setf *default-pathname-defaults* #P"~cwd~")

(load "~corefile~")

(swank:create-server :port ~port~ :dont-close nil)