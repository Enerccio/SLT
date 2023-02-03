(defpackage :slt
    (:use :cl)
    (:export +slt-interpret+))

(in-package :slt)
(defconstant +slt-interpret+ ~interpret~
  "Defines current slt interpret. For SBCL the value is :sbcl")

(load "~qlpath~")
(ql:quickload :swank)
(ql:quickload :eclector)

(setf *default-pathname-defaults* (truename "~cwd~"))

(load "~sbclcorefile~")

(swank:create-server :port ~port~ :dont-close nil)