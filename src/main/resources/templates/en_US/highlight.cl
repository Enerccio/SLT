(defpackage :example
    (:use :cl)
    (:export foobar))

(in-package :example)

(defun foobar (arg1 &rest bar)
    "Implements the foo bar!"
    (cons #1=(if bar
                (loop for x from 0 to 47 collect
                    `(arg1 ,x))
                (loop for x from 10 to 20 collect
                    `(,@arg1 ,x)))
          #1#))

(defclass foo (T)
    ())