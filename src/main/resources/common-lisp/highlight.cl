*| block
comment |*

(defun foo (bar &args baz)
    (setf qux (* 2 baz))
    (progn ;hello
        (let ((a 5)
              (b qux))
          (if bar
            (format T "Hello %s" b)
            (format T "World %s" a)))))