(proclaim '(optimize (debug 3)))

(load "~qlpath~")
(ql:quickload :swank)
(ql:quickload :eclector)

(setf *default-pathname-defaults* (truename "~cwd~"))

(load "~sbclcorefile~")

(swank:create-server :port ~port~ :dont-close nil)