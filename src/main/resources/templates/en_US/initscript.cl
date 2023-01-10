(load "~qlpath~")
(ql:quickload :swank)

(setf *default-pathname-defaults* (truename "~cwd~"))

(load "~sbclcorefile~")

(swank:create-server :port ~port~ :dont-close nil)