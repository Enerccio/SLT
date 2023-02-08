(eval-when (:execute)
  (format T "SLT Interpret ~S~%" slt:+slt-interpret+))

(defun portable-quit (&optional code)
  (declare (ignorable code))
  ;; This group from "clocc-port/ext.lisp"
  #+allegro (excl:exit code)
  #+clisp (#+lisp=cl ext:quit #-lisp=cl lisp:quit code)
  #+cmu (ext:quit code)
  #+cormanlisp (win32:exitprocess code)
  #+gcl (lisp:bye code)                     ; XXX Or is it LISP::QUIT?
  #+lispworks (lw:quit :status code)
  #+lucid (lcl:quit code)
  #+sbcl (sb-ext:exit :code code)
  ;; This group from Maxima
  #+kcl (lisp::bye)                         ; XXX Does this take an arg?
  #+scl (ext:quit code)                     ; XXX Pretty sure this *does*.
  #+(or openmcl mcl) (ccl::quit)
  #+abcl (cl-user::quit)
  #+ecl (si:quit)
  ;; This group from <hebi...@math.uni.wroc.pl>
  #+poplog (poplog::bye)                    ; XXX Does this take an arg?
  #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbc
        kcl scl openmcl mcl abcl ecl)
  (error 'not-implemented :proc (list 'quit code)))

(defun starts-with-p (start s)
  (let ((start-length (length (string start))))
    (when (>= (length s) start-length)
      (string-equal s start :start1 0 :end1 start-length))))

(format T "Current lisp interpret: ~S~%" (lisp-implementation-type))

(case slt:+slt-interpret+
  (:sbcl (unless (string= "SBCL"
             (lisp-implementation-type))
           (format *error-output* "Invalid lisp instance. Maybe a configuration error? This is not SBCL!~%")
           (portable-quit 1)))
  (:abcl (unless (string= "Armed Bear Common Lisp"
               (lisp-implementation-type))
             (format *error-output* "Invalid lisp instance. Maybe a configuration error? This is not ABCL!~%")
             (portable-quit 1)))
  (:ccl (unless (string= "Clozure Common Lisp"
               (lisp-implementation-type))
             (format *error-output* "Invalid lisp instance. Maybe a configuration error? This is not Clozure Common Lisp!~%")
             (portable-quit 1)))
  (:allegro (unless (starts-with-p "International Allegro CL"
                       (lisp-implementation-type))
               (format *error-output* "Invalid lisp instance. Maybe a configuration error? This is not Allegro Common Lisp!~%")
               (portable-quit 1)))
  (otherwise
   (format *error-output* "Unsupported lisp instance. Maybe a configuration error?~%")
   (portable-quit 1)))

(load (merge-pathnames "swank/swank.lisp" *load-truename*))
(load (merge-pathnames "slt/slt.lisp" *load-truename*))

(in-package :cl-user)