(format T "SLT Interpret ~S~%" +slt-interpret+)

(defun quit (&optional code)
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

(case +slt-interpret+
  (:sbcl (unless (string= "SBCL"
             (lisp-implementation-type))
           (format *error-output* "Invalid lisp instance. Maybe a configuration error? This is not SBCL!~%")
           (quit 1)))
  (otherwise
   (format *error-output* "Unsupported lisp instance. Maybe a configuration error?~%")
   (quit 1)))

(load (merge-pathnames "swank/swank.lisp" *load-truename*))
(load (merge-pathnames "slt/slt.lisp" *load-truename*))

(in-package :cl-user)