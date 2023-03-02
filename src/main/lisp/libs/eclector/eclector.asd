(defsystem "eclector"
  :description "A portable, extensible Common Lisp reader."
  :license     "BSD"
  :author      ("Robert Strandh"
                "Jan Moringen")
  :maintainer  "Jan Moringen"

  :version     (:read-file-form "version.sexp")
  :depends-on  ("alexandria"
                "closer-mop"
                "acclimation")

  :components  ((:module "base"
                 :pathname "code/base"
                 :components ((:file "package")
                              (:file "utilities")
                              (:file "variables")
                              (:file "generic-functions")
                              (:file "conditions")
                              (:file "read-char")
                              ;; Translatable messages
                              (:file "messages-english")))

                (:module "readtable"
                 :pathname "code/readtable"
                 :depends-on ("base")
                 :serial t
                 :components ((:file "package")
                              (:file "variables")
                              (:file "conditions")
                              (:file "generic-functions")
                              ;; Translatable messages
                              (:file "messages-english")))

                (:module "simple-readtable"
                 :pathname "code/readtable/simple"
                 :depends-on ("base"
                              "readtable")
                 :serial t
                 :components ((:file "package")
                              (:file "readtable")
                              (:file "methods")
                              ;; Translatable messages
                              (:file "messages-english")))

                (:module "reader"
                 :pathname "code/reader"
                 :depends-on ("base"
                              "readtable")
                 :serial t
                 :components ((:file "package")
                              ;; The file variables defines standard
                              ;; variables with names in the
                              ;; COMMON-LISP package which control the
                              ;; reader. Loading the file can be
                              ;; useful when Eclector is used as the
                              ;; reader of a Common Lisp
                              ;; implementation.
                              (:file "variables"
                               :if-feature :eclector-define-cl-variables)
                              (:file "generic-functions")
                              (:file "more-variables")
                              (:file "additional-conditions")
                              (:file "utilities")
                              (:file "tokens")
                              (:file "read-common")
                              (:file "read")
                              (:file "macro-functions")
                              (:file "init")
                              (:file "quasiquote-macro")
                              (:file "fixup")
                              ;; Translatable messages
                              (:file "messages-english")))

                (:module "parse-result"
                 :pathname "code/parse-result"
                 :depends-on ("reader")
                 :serial t
                 :components ((:file "package")
                              (:file "client")
                              (:file "generic-functions")
                              (:file "read")
                              (:file "deprecation" :if-feature :sbcl)))

                (:static-file "README.md")
                (:static-file "LICENSE"))

  :in-order-to ((test-op (test-op "eclector/test"))))

(defsystem "eclector/test"
  :depends-on  ("alexandria"
                "eclector"
                (:version "fiveam" "1.4"))

  :components  ((:module "test"
                 :components ((:file "package")
                              (:file "utilities")))

                (:module "readtable"
                 :pathname "test/readtable"
                 :depends-on ("test")
                 :serial t
                 :components ((:file "package")
                              (:file "generic-functions")))

                (:module "simple-readtable"
                 :pathname "test/readtable/simple"
                 :depends-on ("readtable")
                 :serial t
                 :components ((:file "package")
                              (:file "methods")))

                (:module "reader"
                 :pathname "test/reader"
                 :depends-on ("test")
                 :serial t
                 :components ((:file "package")

                              (:file "helpers")
                              (:file "gen-quasiquote")

                              (:file "utilities")
                              (:file "tokens")
                              (:file "read")
                              (:file "macro-functions")
                              (:file "quasiquote-macro")
                              (:file "fixup")

                              (:file "readtable")

                              (:file "client")

                              (:file "recover")))

                (:module "parse-result"
                 :pathname "test/parse-result"
                 :depends-on ("test")
                 :serial t
                 :components ((:file "package")
                              (:file "read"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:eclector.test '#:run-tests)))
