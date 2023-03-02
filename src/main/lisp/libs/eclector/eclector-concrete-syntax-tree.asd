(defsystem "eclector-concrete-syntax-tree"
  :description "Reading into concrete syntax tree objects."
  :license     "BSD"
  :author      ("Robert Strandh"
                "Jan Moringen")
  :maintainer  "Jan Moringen"

  :version     (:read-file-form "version.sexp")
  :depends-on  ("alexandria"
                "concrete-syntax-tree"

                "eclector")

  :components  ((:module "concrete-syntax-tree"
                 :pathname "code/concrete-syntax-tree"
                 :serial t
                 :components ((:file "package")
                              (:file "read"))))

  :in-order-to ((test-op (test-op "eclector-concrete-syntax-tree/test"))))

(defsystem :eclector-concrete-syntax-tree/test
  :depends-on  ("eclector-concrete-syntax-tree"
                "eclector/test"
                (:version "fiveam" "1.4"))

  :components  ((:module "concrete-syntax-tree"
                 :pathname "test/concrete-syntax-tree"
                 :serial t
                 :components ((:file "package")
                              (:file "read"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:eclector.concrete-syntax-tree.test '#:run-tests)))
