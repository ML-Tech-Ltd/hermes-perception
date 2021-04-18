(defsystem "hermes-perception"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:alexandria
	       :defenum
	       :hermes-input)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "hermes-perception/tests"))))

(defsystem "hermes-perception/tests"
  :author ""
  :license ""
  :depends-on ("hermes-perception"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for hermes-perception"

  :perform (test-op (op c) (symbol-call :rove :run c)))
