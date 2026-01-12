(defsystem "nyxt-proton"
  :version "0.0.1"
  :author "Peter Selby"
  :mailto "pselby@gmail.com"
  :license "LLGPL"
  :depends-on (#:nyxt
               #:quri
               #:jonathan
               #:trivial-clipboard
               #:bordeaux-threads)
  :components ((:module "src"
                :serial t
                :components
                ((:file "protocol")
                 (:module "adapters"
                  :components
                  ((:file "proton-pass-cli")))
                 (:file "fill")
                 (:file "mode")
                 (:file "main"))))
  :description "Proton Pass password manager integration for Nyxt browser"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "nyxt-proton/tests"))))

(defsystem "nyxt-proton/tests"
  :author "Peter Selby"
  :license "LLGPL"
  :depends-on ("nyxt-proton"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for nyxt-proton"
  :perform (test-op (op c) (symbol-call :rove :run c)))
