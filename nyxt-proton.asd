(defsystem "nyxt-proton"
  :version "0.0.1"
  :author "Peter Selby"
  :mailto "pselby@gmail.com"
  :license "LLGPL"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
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
