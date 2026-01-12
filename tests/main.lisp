(defpackage nyxt-proton/tests/main
  (:use :cl
        :nyxt-proton
        :rove))
(in-package :nyxt-proton/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :nyxt-proton)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
