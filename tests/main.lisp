;;;; Main Test Suite
;;;; Entry point for all Nyxt-Proton tests

(defpackage nyxt-proton/tests/main
  (:use :cl :rove)
  (:import-from :nyxt-proton/tests/protocol)
  (:import-from :nyxt-proton/tests/adapter)
  (:import-from :nyxt-proton/tests/integration))

(in-package :nyxt-proton/tests/main)

;; NOTE: To run this test suite, execute `(asdf:test-system :nyxt-proton)' in your Lisp.
;;
;; Test Organization:
;; - protocol-test.lisp    : Tests for PMI protocol data structures and utilities
;; - adapter-test.lisp     : Tests for Proton Pass CLI adapter
;; - integration-test.lisp : Integration tests (require Proton Pass CLI)
;;
;; By default, integration tests are SKIPPED. To run them:
;; 1. Install and configure Proton Pass CLI
;; 2. Set environment variable: NYXT_PROTON_RUN_INTEGRATION_TESTS=1
;; 3. Run: (asdf:test-system :nyxt-proton)

(deftest nyxt-proton-test-suite
  (testing "Nyxt-Proton Test Suite"
    (pass "All test files loaded successfully")))

(deftest check-test-coverage
  (testing "Test coverage summary"
    (format t "~%~%")
    (format t "╔═══════════════════════════════════════════════════════════════╗~%")
    (format t "║              Nyxt-Proton Test Coverage Summary               ║~%")
    (format t "╚═══════════════════════════════════════════════════════════════╝~%")
    (format t "~%")
    (format t "✓ Protocol Tests (protocol-test.lisp)~%")
    (format t "  - Data structure creation (pm-item, pm-credential, pm-otp)~%")
    (format t "  - URL normalization~%")
    (format t "  - Error conditions~%")
    (format t "  - Mock backend implementation~%")
    (format t "~%")
    (format t "✓ Adapter Tests (adapter-test.lisp)~%")
    (format t "  - Backend creation and configuration~%")
    (format t "  - Capabilities reporting~%")
    (format t "  - URL origin matching~%")
    (format t "  - JSON parsing~%")
    (format t "  - Item parsing~%")
    (format t "  - Error handling~%")
    (format t "~%")
    (format t "⚠ Integration Tests (integration-test.lisp)~%")
    (if (uiop:getenv "NYXT_PROTON_RUN_INTEGRATION_TESTS")
        (format t "  - ENABLED: Testing with real Proton Pass CLI~%")
        (format t "  - SKIPPED: Set NYXT_PROTON_RUN_INTEGRATION_TESTS=1 to enable~%"))
    (format t "~%")
    (format t "To run integration tests:~%")
    (format t "  export NYXT_PROTON_RUN_INTEGRATION_TESTS=1~%")
    (format t "  sbcl --eval '(asdf:test-system :nyxt-proton)'~%")
    (format t "~%")
    (pass "Test coverage summary displayed")))

;; Run this function to get a quick test status
(defun test-status ()
  "Display test status and information."
  (format t "~%Nyxt-Proton Test Suite~%")
  (format t "=====================~%~%")
  (format t "Run all tests:       (asdf:test-system :nyxt-proton)~%")
  (format t "Run with integration: NYXT_PROTON_RUN_INTEGRATION_TESTS=1~%")
  (format t "~%")
  (format t "Test files:~%")
  (format t "  - tests/protocol-test.lisp~%")
  (format t "  - tests/adapter-test.lisp~%")
  (format t "  - tests/integration-test.lisp~%")
  (format t "~%"))

;; Export test status function
(export 'test-status)
