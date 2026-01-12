;;;; Integration Tests
;;;; These tests require a working Proton Pass CLI installation and Nyxt browser.
;;;;
;;;; To run these tests:
;;;; 1. Install and configure Proton Pass CLI
;;;; 2. Log in to Proton Pass: proton-pass auth login
;;;; 3. Ensure you have at least one item in your vault
;;;; 4. Set environment variable: NYXT_PROTON_RUN_INTEGRATION_TESTS=1
;;;; 5. Run: (asdf:test-system :nyxt-proton)
;;;;
;;;; These tests are SKIPPED by default to avoid requiring external dependencies.

(defpackage nyxt-proton/tests/integration
  (:use :cl :rove)
  (:import-from :nyxt-proton
                #:make-proton-pass-backend
                #:*default-backend*)
  (:import-from :nyxt-proton/protocol
                #:pm-search-items
                #:pm-get-item
                #:pm-capabilities
                #:item-id
                #:item-title))

(in-package :nyxt-proton/tests/integration)

(defparameter *run-integration-tests*
  (not (null (uiop:getenv "NYXT_PROTON_RUN_INTEGRATION_TESTS")))
  "Set to t to run integration tests that require Proton Pass CLI.")

(defmacro integration-test (name &body body)
  "Define an integration test that only runs when *run-integration-tests* is true."
  `(deftest ,name
     (if *run-integration-tests*
         (progn ,@body)
         (skip "Integration tests disabled. Set NYXT_PROTON_RUN_INTEGRATION_TESTS=1 to enable."))))

;;;; CLI Integration Tests

(integration-test test-cli-list-items
  (testing "can list items from actual Proton Pass CLI"
    (let ((backend (make-proton-pass-backend)))
      (handler-case
          (let ((items (pm-search-items backend nil nil)))
            (ok (listp items))
            (format t "~&Found ~a item(s) in Proton Pass~%" (length items)))
        (error (e)
          (fail (format nil "Failed to list items: ~a" e)))))))

(integration-test test-cli-search-by-query
  (testing "can search items by query"
    (let ((backend (make-proton-pass-backend)))
      (handler-case
          (let ((items (pm-search-items backend "test" nil)))
            (ok (listp items))
            (format t "~&Found ~a item(s) matching 'test'~%" (length items)))
        (error (e)
          (fail (format nil "Failed to search items: ~a" e)))))))

(integration-test test-cli-get-item
  (testing "can retrieve a specific item"
    (let ((backend (make-proton-pass-backend)))
      (handler-case
          (let* ((items (pm-search-items backend nil nil))
                 (first-item (first items)))
            (when first-item
              (let ((full-item (pm-get-item backend (item-id first-item))))
                (ok (not (null full-item)))
                (ok (string= (item-id full-item) (item-id first-item)))
                (format t "~&Retrieved item: ~a~%" (item-title full-item)))))
        (error (e)
          (fail (format nil "Failed to get item: ~a" e)))))))

(integration-test test-cli-capabilities
  (testing "backend reports correct capabilities"
    (let ((backend (make-proton-pass-backend)))
      (let ((caps (pm-capabilities backend)))
        (ok (member :fill caps))
        (ok (member :totp caps))))))

;;;; Nyxt Integration Tests

;; These tests would require a running Nyxt instance and are more complex
;; to set up. They are documented here for future implementation.

(deftest test-nyxt-integration-placeholder
  (skip "Nyxt integration tests require a running Nyxt instance and are not yet implemented.

These tests would verify:
- Field discovery in actual web pages
- Credential filling on real login forms
- TOTP code generation and display
- Command execution from Nyxt
- Clipboard operations

To implement these tests:
1. Set up a test Nyxt instance
2. Create test HTML pages with login forms
3. Mock or use test Proton Pass credentials
4. Verify filling, TOTP, and other operations"))

;;;; Performance Tests

(integration-test test-search-performance
  (testing "search performance is acceptable"
    (let ((backend (make-proton-pass-backend)))
      (handler-case
          (let ((start-time (get-internal-real-time)))
            (pm-search-items backend nil nil)
            (let* ((end-time (get-internal-real-time))
                   (elapsed (/ (- end-time start-time)
                             internal-time-units-per-second)))
              (format t "~&Search took ~,3f seconds~%" elapsed)
              ;; Search should complete in under 5 seconds
              (ok (< elapsed 5.0))))
        (error (e)
          (fail (format nil "Performance test failed: ~a" e)))))))

;;;; Summary

(deftest integration-tests-summary
  (if *run-integration-tests*
      (pass "Integration tests are enabled")
      (skip "Integration tests are disabled. To enable them:

1. Install Proton Pass CLI
2. Log in: proton-pass auth login
3. Add test items to your vault
4. Set environment variable: NYXT_PROTON_RUN_INTEGRATION_TESTS=1
5. Run: (asdf:test-system :nyxt-proton)

Note: Integration tests will interact with your real Proton Pass vault.")))
