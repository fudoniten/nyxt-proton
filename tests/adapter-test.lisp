;;;; Adapter Tests
;;;; Tests for the Proton Pass CLI adapter

(defpackage nyxt-proton/tests/adapter
  (:use :cl :rove)
  (:import-from :nyxt-proton/protocol
                #:pm-item
                #:item-id
                #:item-title
                #:item-urls
                #:item-username
                #:item-password
                #:pm-error
                #:pm-capabilities)
  (:import-from :nyxt-proton/adapters/proton-pass-cli
                #:proton-pass-cli-backend
                #:make-proton-pass-backend
                #:cli-path
                #:active-vault))

(in-package :nyxt-proton/tests/adapter)

;;;; Backend Creation Tests

(deftest test-backend-creation
  (testing "can create backend with default CLI path"
    (let ((backend (make-proton-pass-backend)))
      (ok (typep backend 'proton-pass-cli-backend))
      (ok (string= (cli-path backend) "proton-pass"))
      (ok (null (active-vault backend)))))

  (testing "can create backend with custom CLI path"
    (let ((backend (make-proton-pass-backend :cli-path "/usr/local/bin/proton-pass")))
      (ok (string= (cli-path backend) "/usr/local/bin/proton-pass"))))

  (testing "can create backend with active vault"
    (let ((backend (make-proton-pass-backend :active-vault "my-vault")))
      (ok (string= (active-vault backend) "my-vault")))))

(deftest test-backend-capabilities
  (testing "Proton Pass CLI backend reports correct capabilities"
    (let ((backend (make-proton-pass-backend)))
      (let ((caps (pm-capabilities backend)))
        (ok (member :fill caps))
        (ok (member :totp caps))
        ;; Create/update not yet supported
        (ok (not (member :create caps)))
        (ok (not (member :update caps)))))))

;;;; URL Matching Tests

(deftest test-origin-matching
  (testing "exact origin match"
    (ok (nyxt-proton/adapters/proton-pass-cli::origin-matches-p
         "https://example.com"
         "https://example.com")))

  (testing "subdomain matching"
    (ok (nyxt-proton/adapters/proton-pass-cli::origin-matches-p
         "https://login.example.com"
         "https://example.com")))

  (testing "different origins don't match"
    (ng (nyxt-proton/adapters/proton-pass-cli::origin-matches-p
         "https://example.com"
         "https://different.com")))

  (testing "different schemes don't match"
    (ng (nyxt-proton/adapters/proton-pass-cli::origin-matches-p
         "http://example.com"
         "https://example.com")))

  (testing "port differences matter"
    (ng (nyxt-proton/adapters/proton-pass-cli::origin-matches-p
         "https://example.com:8443"
         "https://example.com"))))

;;;; JSON Parsing Tests

(deftest test-json-parsing
  (testing "parse simple item JSON"
    (let* ((json "{\"id\":\"test-123\",\"name\":\"Test Item\",\"vaultId\":\"vault-1\"}")
           (parsed (jonathan:parse json :as :plist)))
      (ok (string= (getf parsed :|id|) "test-123"))
      (ok (string= (getf parsed :|name|) "Test Item"))
      (ok (string= (getf parsed :|vaultId|) "vault-1"))))

  (testing "parse item with URLs"
    (let* ((json "{\"id\":\"test-123\",\"urls\":[\"https://example.com\"]}")
           (parsed (jonathan:parse json :as :plist)))
      (ok (listp (getf parsed :|urls|)))
      (ok (string= (first (getf parsed :|urls|)) "https://example.com")))))

;;;; URL Parsing Tests

(deftest test-url-parsing
  (testing "parse single URL string"
    (let ((urls (nyxt-proton/adapters/proton-pass-cli::parse-urls "https://example.com")))
      (ok (listp urls))
      (ok (= (length urls) 1))
      (ok (string= (first urls) "https://example.com"))))

  (testing "parse URL list"
    (let ((urls (nyxt-proton/adapters/proton-pass-cli::parse-urls
                 '("https://example.com" "https://example.org"))))
      (ok (= (length urls) 2))))

  (testing "parse nil URLs"
    (let ((urls (nyxt-proton/adapters/proton-pass-cli::parse-urls nil)))
      (ok (null urls)))))

;;;; Custom Fields Parsing Tests

(deftest test-custom-fields-parsing
  (testing "parse custom fields"
    (let* ((fields-data '((:|name| "API Key" :|value| "secret123")
                         (:|name| "Note" :|value| "Important")))
           (parsed (nyxt-proton/adapters/proton-pass-cli::parse-custom-fields fields-data)))
      (ok (= (length parsed) 2))
      (ok (string= (cdr (assoc "API Key" parsed :test #'string=)) "secret123"))
      (ok (string= (cdr (assoc "Note" parsed :test #'string=)) "Important"))))

  (testing "parse nil custom fields"
    (let ((parsed (nyxt-proton/adapters/proton-pass-cli::parse-custom-fields nil)))
      (ok (null parsed)))))

;;;; Item Parsing Tests

(deftest test-item-parsing
  (testing "parse item with metadata only"
    (let* ((plist '(:|id| "item-123"
                   :|name| "GitHub"
                   :|vaultId| "vault-1"
                   :|urls| ("https://github.com")))
           (item (nyxt-proton/adapters/proton-pass-cli::parse-item plist :full nil)))
      (ok (typep item 'pm-item))
      (ok (string= (item-id item) "item-123"))
      (ok (string= (item-title item) "GitHub"))
      (ok (equal (item-urls item) '("https://github.com")))
      ;; Metadata-only parsing doesn't include secrets
      (ok (null (item-username item)))
      (ok (null (item-password item)))))

  (testing "parse full item with secrets"
    (let* ((plist '(:|id| "item-123"
                   :|name| "GitHub"
                   :|username| "user@example.com"
                   :|password| "secret123"
                   :|totpUri| "otpauth://totp/github"
                   :|note| "My GitHub account"))
           (item (nyxt-proton/adapters/proton-pass-cli::parse-item plist :full t)))
      (ok (string= (item-username item) "user@example.com"))
      (ok (string= (item-password item) "secret123"))
      (ok (string= (item-totp-uri item) "otpauth://totp/github"))
      (ok (string= (item-notes item) "My GitHub account")))))

;;;; Error Handling Tests

(deftest test-error-handling
  (testing "locked vault error"
    (ok (signals
         (nyxt-proton/adapters/proton-pass-cli::handle-cli-error
          "Error: Vault is locked" 1)
         'nyxt-proton/protocol:pm-locked-error)))

  (testing "authentication error"
    (ok (signals
         (nyxt-proton/adapters/proton-pass-cli::handle-cli-error
          "Error: Authentication required" 1)
         'nyxt-proton/protocol:pm-auth-expired-error)))

  (testing "not found error"
    (ok (signals
         (nyxt-proton/adapters/proton-pass-cli::handle-cli-error
          "Error: Item not found" 1)
         'nyxt-proton/protocol:pm-not-found-error)))

  (testing "generic error"
    (ok (signals
         (nyxt-proton/adapters/proton-pass-cli::handle-cli-error
          "Unknown error" 42)
         'pm-error))))

;;;; Integration Tests (require actual CLI)

(deftest test-cli-availability
  (testing "backend-available-p with nonexistent CLI"
    (let ((backend (make-proton-pass-backend :cli-path "/nonexistent/proton-pass")))
      ;; Should return nil when CLI is not available
      (ok (not (nyxt-proton/adapters/proton-pass-cli::backend-available-p backend))))))

;; Note: Full integration tests that actually call the Proton Pass CLI
;; are in integration-test.lisp and require a working Proton Pass CLI installation.
