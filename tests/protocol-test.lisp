;;;; Protocol Tests
;;;; Tests for the Password Manager Interface (PMI) protocol

(defpackage nyxt-proton/tests/protocol
  (:use :cl :rove)
  (:import-from :nyxt-proton/protocol
                #:pm-item
                #:pm-credential
                #:pm-otp
                #:pm-passkey
                #:pm-backend
                #:item-id
                #:item-vault
                #:item-title
                #:item-urls
                #:item-username
                #:item-password
                #:item-totp-uri
                #:item-notes
                #:item-custom-fields
                #:credential-username
                #:credential-password
                #:otp-code
                #:otp-expiry-seconds
                #:make-credential
                #:make-otp
                #:normalize-origin
                #:pm-error
                #:pm-locked-error
                #:pm-auth-expired-error
                #:pm-not-found-error
                #:pm-unsupported-error))

(in-package :nyxt-proton/tests/protocol)

;;;; Data Structure Tests

(deftest test-pm-item-creation
  (testing "pm-item can be created with all fields"
    (let ((item (make-instance 'pm-item
                              :id "test-id-123"
                              :vault "vault-1"
                              :title "Test Login"
                              :urls '("https://example.com")
                              :username "user@example.com"
                              :password "secret123"
                              :totp-uri "otpauth://totp/test"
                              :notes "Test notes"
                              :custom-fields '(("field1" . "value1")))))
      (ok (string= (item-id item) "test-id-123"))
      (ok (string= (item-vault item) "vault-1"))
      (ok (string= (item-title item) "Test Login"))
      (ok (equal (item-urls item) '("https://example.com")))
      (ok (string= (item-username item) "user@example.com"))
      (ok (string= (item-password item) "secret123"))
      (ok (string= (item-totp-uri item) "otpauth://totp/test"))
      (ok (string= (item-notes item) "Test notes"))
      (ok (equal (item-custom-fields item) '(("field1" . "value1"))))))

  (testing "pm-item can be created with minimal fields"
    (let ((item (make-instance 'pm-item
                              :id "minimal-id"
                              :title "Minimal Item")))
      (ok (string= (item-id item) "minimal-id"))
      (ok (string= (item-title item) "Minimal Item"))
      (ok (null (item-urls item)))
      (ok (null (item-username item)))
      (ok (null (item-password item))))))

(deftest test-pm-credential-creation
  (testing "pm-credential can be created"
    (let ((cred (make-credential "testuser" "testpass")))
      (ok (typep cred 'pm-credential))
      (ok (string= (credential-username cred) "testuser"))
      (ok (string= (credential-password cred) "testpass")))))

(deftest test-pm-otp-creation
  (testing "pm-otp can be created"
    (let ((otp (make-otp "123456" 30)))
      (ok (typep otp 'pm-otp))
      (ok (string= (otp-code otp) "123456"))
      (ok (= (otp-expiry-seconds otp) 30)))))

(deftest test-pm-passkey-creation
  (testing "pm-passkey can be created"
    (let ((passkey (make-instance 'pm-passkey
                                 :id "pk-123"
                                 :credential-id "cred-456"
                                 :user-handle "user-789")))
      (ok (typep passkey 'pm-passkey)))))

;;;; Utility Function Tests

(deftest test-normalize-origin
  (testing "normalizes HTTPS URLs correctly"
    (ok (string= (normalize-origin "https://example.com/path?query=1")
                "https://example.com")))

  (testing "normalizes HTTP URLs correctly"
    (ok (string= (normalize-origin "http://example.com/path")
                "http://example.com")))

  (testing "preserves non-standard ports"
    (ok (string= (normalize-origin "https://example.com:8443/path")
                "https://example.com:8443")))

  (testing "removes standard HTTPS port"
    (ok (string= (normalize-origin "https://example.com:443/path")
                "https://example.com")))

  (testing "removes standard HTTP port"
    (ok (string= (normalize-origin "http://example.com:80/path")
                "http://example.com")))

  (testing "handles subdomains"
    (ok (string= (normalize-origin "https://login.example.com/auth")
                "https://login.example.com")))

  (testing "handles malformed URLs gracefully"
    (ok (normalize-origin "not-a-url"))))

;;;; Error Condition Tests

(deftest test-error-conditions
  (testing "pm-error can be signaled"
    (ok (signals (error 'pm-error :message "test error")
                'pm-error)))

  (testing "pm-locked-error can be signaled"
    (ok (signals (error 'pm-locked-error)
                'pm-locked-error)))

  (testing "pm-auth-expired-error can be signaled"
    (ok (signals (error 'pm-auth-expired-error)
                'pm-auth-expired-error)))

  (testing "pm-not-found-error can be signaled with item-id"
    (ok (signals (error 'pm-not-found-error :item-id "missing-123")
                'pm-not-found-error)))

  (testing "pm-unsupported-error can be signaled with operation"
    (ok (signals (error 'pm-unsupported-error :operation "test-op")
                'pm-unsupported-error))))

;;;; Mock Backend for Testing

(defclass mock-backend (pm-backend)
  ((items :initarg :items :accessor backend-items :initform nil)
   (capabilities-list :initarg :capabilities :accessor backend-capabilities-list
                     :initform '(:fill :totp)))
  (:documentation "Mock backend for testing."))

(defmethod nyxt-proton/protocol:pm-capabilities ((backend mock-backend))
  (backend-capabilities-list backend))

(defmethod nyxt-proton/protocol:pm-search-items ((backend mock-backend) query origin)
  (let ((items (backend-items backend)))
    (when origin
      (setf items (remove-if-not
                   (lambda (item)
                     (member origin (item-urls item) :test #'string=))
                   items)))
    (when query
      (setf items (remove-if-not
                   (lambda (item)
                     (or (search query (item-title item) :test #'char-equal)
                         (and (item-username item)
                              (search query (item-username item) :test #'char-equal))))
                   items)))
    items))

(defmethod nyxt-proton/protocol:pm-get-item ((backend mock-backend) item-id)
  (or (find item-id (backend-items backend)
           :key #'item-id
           :test #'string=)
      (error 'pm-not-found-error :item-id item-id)))

(deftest test-mock-backend
  (testing "mock backend capabilities"
    (let ((backend (make-instance 'mock-backend)))
      (ok (equal (nyxt-proton/protocol:pm-capabilities backend)
                '(:fill :totp)))))

  (testing "mock backend search by origin"
    (let* ((items (list
                   (make-instance 'pm-item
                                 :id "1"
                                 :title "Example Login"
                                 :urls '("https://example.com"))
                   (make-instance 'pm-item
                                 :id "2"
                                 :title "Test Login"
                                 :urls '("https://test.com"))))
           (backend (make-instance 'mock-backend :items items)))
      (let ((results (nyxt-proton/protocol:pm-search-items backend nil "https://example.com")))
        (ok (= (length results) 1))
        (ok (string= (item-id (first results)) "1")))))

  (testing "mock backend search by query"
    (let* ((items (list
                   (make-instance 'pm-item
                                 :id "1"
                                 :title "GitHub Login"
                                 :username "user@github.com")
                   (make-instance 'pm-item
                                 :id "2"
                                 :title "GitLab Login"
                                 :username "user@gitlab.com")))
           (backend (make-instance 'mock-backend :items items)))
      (let ((results (nyxt-proton/protocol:pm-search-items backend "GitHub" nil)))
        (ok (= (length results) 1))
        (ok (string= (item-title (first results)) "GitHub Login")))))

  (testing "mock backend get item"
    (let* ((item (make-instance 'pm-item
                               :id "test-123"
                               :title "Test Item"))
           (backend (make-instance 'mock-backend :items (list item))))
      (let ((found (nyxt-proton/protocol:pm-get-item backend "test-123")))
        (ok (string= (item-id found) "test-123")))))

  (testing "mock backend get item not found"
    (let ((backend (make-instance 'mock-backend)))
      (ok (signals (nyxt-proton/protocol:pm-get-item backend "nonexistent")
                  'pm-not-found-error)))))
