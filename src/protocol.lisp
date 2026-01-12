;;;; Password Manager Interface (PMI) Protocol
;;;; This file defines the generic interface that Nyxt code calls,
;;;; independent of any specific password manager backend.

(uiop:define-package :nyxt-proton/protocol
  (:use #:cl)
  (:export
   ;; Core entities
   #:pm-item
   #:pm-credential
   #:pm-otp
   #:pm-passkey
   #:pm-backend
   ;; Accessors for pm-item
   #:item-id
   #:item-vault
   #:item-title
   #:item-urls
   #:item-username
   #:item-password
   #:item-totp-uri
   #:item-totp-secret-ref
   #:item-notes
   #:item-custom-fields
   #:item-metadata
   ;; Accessors for pm-credential
   #:credential-username
   #:credential-password
   ;; Accessors for pm-otp
   #:otp-code
   #:otp-expiry-seconds
   ;; Accessors for pm-passkey
   #:passkey-id
   #:passkey-credential-id
   #:passkey-user-handle
   ;; Core protocol operations
   #:pm-search-items
   #:pm-get-item
   #:pm-fill
   #:pm-generate-totp
   #:pm-save-login
   #:pm-update-login
   #:pm-list-vaults
   #:pm-set-vault
   #:pm-capabilities
   ;; Errors
   #:pm-error
   #:pm-locked-error
   #:pm-auth-expired-error
   #:pm-not-found-error
   #:pm-unsupported-error))

(in-package :nyxt-proton/protocol)

;;;; Error conditions

(define-condition pm-error (error)
  ((message :initarg :message :reader error-message))
  (:report (lambda (condition stream)
             (format stream "Password manager error: ~a" (error-message condition)))))

(define-condition pm-locked-error (pm-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Password manager vault is locked"))))

(define-condition pm-auth-expired-error (pm-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Password manager authentication has expired"))))

(define-condition pm-not-found-error (pm-error)
  ((item-id :initarg :item-id :reader error-item-id))
  (:report (lambda (condition stream)
             (format stream "Item not found: ~a" (error-item-id condition)))))

(define-condition pm-unsupported-error (pm-error)
  ((operation :initarg :operation :reader error-operation))
  (:report (lambda (condition stream)
             (format stream "Operation not supported: ~a" (error-operation condition)))))

;;;; Core data structures

(defclass pm-backend ()
  ()
  (:documentation "Abstract base class for password manager backends."))

(defclass pm-item ()
  ((id
    :initarg :id
    :accessor item-id
    :type string
    :documentation "Unique identifier for this item")
   (vault
    :initarg :vault
    :accessor item-vault
    :initform nil
    :type (or null string)
    :documentation "Vault/collection this item belongs to")
   (title
    :initarg :title
    :accessor item-title
    :type string
    :documentation "Human-readable title for this item")
   (urls
    :initarg :urls
    :accessor item-urls
    :initform nil
    :type list
    :documentation "List of normalized URLs/origins associated with this item")
   (username
    :initarg :username
    :accessor item-username
    :initform nil
    :type (or null string)
    :documentation "Username or email (may be lazily fetched)")
   (password
    :initarg :password
    :accessor item-password
    :initform nil
    :type (or null string)
    :documentation "Password (may be lazily fetched)")
   (totp-uri
    :initarg :totp-uri
    :accessor item-totp-uri
    :initform nil
    :type (or null string)
    :documentation "TOTP URI (otpauth://...)")
   (totp-secret-ref
    :initarg :totp-secret-ref
    :accessor item-totp-secret-ref
    :initform nil
    :type (or null string)
    :documentation "Reference to TOTP secret (backend-specific)")
   (notes
    :initarg :notes
    :accessor item-notes
    :initform nil
    :type (or null string)
    :documentation "Free-form notes")
   (custom-fields
    :initarg :custom-fields
    :accessor item-custom-fields
    :initform nil
    :type list
    :documentation "Alist of custom field name/value pairs")
   (metadata
    :initarg :metadata
    :accessor item-metadata
    :initform nil
    :type list
    :documentation "Backend-specific metadata (plist)"))
  (:documentation "Represents a password manager item with all its fields."))

(defclass pm-credential ()
  ((username
    :initarg :username
    :accessor credential-username
    :type string
    :documentation "Username or email")
   (password
    :initarg :password
    :accessor credential-password
    :type string
    :documentation "Password"))
  (:documentation "Simple username/password credential pair."))

(defclass pm-otp ()
  ((code
    :initarg :code
    :accessor otp-code
    :type string
    :documentation "TOTP code (typically 6 digits)")
   (expiry-seconds
    :initarg :expiry-seconds
    :accessor otp-expiry-seconds
    :type integer
    :documentation "Seconds until this code expires"))
  (:documentation "Time-based one-time password with expiry information."))

(defclass pm-passkey ()
  ((id
    :initarg :id
    :accessor passkey-id
    :type string
    :documentation "Passkey identifier")
   (credential-id
    :initarg :credential-id
    :accessor passkey-credential-id
    :type string
    :documentation "WebAuthn credential ID")
   (user-handle
    :initarg :user-handle
    :accessor passkey-user-handle
    :type string
    :documentation "User handle for this passkey"))
  (:documentation "Passkey/WebAuthn credential (placeholder for future support)."))

;;;; Protocol operations

(defgeneric pm-capabilities (backend)
  (:documentation
   "Returns a list of keywords indicating which operations this backend supports.
    Possible capabilities: :fill :totp :create :update :passkeys :vaults
    Example: (:fill :totp :create)"))

(defgeneric pm-search-items (backend query origin)
  (:documentation
   "Search for items matching QUERY and ORIGIN.
    QUERY can be a string (title/username search) or nil (match by origin only).
    ORIGIN should be a normalized origin (scheme://host:port).
    Returns a list of pm-item objects with metadata only (secrets may not be loaded)."))

(defgeneric pm-get-item (backend item-id)
  (:documentation
   "Retrieve a complete pm-item by ID, including all secrets.
    Returns a pm-item object with username, password, TOTP, etc. fully populated.
    Signals pm-not-found-error if the item doesn't exist."))

(defgeneric pm-fill (backend buffer item strategy)
  (:documentation
   "Fill credentials from ITEM into the BUFFER using the specified STRATEGY.
    BUFFER is a Nyxt buffer object.
    ITEM is a pm-item (or item ID).
    STRATEGY can be :auto, :username-only, :password-only, or :both.
    Returns t on success, nil on failure."))

(defgeneric pm-generate-totp (backend item-or-id)
  (:documentation
   "Generate a TOTP code for the given item.
    ITEM-OR-ID can be a pm-item object or an item ID string.
    Returns a pm-otp object with the code and expiry time.
    Signals pm-unsupported-error if TOTP is not available."))

(defgeneric pm-save-login (backend origin username password title notes)
  (:documentation
   "Create a new login item in the password manager.
    ORIGIN is the normalized origin (scheme://host:port).
    USERNAME and PASSWORD are the credentials.
    TITLE is a human-readable title (can be auto-generated if nil).
    NOTES are optional free-form notes.
    Returns the ID of the newly created item.
    Signals pm-unsupported-error if creation is not supported."))

(defgeneric pm-update-login (backend item-id patches)
  (:documentation
   "Update an existing login item.
    ITEM-ID identifies the item to update.
    PATCHES is a plist of fields to update (:username \"...\" :password \"...\").
    Returns t on success.
    Signals pm-not-found-error if the item doesn't exist.
    Signals pm-unsupported-error if updates are not supported."))

(defgeneric pm-list-vaults (backend)
  (:documentation
   "List all available vaults/collections.
    Returns a list of vault descriptors (plists with :id :name :item-count).
    Signals pm-unsupported-error if vaults are not supported."))

(defgeneric pm-set-vault (backend vault-id)
  (:documentation
   "Set the active vault for subsequent operations.
    VAULT-ID is the vault identifier from pm-list-vaults.
    Returns t on success.
    Signals pm-unsupported-error if vaults are not supported."))

;;;; Helper functions

(defun normalize-origin (url)
  "Extract and normalize the origin from URL (scheme://host:port)."
  (handler-case
      (let* ((uri (quri:uri url))
             (scheme (quri:uri-scheme uri))
             (host (quri:uri-host uri))
             (port (quri:uri-port uri)))
        (format nil "~a://~a~@[:~a~]"
                scheme
                host
                (unless (or (and (string= scheme "http") (= port 80))
                           (and (string= scheme "https") (= port 443)))
                  port)))
    (error ()
      ;; If parsing fails, return the URL as-is
      url)))

(defun make-credential (username password)
  "Create a pm-credential object."
  (make-instance 'pm-credential
                 :username username
                 :password password))

(defun make-otp (code expiry-seconds)
  "Create a pm-otp object."
  (make-instance 'pm-otp
                 :code code
                 :expiry-seconds expiry-seconds))
