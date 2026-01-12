;;;; Proton Pass CLI Adapter
;;;; This file implements the PMI protocol using the Proton Pass CLI.

(uiop:define-package :nyxt-proton/adapters/proton-pass-cli
  (:use #:cl)
  (:import-from :nyxt-proton/protocol
                #:pm-backend
                #:pm-item
                #:pm-otp
                #:pm-error
                #:pm-locked-error
                #:pm-auth-expired-error
                #:pm-not-found-error
                #:pm-unsupported-error
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
                #:make-otp)
  (:export
   #:proton-pass-cli-backend
   #:make-proton-pass-backend
   #:cli-path
   #:active-vault))

(in-package :nyxt-proton/adapters/proton-pass-cli)

;;;; Backend class

(defclass proton-pass-cli-backend (pm-backend)
  ((cli-path
    :initarg :cli-path
    :accessor cli-path
    :initform "proton-pass"
    :type string
    :documentation "Path to the Proton Pass CLI executable")
   (active-vault
    :initarg :active-vault
    :accessor active-vault
    :initform nil
    :type (or null string)
    :documentation "Currently active vault ID"))
  (:documentation "Backend adapter for Proton Pass CLI."))

(defun make-proton-pass-backend (&key (cli-path "proton-pass") active-vault)
  "Create a new Proton Pass CLI backend."
  (make-instance 'proton-pass-cli-backend
                 :cli-path cli-path
                 :active-vault active-vault))

;;;; CLI execution helpers

(defun run-cli-command (backend args &key input)
  "Execute a Proton Pass CLI command with ARGS.
   Returns the output as a string.
   INPUT is optional stdin for the command.
   Signals appropriate pm-error conditions on failure."
  (let* ((full-command (append (list (cli-path backend)) args))
         (result (multiple-value-list
                  (uiop:run-program full-command
                                   :input (when input (make-string-input-stream input))
                                   :output '(:string :stripped t)
                                   :error-output '(:string :stripped t)
                                   :ignore-error-status t))))
    (destructuring-bind (output error-output exit-code) result
      (unless (zerop exit-code)
        (handle-cli-error error-output exit-code))
      output)))

(defun handle-cli-error (error-output exit-code)
  "Handle CLI errors and signal appropriate conditions."
  (cond
    ((search "locked" error-output :test #'char-equal)
     (error 'pm-locked-error))
    ((or (search "auth" error-output :test #'char-equal)
         (search "session" error-output :test #'char-equal))
     (error 'pm-auth-expired-error))
    ((search "not found" error-output :test #'char-equal)
     (error 'pm-not-found-error :item-id "unknown"))
    (t
     (error 'pm-error :message (format nil "CLI error (exit ~a): ~a" exit-code error-output)))))

(defun parse-json-output (output)
  "Parse JSON output from CLI command.
   Returns the parsed JSON structure."
  (handler-case
      (jonathan:parse output :as :plist)
    (error (e)
      (error 'pm-error :message (format nil "Failed to parse JSON: ~a" e)))))

;;;; Item parsing

(defun parse-item (item-plist &key (full nil))
  "Parse a Proton Pass item from a plist.
   FULL indicates whether this includes secrets (from get-item) or just metadata (from search)."
  (make-instance 'pm-item
                 :id (getf item-plist :|id|)
                 :vault (getf item-plist :|vaultId|)
                 :title (getf item-plist :|name|)
                 :urls (parse-urls (getf item-plist :|urls|))
                 :username (when full (getf item-plist :|username|))
                 :password (when full (getf item-plist :|password|))
                 :totp-uri (when full (getf item-plist :|totpUri|))
                 :notes (getf item-plist :|note|)
                 :custom-fields (parse-custom-fields (getf item-plist :|customFields|))
                 :metadata item-plist))

(defun parse-urls (urls-data)
  "Parse the URLs field from Proton Pass item."
  (cond
    ((null urls-data) nil)
    ((stringp urls-data) (list urls-data))
    ((listp urls-data) urls-data)
    (t nil)))

(defun parse-custom-fields (fields-data)
  "Parse custom fields into an alist."
  (when fields-data
    (loop for field in fields-data
          collect (cons (getf field :|name|)
                       (getf field :|value|)))))

;;;; Protocol implementation

(defmethod nyxt-proton/protocol:pm-capabilities ((backend proton-pass-cli-backend))
  "Proton Pass CLI currently supports fill and TOTP.
   Create/update support depends on CLI version and will be added later."
  '(:fill :totp))

(defmethod nyxt-proton/protocol:pm-search-items ((backend proton-pass-cli-backend) query origin)
  "Search for items using Proton Pass CLI.
   Note: The exact CLI API may vary; this is a template implementation."
  (let* ((args (list "item" "list" "--format" "json"))
         ;; Add search filters if supported
         (args (if query
                   (append args (list "--filter" query))
                   args))
         (output (run-cli-command backend args))
         (data (parse-json-output output))
         (items-list (if (listp data) data (list data))))
    ;; Filter by origin if provided
    (let ((all-items (mapcar (lambda (item) (parse-item item :full nil)) items-list)))
      (if origin
          (remove-if-not (lambda (item)
                          (some (lambda (url)
                                  (origin-matches-p url origin))
                                (item-urls item)))
                        all-items)
          all-items))))

(defun origin-matches-p (item-url origin)
  "Check if ITEM-URL matches the given ORIGIN.
   Handles exact matches and subdomain matching."
  (let ((normalized-item (nyxt-proton/protocol:normalize-origin item-url))
        (normalized-origin (nyxt-proton/protocol:normalize-origin origin)))
    (or (string= normalized-item normalized-origin)
        ;; Allow subdomain matching: login.example.com matches example.com
        (and (> (length normalized-item) (length normalized-origin))
             (string= normalized-origin
                     (subseq normalized-item
                            (max 0 (- (length normalized-item)
                                     (length normalized-origin)))))))))

(defmethod nyxt-proton/protocol:pm-get-item ((backend proton-pass-cli-backend) item-id)
  "Retrieve a complete item with all secrets."
  (let* ((args (list "item" "get" item-id "--format" "json"))
         (output (run-cli-command backend args))
         (data (parse-json-output output)))
    (parse-item data :full t)))

(defmethod nyxt-proton/protocol:pm-fill ((backend proton-pass-cli-backend) buffer item strategy)
  "Fill credentials into a Nyxt buffer.
   Delegates to the fill module for DOM manipulation."
  (declare (ignore backend strategy))
  ;; This will be implemented by the fill module
  ;; For now, just ensure we have the credentials
  (unless (item-username item)
    (error 'pm-error :message "Item has no username"))
  (unless (item-password item)
    (error 'pm-error :message "Item has no password"))
  ;; The actual filling will be done by nyxt-proton/fill:fill-credentials
  ;; which will be called from the mode commands
  (values buffer item))

(defmethod nyxt-proton/protocol:pm-generate-totp ((backend proton-pass-cli-backend) item-or-id)
  "Generate a TOTP code for the given item."
  (let* ((item-id (if (stringp item-or-id)
                     item-or-id
                     (item-id item-or-id)))
         (args (list "item" "totp" item-id))
         (output (run-cli-command backend args)))
    ;; Parse TOTP output - format may vary by CLI version
    ;; Typical format: "123456" or JSON with code and expiry
    (handler-case
        (let ((data (parse-json-output output)))
          (make-otp (getf data :|code|)
                   (or (getf data :|expiresIn|) 30)))
      (error ()
        ;; Fallback: assume output is just the code
        (make-otp (string-trim '(#\Space #\Newline #\Return) output)
                 30)))))

(defmethod nyxt-proton/protocol:pm-save-login ((backend proton-pass-cli-backend) origin username password title notes)
  "Create a new login item.
   Currently unsupported - will be implemented when CLI supports it."
  (declare (ignore backend origin username password title notes))
  (error 'pm-unsupported-error :operation "save-login"))

(defmethod nyxt-proton/protocol:pm-update-login ((backend proton-pass-cli-backend) item-id patches)
  "Update an existing login item.
   Currently unsupported - will be implemented when CLI supports it."
  (declare (ignore backend item-id patches))
  (error 'pm-unsupported-error :operation "update-login"))

(defmethod nyxt-proton/protocol:pm-list-vaults ((backend proton-pass-cli-backend))
  "List all available vaults."
  (let* ((args (list "vault" "list" "--format" "json"))
         (output (run-cli-command backend args))
         (data (parse-json-output output)))
    (mapcar (lambda (vault)
              (list :id (getf vault :|id|)
                    :name (getf vault :|name|)
                    :item-count (or (getf vault :|itemCount|) 0)))
            (if (listp data) data (list data)))))

(defmethod nyxt-proton/protocol:pm-set-vault ((backend proton-pass-cli-backend) vault-id)
  "Set the active vault."
  (setf (active-vault backend) vault-id)
  t)

;;;; Utility functions

(defun backend-available-p (backend)
  "Check if the Proton Pass CLI is available and working."
  (handler-case
      (progn
        (run-cli-command backend (list "version"))
        t)
    (error () nil)))
