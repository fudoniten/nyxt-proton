;;;; Nyxt Password Manager Mode
;;;; This file defines the Nyxt mode and commands for password management.

(uiop:define-package :nyxt-proton/mode
  (:use #:cl)
  (:import-from :nyxt-proton/protocol
                #:pm-backend
                #:pm-item
                #:pm-capabilities
                #:pm-search-items
                #:pm-get-item
                #:pm-generate-totp
                #:item-id
                #:item-title
                #:item-username
                #:item-password
                #:otp-code
                #:otp-expiry-seconds)
  (:import-from :nyxt-proton/fill
                #:discover-login-fields
                #:fill-credentials
                #:get-page-origin
                #:detect-login-form)
  (:export
   #:password-manager-mode
   #:pm-fill-for-page
   #:pm-copy-username
   #:pm-copy-password
   #:pm-show-totp
   #:pm-search
   #:*default-backend*))

(in-package :nyxt-proton/mode)

;;;; Global backend

(defvar *default-backend* nil
  "Default password manager backend. Should be set to a pm-backend instance.")

;;;; Mode definition

(define-mode password-manager-mode ()
  "Mode for password manager integration."
  ((backend
    :initform nil
    :type (or null pm-backend)
    :documentation "Password manager backend for this buffer. Uses *default-backend* if nil.")
   (last-filled-item
    :initform nil
    :type (or null pm-item)
    :documentation "Last item that was filled in this buffer."))
  (:toggler-command-p nil))

;;;; Helper functions

(defun get-backend (mode)
  "Get the backend for MODE, falling back to *default-backend*."
  (or (slot-value mode 'backend)
      *default-backend*
      (error "No password manager backend configured. Set *default-backend* or mode backend.")))

(defun prompt-select-item (items &key (prompt "Select login:"))
  "Prompt user to select an item from ITEMS.
   Returns the selected pm-item or nil if cancelled."
  (when items
    (nyxt:prompt
     :prompt prompt
     :sources (make-instance 'nyxt:source
                            :name "Logins"
                            :constructor items
                            :actions (list (make-instance 'nyxt:action
                                                         :name "Select"
                                                         :function #'identity))))))

;;;; Commands

(define-command pm-fill-for-page (&key (buffer (nyxt:current-buffer)))
  "Fill credentials for the current page.
   Searches for logins matching the current origin and prompts user to select one."
  (let* ((mode (find-submode 'password-manager-mode buffer))
         (backend (get-backend mode))
         (origin (get-page-origin buffer)))

    ;; Search for items matching this origin
    (let ((items (pm-search-items backend nil origin)))
      (cond
        ((null items)
         (echo "No logins found for ~a" origin))

        ((= 1 (length items))
         ;; Auto-fill if only one match
         (let ((item (first items)))
           ;; Ensure we have the full item with secrets
           (let ((full-item (pm-get-item backend (item-id item))))
             (fill-credentials buffer full-item :strategy :both)
             (setf (slot-value mode 'last-filled-item) full-item)
             (echo "Filled credentials for: ~a" (item-title full-item)))))

        (t
         ;; Multiple matches - prompt user
         (let ((selected (prompt-select-item items)))
           (when selected
             (let ((full-item (pm-get-item backend (item-id selected))))
               (fill-credentials buffer full-item :strategy :both)
               (setf (slot-value mode 'last-filled-item) full-item)
               (echo "Filled credentials for: ~a" (item-title full-item))))))))))

(define-command pm-search (&key (buffer (nyxt:current-buffer)))
  "Search for a login by title or username."
  (let* ((mode (find-submode 'password-manager-mode buffer))
         (backend (get-backend mode))
         (query (nyxt:prompt1
                 :prompt "Search logins:"
                 :input "")))

    (when query
      (let ((items (pm-search-items backend query nil)))
        (cond
          ((null items)
           (echo "No logins found matching: ~a" query))

          (t
           (let ((selected (prompt-select-item items)))
             (when selected
               (let ((full-item (pm-get-item backend (item-id selected))))
                 (fill-credentials buffer full-item :strategy :both)
                 (setf (slot-value mode 'last-filled-item) full-item)
                 (echo "Filled credentials for: ~a" (item-title full-item)))))))))))

(define-command pm-copy-username (&key (buffer (nyxt:current-buffer)))
  "Copy the username of the last filled item to clipboard."
  (let* ((mode (find-submode 'password-manager-mode buffer))
         (item (slot-value mode 'last-filled-item)))
    (cond
      ((null item)
       (echo "No item has been filled yet. Use pm-fill-for-page first."))

      ((null (item-username item))
       (echo "Selected item has no username"))

      (t
       (trivial-clipboard:text (item-username item))
       (echo "Username copied to clipboard")))))

(define-command pm-copy-password (&key (buffer (nyxt:current-buffer)))
  "Copy the password of the last filled item to clipboard."
  (let* ((mode (find-submode 'password-manager-mode buffer))
         (item (slot-value mode 'last-filled-item)))
    (cond
      ((null item)
       (echo "No item has been filled yet. Use pm-fill-for-page first."))

      ((null (item-password item))
       (echo "Selected item has no password"))

      (t
       (trivial-clipboard:text (item-password item))
       (echo "Password copied to clipboard")
       ;; Optional: clear clipboard after timeout
       ;; (schedule-clipboard-clear 30)
       ))))

(define-command pm-show-totp (&key (buffer (nyxt:current-buffer)))
  "Generate and show TOTP code for the last filled item or selected item."
  (let* ((mode (find-submode 'password-manager-mode buffer))
         (backend (get-backend mode))
         (item (slot-value mode 'last-filled-item)))

    ;; Check if backend supports TOTP
    (unless (member :totp (pm-capabilities backend))
      (echo "TOTP not supported by current backend")
      (return-from pm-show-totp))

    (cond
      ((null item)
       (echo "No item selected. Use pm-fill-for-page first or pm-search."))

      (t
       (handler-case
           (let ((otp (pm-generate-totp backend item)))
             (trivial-clipboard:text (otp-code otp))
             (echo "TOTP: ~a (expires in ~a seconds, copied to clipboard)"
                   (otp-code otp)
                   (otp-expiry-seconds otp)))
         (error (e)
           (echo "Failed to generate TOTP: ~a" e)))))))

;;;; Utility functions

(defun schedule-clipboard-clear (seconds)
  "Schedule clipboard clearing after SECONDS.
   This is optional security feature."
  (bt:make-thread
   (lambda ()
     (sleep seconds)
     (trivial-clipboard:text "")
     (echo "Clipboard cleared"))
   :name "clipboard-clear"))

(defun echo (format-string &rest args)
  "Echo a message to the user."
  (nyxt:echo (apply #'format nil format-string args)))

;;;; Auto-detection and prompts (for future save/update features)

(defun maybe-prompt-save (buffer username password origin)
  "Prompt user to save new credentials (called from form submission detection).
   This is a placeholder for future implementation."
  (declare (ignore buffer username password origin))
  ;; Future implementation:
  ;; 1. Check if credentials already exist for this origin
  ;; 2. If not, prompt user: 'Save these credentials to Proton Pass?'
  ;; 3. Call pm-save-login if user confirms
  nil)

(defun maybe-prompt-update (buffer username password origin)
  "Prompt user to update existing credentials.
   This is a placeholder for future implementation."
  (declare (ignore buffer username password origin))
  ;; Future implementation:
  ;; 1. Find existing item for this origin and username
  ;; 2. If password has changed, prompt: 'Update saved password?'
  ;; 3. Call pm-update-login if user confirms
  nil)
