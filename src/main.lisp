;;;; Nyxt-Proton: Proton Pass Password Manager for Nyxt Browser
;;;; Main entry point and public API

(uiop:define-package :nyxt-proton
  (:use #:cl)
  (:import-from :nyxt-proton/protocol
                #:pm-backend
                #:pm-item
                #:pm-capabilities
                #:pm-search-items
                #:pm-get-item
                #:pm-generate-totp
                #:pm-save-login
                #:pm-update-login)
  (:import-from :nyxt-proton/adapters/proton-pass-cli
                #:proton-pass-cli-backend
                #:make-proton-pass-backend)
  (:import-from :nyxt-proton/mode
                #:password-manager-mode
                #:pm-fill-for-page
                #:pm-copy-username
                #:pm-copy-password
                #:pm-show-totp
                #:pm-search
                #:*default-backend*)
  (:export
   ;; Backend creation
   #:make-proton-pass-backend
   #:*default-backend*
   ;; Mode
   #:password-manager-mode
   ;; Commands
   #:pm-fill-for-page
   #:pm-copy-username
   #:pm-copy-password
   #:pm-show-totp
   #:pm-search
   ;; Protocol (for advanced usage)
   #:pm-backend
   #:pm-item
   #:pm-capabilities
   #:pm-search-items
   #:pm-get-item
   #:pm-generate-totp
   #:pm-save-login
   #:pm-update-login))

(in-package :nyxt-proton)

;;;; Initialization

(defun initialize-proton-pass (&key (cli-path "proton-pass"))
  "Initialize Proton Pass backend and set it as default.
   CLI-PATH should point to the Proton Pass CLI executable.

   Example usage in your Nyxt config:
     (nyxt-proton:initialize-proton-pass)
     (define-configuration buffer
       ((default-modes (append '(password-manager-mode) %slot-default%))))"
  (let ((backend (make-proton-pass-backend :cli-path cli-path)))
    (setf *default-backend* backend)
    (format t "~&Proton Pass backend initialized~%")
    backend))

;;;; Quick start helpers

(defun quick-start ()
  "Quick start guide for Nyxt-Proton."
  (format t "~&~
╔═══════════════════════════════════════════════════════════════╗
║                    Nyxt-Proton Quick Start                    ║
╚═══════════════════════════════════════════════════════════════╝

1. Initialize the backend in your Nyxt config (~/.config/nyxt/config.lisp):

   (nyxt-proton:initialize-proton-pass)

2. Enable password-manager-mode for all buffers (optional):

   (define-configuration buffer
     ((default-modes (append '(password-manager-mode) %slot-default%))))

3. Available commands:

   pm-fill-for-page    - Fill credentials for current page
   pm-search           - Search for a login by name
   pm-copy-username    - Copy username to clipboard
   pm-copy-password    - Copy password to clipboard
   pm-show-totp        - Generate and copy TOTP code

4. Keyboard shortcuts (recommended, add to your config):

   (define-configuration buffer
     ((override-map
       (let ((map (make-keymap \"password-manager\")))
         (define-key map
           \"C-c p f\" 'pm-fill-for-page
           \"C-c p s\" 'pm-search
           \"C-c p u\" 'pm-copy-username
           \"C-c p p\" 'pm-copy-password
           \"C-c p t\" 'pm-show-totp)
         map))))

5. Prerequisites:

   - Proton Pass CLI must be installed and configured
   - You must be logged in to Proton Pass via the CLI

For more information, see the README.

~%"))

;;;; Module loaded message

(format t "~&Nyxt-Proton loaded. Run (nyxt-proton:quick-start) for usage instructions.~%")
