;;;; DOM Field Discovery and Credential Filling
;;;; This module handles finding and filling password fields in web pages.

(uiop:define-package :nyxt-proton/fill
  (:use #:cl)
  (:import-from :nyxt-proton/protocol
                #:pm-item
                #:item-username
                #:item-password)
  (:export
   #:discover-login-fields
   #:fill-credentials
   #:fill-username
   #:fill-password
   #:detect-login-form
   #:get-page-origin))

(in-package :nyxt-proton/fill)

;;;; JavaScript snippets for DOM interaction

(defparameter *field-discovery-script*
  "
// Find all potential username and password fields
(function() {
  const result = {
    username: [],
    password: [],
    forms: []
  };

  // Find password fields first (they're more reliable)
  const passwordFields = Array.from(document.querySelectorAll('input[type=password]'));
  result.password = passwordFields.map(field => ({
    id: field.id,
    name: field.name,
    autocomplete: field.autocomplete,
    placeholder: field.placeholder,
    visible: field.offsetParent !== null,
    xpath: getXPath(field)
  }));

  // Find potential username fields
  const usernameSelectors = [
    'input[type=email]',
    'input[type=text][autocomplete*=username]',
    'input[type=text][autocomplete*=email]',
    'input[type=text][name*=user]',
    'input[type=text][name*=email]',
    'input[type=text][name*=login]',
    'input[type=tel]',
    'input[type=text]' // catch-all, filtered below
  ];

  const usernameFields = new Set();
  for (const selector of usernameSelectors) {
    document.querySelectorAll(selector).forEach(field => {
      if (!usernameFields.has(field) && field.type !== 'password') {
        usernameFields.add(field);
      }
    });
  }

  result.username = Array.from(usernameFields).map(field => ({
    id: field.id,
    name: field.name,
    type: field.type,
    autocomplete: field.autocomplete,
    placeholder: field.placeholder,
    visible: field.offsetParent !== null,
    xpath: getXPath(field),
    score: scoreUsernameField(field)
  })).sort((a, b) => b.score - a.score);

  // Find forms containing these fields
  const forms = new Set();
  [...passwordFields, ...Array.from(usernameFields)].forEach(field => {
    const form = field.closest('form');
    if (form) forms.add(form);
  });

  result.forms = Array.from(forms).map(form => ({
    id: form.id,
    name: form.name,
    action: form.action,
    method: form.method
  }));

  return result;

  function getXPath(element) {
    if (element.id) return `//*[@id='${element.id}']`;

    const parts = [];
    while (element && element.nodeType === Node.ELEMENT_NODE) {
      let index = 0;
      let sibling = element.previousSibling;
      while (sibling) {
        if (sibling.nodeType === Node.ELEMENT_NODE &&
            sibling.nodeName === element.nodeName) {
          index++;
        }
        sibling = sibling.previousSibling;
      }
      const tagName = element.nodeName.toLowerCase();
      const pathIndex = index ? `[${index + 1}]` : '';
      parts.unshift(tagName + pathIndex);
      element = element.parentNode;
    }
    return '/' + parts.join('/');
  }

  function scoreUsernameField(field) {
    let score = 0;

    // Autocomplete attribute is a strong signal
    const ac = (field.autocomplete || '').toLowerCase();
    if (ac.includes('username') || ac === 'email') score += 50;

    // Type email is very reliable
    if (field.type === 'email') score += 40;

    // Name/ID patterns
    const nameId = ((field.name || '') + (field.id || '')).toLowerCase();
    if (nameId.match(/user(name)?/)) score += 30;
    if (nameId.match(/email/)) score += 25;
    if (nameId.match(/login/)) score += 20;

    // Visible fields score higher
    if (field.offsetParent !== null) score += 10;

    // Position: fields earlier in DOM score higher
    const allInputs = Array.from(document.querySelectorAll('input'));
    const position = allInputs.indexOf(field);
    if (position >= 0) score += Math.max(0, 10 - position);

    return score;
  }
})();
"
  "JavaScript to discover login fields in the current page.")

(defparameter *fill-field-script*
  "
// Fill a field by XPath and trigger appropriate events
(function(xpath, value) {
  const field = document.evaluate(xpath, document, null,
                                  XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue;
  if (!field) return false;

  // Set the value
  const nativeInputValueSetter = Object.getOwnPropertyDescriptor(
    window.HTMLInputElement.prototype, 'value').set;
  nativeInputValueSetter.call(field, value);

  // Trigger events that frameworks listen for
  const events = [
    new Event('input', { bubbles: true }),
    new Event('change', { bubbles: true }),
    new Event('blur', { bubbles: true })
  ];

  events.forEach(event => field.dispatchEvent(event));

  // Focus and blur to trigger validation
  field.focus();
  setTimeout(() => field.blur(), 10);

  return true;
})(~S, ~S);
"
  "JavaScript to fill a field and trigger events. Takes xpath and value as arguments.")

;;;; Field discovery

(defun discover-login-fields (buffer)
  "Discover username and password fields in BUFFER.
   Returns a plist with :username, :password, and :forms keys."
  (let ((result (nyxt:peval *field-discovery-script* :buffer buffer)))
    ;; Convert from JSON to plist
    (when result
      (list :username (nyxt:aget result :|username|)
            :password (nyxt:aget result :|password|)
            :forms (nyxt:aget result :|forms|)))))

(defun detect-login-form (buffer)
  "Check if the current buffer contains a login form.
   Returns t if login fields are detected, nil otherwise."
  (let ((fields (discover-login-fields buffer)))
    (and fields
         (or (getf fields :password)
             (getf fields :username)))))

;;;; Credential filling

(defun fill-credentials (buffer item &key (strategy :auto))
  "Fill credentials from ITEM into BUFFER using STRATEGY.
   STRATEGY can be :auto, :username-only, :password-only, or :both."
  (let ((fields (discover-login-fields buffer)))
    (unless fields
      (error "No login fields found on page"))

    (let ((username-fields (getf fields :username))
          (password-fields (getf fields :password))
          (username (item-username item))
          (password (item-password item)))

      ;; Fill based on strategy
      (case strategy
        (:username-only
         (when (and username username-fields)
           (fill-field buffer (first username-fields) username)))

        (:password-only
         (when (and password password-fields)
           (fill-field buffer (first password-fields) password)))

        ((:both :auto)
         (when (and username username-fields)
           (fill-field buffer (first username-fields) username))
         (when (and password password-fields)
           (fill-field buffer (first password-fields) password)))

        (t
         (error "Unknown fill strategy: ~a" strategy)))

      t)))

(defun fill-username (buffer username)
  "Fill USERNAME into the first suitable field in BUFFER."
  (let* ((fields (discover-login-fields buffer))
         (username-fields (getf fields :username)))
    (when username-fields
      (fill-field buffer (first username-fields) username))))

(defun fill-password (buffer password)
  "Fill PASSWORD into the first password field in BUFFER."
  (let* ((fields (discover-login-fields buffer))
         (password-fields (getf fields :password)))
    (when password-fields
      (fill-field buffer (first password-fields) password))))

(defun fill-field (buffer field-info value)
  "Fill a single field identified by FIELD-INFO with VALUE.
   FIELD-INFO should be a plist with at least :xpath."
  (let ((xpath (getf field-info :xpath)))
    (unless xpath
      (error "Field info missing :xpath"))

    ;; Execute the fill script
    (nyxt:peval (format nil *fill-field-script* xpath value)
               :buffer buffer)))

;;;; Origin detection

(defun get-page-origin (buffer)
  "Extract the normalized origin from BUFFER's current URL."
  (let ((url (nyxt:url buffer)))
    (nyxt-proton/protocol:normalize-origin (quri:render-uri url))))

;;;; Form detection for save/update prompts

(defun detect-form-submission (buffer callback)
  "Install a listener to detect form submissions in BUFFER.
   CALLBACK will be called with (username password origin) when a form is submitted.
   Returns a cleanup function to remove the listener."
  (let ((script "
(function() {
  const forms = document.querySelectorAll('form');
  const handler = function(event) {
    // Find username and password fields in this form
    const form = event.target;
    const passwordField = form.querySelector('input[type=password]');
    if (!passwordField || !passwordField.value) return;

    const usernameField = form.querySelector(
      'input[type=email], input[type=text][autocomplete*=username], input[type=text]'
    );

    const data = {
      username: usernameField ? usernameField.value : '',
      password: passwordField.value,
      origin: window.location.origin
    };

    // Store in sessionStorage for retrieval
    sessionStorage.setItem('nyxt_proton_pending_save', JSON.stringify(data));
  };

  forms.forEach(form => {
    form.addEventListener('submit', handler);
  });

  return 'installed';
})();
"))
    ;; Install the listener
    (nyxt:peval script :buffer buffer)

    ;; Return a function to poll for submissions
    (lambda ()
      (let ((data-json (nyxt:peval
                        "sessionStorage.getItem('nyxt_proton_pending_save')"
                        :buffer buffer)))
        (when data-json
          ;; Clear the stored data
          (nyxt:peval "sessionStorage.removeItem('nyxt_proton_pending_save')"
                     :buffer buffer)
          ;; Parse and call callback
          (let* ((data (jonathan:parse data-json :as :plist))
                 (username (getf data :|username|))
                 (password (getf data :|password|))
                 (origin (getf data :|origin|)))
            (funcall callback username password origin)))))))
