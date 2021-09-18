(in-package #:matrix-moonbot)


;;;;conditions

(define-condition moonbot-condition (error)
  ()
  (:documentation "Top level class for all Moonbot defined conditions"))

(define-condition cannot-perform-action (moonbot-condition)
  ((cannot-perform-action-action
    :accessor cannot-perform-action-action
    :initarg :cannot-perform-action-action)
   (cannot-perform-action-message
    :accessor cannot-perform-action-message
    :initarg :cannot-perform-action-message))
  (:documentation "Signalled when the user has attempted to do something the bot shouldn't
do like removing the last ubermensch, or the last listen-in room")
  (:report
   (lambda (obj stream)
     (format stream "Attempted an action I shoudln't have. Action: ~A. Message: ~A."
             (cannot-perform-action-action obj)
             (cannot-perform-action-message obj)))))

;;;validator errors

(define-condition validator-condition (moonbot-condition)
  ())

(define-condition missing-key (validator-condition)
  ((missing-key-key
    :initarg :missing-key-key
    :accessor missing-key-key))
  (:documentation "Signalled when an attempt was made to validate args but the key
is missing")
  (:report
   (lambda (obj stream)
     (format stream "Key ~A missing from validator. Try defining or removing.~%"
             (missing-key-key obj)))))

(define-condition validation-failed (validator-condition)
  ((validation-failed-key
    :initarg :validation-failed-key
    :accessor validation-failed-key)
   (validation-failed-entry
    :initarg :validation-failed-entry
    :accessor validation-failed-entry)
   (validation-failed-message
    :initarg :validation-failed-message
    :accessor validation-failed-message))
  (:documentation "Signalled when an attempt to validate an entry fails.")
  (:report
   (lambda (obj stream)
     (format stream "Failed to validate entry: ~S~%Against validator: ~A~%Message: ~A~%"
             (validation-failed-entry obj)
             (validation-failed-key obj)
             (validation-failed-message obj)))))

(defmethod print-object ((obj validation-failed) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "Failed on ~A. With entry '~A'. Your input is bad, try again."
            (validation-failed-key obj)
            (validation-failed-entry obj))))

(define-condition bad-symbols-used (validator-condition)
  ((bad-symbols-used-symbols
    :initarg :bad-symbols-used-symbols
    :accessor bad-symbols-used-symbols)
   (bad-symbols-used-used
    :initarg :bad-symbols-used-used
    :accessor bad-symbols-used-used))
  (:documentation "Signalled when a symbol matches one that is supposed to be 
excluded")
  (:report
   (lambda (obj stream)
     (format stream "You have tried to use an excluded symbol. Can't use: ~A~% ~
                     You used: ~A~%If I allowed you to do this you would have ~
                     overridden a variable, potentially breaking stuff."
             (bad-symbols-used-symbols obj)
             (bad-symbols-used-used obj)))))

;;;sync errors

(define-condition missing-filter-for-key (moonbot-condition)
  ((missing-filter-for-key-key
    :initarg :missing-filter-for-key-key
    :accessor missing-filter-for-key-key)
   (missing-filter-for-key-community
    :initarg :missing-filter-for-key-community
    :accessor missing-filter-for-key-community)
   (missing-filter-for-key-message
    :initarg :missing-filter-for-key-message
    :accessor missing-filter-for-key-message))
  (:documentation "Signalled when a /sync was attempted with an unknown filter")
  (:report
   (lambda (obj stream)
     (format stream "Key: ~A. Missing from (filters ~A). Message: ~A"
             (missing-filter-for-key-key obj)
             (missing-filter-for-key-community obj)
             (missing-filter-for-key-message obj)))))

;;;config errors
(define-condition config-condition (moonbot-condition)
  ())

(define-condition expected-key-missing-from-config (warning config-condition)
  ((missing-key-message
    :initarg :missing-key-message
    :accessor missing-key-message)
   (missing-key-expected-key
    :initarg :missing-key-expected-key
    :accessor missing-key-expected-key))
  (:documentation "Signalled when a critical key is missing from config")
  (:report
   (lambda (obj stream)
     (format stream "Key: ~A. Missing from config. Message: ~A"
             (missing-key-expected-key obj)
             (missing-key-message obj)))))

(define-condition config-missing (config-condition)
  ((config-missing-message
    :accessor config-missing-message
    :initform "default"
    :initarg config-missing-message
    :type string))
  (:report
   (lambda (con stream)
     (format stream "~&Configuration is missing. Message: ~A~%"
             (config-missing-message con)))))

;;;module errors

(define-condition module-error (moonbot-condition)
  ((module-error-message
    :initarg :module-error-message
    :accessor module-error-message)
   (module-error-module
    :initarg :module-error-module
    :accessor module-error-module))
  (:report
   (lambda (module-error stream)
     (format stream "~&Module: ~S~%Message: ~A~%"
             (module-error-module module-error)
             (module-error-message module-error)))))

(define-condition module-already-loaded (module-error)
  ())

(define-condition missing-module (module-error)
  ())

(define-condition malformed-module (module-error)
  ()
  (:documentation "Signalled when an attempt is made to find the module, the package is 
found but the symbol *module* is unbound."))

;;;api related errors

(define-condition api-error (moonbot-condition)
  ((api-error-error
    :accessor api-error-error
    :initarg :api-error-error
    :initform "default"
    :type string)
   (api-error-code
    :accessor api-error-code
    :initarg :api-error-code
    :initform "default"
    :type string)
   (api-error-args
    :accessor api-error-args
    :initform "default"
    :initarg :api-error-args)
   (api-error-description
    :accessor api-error-description
    :initarg :api-error-description
    :initform "default"))
  (:report
   (lambda (api-error stream)
     (format stream "~&Error code: ~S~%Error value: ~S~%Description: ~S~%~A"
             (api-error-code api-error)
             (api-error-error api-error)
             (api-error-description api-error)
             (when (api-error-args api-error)
               (format nil "Args: ~A~%" (api-error-args api-error)))))))

(define-condition moonbot-still-running (moonbot-condition)
  ((moonbot-still-running-message
    :accessor moonbot-still-running-message
    :initarg :moonbot-still-running-message
    :initform nil)))

;;;message parsing erros

(define-condition message-process-failure (moonbot-condition)
  ((message-process-failure-message
    :accessor message-process-failure-message
    :initform nil
    :initarg message-process-failure-message)
   (message-process-failure-culprit
    :accessor message-process-failure-culprit
    :initform nil
    :initarg message-process-failure-culprit))
  (:report
   (lambda (con stream)
     (format stream "Failed to process message. Culprit: ~A~%Message: ~A~%"
             (message-process-failure-culprit con)
             (message-process-failure-message con)))))

(define-condition unknown-message-type (message-process-failure)
  ((message-process-failure-type
    :accessor message-process-failure-type
    :initarg :message-process-failure-type))
  (:documentation "signalled when a message is received with an unexpected message 
type"))

(define-condition invalid-prefix (message-process-failure)
  ())

(define-condition missing-invoker (message-process-failure)
  ())

(define-condition missing-command (message-process-failure)
  ())

(define-condition missing-expected-key (message-process-failure)
  ())

(define-condition failed-to-parse-msg (message-process-failure)
  ())

(define-condition already-processed (message-process-failure)
  ())

(define-condition total-failure (message-process-failure)
  ((total-failure-caught-condition
    :accessor total-failure-caught-condition
    :initarg :total-failure-caught-condition)))

(define-condition invalid-command (moonbot-condition)
  ((invalid-command-message
    :accessor invalid-command-message
    :initarg :invalid-command-message
    :type string)
   (invalid-command-command
    :accessor invalid-command-command
    :initarg :invalid-command-command)))

(define-condition invalid-arguments (invalid-command)
  ((invalid-arguments-arguments
    :accessor invalid-arguments-arguments
    :initarg :invalid-arguments-arguments)))

(define-condition invalid-argument-count (invalid-arguments)
  ((invalid-argument-count-count
    :accessor invalid-argument-count-count
    :initarg :invalid-argument-count-count)
   (invalid-argument-count-expected
    :accessor invalid-argument-count-expected
    :initarg :invalid-argument-count-expected))
  (:documentation "Signalled when the user doesn't send the exact number of arguments
expected"))

(define-condition api-timeout (moonbot-condition)
  ((api-timeout-message
    :accessor api-timeout-message
    :initarg :api-timeout-message)
   (api-timeout-condition
    :accessor api-timeout-condition
    :initarg :api-timeout-condition))
  (:report
   (lambda (con stream)
     (format stream "Connection Timeout.~%Message: ~A~%Original condition: ~A~%"
             (api-timeout-message con)
             (api-timeout-condition con)))))

(define-condition api-no-connection (api-timeout)
  ())

(define-condition m-forbidden (api-error)
  ((api-error-description
    :initform "Forbidden access, e.g. joining a room without permission, failed login.")))

(define-condition m-unknown-token (api-error)
  ((api-error-description
    :initform "The access token specified was not recognised.
An additional response parameter, soft_logout, might be present on the response for 401 HTTP status codes. See the soft logout section for more information.")))

(define-condition m-missing-token (api-error)
  ((api-error-description
    :initform
    "No access token was specified for the request.")))

(define-condition m-bad-json (api-error)
  ((api-error-description
    :initform
    "Request contained valid JSON, but it was malformed in some way, e.g. missing required keys, invalid values for keys.")))

(define-condition m-not-json (api-error)
  ((api-error-description
    :initform "Request did not contain valid JSON.")))

(define-condition m-not-found (api-error)
  ((api-error-description
    :initform "No resource was found for this request.")))

(define-condition m-limit-exceeded (api-error)
  ((api-error-description
    :initform "Too many requests have been sent in a short period of time. Wait a while then try again.")))

(define-condition m-unknown (api-error)
  ((api-error-description
    :initform "An unknown error has occurred.")))

(define-condition m-unrecognized (api-error)
  ((api-error-description
    :initform "The server did not understand the request.")))

(define-condition m-unauthorized (api-error)
  ((api-error-description
    :initform "The request was not correctly authorized. Usually due to login failures.")))

(define-condition m-invalid-param (api-error)
  ((api-error-description
    :initform "A parameter is invalid")))

(define-condition m-room-in-use (api-error)
  ((api-error-description
    :initform "Attempting to a use a room that already exists")))

(define-condition m-bad-state (api-error)
  ((api-error-description
    :initform "The state change requested cannot be performed, such as attempting to unban a user who is not banned")))

(defparameter *string->condition* (make-hash-table :test #'equal))

(defun add-string->condition (string condition-sym)
  (setf (gethash string *string->condition*) condition-sym))

(defun get-string->condition (string)
  (let ((condition (gethash string *string->condition*)))
    (unless condition
      (error (format nil "Condition for ~S not defined" string)))
    condition))

(defmacro pkv (plist key)
  `(getf ,plist ,key))

(add-string->condition "M_FORBIDDEN" 'm-forbidden)
(add-string->condition "M_UNKNOWN_TOKEN" 'm-unknown-token)
(add-string->condition "M_MISSING_TOKEN" 'm-missing-token)
(add-string->condition "M_BAD_JSON" 'm-bad-json)
(add-string->condition "M_NOT_JSON" 'm-not-json)
(add-string->condition "M_NOT_FOUND" 'm-not-found)
(add-string->condition "M_LIMIT_EXCEEDED" 'm-limit-exceeded)
(add-string->condition "M_UNKNOWN" 'm-unknown)
(add-string->condition "M_UNRECOGNIZED" 'm-unrecognized)
(add-string->condition "M_UNAUTHORIZED" 'm-unauthorized)
(add-string->condition "M_INVALID_PARAM" 'm-invalid-param)
(add-string->condition "M_ROOM_IN_USE" 'm-room-in-use)
(add-string->condition "M_BAD_STATE" 'm-bad-state)

(defun signal-condition-from-response (response)
  (let* ((code (getf response :|errcode|))
         (error-val (getf response :|error|))
         (args (getf response :|retry_after_ms|))
         (condition (get-string->condition code)))
    (error condition :api-error-code code
                     :api-error-error error-val
                     :api-error-args args)))

