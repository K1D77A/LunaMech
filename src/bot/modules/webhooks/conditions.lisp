(in-package #:mm-module.webhook)


(define-condition webhook-condition (moonbot-condition)
  ()
  (:documentation "The toplevel webhook condition."))

(define-condition missing-values (webhook-condition)
  ()
  (:documentation "Signalled when something is missing values."))

(define-condition application-missing-private-key (missing-values)
  ((application
    :accessor application
    :initarg :application
    :documentation "The application you tried to lookup."))
  (:documentation "Signalled when there is no private key associated with an application.")
  (:report
   (lambda (obj stream)
     (format stream "No private-key found for application: ~A."
             (application obj)))))

(define-condition webhook-not-found (webhook-condition )
  ((webhook
    :accessor webhook
    :initarg :webhook
    :documentation "The webhook you tried to lookup."))
  (:documentation "Signalled when webhook couldn't be found.")
  (:report
   (lambda (obj stream)
     (format stream "No webhook found with the name: ~A."
             (webhook obj)))))

(define-condition bad-private-key (webhook-condition)
  ((private-key
    :accessor private-key
    :initarg :private-key
    :documentation "The private-key provided")
   (webhook
    :accessor webhook
    :initarg :webhook
    :documentation "the webhook called."))
  (:documentation "Called when an attempt is made to execute WEBHOOK but the PRIVATE-KEY 
was wrong.")
  (:report
   (lambda (obj stream)
     (format stream "Private key ~A for webhook ~A is wrong."
             (private-key obj)
             (webhook obj)))))

(define-condition hook-validation-failed (webhook-condition)
  ((signalled
    :accessor signalled
    :initarg :signalled
    :documentation "An optional condition that is signalled when validating.")))




