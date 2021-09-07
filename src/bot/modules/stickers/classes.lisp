(in-package #:mm-module.sticker)



(defmodule sticker (mm-module.sticker STICKER normie-privilege)
           sticker-command ()
           sticker-module
           ((stickerpicker-url
             :accessor stickerpicker-url
             :initform "https://lunamech.com/stickerpicker/"
             :type string
             :documentation "The url of the stickerpicker.")
            (stickerpicker-testing-url
             :accessor stickerpicker-testing-url
             :initform "https://test.lunamech.com/stickerpicker/"
             :type string
             :documentation "The url of the test stickerpicker.")
            (testingp
             :accessor testingp
             :initform nil )
            (sticker-rooms
             :accessor sticker-rooms
             :initarg :sticker-rooms
             :initform '(:%TESTING "!WJvFXSrAnfoqNgwqpE:scyldings.com")
             :type list)
            (server-information
             :accessor server-information
             :initarg :server-information
             :initform (make-hash-table :test #'equal)
             :type hash-table)))

(define-condition sticker-condition (moonbot-condition)
  ((message
    :accessor message
    :initarg :message
    :initform "")))

(define-condition remote-server-condition (sticker-condition)
  ())

(define-condition missing-server-information (remote-server-condition)
  ((server
    :accessor server
    :initarg :server
    :initform ""))
  (:documentation "Signalled when a piece of information is required for a server but it 
doesn't exist.")
  (:report
   (lambda (obj stream)
     (format stream "Missing information for server ~A. Message: ~A"
             (server obj)
             (message obj)))))

(define-condition missing-server-dimensions (missing-server-information)
  ((message :initform "Missing upload dimensions for server."))
  (:documentation "signalled when trying to receive the dimensions required for a server 
but they haven't been downloaded yet, or the download failed.")
  (:report
   (lambda (obj stream)
     (format stream "Missing dimensions for server: ~A. Message: ~A"
             (server obj)
             (message obj)))))

(define-condition stickerpicker-down (remote-server-condition)
  ()
  (:documentation "Signalled when the stickerpicker is down."))

(define-condition missing-room-for-domain (sticker-condition)
  ((domain
    :accessor domain
    :initarg :domain
    :documentation "The domain searched for."))
  (:documentation "Signalled when an attempt has been made to find a sticker that 
doesn't exist.")
  (:report
   (lambda (obj stream)
     (format stream "No associated stickerpicker room found for DOMAIN: ~A"
             (domain obj)))))
