(in-package #:mm-module.twitter)

(defclass twitter-api ()
  ((room-id
    :accessor room-id
    :initarg :room-id
    :type string
    :documentation "The room id on Matrix associated with this twitter account")
   (api-key
    :accessor api-key
    :initarg :api-key
    :type string)
   (api-secret
    :accessor api-secret
    :initarg :api-secret
    :type string)
   (access-token
    :accessor access-token
    :initarg :access-token
    :type string)
   (access-secret
    :accessor access-secret
    :initarg :access-secret
    :type string)
   (pin
    :accessor pin
    :initarg :pin
    :type string)
   (means
    :accessor means
    :initarg :means
    :initform ()
    :type list
    :documentation "A list of keywords denoting the means these creds are going to be used")
   (composer
    :accessor composer
    :initarg :composer
    :initform :DEFAULT
    :type :keyword
    :documentation "Used to compose status updates for the event types."))
  (:documentation "An object used to represent all of the parts required to access a 
twitter account"))

(defclass media ()
  ((sender
    :accessor sender
    :initarg :sender
    :type string
    :documentation "The user-id of the person who sent the image.")
   (room-id
    :accessor room-id
    :initarg :room-id
    :type string
    :documentation "The room-id that the image was sent in.")
   (content
    :accessor content
    :initarg :content
    :documentation "The message content"))
  (:documentation "This class is used to store a little bit of information about image 
events that are sent to specific rooms. These events are used to download, and submit 
these image events to Twitter."))

(defclass media%uploaded (media)
  ((ext
    :accessor ext
    :initarg :ext
    :type string
    :documentation "The extension of the uploaded file.")
   (mxc
    :accessor mxc
    :initarg :mxc
    :type sring
    :documentation "The matrix content URL used to download the content.")
   (data 
    :accessor data)))

(defclass media%image (media%uploaded)
  ())

(defclass media%text (media)
  ((text
    :accessor text
    :initarg :text)))

(define-condition twitter-condition (error)
  ())

(define-condition cant-process-type (twitter-condition)
  ((event
    :accessor event 
    :initarg :event)))

(define-condition unknown-event-type (twitter-condition)
  ((event
    :accessor event
    :initarg :event)))
