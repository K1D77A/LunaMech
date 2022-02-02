(in-package #:mm-module.twitter)

#||
This file contains the code to process different types of messages received
||#

(defvar *type-map* (make-hash-table :test #'equal))

(defun new-type-map (string class)
  (setf (gethash string *type-map*) class))

(new-type-map "m.text" 'media%text)
(new-type-map "m.image" 'media%image)

(defun determine-media-type (event)
  (push event *media*)
  (let ((type (pkv event :|type|)))
    (unless (string-equal type "m.room.message")
      (error 'unknown-event-type :event event))
    (let* ((content (pkv event :|content|)))
      (gethash (pkv content :|msgtype|) *type-map*))))

(defun event->object (room-id event)
  (let ((type (determine-media-type event)))
    (unless type (error 'cant-process-type :event event))
    (let ((sender (pkv event :|sender|))
          (content (pkv event :|content|)))
      (make-instance type :room-id room-id
                          :sender sender
                          :content content))))

(defun process-events (room-id events)
  (let ((twitter-api (find-api-from-room-id room-id)))
    (mapc (lambda (hash)
            (handler-case
                (let ((event (event->object room-id hash)))
                  (unless (is-me-p *luna* (sender event))
                    (process-event event)
                    (post-to-twitter twitter-api  event)))
              (condition (c)
                (report-twitter-condition room-id c))))
          events)))

(defgeneric report-twitter-condition (room-id condition)
  (:documentation "Reports the condition caught when trying to post-to-twitter to room-id."))

(defmethod report-twitter-condition :around (room-id condition)
  (ignore-errors
   (let ((message (call-next-method)))
     (send-message-to-room (conn *luna*) room-id message))))

(defmethod report-twitter-condition (room-id (c bad-status))
  (format nil "'~A' is ~D long, it needs to be no greater than: ~D"
          (status c)
          (length (status c))
          *char-count*))

(defmethod report-twitter-condition (room-id (c send-failed))
  (format nil "An unknown error has occurred while attempting to Tweet: ~A"
          (c c)))

(defmethod report-twitter-condition (room-id (c cant-process-type))
  (format nil "I do not know how to process an event of that type.. sorry :( "))

(defmethod report-twitter-condition (room-id c)
  (format nil "An unknown error has occurred: ~A~%Please contact an administrator." c))


(defgeneric process-event (event)
  (:method-combination progn :most-specific-last))

(defmethod process-event progn ((event media%uploaded))
  (with-accessors ((content content))
      event
    (let* ((ext (pathname-type (pkv content :|body|)))
           (mxc (pkv content :|url|)))
      (setf (ext event) ext
            (mxc event) mxc)
      (let ((bytes (lmav2:download-content (conn *luna*) mxc)))
        (setf (data event) bytes)))))

(defmethod process-event progn ((event media%text))
  (with-accessors ((content content))
      event
    (setf (text event) (pkv content :|body|))))

(defgeneric supported-by-means-p (twitter-api event)
  (:documentation "Checks if EVENT is supported by TWITTER-API by looking for a 
keyword associated with that class of EVENT within (means TWITTER-API)"))

(defmethod supported-by-means-p (twitter-api (event media%uploaded))
  (member :repeat-images (means twitter-api)))

(defmethod supported-by-means-p (twitter-api (event media%text))
  (member :repeat-text (means twitter-api)))

(defun post-to-twitter (twitter-api event)
  (when (supported-by-means-p twitter-api event)
    (poster twitter-api event)))

(defmacro with-twitter-api ((room-var twitter-api) &body body)
  "Lexically rebinds all of the important variables required for Chirp to work using a 
twitter-api object. ROOM-VAR is the variable name you want the room-id accessor bound to."
  `(with-accessors ((,room-var room-id)
                    (api-key api-key)
                    (api-secret api-secret)
                    (access-token access-token)
                    (access-secret access-secret))
       ,twitter-api
     (let ((chirp-extra:*oauth-api-key* api-key)
           (chirp-extra:*oauth-api-secret* api-secret)
           (chirp-extra:*oauth-access-token* access-token)
           (chirp:*oauth-access-secret* access-secret))
       (locally ,@body))))

(defgeneric poster (twitter-api event)
  (:documentation "Posts EVENT to Twitter using the credentials in TWITTER-API"))

(defmethod poster :around (twiter-api event)
  (handler-case
      (call-next-method)
    (chirp:oauth-error (c)
      (error 'send-failed :c c))))

(defmethod poster (twitter-api (event media%text))
  (with-twitter-api (room twitter-api)
    (let ((status
            (chirp:statuses/update (compose-status twitter-api event))))
      (send-message-to-room (conn *luna*)
                            room
                            (form-response twitter-api event status)))))

(defmethod poster (twitter-api (event media%image))
  (with-accessors ((data data)
                   (ext ext))
      event
    (uiop:with-temporary-file (:stream stream
                               :pathname p
                               :element-type '(unsigned-byte 8)
                               :type ext)
      (write-sequence data stream)
      (force-output stream)
      (with-twitter-api (room twitter-api)
        (let ((status (chirp:statuses/update-with-media
                       (compose-status twitter-api event)
                       p)))
          (send-message-to-room (conn *luna*)
                                room
                                (form-response twitter-api event status)))))))

(defmethod poster (twitter-api event)
  nil)

(defun compose-status (twitter-api event)
  (let ((composer (composer twitter-api)))
    (generate-status composer twitter-api event)))

(defgeneric generate-status (composer twitter-api event)
  (:documentation "Generates a status using COMPOSER for EVENT."))

(defmethod generate-status :around (composer twitter-api event)
  (let ((status (call-next-method)))
    (validate-status status composer twitter-api event)
    status))

(defmethod generate-status (composer twitter-api (event media%image))
  (with-accessors ((sender sender))
      event
    (format nil "Sent by: ~A" (subseq (first (str:split ":" sender)) 1))))

(defmethod generate-status (composer twitter-api (event media%text))
  (text event))


(defgeneric validate-status (status composer twitter-api event)
  (:documentation "Makes sure that STATUS can actually send."))

(defmethod validate-status (status componser twitter-api event)
  (or (<= (length status) *char-count*)
      (error 'bad-status :status status)))


(defgeneric form-response (twitter-api event status)
  (:documentation "Forms the response that is sent to the submitter."))

(defmethod form-response (twitter-api (event media%image) status)
  (format nil "Thank you for submitting your artwork. Tweet: ~A"
          (first (last (str:split " " (chirp-objects:text status))))))

(defmethod form-response (twitter-api (event media%text) status)
  (format nil "I tweeted '~A' successfully" (chirp-objects:text status)))



