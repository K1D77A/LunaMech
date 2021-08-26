(defpackage #:mm-module.twitter
  (:use #:cl #:matrix-moonbot)
  (:export #:twitter-command
           #:twitter-module
           #:*module*))

(in-package #:mm-module.twitter)

#||
This module aims to achieve a relatively simple goal, we want the bot to grab all image 
events from certain rooms and upload them to certain twitter accounts. 
In its current state the bot is able to grab image events, download the images and then 
upload them to the twitter-api associated with that rooms id.
||#


(defmodule twitter (mm-module.twitter TWITTER ubermensch-privilege)
           twitter-command ()
           twitter-module  ((special-rooms
                             :accessor special-rooms
                             :initform nil
                             :type list
                             :documentation
                             "A list of twitter-api objects. Used for
 associating a room with 
a twitter account so that images posted are automatically uploaded to that account")))

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
    :type string))
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
   (ext
    :accessor ext
    :initarg :ext
    :type string
    :documentation "The extension of the uploaded file.")
   (mxc
    :accessor mxc
    :initarg :mxc
    :type sring
    :documentation "The matrix content URL used to download the content.")
   (content
    :accessor content
    :documentation "The downloaded content"))
  (:documentation "This class is used to store a little bit of information about image 
events that are sent to specific rooms. These events are used to download, and submit 
these image events to Twitter."))


(defun twitter-api-list (list)
  (and (find :room-id list)
       (find :api-key list)
       (find :api-secret list)
       (find :access-token list)
       (find :access-secret list)
       (find :pin list)))

(deftype twitter-api-list () `(satisfies twitter-api-list))

(defun twitter-api-list->object (list)
  "Convert a twitter-api-list into a twitter-api object"
  (check-type list twitter-api-list)
  (make-instance 'twitter-api
                 :room-id (getf list :room-id)
                 :api-key (getf list :api-key)
                 :api-secret (getf list :api-secret)
                 :access-token (getf list :access-token)
                 :access-secret (getf list :access-secret)
                 :pin (getf list :pin)))

(defun twitter-api-object->list (obj)
  "Convert a twitter-api object into a twitter-api-list"
  (check-type obj twitter-api)
  (with-accessors ((room-id room-id)
                   (api-key api-key)
                   (api-secret api-secret)
                   (access-token access-token)
                   (access-secret access-secret)
                   (pin pin))
      obj
    `(:room-id ,room-id :api-key ,api-key
      :api-secret ,api-secret
      :access-token ,access-token :access-secret ,access-secret
      :pin ,pin)))

(defmacro with-twitter-api (room-var twitter-api &body body)
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
       ,@body)))

(defun save-results ()
  "Converts all of the twitter-api objects into twitter-api-lists and saves them into 
'config/twitter-config.lisp'"
  (when (special-rooms *module*);dont write empty results
    (alexandria:write-string-into-file
     (format nil "~S"
             (mapcar #'twitter-api-object->list
                     (special-rooms *module*)))
     "config/twitter-config.lisp"
     :if-does-not-exist :create
     :if-exists :supersede)))

(defun results-from-file ()
  "Grabs the list objects from 'config/twitter-config.lisp' and converts them into 
twitter-api objects and stores within (special-rooms *module*). The lists within 
that file must be of type twitter-api-list otherwise will signal a type condition"
  (handler-case
      (let ((form (uiop:safe-read-file-form "config/twitter-config.lisp")))
        (setf (special-rooms *module*)
              (mapcar #'twitter-api-list->object form)))
    (file-error ()
      (warn "config/twitter-config.lisp does not exist.")
      nil)))

(defmethod on-load-up (moonbot (module twitter-module))
  (log:info "Loading Twitter config from twitter-config.lisp")
  (results-from-file))

(defmethod on-save (moonbot (module twitter-module))
  (log:info "Saving Twitter config to twitter-config.lisp")
  (save-results)
  t)

(defmethod locate-command ((module twitter-module) (priv ubermensch-privilege)
                           invoker community)
  "When prefix is 'admin with no privileges search for twitter-command"
  (or (type-find 'twitter-command invoker *commands*
                 :key #'name :test #'sym-name-equal)
      (error 'missing-command)))

(defmethod locate-command ((module twitter-module) priv invoker community)
  "When prefix is twitter with no privilege just signal 'missing-command"
  (error 'missing-command))

(defmethod execute-command ((moonbot moonbot) (priv ubermensch-privilege)
                            (command twitter-command)
                            community room message rest)
  (if (string-equal (first rest) "help")
      (print-command-information command community room)
      (safe-execution command community room message rest moonbot)))

(defmethod inform-command-is-missing
    ((priv admin-privilege) (module twitter-module) community room)
  "ADMIN & TWITTER command."
  nil)

(defun write-to-temp-file (array path)
  (alexandria:write-byte-vector-into-file array path
                                          :if-exists :supersede
                                          :if-does-not-exist :create))

(defun form-tweet (sender)
  "Create a nice human readable text for a tweet using SENDER"
  (format nil "Sent by: ~A" (subseq (first (str:split ":" sender)) 1)))

(defun form-response (chirp-text)
  "Create a nice human readable response to the person who uploaded their image. Uses 
CHIRP-TEXT which is the text returned from submitting a new status."
  (format nil "Thank you for submitting your artwork. Tweet: ~A"
          (first (last (str:split " " chirp-text)))))

(defun special-room-room-ids ()
  "Extract all of the room-ids from (special-rooms *module*)"
  (mapcar #'room-id (special-rooms *module*)))

(defun find-api-from-room-id (room-id)
  "Find a twitter-api object using ROOM-ID"
  (find room-id (special-rooms *module*) :key #'room-id :test #'string=))

(defmethod on-sync (luna (mod twitter-module) sync)
  "Using on-sync we need to check for m.room.message events within the rooms marked for 
automatic image upload. If some are found then we need to extract all of the senders and 
MXC's for that room and then upload them to the twitter API, this is done by first 
downloading the image from Matrix, saving it to a tmp file and then passing the pathname 
to chirp."
  (let ((rooms (find-types-in-rooms-timeline '("m.room.message")
                                             (special-room-room-ids) sync))
        (media nil))
    (when rooms
      (alexandria:doplist (room events rooms)
        (setf media (append media (collect-images room events)))))
    (when media;time to upload
      (mapc (lambda (media-obj)
              (with-twitter-api room-id (find-api-from-room-id (room-id media-obj))
                (let* ((con (conn luna))
                       (content (download-content con (mxc media-obj)))
                       (path (pathname (format nil "/tmp/mm-module-twitter.~A"
                                               (ext media-obj)))))
                  (setf (content media-obj)
                        content)
                  (write-to-temp-file content path);hacky time
                  (unwind-protect 
                       (handler-case 
                           (let ((status 
                                   (chirp:statuses/update-with-media
                                    (form-tweet (sender media-obj))
                                    path)))
                             (send-message-to-room con room-id 
                                                   (form-response
                                                    (chirp-objects:text status))))
                         (condition ()
                           (send-message-to-room
                            con room-id
                            "[Luna] I encountered an error while uploading, sorry :(")))
                    (handler-case (delete-file path)
                      (file-error ()
                        nil))))))
            media))))

(defun collect-images (room-id events)
  "Maps over the events within EVENTS and looks for events whose msgtype is m.image, when 
found converts them all into 'media objects and returns them in a list. ROOM-ID is stored 
within the media object so that the correct twitter-api object can be found."
  (let ((media nil))
    (mapc (lambda (event)
            (let ((sender (getf event :|sender|))
                  (content (getf event :|content|)))
              (when (or (string= (getf content :|msgtype|) "m.image")
                        (string= (getf content :|msgtype|) "m.video"))
                (push (make-instance 'media :sender sender
                                            :mxc (getf content :|url|)
                                            :room-id room-id
                                            :ext (car
                                                  (last
                                                   (str:split #\. (getf content :|body|)
                                                              :omit-nulls t))))
                      media))))
          events)
    media))

(defun new-twitter-api (api-key api-secret access-token access-secret)
  (let ((chirp-extra:*oauth-api-key* api-key)
        (chirp-extra:*oauth-api-secret* api-secret)
        (chirp-extra:*oauth-access-token* access-token)
        (chirp:*oauth-access-secret* access-secret))
    (let ((url (chirp:initiate-authentication)))
      (format *query-io* "Please go to the URL ~A and input the key after authenticating.~%"
              url)
      (format *query-io* "Please enter the PIN you received after authenticating followed ~
                          by return.~%")
      (let ((pin (read-line *query-io*)))
        (chirp:complete-authentication pin)
        (chirp:account/verify-credentials)
        (make-instance 'twitter-api
                       :pin pin
                       :access-secret access-secret
                       :access-token access-token
                       :api-secret api-secret
                       :api-key api-key)))))

(command-defining-macro-moonbot new-twitter-command 'twitter-command)

(new-twitter-command help ()
    "attempts an explanation of how commands work"
  (moonhelp 'twitter-command community room))
