(in-package #:mm-module.twitter)

(defparameter *char-count* 280)

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


(defun twitter-api-list (list)
  (and (find :room-id list)
       (find :api-key list)
       (find :api-secret list)
       (find :access-token list)
       (find :access-secret list)
       (find :pin list)
       (find :means list)
       (find :composer list)))

(deftype twitter-api-list () `(satisfies twitter-api-list))

(defun twitter-api-list->object (list)
  "Convert a twitter-api-list into a twitter-api object"
;;  (check-type list twitter-api-list)
  (make-instance 'twitter-api
                 :room-id (getf list :room-id)
                 :api-key (getf list :api-key)
                 :api-secret (getf list :api-secret)
                 :access-token (getf list :access-token)
                 :access-secret (getf list :access-secret)
                 :pin (getf list :pin)
                 :means (getf list :means)
                 :composer (getf list :composer)))

(defun twitter-api-object->list (obj)
  "Convert a twitter-api object into a twitter-api-list"
  (with-accessors ((room-id room-id)
                   (api-key api-key)
                   (api-secret api-secret)
                   (access-token access-token)
                   (access-secret access-secret)
                   (pin pin)
                   (means means)
                   (composer composer))
      obj
    `(:room-id ,room-id :api-key ,api-key
      :api-secret ,api-secret
      :access-token ,access-token :access-secret ,access-secret
      :pin ,pin :means ,means :composer ,composer)))

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
                 :key #'name :test #'string-equal)
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
                                             (special-room-room-ids) sync)))
    (when rooms
      (alexandria:doplist (room events rooms)
        (when events
          (process-events room events))))))

(defun new-twitter-api (api-key api-secret access-token access-secret)
  (let ((url (chirp:initiate-authentication :api-key api-key :api-secret api-secret)))
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
                     :api-key api-key))))

(command-defining-macro-moonbot new-twitter-command 'twitter-command)

(new-twitter-command help ()
    "attempts an explanation of how commands work"
  (moonhelp 'twitter-command community room))
