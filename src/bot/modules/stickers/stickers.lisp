
(in-package #:mm-module.sticker)

#||
This module aims to achieve a relatively simple goal, we want the bot to grab all image 
events from certain rooms and upload them to certain sticker accounts. 
In its current state the bot is able to grab image events, download the images and then 
upload them to the sticker-api associated with that rooms id.
||#

(defparameter *wanted-size* 64)
(defparameter *tmp-dir* #P"/tmp/stibs")

(defun temporarize-file (file)
  (format nil "~A/~A" *tmp-dir* file))

(defun save-results ()
  "Converts all of the sticker-api objects into sticker-api-lists and saves them into 
'config/sticker-config.lisp'"
  (when (sticker-rooms *module*);dont write empty results
    (alexandria:write-string-into-file
     (format nil "~S" (sticker-rooms *module*))
     "config/sticker-config.lisp"
     :if-does-not-exist :create
     :if-exists :supersede)))

(defun results-from-file ()
  "Grabs the list objects from 'config/sticker-config.lisp' and converts them into 
sticker-api objects and stores within (special-rooms *module*). The lists within 
that file must be of type sticker-api-list otherwise will signal a type condition"
  (handler-case
      (uiop:safe-read-file-form "config/sticker-config.lisp")
    (file-error ()
      (warn "config/sticker-config.lisp does not exist.")
      nil)))

(defmethod on-load-up (moonbot (module sticker-module))
  (log:info "Loading Sticker config from sticker-config.lisp")
  (results-from-file)
  (log:info "Getting information for each server from lunamech.com/stickerpicker")
  (grab-stickerpicker-information))

(defmethod on-module-hotload (moonbot (module sticker-module))
  (log:info "Getting information for each server from lunamech.com/stickerpicker")
  (results-from-file)
  (grab-stickerpicker-information))

(defmethod on-save (moonbot (module sticker-module))
  (log:info "Saving Sticker config to sticker-config.lisp")
  (save-results)
  t)

(defun get-sticker-room (server)
  (let ((room (getf (sticker-rooms *module*)
                    (intern (string-upcase server) :keyword))))
    (or room (error 'missing-room-for-domain :domain server))))

(defmethod locate-command ((module sticker-module) (priv ubermensch-privilege)
                           invoker community)
  "When prefix is 'admin with no privileges search for sticker-command"
  (or (type-find 'sticker-command invoker *commands*
                 :key #'name :test #'string-equal)
      (error 'missing-command)))

(defmethod locate-command ((module sticker-module) (priv normie-privilege)
                           invoker community)
  ""
  (or (type-find 'sticker-command invoker *commands*
                 :key #'name :test #'string-equal)
      (error 'missing-command)))

(defmethod locate-command ((module sticker-module) priv invoker community)
  "When prefix is sticker with no privilege just signal 'missing-command"
  (error 'missing-command))

(defmethod execute-command ((moonbot moonbot) (priv ubermensch-privilege)
                            (command sticker-command)
                            community room message rest)
  (if (string-equal (first rest) "help")
      (print-command-information command community room)
      (safe-execution command community room message rest moonbot)))

(defmethod execute-command ((moonbot moonbot) (priv normie-privilege)
                            (command sticker-command)
                            community room message rest)
  (if (string-equal (first rest) "help")
      (print-command-information command community room)
      (safe-execution command community room message rest moonbot)))

;;all commands have moonbot as an argument so need to be careful as they can all be invoked
;;by plebs

(defmethod inform-command-is-missing
    ((priv admin-privilege) (module sticker-module) community room)
  "ADMIN & STICKER command."
  nil)


(defmethod on-sync (luna (mod sticker-module) sync)
  "Using on-sync we need to check for m.room.message events within the rooms marked for 
automatic image upload. If some are found then we need to extract all of the senders and 
MXC's for that room and then upload them to the sticker API, this is done by first 
downloading the image from Matrix, saving it to a tmp file and then passing the pathname 
to chirp."
  (let ((rooms (find-types-in-rooms-timeline '("m.room.message")
                                             (sticker-rooms *module*) sync))
        (media nil))
    (when rooms
      (alexandria:doplist (room events rooms)
        (setf media (append media (collect-images room events)))))
    (when media
      ;;can put lparallel mapc here
      (funcall (if (parallel-p *luna*)
                   #'lparallel:pmapc
                   #'mapc)
               (lambda (media)
                 (handler-case 
                     (upload-event-to-sticker-site luna media)
                   (imagemagick-condition (c)
                     (log:error "Condition signalled when calling ImageMagick. ~
                             Make sure it is installed. ~A" c)
                     nil)
                   (processor-condition ()
                     (log:error "Caught resignalled processor error"))
                   (condition (c)
                     (log:error "Unknown error occurred. Chances are its network related. ~A"
                                c))))
               media))))
;;(print media)))

(defun mxc->server (mxc)
  (second (str:split "/" mxc :omit-nulls t)))

(defun server-info (server &key (server-condition 'missing-server-information))
  (or (gethash server (server-information *module*))
      (error server-condition :server server
                              :message "Missing information for that server.")))

(defun destructure-dimensions (server)
                                        ;if no server need a condition
  (let* ((dimensions (getf (server-info server
                                        :server-condition 'missing-server-dimensions)
                           :dimensions))
         (big (getf dimensions :big))
         (small (getf dimensions :small))
         (bigx (getf big :x))
         (bigy (getf big :y))
         (smallx (getf small :x))
         (smally (getf small :x)))
    (values bigx bigy smallx smally)))

(defun upload-event-to-sticker-site (luna event-list)
  "Destructures an event received in a room into its components parts, uses that to 
generate two HTTP post requests which it makes to the stickerpicker site. If the 
event contains an image that needs to be resized then it is resized before it is 
sent to the server. This sends two images, a big one and a small one."
  (let* ((event (getf event-list :event))
         (content (getf event :|content|))
         (url (getf content :|url|))
         (server (mxc->server url))
         (info (getf content :|info|))         
         (height (getf info :|h|))
         (width (getf info :|w|))
         (sender (getf event :|sender|))
         (filename (getf content :|body|))
         (mimetype (getf info :|mimetype|))
         (con (conn luna))
         (room-id (getf event-list :room-id)))
    (let* ((top-image (make-instance 'image-event
                                     :content-type mimetype
                                     :url url
                                     :height height :width width
                                     :filename filename
                                     :overwritep nil :user sender
                                     :server server))
           (images
             (handler-case
                 (process-image-event top-image)
               (processor-condition (c)
                 (log:error "Processor condition encountered: ~A" c)
                 (module-moonmat-message (conn *luna*) (symbol-name room-id)
                                         (format nil "Encountered a processor error ~A" c))
                 (error c)))))
      (if images 
          (handler-case
              (progn
                (alexandria:doplist (size image images)
                  (upload-image-event image))
                (module-moonmat-message con (symbol-name room-id)
                                        "Success. Uploader: ~A. Server: ~A. Name: ~A~%"
                                        sender server filename))
            (dexador:http-request-failed (c)
              (module-moonmat-message con (symbol-name room-id) "~A"
                                      (dexador.error:response-body c)))
            (condition (c)
              (module-moonmat-message con (symbol-name room-id)
                                      "Encountered ~A when processing." c)))
          (module-moonmat-message con (symbol-name room-id)
                                  "Encountered a condition when processing the image event.")))))


(defun collect-images (room-id events)
  "Maps over the events within EVENTS and looks for events whose msgtype is m.image, when 
found converts them all into 'media objects and returns them in a list. ROOM-ID is stored 
within the media object so that the correct sticker-api object can be found."
  (let ((media ()))
    (mapc (lambda (event)
            (let ((sender (getf event :|sender|))
                  (content (getf event :|content|)))
              (when (string= (getf content :|msgtype|) "m.image")
                (push (list :event event :room-id room-id) media))))
          events)
    media))

(defun get-url (key)
  (cond ((eq key :upload)
         (format nil "~Aadd"
                 (if (testingp *module*)
                     (stickerpicker-testing-url *module*)
                     (stickerpicker-url *module*))))
        ((eq key :command)
         (format nil "~Acommand-system"
                 (if (testingp *module*)
                     (stickerpicker-testing-url *module*)
                     (stickerpicker-url *module*))))))

(defun upload-image (content)
  (let ((content (jojo:to-json content :from :plist)))
    (dex:post (get-url :upload)
              :headers `(("Content-Type" . "application/json")
                         ("Authorization" .
                                          ,(mm-module.private-keys:get-key :sticker/upload)))
              :content content 
              :use-connection-pool nil)))

(command-defining-macro-moonbot new-sticker-command 'sticker-command)

(new-sticker-command help ()
    "attempts an explanation of how commands work"
  (moonhelp 'sticker-command community room))

(new-sticker-command list-stickers ()
    "Lists the names of the stickers associated with the server that uploads from this room"
  (construct-user-command-external-request room message "list-stickers" nil))

(new-sticker-command rename-sticker ((sticker-name (:maxlen 50)
                                                   (:minlen 1))
                                     (new-name (:maxlen 50)
                                               (:minlen 1)))
    "Renames a sticker associated with the name STICKER-NAME to NEW-NAME."
  (construct-user-command-external-request room message
                                           "rename-sticker"
                                           sticker-name new-name))

(new-sticker-command delete-sticker ((sticker-name (:maxlen 100)
                                                   (:minlen 1)))
    "Deletes the sticker associated with the name STICKER-NAME within the server found from the domain."
  (construct-user-command-external-request room message
                                           "delete-sticker" sticker-name))

(new-sticker-command uploaders ()
    "Checks the authorized-uploaders for the server associated with this uploading room."
  (construct-user-command-external-request room message "check-uploaders"))

(new-sticker-command administrators ()
    "Checks the administrators for the server associated with this uploading room."
  (construct-user-command-external-request room message
                                           "check-administrators" nil))

(new-sticker-command add-administrator ((username :valid-id))
    "Adds a new administrator for the server associated with this uploading room."
  (construct-user-command-external-request room message
                                           "add-administrator" username))

(new-sticker-command remove-administrator ((username :valid-id))
    "The opposite of add-administrator"
  (construct-user-command-external-request room message
                                           "remove-administrator" username))

(new-sticker-command add-uploader ((username :valid-id))
    "Adds USERNAME to the list of authorized uploaders."
  (construct-user-command-external-request room message
                                           "add-authorized-uploader" username))

(new-sticker-command remove-uploader ((username :valid-id))
    "The opposite of add-uploader."
  (construct-user-command-external-request room message
                                           "remove-authorized-uploader" username))

(defun construct-user-command-headers (room message)
  (let ((sender (getf message :|sender|))
        (server (second (str:split #\: room :omit-nulls t :limit 2))))
    `(("Content-Type" . "application/json")
      ("Token" . ,(mm-module.private-keys:get-key :sticker/user-command))
      ("Server" . ,server)
      ("Command-Type" . "user")
      ;;because of this we are locked to a per server execution, literally the domain
      ;;has to be associated with a server for this to work.
      ;;if don signed up then luna would have to be listening for commands from don
      ;;in a :plebmedia.com or whatever domain.
      ;;this isn't a problem because this is only for commands that the user would
      ;;input into Luna
      ("Uploader" . ,sender))))

(defun construct-user-command-external-request (room message command &rest args)
  (format t "~A"
          (handler-case
              (dex:post (get-url :command)
                        :headers (construct-user-command-headers room message)
                        :content (jojo:to-json
                                  (list :command command :args args))
                        :use-connection-pool nil)
            (dex:http-request-failed (c)
              (format nil "~A"  (slot-value c 'dexador.error::body))))))

(defun execute-external-command (command)
  "Executes an external command that was constructed with construct-privileged-command. 
This has wrapped the funcall with 3 restarts, unload-module, do-nothing and resignal."
  (restart-case
      (let ((retry-request (dex:retry-request 1 :interval 0)))
        (handler-bind ((CL+SSL::SSL-ERROR-SYSCALL retry-request))
          (funcall command)))
    (unload-module ();could possibly have a per feature disabling based on failures
      ;;for example if we had a variable like *enabled-uploading-p* and this was set to
      ;;nil then uploading could be disabled, although right now its not going to matter
      ;;if we fail to grab information on one part of the server we will fail on it all.
      :report "Failed to grab the information required for uploading, unload?"
      (log:error "Failed to grab the information required to upload so unloading.")
      (matrix-moonbot:unload-module matrix-moonbot::*luna* 'sticker))
    (do-nothing ()
      :report "do nothing?"
      nil)
    (resignal-condition (c)
      :report "Resignal the condition?"
      (error c))))

(defmacro invoke-X-on-failure (restart condition &body body)
  `(handler-bind ((,condition (lambda (c)
                                (declare (ignore c))
                                (invoke-restart ',restart))))
     (locally ,@body)))

(defmacro unload-on-failure ((condition) command &rest args)
  "Use this in the instance that not having that information would completely break 
the functioning of Luna's stickerpicker module."
  `(invoke-x-on-failure unload-module ,condition
     (execute-external-command (construct-privileged-command ,command ,@args))))

(defun grab-stickerpicker-information (&optional (silentp nil))
  "Attempts to grab the information regarding what the servers "
  (let* ((res (unload-on-failure (dex:http-request-failed)
                                 "download-system")))
    (if res
        (let ((plist (jojo:parse res)))
          (with-accessors ((server-information server-information))
              *module*
            (alexandria:doplist (server info plist)
              (unless silentp
                (log:info server info))
              (setf (gethash server server-information) (second plist)))
            (process-stickerpicker-information))))))

(defun process-stickerpicker-information ()
  (with-accessors ((server-information server-information)
                   (sticker-rooms sticker-rooms))
      *module*
    (maphash (lambda (key list)
               (let ((interned-server (intern (string-upcase key) :keyword)))
                 (setf (getf sticker-rooms interned-server)
                       (getf list :listening-room))))
             server-information)))

(defun construct-privileged-command (command &rest args)
  (lambda ()
    (dex:post (get-url :command)
              :headers `(("Token" . ,(mm-module.private-keys:get-key :sticker/priv-command))
                         ("Content-Type" . "application/json")
                         ("Command-Type" . "privileged"))
              :content (jojo:to-json (list :command command :args args))
              :use-connection-pool nil)))
