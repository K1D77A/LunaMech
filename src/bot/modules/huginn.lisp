(defpackage #:mm-module.huginn
  (:use #:cl #:matrix-moonbot)
  (:export #:huginn-command
           #:huginn-module
           #:*module*))

(in-package #:mm-module.huginn)

(defparameter *huginn-privileged* ())

(defmodule huginn (mm-module.huginn HUGINN normie-privilege)
           huginn-command ()
           huginn-module ())

(defclass api-object ()
  ((username
    :initarg :username
    :type string)
   (message
    :initarg :message
    :type string)
   (pfp
    :initarg :pfp))
  (:documentation "This is used to send nicely formatted json to the Huginn api"))

(defmethod jojo:%to-json ((obj api-object))
  (jojo:with-object
    (jojo:write-key-value "username" (slot-value obj 'username))
    (jojo:write-key-value "message" (slot-value obj 'message))))
   ;; (jojo:write-key-value "pfp" (slot-value obj 'pfp))))

(defmethod on-load-up (moonbot (module huginn-module))
  (log:info "Loading compass results from huginn-privilege.lisp")
  (results-from-file))

(defmethod on-save (moonbot (module huginn-module))
  (log:info "Saving privileged Huginn users to huginn-privilege.lisp")
  (save-results)
  t)

(defun save-results ()
  (when *huginn-privileged*;dont write empty results
    (alexandria:write-string-into-file
     (format nil "~S" *huginn-privileged*) "config/huginn-privilege.lisp"
     :if-does-not-exist :create
     :if-exists :overwrite)))

(defun results-from-file ()
  (handler-case
      (let ((form (uiop:safe-read-file-form "config/huginn-privilege.lisp")))
        (setf *huginn-privileged* form))
    (file-error ()
      (warn "config/huginn-privilege.lisp does not exist.")
      nil)))

(defmethod locate-command ((module huginn-module) (priv ubermensch-privilege)
                           invoker community)
  (or (type-find 'huginn-command invoker *commands*
                 :key #'name :test #'string-equal)
      (error 'missing-command)))

(defmethod locate-command ((module huginn-module) priv invoker community)
  "When prefix is #.huginn with no privileges then look for the command and then
check to see if the userid is found in *huginn-privileged*. if command is not found
then signal 'missing-command, if command is found but the user is not privileged
then signal 'missing-command"
  (let ((command (type-find 'huginn-command invoker *commands*
                            :key #'name :test #'string-equal)))
    (unless command
      (error 'missing-command))
    (if (find (user-id priv) *huginn-privileged* :test #'string=)
        command
        (error 'missing-command))))

(defmethod inform-command-is-missing
    ((priv ubermensch-privilege) (module huginn-module) community room)
  "UBER & ADMIN COMMAND"
  (moon-message community room
                (moonmat 
                 "Admin, that huginn command is missing, try #.huginn help")))

(defmethod execute-command ((moonbot moonbot) (priv ubermensch-privilege)
                            (command huginn-command)
                            community room message rest)
  (if (string-equal (first rest) "help")
      (print-command-information command community room)
      (safe-execution command community room message rest)))

(defmethod inform-command-is-missing
    (priv (module huginn-module) community room)
  ""
  (when (find (user-id priv) *huginn-privileged*)
    (moon-message community room
                  (moonmat
                   "User, that huginn command is missing, try #.huginn help"))))

(defmethod inform-command-is-missing
    ((priv admin-privilege) (module huginn-module) community room)
  "ADMIN & ADMIN command."
  nil)

(command-defining-macro-no-moonbot new-huginn-command
                                   'huginn-command)

(new-huginn-command help ()
    "attempts an explanation of how commands work"
  (moon-message community room
                (format nil "~{~A~}"
                        (moonhelp 'huginn-command community room))))

(new-huginn-command echo ((to-send (:maxlen 100) (:minlen 1)))
    "Echos back the message object"
  (moonmat-message community room "message: ~A" to-send))

(new-huginn-command send ((to-send (:maxlen 100)(:minlen 1)))
    "Sends a message from the sender to the Huginn agent"
  (let* ((dirty-id (pkv message :|user_id|))
         (id (subseq (first (str:split ":" dirty-id)) 1)))
    ;;(pfp (second (user-profile-url (connection community) dirty-id)))
    ;;(pfp-bytes (download-content (connection community) pfp)))
    (dex:post "http://automation.scyldings.com/users/3/web_requests/376/moon-net"
              :content (jojo:to-json
                        (make-instance 'api-object
                                       :username id
                                       :message (format nil "~{~A ~}" args))))
    ;;:pfp pfp-bytes)))
    (moonmat-message community room"Sent")))



