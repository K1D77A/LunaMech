(defpackage #:mm-module.admin
  (:use #:cl #:matrix-moonbot)
  (:export #:admin-command
           #:admin-module
           #:*module*))

(in-package #:mm-module.admin)

(defmodule admin (mm-module.admin ADMIN ubermensch-privilege)
           admin-command ()
           admin-module ())

(defmethod locate-command ((module admin-module) (priv ubermensch-privilege)
                           invoker community)
  "When prefix is 'admin with no privileges search for admin-command"
  (or (type-find 'admin-command invoker *commands*
                 :key #'name :test #'string-equal)
      (error 'missing-command)))

(defmethod locate-command ((module admin-module) priv invoker community)
  "When prefix is admin with no privilege just signal 'missing-command"
  (error 'missing-command))

(defmethod execute-command ((moonbot moonbot) (priv ubermensch-privilege)
                            (command admin-command)
                            community room message rest)
  (if (string-equal (first rest) "help")
      (print-command-information command community room)
      (safe-execution command community room message rest moonbot)))

(defmethod inform-command-is-missing
    (priv (module admin-module) community room)
  ""
  nil)

(defmethod inform-command-is-missing
    ((priv admin-privilege) (module admin-module) community room)
  "ADMIN & ADMIN command."
  nil)

(command-defining-macro-moonbot new-admin-command 'admin-command)

(new-admin-command help ()
    "attempts an explanation of how commands work"  
  (moonhelp 'admin-command community room))

(new-admin-command add-ubermensch ((user-id :valid-user))
    "Adds a new ubermensch."
  (make-ubermensch moonbot user-id)
  (format t "Added ~A to list of ubermensch~%" user-id))

(new-admin-command add-module-admin ((user-id :valid-user)
                                     (module (:maxlen 50)
                                             (:minlen 1)))
    "Adds a new module admin."
  (make-module-admin moonbot user-id module)
  (format t "Added ~A to module administrators for ~A~%" user-id module))

(new-admin-command remove-module-admin ((user-id :valid-user)
                                        (module (:maxlen 50)
                                                (:minlen 1)))
    "Adds a new module admin."
  (retract-module-admin moonbot user-id module)
  (format t "Removed ~A from module administrators for ~A~%" user-id module))

(new-admin-command remove-ubermensch ((user-id :valid-user))
    "Removes an ubermensch."
  (if (can-remove-ubermensch-p moonbot)
      (remove-ubermensch moonbot user-id)
      (error 'cannot-perform-action
             :cannot-perform-action-action "Remove the last ubermensch"
             :cannot-perform-action-message "I cannot remove the last ubermensch."))
  (format t "Removed ~A from list of ubermensch~%" user-id))

(new-admin-command print-ubermensch ()
    "Prints the ubermensch."
  (format t "Ubermensch: ~{~A ~}" (mapcar #'clean-user-id (all-ubermensch moonbot))))

(new-admin-command print-permissions ()
    "Prints the permissions tree."
  (format t "~S" (permissions moonbot)))

(new-admin-command users-permissions ((user-id :valid-user))
    "Prints the permissions tree for USER-ID."
  (format t "~S" (find-permissions moonbot user-id)))

(new-admin-command accept-invites-from ((user-id :valid-user))
    "Luna will accept invites from USER-ID."
  (accept-invites-from moonbot user-id)
  (format t "Luna will now accept invites from ~A" user-id))

(new-admin-command stop-accepting-invites-from ((user-id :valid-user))
    "Luna will no longer accept invites from USER-ID."
  (stop-accepting-invites-from moonbot user-id)
  (format t "Luna will no longer accept invites from ~A" user-id))




(new-admin-command list-communities ()
    "Prints all the communities."
  (with-accessors ((communities communities))
      moonbot
    (format t "~{~A~%~}" (mapcar #'name communities))))

(new-admin-command copy-admins ((from :valid-community)(to :valid-community))
    "Copies the admins from community name and puts them into the other"
  (with-accessors ((communities communities))
      moonbot
    (let* ((cf (intern (string-upcase from) :keyword))
           (ct (intern (string-upcase to) :keyword))
           (c1 (find cf communities :key #'name))
           (c2 (find ct communities :key #'name)))
      (setf (admins c2) (copy-list (admins c1)))
      (format t "Set the admins of ~A to the admins of ~A" (name c2) (name c1)))))
;;broke
(new-admin-command create-local-community ((community-name (:maxlen 50)
                                                           (:minlen 1)))
    "Creates a new community by name. Don't include spaces."
  (with-accessors ((communities communities))
      moonbot    
    (let* ((sender (pkv message :|sender|))
           (new-community (make-instance 'community
                                         :extra nil :members (list sender)
                                         :admins (list sender)
                                         :api *api* :url *url*
                                         :name (intern
                                                (string-upcase community-name)
                                                :keyword)
                                         :connection (connection (first communities))
                                         :username (username (first communities))
                                         :rooms nil)))
      (pushnew new-community communities :key #'name)
      (format t "Created a new community: ~A~%" new-community))))

(new-admin-command member-count ()
    "Counts the total number of members in all communities."
  (with-accessors ((communities communities))
      moonbot
    (let* ((members (mapcar #'members communities))
           (count 
             (length 
              (remove-duplicates (apply #'append members) :test #'string=))))
      (format t "There are ~r unique member~:p~%" count))))

(new-admin-command room-count ()
    "Counts the total number of rooms in all communities."
  (with-accessors ((communities communities))
      moonbot
    (let* ((rooms (mapcar #'rooms-id communities))
           (count 
             (length 
              (remove-duplicates (apply #'append rooms) :test #'string=))))
      (format t "There are ~r unique room~:p in Luna ~%" count))))

(new-admin-command load-module ((module-prefix (:maxlen 50) (:minlen 1)))
    "Loads a new module into Luna. Currently the code has to be loaded."
  (handler-case
      (hotload-module moonbot (intern (string-upcase module-prefix)))
    (missing-module ()
      (format t "Could not find module associated with ~A" module-prefix))
    (module-already-loaded ()
      (format t "That module is already in Luna"))))

(new-admin-command unload-module ((module-prefix (:maxlen 50) (:minlen 1)))
    "Unloads a module from Luna"
  (handler-case
      (let ((mod (intern (string-upcase module-prefix))))
        (if (string= mod "ADMIN")
            (format t "You can't unload the admin module.")
            (unload-module moonbot mod)))
    (missing-module ()
      (format t "Could not find module associated with ~A" module-prefix))))

(new-admin-command shadowban ((user-id))
    "Shadow bans USER-ID"
  (format t "Attempting to Shadowban ~A" user-id)
  (lmav2:call-api (make-instance 'lmav2:admin%shadowban-user
                                 :user-id user-id
                                 :connection (connection community)))
  (format t "Success."))

(new-admin-command deactivate-account ((user-id))
    "Deactivates USER-ID's account."
  (format t "Attempting to deactivate ~A's account~%" user-id)
  (lmav2:call-api (make-instance 'lmav2:admin%deactivate-account
                                 :user-id user-id
                                 :connection (connection community)))
  (format t "Success."))

(new-admin-command add-alias ((community-name :valid-community)
                              (alias :valid-alias
                                     (:maxlen 50)
                                     (:minlen 1)))
    "Adds a new alias to community."
  (let ((relevant (find-community community-name moonbot)))
    (add-new-alias (intern (string-upcase alias) :keyword) moonbot relevant)
    (format t "Success.")))

(new-admin-command remove-alias ((community-name :valid-community)
                                 (alias (:maxlen 50)
                                        (:minlen 1)))
    "Adds a new alias to community."
  (let ((relevant (find-community community-name moonbot)))
    (setf (aliases community)
          (remove (intern (string-upcase community-name) :keyword) (aliases relevant)))
    (format t "Success.")))

(new-admin-command add-room-to-community ((community-name :valid-community))
    "Adds the room that Luna receives this command from into COMMUNITY-NAME."
  (let ((add-in (find-community community-name moonbot)))
    (add-room add-in room))
  (format t "Success."))

(new-admin-command make-user-admin-in-room ((room-id (:maxlen 50)
                                                     (:minlen 4))
                                            (user-id (:maxlen 50)
                                                     (:minlen 1)))
    "Uses the Admin API to make USER-ID an admin in ROOM-ID."
  (lmav2:call-api (make-instance 'lmav2:admin%make-user-admin-in-room
                                 :connection (conn *luna*)
                                 :room-id-or-alias (if (string-equal room-id "here")
                                                       room
                                                       room-id)
                                 :user-id user-id))
  (format t "Success."))

(new-admin-command make-me-admin-in-room ((room-id (:maxlen 50)
                                                   (:minlen 4)))
    "Uses the Admin API to make sender an admin in ROOM-ID."
  (log:info room)
  (lmav2:call-api (make-instance 'lmav2:admin%make-user-admin-in-room
                                 :connection (conn *luna*)
                                 :room-id-or-alias (if (string-equal room-id "here")
                                                       room
                                                       room-id)
                                 :user-id (gethash "sender" message)))
  (format t "Success."))


(new-admin-command force-user ((room-id (:maxlen 50)
                                        (:minlen 10))
                               (user-id (:maxlen 50)
                                        (:minlen 1)))
    "Forces USER-ID into ROOM-ID. Luna must have invite perms in that room."
  (lmav2:call-api (make-instance 'lmav2:admin%edit-users-room-membership
                                 :connection (conn *luna*)
                                 :room-id-or-alias room-id 
                                 :user-id user-id))
  (format t "Success."))

(new-admin-command force-community ((community-name :valid-community)
                                    (room-id (:maxlen 50)
                                             (:minlen 10)))
    "Forces the members within COMMUNITY-NAME into ROOM-ID. Removes all users who do not
have the same homeserver as Luna and all of those who are already in the room."
  (let* ((community (find-community community-name *luna*))
         (members-in-room (members-in-room-ids (conn *luna*) room-id))
         (remainder (set-difference (members community) members-in-room :test #'string=))
         (only-same-home (remove-if-not (lambda (username)
                                          (same-homeserver-p (conn *luna*) username))
                                        remainder)))
    (moonmat-message community room "forcing ~r member~:p into ~A"
                     (length only-same-home) room-id)
    (moon-mapc community room
               (lambda (member)
                 (catch-limit-exceeded
                   (lmav2:call-api (make-instance 'lmav2:admin%edit-users-room-membership
                                                  :connection (conn *luna*)
                                                  :room-id-or-alias room-id
                                                  :user-id member))
                   (format t "Forced ~A successfully~%" member)))
               only-same-home)
    (format t "Success.")))

(new-admin-command delete-room ((room-id (:maxlen 50)
                                         (:minlen 5)))
    "Deletes room denoted by ROOM-ID using the Admin API."
  (lmav2:call-api (make-instance 'lmav2:admin%delete-room 
                                 :connection (conn *luna*)
                                 :room-id room-id))
  (format t "Success."))
