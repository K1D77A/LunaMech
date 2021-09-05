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
  (push user-id (ubermensch moonbot))
  (format t "Added ~A to list of ubermensch~%" user-id))

(new-admin-command remove-ubermensch ((user-id :valid-user))
    "Removes an ubermensch."
  (with-accessors ((ubermensch ubermensch))
      moonbot
    (setf ubermensch (remove user-id ubermensch :test #'string=))
    (format t "Removed ~A from list of ubermensch~%" user-id)))

(new-admin-command print-ubermensch ()
    "Prints the ubermensch."
  (with-accessors ((ubermensch ubermensch))
      moonbot
    (format t "Ubermensch: ~{~A ~}" (mapcar #'clean-user-id ubermensch))))

(new-admin-command list-communities ()
    "Prints all the communities."
  (with-accessors ((communities communities))
      moonbot
    (format t "~{~A~%~}" communities)))

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

(new-admin-command create-local-community ((community-name (:maxlen 50)
                                                           (:minlen 1)))
    "Creates a new community by name. Don't include spaces."
  (with-accessors ((communities communities))
      moonbot    
    (let* ((sender (pkv message :|sender|))
           (new-community (make-instance 'community
                                         :extra nil :members (list sender)
                                         :listen-in (list room)
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
  (admin-shadow-ban-user-id (connection community) user-id)
  (format t "Success."))

(new-admin-command deactivate-account ((user-id))
    "Deactivates USER-ID's account."
  (format t "Attempting to deactivate ~A's account" user-id)
  (admin-deactivate-account (connection community) user-id)
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
