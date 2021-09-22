(in-package #:matrix-moonbot)

;;;parse the communities.lisp file into an object.

(defparameter *default-config-location* #P"config/")

(defparameter *config-file* "communities.lisp")
(defparameter *backup-config-file* "communities.lisp.backup")

(defun grab-latest-config (&optional (dir *default-config-location*))
  (let ((files (uiop:directory-files dir)))
    (or (find *config-file* files :key #'file-namestring :test #'equal)
        (error 'config-missing :config-missing-message
               (progn
                 (log:error "The default config file is missing")
                 (format nil "Tried looking for 'communities.lisp' and ~
                            and the latest backup in ~A but ~
                            I could not find it. Either move your config ~
                            to this directory or make a new one." dir))))))

(defun use-backup-config (dir)
  (log:error "Attempting to use backup config")
  (let ((files (uiop:directory-files dir)))
    (or (find *backup-config-file* files :key #'file-namestring :test #'equal)
        (error 'config-missing :config-missing-message
               (progn (log:error
                       "The default and backup config file is missing")
                      (format nil "if you are seeing this message then both your ~
                              communities.lisp and communities.lisp.backup files are
                              missing. Please recreate either and try again."))))))

(defun grab-config (&optional (dir *default-config-location*))
  "Loads in the file 'communities.lisp'. Establishes the restart 'use-backup' 
that can be used to grab the backup config."
  (restart-case
      (uiop:safe-read-file-form (grab-latest-config dir))
    (use-backup ()
      :report "Default config missing; try backup?"
      (use-backup-config dir))))

(defun msoc (key list &optional (signal-condition #'warn))
  (let ((val (second (assoc key list :test #'eql))))
    (when (and signal-condition (not val))
      (funcall signal-condition 'expected-key-missing-from-config
               :missing-key-expected-key key
               :missing-key-message
               (format nil "I am missing this key")))
    val))

(defun list-community->community-object (list)
  (let* ((name (first list));this is pretty slow but who cares.
         (data (second list))
         (url (msoc :url data #'error))
         (api (msoc :api data #'error))
         (space (msoc :top-level-space data))
         (admins (msoc :admins data))
         (aliases (msoc :aliases data nil));no need to worry if they dont have one
         (username (msoc :username data #'error))
         ;; (rooms (msoc :rooms data))
         ;; (members (msoc :members data))
         (extra (msoc :extra data nil)))
    ;;I could possibly do some mop wizardry here
    (make-instance 'community :extra extra :members nil :rooms nil
                              :admins admins :api api
                              :url url :name name :username username :aliases aliases
                              :top-level-space space)))

(defun password-from-file ()
  (str:remove-punctuation
   (alexandria:read-file-into-string "config/lunamechpass.lisp")))

(defmethod community->connection ((community community))
  (with-accessors ((url url)
                   (api api)
                   (username username))
      community
    (make-instance 'connection :password (password-from-file) :username username
                               :api api :url url)))

(defun add-correct-con-to-community (community connections)
  (let* ((curl (url community))
         (con (find curl connections :key #'url :test #'string=)))
    (setf (connection community) con)))

(defun config->luna (&optional (dir *default-config-location*))
  (let* ((restartedp nil)
         (con (handler-bind
                  ((config-missing
                     (lambda (c)
                       (declare (ignore c))
                       (unless restartedp
                         (setf restartedp t)
                         (invoke-restart 'use-backup))
                       nil))
                   (end-of-file
                     (lambda (c)
                       (declare (ignore c))
                       (unless restartedp
                         (log:error
                          "It appears your communities.lisp file ~
                            is broken because and End of File was ~
                              reached. Chances are you haven't closed your ~
                             parens correctly. Now I'll attempt to use your backup.")
                         (setf restartedp t)
                         (invoke-restart 'use-backup)))))
                (grab-config dir)))
         (global-config (first con))
         (communities (mapcar #'list-community->community-object (rest con)))
         (connections (mapcar #'community->connection
                              (remove-duplicates communities :key #'url
                                                             :test #'string=))))
    ;;need some error stuff here
    (mapcar (lambda (community)
              (add-correct-con-to-community community connections))
            communities)
    (make-instance 'moonbot :communities communities
                            :connections connections
                            :ubermensch (second (second global-config))
                            :modules (second (third global-config))
                            :uber-rooms (second (fourth global-config)))))

(defmethod global-config-to-list (moonbot)
  (with-accessors ((ubermensch ubermensch)
                   (modules modules)
                   (uber-rooms uber-rooms))
      moonbot
    (list :GLOBAL-CONFIG
          (list :UBERMENSCH ubermensch)
          (list :MODULES modules)
          (list :UBER-ROOMS uber-rooms))))

(defmethod object-to-list ((moonbot moonbot))
  (append (list (global-config-to-list moonbot))
          (mapcar #'object-to-list (communities moonbot))))

(defmethod object-to-list ((community community))
  (with-accessors ((username username)
                   (commands commands)
                   (from from)
                   (name name)
                   (url url)
                   (api api)
                   (top-level-space top-level-space)
                   (admins admins)
                   (aliases aliases)
                   (rooms rooms)
                   (members members);;might be able to just yeet this entire option
                   (extra extra))
      community
    `(,name
      ((:url ,url)
       (:api ,api)
       (:username ,username)
       (:aliases ,aliases)
       (:top-level-space ,top-level-space)
       (:admins ,admins)
     ;;  (:rooms ,rooms)
      ;; (:members ,members)
       (:extra ,extra)))))

(defun moonbot->config (moonbot &optional (dir *default-config-location*))
  "Accepts an instance of MOONBOT as its first argument, backs up the old config 
by renaming it to *backup-config-file* and then overwrites *config-file* with a
a list version of moonbot generated with (object-to-list MOONBOT). DIR is the
default location where *config-file* is located."
  (let* ((object (object-to-list moonbot))
         (config (format nil "~A/~A" dir *config-file*)))
    (unwind-protect 
         (rename-file config *backup-config-file*)
      (alexandria:write-string-into-file (format nil "~S" object) config
                                         :if-exists :overwrite
                                         :if-does-not-exist :create))))

