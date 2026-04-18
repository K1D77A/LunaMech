(in-package #:matrix-moonbot)

;;;parse the communities.lisp file into an object.

(defparameter *default-config-location* #P"./config/")

(defparameter *config-file* "communities.lisp")
(defparameter *backup-config-file* "communities.lisp.backup")

#|| Josh 4/18/26
Now I will adjust luna to generate multiple instances of 'luna' based on what is
the lunas directory
||#

(defparameter *lunas-directory* (merge-pathnames "lunas/" *default-config-location*))

(defun grab-configs (&optional (luna-dir *lunas-directory*)
                       (other-config-dir *default-config-location*))
  (values (uiop:subdirectories luna-dir)
          (uiop:directory-files other-config-dir)))

(defun grab-latest-config (directory)
  (let ((files (uiop:directory-files directory)))
    (or (find *config-file* files :key #'file-namestring :test #'equal)
        (error 'config-missing :config-missing-message
               (progn
                 (log:error "The default config file is missing")
                 (format nil "Tried looking for 'communities.lisp' and ~
                            and the latest backup in ~A but ~
                            I could not find it. Either move your config ~
                            to this directory or make a new one." directory))))))

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

(defun grab-config (directory)
  "Loads in the file 'communities.lisp'. Establishes the restart 'use-backup' 
that can be used to grab the backup config."
  (restart-case
      (uiop:safe-read-file-form (grab-latest-config directory))
    (use-backup ()
      :report "Default config missing; try backup?"
      (use-backup-config directory))))

(defun msoc (key list &optional (signal-condition #'warn))
  (let ((val (second (assoc key list :test #'eql))))
    (when (and signal-condition (not val))
      (funcall signal-condition 'expected-key-missing-from-config
               :missing-key-expected-key key
               :missing-key-message
               (format nil "I am missing this key")))
    val))

(defun list-community->community-object (name args)
  (destructuring-bind (&key aliases top-level-space admins extra)
      args
    (make-instance 'community :extra extra
                              :admins admins
                              :name name
                              :aliases aliases
                              :top-level-space top-level-space)))

(defun password-from-file (directory)
  (str:remove-punctuation
   (alexandria:read-file-into-string (merge-pathnames directory "lunamechpass.lisp"))))

(defun config->connection (list directory)
  (destructuring-bind (&key username url api)
      list 
    (make-instance 'connection :password (password-from-file directory)
                               :username username
                               :api api
                               :url url)))

(defun add-correct-con-to-community (community connections)
  (let* ((curl (url community))
         (con (find curl connections :key #'url :test #'string=)))
    (setf (connection community) con)))

(defun config->luna (directory)  
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
                (grab-config directory))))
    (destructuring-bind (&key permissions login modules uber-rooms communities)
        con
      (let ((communities (mapcar (lambda (community)
                                   (let ((name (first community)))
                                     (list-community->community-object name
                                                                       (second community))))
                                 communities))
            (connection (config->connection login directory)))
        (make-instance 'luna :communities communities
                             :config-path directory
                             :name (first (last (pathname-directory directory)))
                             :connection connection
                             :permissions permissions 
                             :modules modules 
                             :uber-rooms uber-rooms)))))

(defun global-config-to-list (luna)
  (with-accessors ((permissions permissions)
                   (modules modules)
                   (uber-rooms uber-rooms))
      luna
    (list :luna-config
          (list :PERMISSIONS permissions)
          (list :MODULES modules)
          (list :UBER-ROOMS uber-rooms))))

(defun object-to-list ((luna luna))
  (append (list (global-config-to-list luna))
          (mapcar #'object-to-list (communities luna))))

(defun object-to-list (community)
  (with-accessors ((username username)
                   (commands commands)
                   (from from)
                   (name name)
                   (top-level-space top-level-space)
                   (admins admins)
                   (aliases aliases)
                   (extra extra))
      community
    `(,name
      ((:aliases ,aliases)
       (:top-level-space ,top-level-space)
       (:admins ,admins)
       (:extra ,extra)))))

(defun luna->config (moonbot &optional (dir *default-config-location*))
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


(defun luna->config%new (luna)
  "use cl-binary-store to store luna to disk."
  (let ((path (pathname (format nil "~Aluna.bin" (config-path luna)))))
    (log:info "Storing:~%~A~% at: ~S" luna path)
    (cl-binary-store:store path luna)))

(defun cleanup-restored-luna (luna)
  (with-accessors ((connection connection)
                   (config-path config-path)
                   (timestamp timestamp)
                   (stopp stopp)
                   (thread thread)
                   (controller-thread controller-thread))
      luna
    (with-accessors ((url url)
                     (username username)
                     (api api))
        connection
      (setf connection
            (make-instance 'connection :username username
                                       :password (password-from-file config-path)
                                       :url url)
            timestamp (local-time:now)
            stopp nil)
      luna)))

(defun config->luna%new (path)
  (log:info "Restoring luna from: ~S" path)
  (let ((luna (cl-binary-store:restore path)))
    (log:info "Cleaning up restored instance of Luna")
    (cleanup-restored-luna luna)
    (log:info "Restored:~%~A" luna)
    luna))


(defun ark->config (ark)
  (with-accessors ((global-modules global-modules)
                   (lunas lunas))
      ark                    
    (let ((path (merge-pathnames "ark.bin" *default-config-location*)))
      (log:info "Storing ark:~%~A~%at: ~S" ark path)
      (cl-binary-store:store path global-modules)
      (mapc #'luna->config%new lunas)
      (log:info "Done~%~A" ark))))

(defun config->ark ()
  (log:info "Restoring Ark")
  (let ((configs (mapcar (lambda (path)
                           (merge-pathnames "luna.bin" path))
                         (grab-configs))))
    (log:info "Lunas in the ark: ~S" configs)
    (let* ((lunas (mapcar #'config->luna%new configs))
           (ark-path (merge-pathnames "ark.bin" *default-config-location*))
           (modules (cl-binary-store:restore ark-path))
           (ark 
             (make-instance 'lunas-ark :global-modules modules :lunas lunas)))
      (log:info "Ark restored:~%~A" ark)
      ark)))



