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

(defun grab-config (directory)
  "Loads in the file 'communities.lisp'. Establishes the restart 'use-backup' 
that can be used to grab the backup config."
  (uiop:safe-read-file-form (grab-latest-config directory)))

(defun password-from-file (directory)
  (str:remove-punctuation
   (alexandria:read-file-into-string (merge-pathnames directory "lunamechpass.lisp"))))

(defun config->connection (list directory)
    (apply #'make-instance 'connection :password (password-from-file directory)
           list))

(defun luna->config (luna)
  "use cl-binary-store to store luna to disk."
  (let ((path (pathname (format nil "~Alunamech.conf" (config-path luna)))))
    (log:info "Storing:~%~A~% at: ~S" luna path)
    (cl-binary-store:store path luna :track-references nil)))

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

(defun config->luna (path)
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
    (let ((path (merge-pathnames "ark.conf" *default-config-location*)))
      (log:info "Storing ark:~%~A~%at: ~S" ark path)
      (cl-binary-store:store path global-modules :track-references nil)
      (mapc #'luna->config lunas)
      (log:info "Done~%~A" ark))))

(defun config->ark ()
  (log:info "Restoring Ark")
  (let ((configs (mapcar (lambda (path)
                           (merge-pathnames "lunamech.conf" path))
                         (grab-configs))))
    (log:info "Lunas in the ark: ~S" configs)
    (let* ((lunas (mapcar #'config->luna configs))
           (ark-path (merge-pathnames "ark.conf" *default-config-location*))
           (modules (cl-binary-store:restore ark-path))
           (ark (make-instance 'lunas-ark :global-modules modules :lunas lunas)))
      (log:info "Ark restored:~%~A" ark)
      ark)))



