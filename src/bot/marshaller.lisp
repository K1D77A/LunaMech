(in-package #:lunamech)

;;;parse the communities.lisp file into an object.

(defparameter *default-config-location* #P"./config/")

#|| Josh 4/18/26
Now I will adjust luna to generate multiple instances of 'luna' based on what is
the lunas directory
||#

(defparameter *lunas-directory* (merge-pathnames "lunas/" *default-config-location*))

(defparameter *blank-config-name* "blank.conf")

(defmethod dont-serialize-slots ((o lunamech))
  '(lunamech-matrix-api/v2:filters
    found-modules
    thread
    unloaded-modules
    pause-gate))

(defmethod dont-serialize-slots ((o community))
  '(rooms rooms-spellcheck members
    lunamech-matrix-api/v2:filters %locks))

(defun slots-to-serialize (type)
  (let ((slot-names
          (mapcar #'c2mop:slot-definition-name
                  (c2mop:class-direct-slots (find-class type)))))
    (set-difference slot-names
                    (dont-serialize-slots (c2mop:class-prototype (find-class type))))))


(let ((slots ()))
  (defmethod cl-binary-store:serializable-object-info ((o (eql 'community)))
    (if slots
        (values slots nil)
        (values (setf slots (slots-to-serialize o)) nil))))

(let ((slots ()))
  (defmethod cl-binary-store:serializable-object-info ((o (eql 'lunamech)))
    (if slots
        (values slots nil)
        (values (setf slots (slots-to-serialize o)) nil))))

(let ((slots '(lunamech-matrix-api/v2:username
               lunamech-matrix-api/v2:password
               lunamech-matrix-api/v2:url
               lunamech-matrix-api/v2:api)))
  (defmethod cl-binary-store:serializable-object-info ((o (eql 'lunamech-matrix-api/v2:connection)))
    (values slots nil)))



(defun grab-configs (&optional (luna-dir *lunas-directory*)
                       (other-config-dir *default-config-location*))
  (values (uiop:subdirectories luna-dir)
          (uiop:directory-files other-config-dir)))

(defun password-from-file (directory)
  (str:remove-punctuation
   (alexandria:read-file-into-string (merge-pathnames directory "lunamechpass.lisp"))))

(defun config->connection (list directory)
  (apply #'make-instance 'connection :password (password-from-file directory)
         list))

(defun luna->config (luna)
  "use cl-binary-store to store luna to disk."
  (let ((path (pathname (format nil "~Alunamech.bin" (config-path luna)))))
    (log:info "Storing:~%~A~% at: ~S" luna path)
    (cl-binary-store:store path luna :track-references nil)))

(defmethod cleanup ((o community))
  (setf (rooms o) ()
        (members o) ()
        (filters o) nil
        (rooms-spellcheck o) (make-hash-table :test #'equal)
        (slot-value o '%locks) (apply #'make-locks (locks-for-object o))))


        

(defmethod cleanup ((luna lunamech))
  (with-accessors ((connection connection)
                   (config-path config-path)
                   (timestamp timestamp)
                   (communities communities)
                   (stopp stopp)
                   (thread thread)
                   (found-modules found-modules)
                   (unloaded-modules unloaded-modules)
                   (controller-thread controller-thread)
                   (pause-gate pause-gate))
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
            stopp nil
            found-modules nil
            unloaded-modules (make-hash-table :test #'equalp)
            (slot-value luna '%locks) (apply #'make-locks (locks-for-object luna))
            pause-gate (sb-concurrency:make-gate :name "Luna gate" :open t)))
    (mapc #'cleanup communities)
    luna))

(defun config->luna (path)
  (when (probe-file path)
    (log:info "Restoring luna from: ~S" path)
    (let ((luna (cl-binary-store:restore path)))
      (log:info "Cleaning up restored instance of Luna")
      (cleanup luna)
      (log:info "Restored:~%~A" luna)
      luna)))

(defmethod cleanup ((ark lunas-ark))
  (setf (global-modules ark) nil
        (timestamp ark) (local-time:now)))
                    
(defun ark->config (ark)
  (with-accessors ((wanted-modules wanted-modules)
                   (lunas lunas))
      ark                    
    (let ((path (merge-pathnames "ark.bin" *default-config-location*)))
      (log:info "Storing ark:~%~A~%at: ~S" ark path)
      (cl-binary-store:store path wanted-modules :track-references nil)
      (mapc #'luna->config lunas)
      (log:info "Done~%~A" ark))))

(defun ark-exists-p ()
  (probe-file (merge-pathnames "ark.bin" *default-config-location*)))

(defun config->ark ()
  (log:info "Restoring Ark")
  (let ((configs (mapcar (lambda (path)
                           (merge-pathnames "lunamech.bin" path))
                         (grab-configs))))
    (log:info "Lunas in the ark: ~S" configs)
    (let* ((lunas (mapcar #'config->luna configs))
           (ark-path (merge-pathnames "ark.bin" *default-config-location*))
           (modules (cl-binary-store:restore ark-path))
           (ark (make-instance 'lunas-ark :wanted-modules modules
                                          :lunas (remove nil lunas))))
      (log:info "Ark restored:~%~A" ark)
      ark)))

(defun build-ark-from-blanks ()
  (log:info "Checking for blanks...")
  (let ((blanks (grab-blank-configs)))
    (log:info "Blanks? ~S" blanks)
    (if blanks
        (let* ((lunas (mapcar 'blank-config->luna blanks))
               (ark (make-instance 'lunas-ark :global-modules () :lunas lunas)))
          (ark->config ark)
          (log:info "Done."))
        (error "Missing blanks. Please follow instructions in readme.md for initial setup."))))
          
(defun grab-blank-configs ()
  (let ((blanks ()))
    (mapc (lambda (path)
            (let ((file (first (uiop:directory-files path *blank-config-name*))))
              (when file
                (push (cons path (list (uiop:safe-read-file-form file))) blanks))))
          (grab-configs))
    blanks))

(defun list-community->community-object (name args)
  (destructuring-bind (&key aliases top-level-space admins extra)
      args
    (make-instance 'community :extra extra
                              :admins admins
                              :name name
                              :aliases aliases
                              :top-level-space top-level-space)))

(defun blank-config->luna (alist)
  (destructuring-bind (directory config)
      alist 
    (destructuring-bind (&key permissions login modules uber-rooms communities)
        config
      (let ((communities (mapcar (lambda (community)
                                   (let ((name (first community)))
                                     (list-community->community-object name
                                                                       (second community))))
                                 communities))
            (connection (config->connection login directory)))
        (make-instance 'lunamech :communities communities
                                 :config-path directory
                                 :name (first (last (pathname-directory directory)))
                                 :connection connection
                                 :permissions permissions 
                                 :wanted-modules (if (find "admin" modules :test #'string-equal)
                                                     modules 
                                                     (list* "admin" modules))
                                 :uber-rooms uber-rooms)))))
