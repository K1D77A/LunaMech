(in-package #:matrix-moonbot)

(defparameter *luna* "Thread local instance of lunamech :)")

(defmethod permissions :around ((lunamech lunamech))
  (quicklock (lunamech :permissions)
    (call-next-method)))

(defmethod thread :around ((lunamech lunamech))
  (quicklock (lunamech :thread)
    (call-next-method)))

(defmethod found-modules :around ((lunamech lunamech))
  (quicklock (lunamech :found-modules)
    (call-next-method)))

;; (defmethod (setf found-modules) :after (new-val (lunamech lunamech))
;;   "Remove duplicate found-modules after adding/removing one."
;;   (with-slots (found-modules)
;;       lunamech
;;     (with-slots (modules)
;;         (make-instance 'module)
;;       (let ((no-dupes (remove-duplicates found-modules :test #'eq)))
;;         (setf found-modules no-dupes)


(defmethod (setf permissions) :after (new-val (lunamech lunamech))
  (setf (slot-value lunamech 'permissions)
        (clean-permissions-tree lunamech)))

(defmethod find-room-by-id ((lunamech lunamech) id)
  "Searches all of the communities within LUNAMECH for a room that matches ID, then returns
it. If none are found then returns nil"
  (loop :for community :in (communities lunamech)
          :thereis (loop :for room-plist :in (rooms community)
                         :if (string= id (getf room-plist :id))
                           :return room-plist)))

(defmethod find-room-by-name ((lunamech lunamech) name)
  "Searches all of the communities within LUNAMECH for a room that matches NAME, then returns
it. If none are found then returns nil"
  (loop :for community :in (communities lunamech)
          :thereis (loop :for room-plist :in (rooms community)
                         :if (string= name (getf room-plist :name))
                           :return room-plist)))

(defgeneric add-new-alias (alias luna community))

(defmethod add-new-alias (alias (lunamech lunamech) (community community))
  (check-type alias keyword)
  (unless (loop :for community :in (communities lunamech)
                  :thereis (find alias (aliases community)))
    (push alias (aliases community))))

(defmethod find-community ((community-name symbol) (lunamech lunamech))
  (find community-name (communities lunamech) :key #'name))

(defmethod find-community ((community-name string) (lunamech lunamech))
  (find (intern (string-upcase community-name) :keyword) (communities lunamech) :key #'name))


(defmethod login ((lunamech lunamech) &optional (relog nil))
  "Attempts to log Lunamech's connections into all their appropriate servers. In the event that
a password is incorrect (a m-forbidden is received) then invokes the restart 'new-password
in an attempt to get the user to enter a new password. If the flag RELOG is set to t, then
Lunamech will not evaluate any initiating functions and will login using the same device-id."
  (check-type relog boolean)
  (with-accessors ((connection connection))
      lunamech 
    (handler-bind ((m-forbidden
                     (lambda (c)
                       (with-accessors ((err api-error-error))
                           c
                         (if (string-equal err "invalid password")
                             (progn (log:error "Bad password.")
                                    (invoke-restart
                                     'new-password (new-password)))
                             (log:error 
                              "Unknown error: ~A" c))))))      
      (login connection)
      (unless relog;only perform this on an initial login
        (first-sync connection)))))
        ;;              (initiate-encryption con)
        ;;              (initiate-filters con))))


(defmethod logged-in-p ((lunamech lunamech))
  (logged-in-p (connection lunamech)))

(defmethod lrestart ((lunamech lunamech))
  (handler-case 
      (login lunamech t)
    (condition (c)
      (log:error "Restart error: ~A" c)
      (log:error "Failed to login, retrying in 5 seconds")
      (sleep 5)
      (lrestart lunamech))))

(defmethod initiate-spellcheckers ((lunamech lunamech))
  (mapc #'initiate-room-spellchecker (communities lunamech)))

(defmethod thread-maintainer ((lunamech lunamech))
  (catch 'bail;used to forcefully stop the thread
    (tagbody main
       (handler-bind ((condition
                        (lambda (c)
                          (sleep 1)
                          (handle-conditions lunamech c))))
         (restart-case 
             (listen-and-process lunamech)
           (listen-and-process ()
             :report "Just listen-and-process again?"
             (go main)))))))

(defmethod is-me-p (lunamech (string string))
  "Returns t if STRING matches any of the user-id's associated with open connections."  
  (string-equal (user-id (connection lunamech)) string))

(defmethod is-me-p (lunamech (priv me-privilege))
  t)

(defmethod is-me-p (lunamech s)
  nil)

(defgeneric handle-conditions (lunamech c)
  (:method :around (lunamech c)
    (ignore-errors (log:error c))
    (call-next-method)))

(defmethod handle-conditions (lunamech (c api-timeout))
  (log:error "Socket condition: ~A" (api-timeout-condition c))
  (log:error "Attempting a restart of Lunamech")
  (lrestart lunamech)
  (when (find-restart 'try-again)
    (log:error "Successful reconnection")
    (invoke-restart 'try-again)))

(defmethod handle-conditions (lunamech (c api-no-connection))
  (log:error "Socket condition: ~A" c)
  (log:error "Waiting and trying to restart.")
  (lrestart lunamech)
  (when (find-restart 'try-again)
    (log:error "Successful reconnection")
    (invoke-restart 'try-again)))
  
(defmethod handle-conditions (lunamech (c api-error))
  (log:error "API Error made it through.")
  (cond ((find-restart 'listen-and-process)
         (log:error "Restarting listen-and-process..")
         (invoke-restart 'listen-and-process))        
        ((find-restart 'try-again)
         (log:error "Trying again...")
         (invoke-restart 'try-again))
        (t (log:error "Unable to find either restart... going to die."))))

(defmethod handle-conditions (lunamech (c m-unknown))
  (log:error "Unknown error")
  (lrestart lunamech)
  (when (find-restart 'try-again)
    (log:error "Successful reconnection")
    (invoke-restart 'try-again)))

(defmethod handle-conditions (lunamech c)
  (log:error "Unhandled condition signalled~% ~A~
              Attempting to restart in 5 seconds" c)
  (lrestart lunamech)
  (when (find-restart 'listen-and-process)    
    (invoke-restart 'listen-and-process)))

(defmethod initiate-communities ((lunamech lunamech))
  (log:info "Initiating ")
  (funcall (if (parallel-p lunamech)
               #'lparallel:pmapc
               #'mapc)
           (lambda (community)
             (initiate-rooms lunamech community)
             (initiate-members lunamech community))
           (communities lunamech))
  (log:info "Communities initiated."))

(defmethod initiate-rooms ((lunamech lunamech) (community community))
  (with-accessors ((top-level-space top-level-space)
                   (name name))
      community
    (log:info "Collecting rooms for " name)
    (if top-level-space
        (handler-case
            (catch-potential-conditions 
              (let ((rooms (rooms-in-a-space (conn lunamech) top-level-space)))
                (log:info "- Found ~D room~:P" (length rooms))
                (setf (rooms community) rooms)
                (log:info "- Done")))
          (m-forbidden ()
            (log:warn "Forbidden from checking for the rooms within Space ~A please remedy."
                      top-level-space)))
        (log:warn "No top-level-space set for ~A ignoring.." name))))

(defmethod initiate-members ((lunamech lunamech) (community community))
  (with-accessors ((rooms rooms)
                   (name name)
                   (members members))
      community
    (log:info "Collecting users for " name)
    (dolist (room rooms)
      (destructuring-bind (&key name id &allow-other-keys)
          room
        (log:info "- Checking for users in ~A" name)
        (handler-case
            (catch-potential-conditions 
              (let ((member-hash (gethash "joined" (members-in-room-ids (conn lunamech) id))))
                (maphash (lambda (member hash)
                           (declare (ignore hash))
                           (pushnew member members :test #'string=))
                         member-hash)))
          (m-forbidden (c)
            (log:warn "Forbidden " c)
            nil))))
    (log:info "- Found ~D member~:P" (length members))))


(defmethod start ((lunamech lunamech))
  "Attempts to startup Lunamech. This is done by first finding all of the modules listed 
in (modules CONFIG), this executes on-load-up on each of these modules. Next It checks if
Lunamech is already logged in, if so then it resets the cycle-history and then starts up the 
primary listening thread and executes 'on-restart' on each found module. 
If Lunamech is not logged in then evaluates (login <lunamech>), executes 'on-login' for each 
found-module and then recalls start."
  (let ((*luna* lunamech))
    (unless (found-modules lunamech)
      (find-modules lunamech)
      (mapc-found-modules lunamech #'on-load-up))
    (if (logged-in-p lunamech)
        (progn
          (mapc-found-modules lunamech #'on-restart)
          (log:info "Setting stopp to nil")
          (setf (stopp lunamech) nil)
          (log:info "Resetting cycle-history")
          (setf (cycle-history lunamech) nil)
          (log:info "Starting primary thread")
          (setf 
           (thread lunamech)                    
           (bt2:make-thread
            (lambda ()
              (thread-maintainer lunamech))
            :name (format nil "~A-lunamech-main" (name lunamech))
            :initial-bindings `((*package* . ,*package*)
                                (*error-output* . ,*error-output*)
                                (*standard-output* . ,*standard-output*)
                                (dex:*connection-pool* . ,dex:*connection-pool*)
                                (*luna* . ,lunamech)
                                ,@bt2:*default-special-bindings*)))
          (log:info "Done.")
          (mapc (lambda (room-id)
                  (module-moonmat-message (conn lunamech) room-id "I have started."))
                (uber-rooms lunamech))
          (log:info "Startup complete.")
          lunamech)
        (progn (login lunamech)
               (log:info "Starting lparallel kernel now with ~r workers." 2)
               (mapc-found-modules lunamech #'on-login)
               (initiate-communities lunamech)
               (start lunamech)))))

(defmethod stop ((lunamech lunamech))
  (setf (stopp lunamech) t)
  (ignore-errors 
   (report-to-matrix "I have been told to shutdown"))
  (log:info "Waiting for Lunamech to stop on its own")
  (let ((err-count 0))
    (handler-bind ((luna-still-running
                     (lambda (c)
                       (declare (ignore c))
                       (incf err-count)
                       (if (= err-count 2);;wait 20 seconds total
                           (invoke-restart 'force-stop)
                           (progn
                             "Waiting another 10 seconds"
                             (invoke-restart 'wait-10))))))
      (wait-for-stop lunamech)))
  (log:info "Stopped")
  (setf (stopp lunamech) nil))

(defmethod force-stop ((lunamech lunamech))
  "In the event that Lunamech won't stop on her own then this can be evaluated and it will
interrupt the main execution thread and cause Lunamech to stop."
  (mapc (lambda (mod) (on-shutdown lunamech mod)) (found-modules lunamech))
  (ignore-errors
   (bt2:interrupt-thread (thread lunamech)
                         (lambda () (throw 'bail nil)))))

(defmethod wait-for-stop ((lunamech lunamech))
  "Lunamech stops by checking if the accessor stopp is non nil, 
if it is then it'll return, however because Lunamech currently only runs on a 
single thread it is possible that it could take quite a while
 for it to complete a full cycle of command executions,
say 3 rooms all send an invite command at once it could take quite sometime for 
lunamech to get done processing its current batch of messages. This method will
wait 60 seconds (by default) for Lunamech to stop executing, however two restarts
are provided, the first 'force-stop' does exactly that, it forces Lunamech to stop
by interrupting the thread and telling it to jump to an encapsulating tag, the 
second 'wait-60' recalls 'wait-for-stop' making it wait another 60 seconds."
  (let ((x 0))
    (restart-case
        (loop :while (bt2:thread-alive-p (thread lunamech))
              :do (sleep 5)
                  (incf x 5)
                  (log:info "Lunamech still running. Waited: ~D seconds" x)
                  (when (= x 10);10 seconds
                    (error 'luna-still-running
                           :luna-still-running-message
                           (format nil "Lunamech is still running despite ~
                                          waiting 10 seconds"))))
      (force-stop ()
        :report "Lunamech is still running, force stop?"
        (log:error "Lunamech is still running, force stopping.")
        (force-stop lunamech))
      (wait-10 ()
        :report "Lunamech is still running, wait 10 seconds?"
        (log:error "Lunamech is still running, waiting 10 seconds")
        (wait-for-stop lunamech)))))

#||
Modules
||#

(defmethod find-unloaded-module ((lunamech lunamech) (module module))
  (gethash (name module) (unloaded-modules lunamech)))

(defmethod find-unloaded-module ((lunamech lunamech) (module-name string))
  (gethash module-name (unloaded-modules lunamech)))

(defmethod find-module ((lunamech lunamech) sym &optional (silent t))
  "Attempt to find a module denoted by sym within LUNAMECH within the found-modules slot."
  (restart-case
      (progn (unless silent
               (log:info "Searching for module ~A" sym))
             (with-accessors ((found-modules found-modules))
                 lunamech 
               (or (find sym found-modules
                         :test #'string-equal
                         :key #'name)
                   (error 'missing-module
                          :module-error-module sym
                          :module-error-message "Cant find module"))))
    (ignore ()
      :report "can't find module, ignore it?"
      (log:error "Cannot find module associated with sym: ~S. Discarding" sym)
      nil)))

(defmethod find-modules ((lunamech lunamech))
  "Loops through (wanted-modules LUNAMECH) and makes an instance of each of them.
   This should only be called on initial startup."
  (handler-bind ((module-error
                   (lambda (c)
                     (declare (ignore c))
                     (invoke-restart 'ignore))))
    (with-accessors ((found-modules found-modules))
        lunamech 
      (setf found-modules ())
      (let ((to-load ()))
        (loop :for wanted-module :in (remove-duplicates (wanted-modules lunamech))
              :do (let ((module-descriptor (find-module nil wanted-module)))
                    (if module-descriptor
                        (progn
                          (let ((instance (make-instance (getf module-descriptor :module-class))))
                            (ensure-directories-exist (module-persistent-path lunamech instance))
                            (log:info "Found module: ~A" (getf module-descriptor :name))
                            (push instance to-load)))
                        (log:warn "Couldn't find module by name: ~S" wanted-module))))
        (setf found-modules to-load)
        (log:info "Found modules:~%~S" to-load)))))

(defmethod module-loaded-p ((lunamech lunamech) module)
  (and (find module (found-modules lunamech)) t))

(defmethod load-module ((lunamech lunamech) module-name)
    "Load a module into Lunamech while Lunamech is running."
  (format t "Attempting load of module denoted by ~A~%" module-name)
  (log:info "Attempting load of module denoted by ~A" module-name)
  (let ((module (ignore-errors (find-module lunamech module-name))))
    (if module 
        (error 'module-already-loaded
               :module-error-module module-name
               :module-error-message "Already found module in Lunamech")
        (let ((descriptor (find-module nil module-name)))
          (if descriptor
              (let ((unloaded? (find-unloaded-module lunamech module-name)))
                (if unloaded?
                    (progn (load-module lunamech unloaded?)
                           (format t "Reload of ~A~%Successful~%" (string-downcase module-name))
                           (log:info "Reload of ~S~%Successful" module-name))
                    (let ((instance (make-instance (getf descriptor :module-class))))
                      (load-module lunamech instance)                          
                      (format t "Load of ~A~%Successful~%" (string-downcase module-name))
                      (log:info "Load of ~S~%Successful" module-name))))
              (progn 
                (format t "Couldn't find module ~A~%" (string-downcase module-name))
                (log:info "Couldn't find module ~S" module-name)))))))
 
(defmethod unload-module ((lunamech lunamech) mod-name)
  "Unload a module denoted by SYM from Lunamech. If the modules associated with SYM,
cannot be found then the condition 'missing-module is signalled."
  (let* ((module (ignore-errors (find-module lunamech mod-name))))
    (if module
        (progn
          (unload-module lunamech module)
          (log:info "Module ~S successfully unloaded" mod-name)
          (format t "Module ~A successfully unloaded" (string-downcase mod-name)))
        (progn
          (log:info "Couldn't find module in Luna named: ~A" mod-name)
          (format t "Couldn't find module in Luna named: ~A" (string-downcase mod-name))))))




(defmethod unload-module ((lunamech lunamech) (mod module))
  (with-accessors ((found-modules found-modules)
                   (unloaded-modules unloaded-modules)
                   (wanted-modules wanted-modules))
      lunamech 
    (setf found-modules
          (remove mod found-modules)
          (gethash (name mod) unloaded-modules)
          mod
          wanted-modules
          (remove (name mod) wanted-modules :test #'string-equal)))
  (on-module-unload lunamech mod))

;;the name is fucked.. need to give modules actual names probably just strings.

(defmethod load-module ((lunamech lunamech) (mod module))
  (with-accessors ((found-modules found-modules)            
                   (wanted-modules wanted-modules)
                   (unloaded-modules unloaded-modules))
      lunamech
    (remhash (name mod) unloaded-modules)
    (setf found-modules
          (push mod found-modules)
          wanted-modules
          (pushnew (string-downcase (string (name mod))) wanted-modules :test #'string-equal)))
  (on-module-load lunamech mod))
            

