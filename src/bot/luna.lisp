(in-package #:matrix-moonbot)


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

(defmethod lunamech-restart ((lunamech lunamech))
  (handler-case 
      (login lunamech t)
    (condition (c)
      (log:error "Restart error: ~A" c)
      (log:error "Failed to login, retrying in 5 seconds")
      (sleep 5)
      (lunamech-restart lunamech))))

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
    (log:error c)
    (call-next-method)))

(defmethod handle-conditions (lunamech (c api-timeout))
  (log:error "Socket condition: ~A" (api-timeout-condition c))
  (log:error "Attempting a restart of Lunamech")
  (lunamech-restart lunamech)
  (when (find-restart 'try-again)
    (log:error "Successful reconnection")
    (invoke-restart 'try-again)))

(defmethod handle-conditions (lunamech (c api-no-connection))
  (log:error "Socket condition: ~A" c)
  (log:error "Waiting and trying to restart.")
  (lunamech-restart lunamech)
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
  (lunamech-restart lunamech)
  (when (find-restart 'try-again)
    (log:error "Successful reconnection")
    (invoke-restart 'try-again)))

(defmethod handle-conditions (lunamech c)
  (log:error "Unhandled condition signalled~% ~A~
              Attempting to restart in 5 seconds" c)
  (lunamech-restart lunamech)
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
                              (*lunamech* . ,lunamech)
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
             (start lunamech))))

(defmethod stop ((lunamech lunamech))
  (setf (stopp lunamech) t)
  (ignore-errors 
   (report-to-matrix "I have been told to shutdown"))
  (log:info "Waiting for Lunamech to stop on its own")
  (let ((err-count 0))
    (handler-bind ((lunamech-still-running
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
                    (error 'lunamech-still-running
                           :lunamech-still-running-message
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


(defmethod find-module ((lunamech lunamech) sym &optional (silent t))
  "Takes in a symbol SYM and looks within the alist (modules LUNAMECH) for a 
corresponding package name, then looks for *module* within that package."
  (restart-case
      (handler-case
          (progn (unless silent
                   (log:info "Searching for module ~A" sym))
                 (let ((package (cdr (assoc sym (modules lunamech) :test #'string-equal))))
                   (unless package
                     (error 'missing-module
                            :module-error-module sym
                            :module-error-message "Cant find module (package missing)"))
                   (handler-case
                       (prog1 (symbol-value (find-symbol "*MODULE*" package))
                         (unless silent
                           (log:info "Module found for " sym)))
                     (unbound-variable ()
                       (error 'malformed-module
                              :module-error-module package
                              :module-error-message
                              "Module doesn't have *module* variable."))))))
    (ignore ()
      :report "can't find module, ignore it?"
      (log:error "Cannot find module associated with sym: ~S. Discarding" sym)
      nil)))

(defmethod find-modules ((lunamech lunamech))
  "Loops through (modules LUNAMECH) and attempts to resolve all of the *module* variables 
for each (name . package) pair."
  (handler-bind ((module-error
                   (lambda (c)
                     (declare (ignore c))
                     (invoke-restart 'ignore))))
    (setf (found-modules lunamech)
          (remove-if #'null
                     (loop :for (name . package) :in (modules lunamech)
                           :collect
                           (handler-case
                               (symbol-value (find-symbol "*MODULE*" package))
                             (unbound-variable ()
                               nil)))))))

(defmethod module-loaded-p ((lunamech lunamech) module)
  (and (find module (found-modules lunamech)) t))

(defmethod hotload-module ((lunamech lunamech) sym)
  "Load a module into Lunamech while Lunamech is running. SYM should be the prefix
of the module. If SYM is not a valid module signals 'missing-module. If module is
found but already loaded then 'module-already-loaded is signalled."
  (format t "Attempting hotload of module denoted by ~A~%" sym)
  (log:info "Attempting hotload of module denoted by ~A" sym)
  (with-accessors ((found-modules found-modules))
      lunamech
    (let ((module (find-module lunamech sym)))
      (when (module-loaded-p lunamech module)
        (error 'module-already-loaded
               :module-error-module module
               :module-error-message "Already found module in Lunamech"))
      (push module found-modules) 
      (on-module-hotload lunamech module)
      (format t "Hotload of ~S~%Successful~%" module)
      (log:info "Hotload of ~A~%Successful" module))))



(defmethod sym->module-name ((lunamech lunamech) sym)
  (cdr (assoc sym (modules lunamech) :test #'string=)))

(defmethod unload-module :around (lunamech sym)
  (log:info "Attempting to unload module denoted by ~A from Lunamech" sym)
  (report-to-matrix (format nil "Attempting to unload module denoted by ~A from Lunamech" sym))
  (call-next-method)
  (report-to-matrix "Module successfully unloaded")
  (log:info "Module successfully unloaded"))

(defmethod unload-module ((lunamech lunamech) sym)
  "Unload a module denoted by SYM from Lunamech. If the modules associated with SYM,
cannot be found then the condition 'missing-module is signalled."
  (let ((module-package (sym->module-name lunamech sym)))
    (with-accessors ((found found-modules)
                     (modules modules))
        lunamech
      (let ((module (find-module lunamech sym t)))
        (progn
          (on-module-unload lunamech module)
          (setf modules (remove module-package modules :key #'car)
                found (remove module found :test #'eq)))
        (format t "Module successfully unloaded~%")))))

(defmethod unload-module ((lunamech lunamech) (mod module))
  (on-module-unload lunamech mod)
  (setf (found-modules lunamech)
        (remove mod (found-modules lunamech))))
