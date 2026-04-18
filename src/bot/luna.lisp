(in-package #:matrix-moonbot)


(defmethod login ((luna luna) &optional (relog nil))
  "Attempts to log Luna's connections into all their appropriate servers. In the event that
a password is incorrect (a m-forbidden is received) then invokes the restart 'new-password
in an attempt to get the user to enter a new password. If the flag RELOG is set to t, then
Luna will not evaluate any initiating functions and will login using the same device-id."
  (check-type relog boolean)
  (with-accessors ((connection connection))
      luna 
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


(defmethod logged-in-p ((luna luna))
  (logged-in-p (connection luna)))

(defmethod luna-restart ((luna luna))
  (handler-case 
      (login luna t)
    (condition (c)
      (log:error "Restart error: ~A" c)
      (log:error "Failed to login, retrying in 5 seconds")
      (sleep 5)
      (luna-restart luna))))

(defmethod initiate-spellcheckers ((luna luna))
  (mapc #'initiate-room-spellchecker (communities luna)))

(defmethod thread-maintainer ((luna luna))
  (catch 'bail;used to forcefully stop the thread
    (tagbody main
       (handler-bind ((condition
                        (lambda (c)
                          (sleep 1)
                          (handle-conditions luna c))))
         (restart-case 
             (listen-and-process luna)
           (listen-and-process ()
             :report "Just listen-and-process again?"
             (go main)))))))

(defmethod is-me-p (luna (string string))
  "Returns t if STRING matches any of the user-id's associated with open connections."  
  (string-equal (user-id (connection luna)) string))

(defmethod is-me-p (luna (priv me-privilege))
  t)

(defmethod is-me-p (luna s)
  nil)

(defgeneric handle-conditions (luna c)
  (:method :around (luna c)
    (log:error c)
    (call-next-method)))

(defmethod handle-conditions (luna (c api-timeout))
  (log:error "Socket condition: ~A" (api-timeout-condition c))
  (log:error "Attempting a restart of Luna")
  (luna-restart luna)
  (when (find-restart 'try-again)
    (log:error "Successful reconnection")
    (invoke-restart 'try-again)))

(defmethod handle-conditions (luna (c api-no-connection))
  (log:error "Socket condition: ~A" c)
  (log:error "Waiting and trying to restart.")
  (luna-restart luna)
  (when (find-restart 'try-again)
    (log:error "Successful reconnection")
    (invoke-restart 'try-again)))
  
(defmethod handle-conditions (luna (c api-error))
  (log:error "API Error made it through.")
  (cond ((find-restart 'listen-and-process)
         (log:error "Restarting listen-and-process..")
         (invoke-restart 'listen-and-process))        
        ((find-restart 'try-again)
         (log:error "Trying again...")
         (invoke-restart 'try-again))
        (t (log:error "Unable to find either restart... going to die."))))

(defmethod handle-conditions (luna (c m-unknown))
  (log:error "Unknown error")
  (luna-restart luna)
  (when (find-restart 'try-again)
    (log:error "Successful reconnection")
    (invoke-restart 'try-again)))

(defmethod handle-conditions (luna c)
  (log:error "Unhandled condition signalled~% ~A~
              Attempting to restart in 5 seconds" c)
  (luna-restart luna)
  (when (find-restart 'listen-and-process)    
    (invoke-restart 'listen-and-process)))

(defmethod initiate-communities ((luna luna))
  (log:info "Initiating ")
  (funcall (if (parallel-p luna)
               #'lparallel:pmapc
               #'mapc)
           (lambda (community)
             (initiate-rooms luna community)
             (initiate-members luna community))
           (communities luna))
  (log:info "Communities initiated."))

(defmethod initiate-rooms ((luna luna) (community community))
  (with-accessors ((top-level-space top-level-space)
                   (name name))
      community
    (log:info "Collecting rooms for " name)
    (if top-level-space
        (handler-case
            (catch-potential-conditions 
              (let ((rooms (rooms-in-a-space (conn luna) top-level-space)))
                (log:info "- Found ~D room~:P" (length rooms))
                (setf (rooms community) rooms)
                (log:info "- Done")))
          (m-forbidden ()
            (log:warn "Forbidden from checking for the rooms within Space ~A please remedy."
                      top-level-space)))
        (log:warn "No top-level-space set for ~A ignoring.." name))))

(defmethod initiate-members ((luna luna) (community community))
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
              (let ((member-hash (gethash "joined" (members-in-room-ids (conn luna) id))))
                (maphash (lambda (member hash)
                           (declare (ignore hash))
                           (pushnew member members :test #'string=))
                         member-hash)))
          (m-forbidden (c)
            (log:warn "Forbidden " c)
            nil))))
    (log:info "- Found ~D member~:P" (length members))))


(defmethod start ((luna luna))
  "Attempts to startup Luna. This is done by first finding all of the modules listed 
in (modules CONFIG), this executes on-load-up on each of these modules. Next It checks if
Luna is already logged in, if so then it resets the cycle-history and then starts up the 
primary listening thread and executes 'on-restart' on each found module. 
If Luna is not logged in then evaluates (login <luna>), executes 'on-login' for each 
found-module and then recalls start."
  (unless (found-modules luna)
    (find-modules luna)
    (mapc-found-modules luna #'on-load-up))
  (if (logged-in-p luna)
      (progn
        (mapc-found-modules luna #'on-restart)
        (log:info "Setting stopp to nil")
        (setf (stopp luna) nil)
        (log:info "Resetting cycle-history")
        (setf (cycle-history luna) nil)
        (log:info "Starting primary thread")
        (setf 
         (thread luna)                    
         (bt2:make-thread
          (lambda ()
            (thread-maintainer luna))
          :name (format nil "~A-luna-main" (name luna))
          :initial-bindings `((*package* . ,*package*)
                              (*error-output* . ,*error-output*)
                              (*standard-output* . ,*standard-output*)
                              (dex:*connection-pool* . ,dex:*connection-pool*)
                              (*luna* . ,luna)
                              ,@bt2:*default-special-bindings*)))
        (log:info "Done.")
        (mapc (lambda (room-id)
                (module-moonmat-message (conn luna) room-id "I have started."))
              (uber-rooms luna))
        (log:info "Startup complete.")
        luna)
      (progn (login luna)
             (log:info "Starting lparallel kernel now with ~r workers." 2)
             (mapc-found-modules luna #'on-login)
             (initiate-communities luna)
             (start luna))))

(defmethod stop ((luna luna))
  (setf (stopp luna) t)
  (ignore-errors 
   (report-to-matrix "I have been told to shutdown"))
  (log:info "Waiting for Luna to stop on its own")
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
      (wait-for-stop luna)))
  (log:info "Stopped")
  (setf (stopp luna) nil))

(defmethod force-stop ((luna luna))
  "In the event that Luna won't stop on her own then this can be evaluated and it will
interrupt the main execution thread and cause Luna to stop."
  (mapc (lambda (mod) (on-shutdown luna mod)) (found-modules luna))
  (ignore-errors
   (bt2:interrupt-thread (thread luna)
                         (lambda () (throw 'bail nil)))))

(defmethod wait-for-stop ((luna luna))
  "Luna stops by checking if the accessor stopp is non nil, 
if it is then it'll return, however because Luna currently only runs on a 
single thread it is possible that it could take quite a while
 for it to complete a full cycle of command executions,
say 3 rooms all send an invite command at once it could take quite sometime for 
luna to get done processing its current batch of messages. This method will
wait 60 seconds (by default) for Luna to stop executing, however two restarts
are provided, the first 'force-stop' does exactly that, it forces Luna to stop
by interrupting the thread and telling it to jump to an encapsulating tag, the 
second 'wait-60' recalls 'wait-for-stop' making it wait another 60 seconds."
  (let ((x 0))
    (restart-case
        (loop :while (bt2:thread-alive-p (thread luna))
              :do (sleep 5)
                  (incf x 5)
                  (log:info "Luna still running. Waited: ~D seconds" x)
                  (when (= x 10);10 seconds
                    (error 'luna-still-running
                           :luna-still-running-message
                           (format nil "Luna is still running despite ~
                                          waiting 10 seconds"))))
      (force-stop ()
        :report "Luna is still running, force stop?"
        (log:error "Luna is still running, force stopping.")
        (force-stop luna))
      (wait-10 ()
        :report "Luna is still running, wait 10 seconds?"
        (log:error "Luna is still running, waiting 10 seconds")
        (wait-for-stop luna)))))

#||
Modules
||#


(defmethod find-module ((luna luna) sym &optional (silent t))
  "Takes in a symbol SYM and looks within the alist (modules LUNA) for a 
corresponding package name, then looks for *module* within that package."
  (restart-case
      (handler-case
          (progn (unless silent
                   (log:info "Searching for module ~A" sym))
                 (let ((package (cdr (assoc sym (modules luna) :test #'string-equal))))
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

(defmethod find-modules ((luna luna))
  "Loops through (modules LUNA) and attempts to resolve all of the *module* variables 
for each (name . package) pair."
  (handler-bind ((module-error
                   (lambda (c)
                     (declare (ignore c))
                     (invoke-restart 'ignore))))
    (setf (found-modules luna)
          (remove-if #'null
                     (loop :for (name . package) :in (modules luna)
                           :collect
                           (handler-case
                               (symbol-value (find-symbol "*MODULE*" package))
                             (unbound-variable ()
                               nil)))))))

(defmethod module-loaded-p ((luna luna) module)
  (and (find module (found-modules luna)) t))

(defmethod hotload-module ((luna luna) sym)
  "Load a module into Luna while Luna is running. SYM should be the prefix
of the module. If SYM is not a valid module signals 'missing-module. If module is
found but already loaded then 'module-already-loaded is signalled."
  (format t "Attempting hotload of module denoted by ~A~%" sym)
  (log:info "Attempting hotload of module denoted by ~A" sym)
  (with-accessors ((found-modules found-modules))
      luna
    (let ((module (find-module luna sym)))
      (when (module-loaded-p luna module)
        (error 'module-already-loaded
               :module-error-module module
               :module-error-message "Already found module in Luna"))
      (push module found-modules) 
      (on-module-hotload luna module)
      (format t "Hotload of ~S~%Successful~%" module)
      (log:info "Hotload of ~A~%Successful" module))))



(defmethod sym->module-name ((luna luna) sym)
  (cdr (assoc sym (modules luna) :test #'string=)))

(defmethod unload-module :around (luna sym)
  (log:info "Attempting to unload module denoted by ~A from Luna" sym)
  (report-to-matrix (format nil "Attempting to unload module denoted by ~A from Luna" sym))
  (call-next-method)
  (report-to-matrix "Module successfully unloaded")
  (log:info "Module successfully unloaded"))

(defmethod unload-module ((luna luna) sym)
  "Unload a module denoted by SYM from Luna. If the modules associated with SYM,
cannot be found then the condition 'missing-module is signalled."
  (let ((module-package (sym->module-name luna sym)))
    (with-accessors ((found found-modules)
                     (modules modules))
        luna
      (let ((module (find-module luna sym t)))
        (progn
          (on-module-unload luna module)
          (setf modules (remove module-package modules :key #'car)
                found (remove module found :test #'eq)))
        (format t "Module successfully unloaded~%")))))

(defmethod unload-module ((luna luna) (mod module))
  (on-module-unload luna mod)
  (setf (found-modules luna)
        (remove mod (found-modules luna))))
