(in-package #:matrix-moonbot)

(defparameter *luna* "The toplevel instance of Luna :)")

(defun setup-and-go ()
  "This is the default entry function for the dumped lisp image."
  (ql:quickload :slynk)
  (setup-log4cl)
  (log:info "Luna is booting.")
  (slynk:create-server :port 54000 :dont-close t);primary sly connection
  (log:info "Starting primary Slynk server.")
  (slynk:create-server :port 54001 :dont-close t);backup sly
  (log:info "Starting backup Slynk server.")
  (setf slynk:*use-dedicated-output-stream* nil)
  (log:info "You can now connect with Slynk.")
  (log:info "Restoring Luna from config...")
  (handler-bind ((warning
                   (lambda (c)
                     (declare (ignore c))
                     (invoke-restart 'muffle-warning))))
    (setf *luna* (config->luna)))
  (add-exit-hooks)
  (log:info "Booting Luna.")
  (start *luna*)
  (log:info "Dropping into the SBCL Top Level.")
  (handler-case 
      (front-end-loop)
    (SB-SYS:INTERACTIVE-INTERRUPT (c)
      (when (slot-boundp *luna* 'thread)
        (stop *luna*))
      (sb-ext:quit))))


(defun front-end-loop ()
  "systemd just restarts Luna if we enter the toplevel so instead we will just loop 
and sleep over and over and over again."
  (log:info "Starting the top level loop.")
  (loop :do (sleep 1000)))

(defun setup-log4cl ()
  (log:info "Starting daily logging to log/")
  (log:config :daily "logs/lunamech-log"))

(defun add-exit-hooks ()
  (log:info "Adding exit hooks for Luna.")
  (push (lambda ()
          (log:info "Exit hook invoked. Shutting down.")
          (when (slot-boundp *luna* 'thread)
            (stop *luna*)))
        sb-ext:*exit-hooks*))

(defun new-password ()
  (format *query-io* "Enter a new password: ")
  (read-line *query-io*))

(defmethod login ((connection connection) &optional (relog nil))
  "Attempts to login a CONNECTION to its appropriate server with its appropriate username 
and password. In this method the flag RELOG is completely ignored as the 
function (password-login ..) automatically relogs with the previous device-id. 
This method establishes the restart 'new-password' that will prompt the user for a new pass
and recall login."
  (declare (ignore relog))
  (log:info "Attempting login at url ~A with username ~A"
            (url connection) (username connection))
  (restart-case (progn (password-login connection)
                       (log:info "Successful login"))
    (new-password (pass)
      :report "Password is incorrect, enter another?"
      :interactive new-password
      (setf (password connection) pass)
      (login connection))))

(defmethod first-sync ((connection connection))
  (log:info "Performing initial sync for device-id ~A" (device-id connection))
  (sync connection))

(defgeneric login (moonbot &optional relog))

(defmethod login ((moonbot moonbot) &optional (relog nil))
  "Attempts to log Luna's connections into all their appropriate servers. In the event that
a password is incorrect (a m-forbidden is received) then invokes the restart 'new-password
in an attempt to get the user to enter a new password. If the flag RELOG is set to t, then
Luna will not evaluate any initiating functions and will login using the same device-id."
  (check-type relog boolean)
  (mapc (lambda (con)
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
            (login con)
            (unless relog;only perform this on an initial login
              (first-sync con)
              (initiate-encryption con)
              (initiate-filters con))))
        (connections moonbot)))

(defmethod logged-in-p ((moonbot moonbot))
  (every #'logged-in-p (connections moonbot)))

(defmethod moonbot-restart ((moonbot moonbot))
  (handler-case 
      (login moonbot t)
    (condition (c)
      (log:error "Restart error: ~A" c)
      (log:error "Failed to login, retrying in 5 seconds")
      (sleep 5)
      (moonbot-restart moonbot))))

(defmethod initiate-room-spellchecker ((community community))
  (with-accessors ((rooms-spellcheck rooms-spellcheck)
                   (rooms rooms))
      community
    (spellcheck:train
     (mapcar (lambda (room-plist)
               (string-downcase (pkv room-plist :NAME)))
             rooms))))

(defmethod initiate-spellcheckers ((moonbot moonbot))
  (mapc #'initiate-room-spellchecker (communities moonbot)))

(defmethod thread-maintainer ((moonbot moonbot))
  (labels ((run ()
             (listen-and-process moonbot)))
    (catch 'bail;used to forcefully stop the thread
      (handler-bind ((api-timeout
                       ;;in the case of a DC mid request, then this will initiate
                       ;;the process that attempts to reconnect to the server
                       ;;once a new connection has been made then just retries
                       ;;the call that was being made previously, meaning the bot
                       ;;doesn't lose its position.
                       (lambda (c)
                         (log:error
                          "Socket condition: ~A"
                          (api-timeout-condition c))
                         (log:error
                          "Attempting a restart of Luna")
                         (moonbot-restart moonbot)
                         (when (find-restart 'try-again)
                           (log:error "Successful reconnection")
                           (invoke-restart 'try-again))))
                     (api-error
                       (lambda (c)
                         (log:error "API Error made it through.")
                         (log:error (api-timeout-condition c))
                         (log:error "Restarting listen-and-process")
                         (run))))
        (handler-case
            (run)
          (error (c)
            ;;in the most fatal conditions this will catch and stop the thread from
            ;;crashing and simply attempt a restart of the bot
            (log:error "Unhandled condition signalled~% ~A~
                                  Attempting to restart in 5 seconds" c)
            (moonbot-restart moonbot)
            (run)))))));;in the event of a catastrophic failure just restart

(defmethod initiate-communities ((luna luna))
  (log:info "Initiating ")
  (lparallel:pmapc (lambda (community)
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
            (catch-limit-exceeded 
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
            (catch-limit-exceeded 
              (let ((member-list (members-in-room%ids (conn luna) id)))
                (dolist (member member-list)
                  (pushnew member members :test #'string=))))
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
    (dolist (mod (found-modules luna))
      (on-load-up luna mod)))
  (if (logged-in-p luna)
      (progn
        (dolist (mod (found-modules luna))
          (on-restart luna mod))
        (log:info "Setting stopp to nil")
        (setf (stopp luna) nil)
        (log:info "Resetting cycle-history")
        (setf (cycle-history luna) nil)
        (log:info "Starting primary thread")
        (setf 
         (thread luna)                    
         (bt:make-thread
          (lambda ()
            (thread-maintainer luna))
          :name "luna-main"
          :initial-bindings `((*package* . ,*package*)
                              (*error-output* . ,*error-output*)
                              (*standard-output* . ,*standard-output*))))
        (log:info "Done.")
        (mapc (lambda (room-id)
                (module-moonmat-message (conn luna) room-id "I have started."))
              (uber-rooms luna))
        (log:info "Startup complete.")
        luna)
      (progn (login luna)
             (log:info "Starting lparallel kernel now with ~r workers." 2)
             (setf lparallel:*kernel* (lparallel:make-kernel 2))
             (dolist (mod (found-modules luna))
               (on-login luna mod))
             (initiate-communities luna)
             (start luna))))

(defmethod stop ((luna luna))
  (setf (stopp luna) t)
  (mapc (lambda (room-id)
          (module-moonmat-message (conn luna) room-id "I have been told to shutdown."))
        (uber-rooms luna))
  (log:info "Waiting for Luna to stop on its own")
  (let ((err-count 0))
    (handler-bind ((moonbot-still-running
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
interrupt the main execution thread and cause Luan to stop."
  (log:warn "Graceful shutdown failed. Forcing shutdown now.")
  (ignore-errors
   (bt:interrupt-thread (thread luna)
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
        (loop :while (bt:thread-alive-p (thread luna))
              :do (sleep 5)
                  (incf x 5)
                  (log:info "Luna still running. Waited: ~D seconds" x)
                  (when (= x 10);10 seconds
                    (error 'moonbot-still-running
                           :moonbot-still-running-message
                           (format nil "Luna is still running despite ~
                                          waiting 60 seconds"))))
      (force-stop ()
        :report "Luna is still running, force stop?"
        (log:error "Luna is still running, force stopping.")
        (force-stop luna))
      (wait-10 ()
        :report "Luna is still running, wait 10 seconds?"
        (log:error "Luna is still running, waiting 10 seconds")
        (wait-for-stop luna)))))


