(in-package #:matrix-moonbot)

(defun extract-message (message)
  "Extracts the content for the key :|body| within the content for the key :|content|"
  (getf (getf message :|content|) :|body|))

(defun extract-messages (list-of-lists)
  (mapcar #'extract-message list-of-lists))

(defun message-from? (message &optional (sigcon-when-nil-p nil))
  "Normal - looks within the list to find the key :|user_id| and returns this.
Exceptional - When SIGCON-WHEN-NIL-P is non nil signals a MISSING-EXPECTED-KEY 
condition"
  (destructuring-bind (&key |sender| |user_id| &allow-other-keys)
      message
    (let ((val (or |sender| |user_id|)))
      (if (and sigcon-when-nil-p (not val))
          (error 'missing-expected-key
                 :message-process-failure-culprit "user_id or sender"
                 :message-process-failure-message "Missing user_id or sender in list")
          val))))

(defun ubermensch-p (moonbot string)
  (and (find string (ubermensch moonbot) :test #'string=) t))

(defun community-admin-p (community string)
  (and (find string (admins community) :test #'string=) t))

(defun determine-privilege (moonbot community message)
  "Determines the privilege of a message sender by checking the name first against
(ubermensch moonbot) then by checking if they are in (admins community) and finally
defaulting to normie. Returns either an ubermensch, admin, or normie privilege 
object"
  (let ((sender (message-from? message t)))
    (make-instance
     (cond ((ubermensch-p moonbot sender)
            'ubermensch-privilege)
           ((community-admin-p community sender)
            'admin-privilege)
           (t 'normie-privilege))
     :user-id sender)))

(defun check-valid-prefix (string)
  "Attemps to determine if STRING has a valid prefix. Currently the prefix is just the 
character #\. In the case that it does returns t otherwise signals condition 'invalid-prefix"
  (or (char= (aref string 0) #\.)
      (let ((trimmed (%trim-message 25 string)))
        (error 'invalid-prefix
               :message-process-failure-culprit trimmed
               :message-process-failure-message
               (format nil "String has invalid prefix ~A" trimmed)))))

(defun community-prefix-p (community string)
  (string-equal string (format nil ".~A" (name community))))

(defun community-alias-p (community string)
  (some (lambda (alias)
          (string-equal string (format nil ".~A" alias)))
        (aliases community)))

(defun extract-command-and-args (moonbot community string)
  "Normal - Takes in an instance of moonbot a community and a string and
splits it by #\Space and removes nulls, then checks if the
string is a community-prefix-p, if so returns a list of an interned prefix 
and the args to the functions. If it not community-prefix-p then checks if the
interned prefix can be found within any of the modules associated with MOONBOT.
If one is found then returns a list of the module and the remainder of the split.
Exceptional - If neither community-prefix-p or part of a module then signals
a 'invalid-prefix' condition."
  (handler-case
      (when (check-valid-prefix string)
        (let* ((split (str:split " " (str:trim (str:collapse-whitespaces string)) :limit 3))
               (prefix (first split))
               (in (intern (string-upcase (subseq prefix 1)))))          
          (if (or (community-prefix-p community prefix)
                  (community-alias-p community prefix))
              (cons in (rest split))
              (let ((mod (check-found-modules moonbot in)))
                (if mod
                    (progn (unless (rest split)
                             (error 'missing-invoker
                                    :message-process-failure-culprit split
                                    :message-proceess-failure-message "Missing invoker"))
                           (cons mod (rest split)))
                    (let ((trimmed (%trim-message 25 prefix)))
                      (error 'invalid-prefix
                             :message-process-failure-culprit trimmed
                             :message-process-failure-message
                             (format nil "Prefix is invalid ~A" trimmed))))))))
    ((and error (not invalid-prefix)) ()
      (error 'message-process-failure
             :message-process-failure-culprit string
             :message-process-failure-message
             (format nil "Message is invalid")))))

(defun %trim-message (n message)
  "Shortens MESSAGE if its greater than length N"
  (let ((len (length message)))
    (if (< n len)
        (let ((trimmed (subseq message 0 n))
              (rem (- len n)))
          (format nil "~A (~D remaining)" trimmed rem))
        message)))

(defun process-message (moonbot community room message)
  "Normal - initially determines the privilege of the user from the message, then
extracts the commands and arguments from the message, and starts the process for
handling the command. Exceptional - There are two restarts available, return-nil
which returns nil and resignal, which causes the condition to be signalled again."
  (let ((text (extract-message message)))
    (when text
      (restart-case
          (let ((priv (determine-privilege moonbot community message)))
            (destructuring-bind (prefix/module invoker &rest rest)
                (extract-command-and-args moonbot community text)
              (handler-bind ((missing-command
                               (lambda (c)
                                 (declare (ignore c))
                                 (invoke-restart 'tell-user))))
                (initiate-command-execution moonbot priv prefix/module
                                            (intern (string-upcase invoker)
                                                    :matrix-moonbot)
                                            community room message rest))))
        (return-nil ()
          :report "Return nil?"
          nil)
        (resignal (c)
          :report "Resignal the condition?"
          (error c))))))

(defun process-encrypted (moonbot community room message)
  "This function is supposed to process encrypted messages, but this has not been 
implemented yet."
  (declare (ignorable moonbot community room message))
  nil)

(defmethod process-messages ((moonbot moonbot) (community community) room messages)
  "Given a list of messages in MESSAGES, and its associated ROOM id, attempts to determine
the type of message that has been sent, either m.room.message or m.room.encrypted and 
then calls either process-message or process-encrypted with that message. In the case 
that the message type is unknown then signals the condition 'unknown-message-type."
  (loop :for message :in messages
        :do (handler-bind ((message-process-failure
                             (lambda (c)
                               (declare (ignore c))
                               (invoke-restart 'return-nil))))
              (destructuring-bind (&key |type| |event_id| &allow-other-keys)
                  message
                (unless (%already-processed-message-p moonbot |event_id|)
                  (cond ((string= |type| "m.room.message")
                         (process-message moonbot community room message))
                        ((string= |type| "m.room.encrypted")
                         (process-encrypted moonbot community room message))
                        (t (error 'unknown-message-type
                                  :message-process-failure-type |type|
                                  :message-process-failure-message
                                  "Unknown type"))))))))

(defgeneric grab-messages-and-process (luna community sync)
  (:documentation "Given Luna a community and a sync list, attempts to find all of 
the messages from the rooms that luna is listening in for that community and then 
passes them to process-messages"))

(defmethod grab-messages-and-process ((moonbot moonbot) (community community) sync)
  (with-accessors ((connection connect))
      community
    (let ((messages (extract-all-relevant-messages moonbot community sync)))
      ;;            (find-messages-from-listen-in community sync)))
      (when messages
        (alexandria:doplist (room messages messages)
          (when messages
            (process-messages moonbot community (symbol-name room) messages)))))))

(defmethod listen-and-process ((moonbot moonbot))
  "This is the primary loop used to run Luna. 
It executes in the following order. 
First it checks if Lunas (stopp ) accessor is non nil, in the case it is it stops.
Second it maps over all of the connections within (connections ) and performs a 
sync with the :junk-removed filter, within this same map it
uses this new sync to then map over the (communities ) calling 
grab-messages-and-process with that community and the sync object.
Once again within the same map Luna maps over each module in (found-modules ) and calls 
the on-sync method with the latest sync object. 
Now that map is complete.
Next if 50 loops have been completed then Luna will map over (connections ) and 
check for any invites by syncing using the :invites filter, this sync object is 
then passed to process-invites.
Next if 100 loops have been completed then Luna resets the (cycle-history ) variable.
Next if 2000 loops have been completed then Luna will backup to file and call 
the on-save method for all found-modules.
Finally at the end of every loop (timestamp ) is reset to (local-time:now)"
  (with-accessors ((stopp stopp)
                   (parallel-p parallel-p)
                   (timers timers)
                   (found-modules found-modules)
                   (timestamp timestamp)
                   (cycle-history cycle-history)
                   (connections connections)
                   (communities communities))
      moonbot
    (loop :while (not (stopp moonbot))
          :do (mapc (lambda (connection)
                      (sleep 0.000001)
                      (let ((sync (key-sync connection :junk-removed)))
                        (funcall (if parallel-p #'lparallel:pmapc #'mapc)
                                 (lambda (com)
                                   (grab-messages-and-process moonbot com sync))
                                 communities)
                        (funcall (if parallel-p #'lparallel:pmapc #'mapc)
                                 (lambda (module) 
                                   (handler-case
                                       (bt:with-timeout (30)
                                         ;;smashout after 30 seconds
                                         (on-sync moonbot module sync)
                                         (execute-all-communications-between-modules
                                          moonbot module sync))
                                     (sb-ext:timeout ()
                                       (log:error "An on-sync method timed out."))))
                                 found-modules)
                        (process-invites moonbot connection sync)))
                    connections)
              (execute-stamp-n-after-luna ((find-timer timers :clear-cycle)
                                           5)
                (setf cycle-history nil))
              (execute-stamp-n-after-luna ((find-timer timers :backup)
                                           300)
                (log:info "Backing up Luna to ~A" *config-file*)
                (dolist (mod found-modules)
                  (on-save moonbot mod))
                (moonbot->config moonbot)
                (log:info "Luna backed up"))
              (setf timestamp (local-time:now))
          :finally (log:info "Luna going down"))))
