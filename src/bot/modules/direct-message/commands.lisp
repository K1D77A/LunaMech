(in-package #:mm-module.direct-message)

(new-direct-message-command send-dm ((user-id :valid-user)
                                     (context :valid-context))
    "Initiates a DM with USER-ID within a given CONTEXT"
  (log:info  (intern (string-upcase  context)))
  (format t "Starting a DM with ~A." (subseq user-id 1))
  (catch-limit-exceeded 
    (let* ((context (gen-context (intern (string-upcase context) :keyword)))
           (room (create-private-room (connection community)
                                      (list user-id)))
           (room-id (pkv room :|room_id|)))
      (add-dm-room user-id room-id context)
      ;; (upload-current-rooms community)
      (format t "~A" (initial-message context)))))

(new-direct-message-command leave-dms ()
    "Causes Luna to leave all DM's"
  (format t "Leaving DMs")
  (mapc (lambda (private-room)
          (let ((user-id (with private-room))
                (rooms (rooms private-room)))
            (declare (ignore user-id))
            (mapc (lambda (proom)
                    (leave-room (connection community) (room-id proom)))
                  rooms)))
        (current-rooms *module*))
  (setf (current-rooms *module*) nil)  
  (format t "Done"))

(new-direct-message-command current-dms ()
    "Returns the names of people Luna is currently in a DM room with"
  (with-accessors ((current-rooms current-rooms))
      *module*
    (if current-rooms
        (let ((user-ids (mapcar #'with (current-rooms *module*))))
          (format t "~{~A~%~}" user-ids))
        (format t "I am not in any direct messages"))))

(new-direct-message-command contexts ()
    "Prints out the names of all of the available contexts"
  (let ((contexts nil))
    (maphash (lambda (key val)
               (declare (ignore val))
               (push key contexts));this is just a quick test one
             (contexts *module*))
    (format t "~{~A~%~}" contexts)))
;;;this is wrong
(new-direct-message-command collectors ()
    "Prints out the names of all of the available collectors"
  (let ((collector nil))
    (maphash (lambda (key val)
               (declare (ignore val))
               (push key collector));this is just a quick test one
             (collectors *module*))
    (format t "~{~A~%~}" collector)))

(new-direct-message-command name-failed ()
    "Prints out the names of those who failed to complete their context"
  (with-accessors ((failed-contexts failed-contexts))
      *module*
    (let ((failures nil))
      (maphash (lambda (key val)
                 (declare (ignore val))
                 (push (subseq key 1) failures))
               failed-contexts)
      (format t "~{ ~A~%~}" failures))))

(new-direct-message-command get-contexts ()
    "Uploads .json file containing the results of all the current contexts"
  (upload-module (connection community) room *module*))

(new-direct-message-command clear-failed-contexts ()
    "Clears the incomplete contexts"
  (setf (failed-contexts *module*) (make-hash-table :test #'equalp))
  (format t "Cleared failed contexts"))

(new-direct-message-command clear-completed-contexts ()
    "Clears the completed contexts"
  (setf (completed-contexts *module*) (make-hash-table :test #'equalp))
  (format t "Cleared completed contexts"))

(defun contains-open-context-p (user-id context-key)
  (let ((private-room (find-private-room user-id)))
    (when private-room
      (some (lambda (proom)
              (let ((context (context proom)))
                (eql context-key (context-type context))))
            (rooms private-room)))))

(new-direct-message-command community-compass-collect ((community-name))
    "Initiates a compass DM with all the online, non bot, members in community, 
does check those who have already given results."
  (let ((found-community
          (find (intern (string-upcase community-name) :keyword) (communities moonbot)
                :key #'name)))
    (format t "Starting collection of compass results")
    (if found-community 
        (let* ((users (remove (user-id (connection community))
                              (non-bot-members found-community)
                              :test #'string=))
               (online
                 (remove-if-not (lambda (user)
                                  (handler-case
                                      (and (not (contains-open-context-p user :COMPASS))
                                           (user-online-p (connection community) user))
                                    (m-forbidden ()
                                      (format t "Not allowed to see ~A's presence"
                                              (subseq user 1))
                                      nil)))
                                users))
               (no-res (remove-if #'compass:result-set-p online)))
          (mapc (lambda (user-id)
                  (format t "Starting DM with ~A" (subseq user-id 1))
                  (catch-limit-exceeded 
                    (let* ((context (gen-context :COMPASS))
                           (room (create-private-room (connection community)
                                                      (list user-id)))
                           (room-id (pkv room :|room_id|)))
                      (add-dm-room user-id room-id context)
                      (moonmat-message community room-id "~A" (initial-message context)))))
                no-res)
          (format t "Started DM's with all users."))
        (format t "Couldn't find community"))))


(new-direct-message-command collect-results ((context-type :valid-context)
                                             (collector (:valid-collector context-type)))

    "Collects the results using a collector"
  (let ((completed (completed-contexts *module*)));hashtable 
    (maphash (lambda (user-id contexts)
               (let ((correct-contexts
                       (remove-if-not (lambda (context)
                                        (eql (context-type context)
                                             (intern (string-upcase context-type)
                                                     :keyword)))
                                      contexts)))
                 (when correct-contexts
                   (mapc (lambda (context)
                           (moonmat-message
                            community room "Collecting context ~A for ~
                                             ID: ~A with '~A' collector"
                            (context-type context) (subseq user-id 1) collector)
                           (invoke-collector moonbot user-id
                                             (string-downcase collector) context))
                         correct-contexts))))
             completed)))

(new-direct-message-command message-open-dms ((context :valid-context)
                                              (to-send (:maxlen 500)
                                                       (:minlen 1)))
    "Sends a message to all the open DM's"
  (format t "Sending your message to ~r room~:p" (length (current-rooms *module*)))
  (message-current-open-rooms-of-context-type
   community (intern (string-upcase context) :keyword) to-send))
