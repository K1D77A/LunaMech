(in-package #:mm-module.direct-message)


(defmodule direct-message (mm-module.direct-message DIRECT-MESSAGE ubermensch-privilege)
           direct-message-command ()
           direct-message-module
           ((current-rooms
             :accessor current-rooms
             :initform nil
             :type list
             :documentation "A list of private-room object that the bot has started")
            (failed-contexts
             :accessor failed-contexts
             :initform (make-hash-table :test #'equalp)
             :type hash-table
             :documentation "A hash-table containing a user-id and their failed contexts")
            (completed-contexts
             :accessor completed-contexts
             :initform (make-hash-table :test #'equalp)
             :type hash-table
             :documentation "A hash-table containing a user-id and their completed contexts")
            (collectors
             :accessor collectors
             :initform (make-hash-table :test #'equalp)
             :type hash-table
             :documentation "A list of two argument functions that can be used to conver the results
in a context to something useful")
            (contexts
             :accessor contexts
             :initform (make-hash-table :test #'equalp)
             :type hash-table
             :documentation "A hash-table of the currently available contexts used for starting
new private messages with individuals")))
           

(defclass private-room ()
  ((with
    :accessor with
    :initarg :with
    :type string
    :documentation "A user-id")
   (rooms
    :accessor rooms
    :initarg :rooms
    :initform nil
    :type list
    :documentation "A list of prooms that are associated with this user-id")))

(defmethod print-object ((obj private-room) stream)
  (print-unreadable-object (obj stream)
    (format stream "with: ~A" (with obj))))

(defclass proom ()
  ((room-id
    :accessor room-id
    :initarg :room-id
    :type string)
   (context
    :accessor context
    :initarg :context
    :type keyword
    :documentation "The context in which this proom was created. This dicates which
responses the bot expects from the user.")
   (input-stream
    :accessor input-stream
    :initarg :input-stream
    :initform (make-string-output-stream)
    :documentation "All messages sent from the user are put in here")
   (output-stream
    :accessor output-stream
    :initarg :output-stream
    :initform (make-string-output-stream)
    :documentation "All messages to send to the room are put in here.")))

(define-condition direct-message-condition (moonbot-condition)
  ())

(define-condition context-incomplete (direct-message-condition)
  ((context-incomplete-context
    :accessor context-incomplete-context
    :initarg :context-incomplete-context
    :documentation "The context that the user failed to complete")
   (context-incomplete-user-id
    :accessor context-incomplete-user-id
    :initarg :context-incomplete-user-id
    :documentation "The user-id of the individual who failed to complete the context"))
  (:documentation "This condition is signalled when a user leaves a room but the context 
hasn't been completed."))

(defmethod jojo:%to-json ((obj direct-message-module))
  (jojo:with-object
    (jojo:write-key-value "content" (slot-value obj 'current-rooms))
    (jojo:write-key-value "type" "m.direct")))

(defmethod jojo:%to-json ((obj private-room))
  (jojo:with-object
    (jojo:write-key (slot-value obj 'with))
    (jojo:write-value (extract-current-rooms))))

(defmethod on-sync (luna (mod direct-message-module) sync)
  "Using on-sync we need to check for a few things, first we have to check whether a user
has actually left a private room so that Luna can also leave, which this does, we also need
to check for new messages in any of the private-rooms that have been created."
  (let* ((dm-rooms (extract-current-rooms)))
    (when dm-rooms
      (let ((room-leaves ())
            (room-joins ())
            (messages (find-messages-from-rooms dm-rooms sync)))
        (dolist (room-id dm-rooms)
          (let* ((events (membership-events sync room-id))
                 (leaves (room-leaves events))
                 (joins (room-joins events)))
            (when leaves
              (setf room-leaves (nconc room-leaves (list room-id))))
            (when joins
              (setf room-joins (nconc room-joins (list room-id))))))
        (when room-joins
          (mapc (lambda (room-id)
                  (let ((proom (find-proom room-id)))
                    (on-room-join (context proom) proom)))
                room-joins))
        (when messages
          (handle-messages luna messages))
        (when room-leaves
          (log:info room-leaves)
          (mapc (lambda (room-id)
                  (let ((proom (find-proom room-id)))
                    (on-room-leave (context proom) proom)))
                room-leaves)
          (handler-bind ((context-incomplete
                           (lambda (c)
                             (declare (ignore c))
                             (invoke-restart 'record-incomplete))))
            (handle-leaves luna room-leaves)))))))

(defun handle-leaves (luna rooms)
  "When a user has left the room then Luna simply leaves that room. Establishes two restarts
in the case of a 'context-incomplete signal, record-incomplete which will record the user-id
and context in an alist in (failed-contexts *module*), or resignal, which will simply 
signal the condition again."
  (mapc (lambda (room)
          (let* ((private-room (find-room room))
                 (proom (find room (rooms private-room) :test #'string= :key #'room-id))
                 (context (context proom))
                 (with (with private-room)))
            (restart-case
                (if (completep context)
                    (leave-dm-room room (conn luna))
                    (error 'context-incomplete
                           :context-incomplete-user-id with
                           :context-incomplete-context context))
              (record-incomplete ()
                :report "A user has left a room without completing their context. Record?"
                (let ((current (gethash with (failed-contexts *module*))))
                  ;;need a failed context function
                  (setf (completed-time context) (get-universal-time)
                        (gethash with (failed-contexts *module*))
                        (append current (list context)))
                  (on-context-failure context)
                  (leave-dm-room proom (conn luna))))
              (resignal (c)
                :report "A user has left a room without completing their context. Resignal?"
                (error c)))))
        rooms))

;;;private session would make more sense now
(defun handle-messages (luna message-events)
  "Having extracted the message events for all the rooms that Luna is currently in, 
this function will iterate over the plist, find the private-room associated with the 
sender, then the rooms accessor is checked for a proom that is associated with room-id.
Now the important stuff happens, once all the correct objects have been found, 
The message that was sent is output into the prooms input-stream and the function 
'execute-context' (see context-executor.lisp) is called, this allows per user 
interactive sessions to be programmed. Once evaluation is complete Luna grabs the contents 
of output-stream and sends it to room-id. This is repeated for all of room-ids in 
message-events"
  (alexandria:doplist (room-id messages message-events)
    (loop :for event :in messages
          :do (let* ((sender (getf event :|sender|));;it would be possible to do a membership
                     ;;in room event to update but like thats a fucking slow arse thing to
                     ;;do if we tryna keep everything responsive
                     (private-room (find-private-room sender)))
                ;;this should ignore the messages sent by Luna
                (when private-room
                  (let* ((rooms (rooms private-room))
                         (proom (find room-id rooms :key #'room-id :test #'string=))
                         (message (extract-message event))
                         (stream (input-stream proom))
                         (with (with private-room)))
                    (with-accessors ((output-stream output-stream)
                                     (context context))
                        proom
                      (format stream "~A~%" message)
                      (finish-output stream)
                      (execute-context proom)
                      (let ((output-string (get-output-stream-string (output-stream proom))))
                        (unless (string= output-string "")
                          (catch-limit-exceeded 
                            (moonmat-message (first (communities luna))
                                             (symbol-name room-id)
                                             "~A" output-string))))
                      (when (completep context)
                        (on-context-completion context)
                        ;;need a complete-context function
                        (let ((current
                                (gethash with (completed-contexts *module*))))
                          (setf (gethash with (completed-contexts *module*))
                                (append current (list context)))
                          (leave-dm-room proom (conn luna)))))))))))

(defun private-rooms-to-json ()
  (if (current-rooms *module*)
      (jojo:to-json *module*)
      (jojo:to-json '(:NO-VALS t))))

(defun extract-current-rooms ()
  "Evaluates to a list of all of the room-ids within (current-rooms *module*)"
  (let ((rooms (current-rooms *module*)))
    (when rooms
      (let ((collected nil))
        (mapc (lambda (private-room)
                (let ((rooms (rooms private-room)))
                  (mapc (lambda (room)
                          (push (room-id room) collected))
                        rooms)))
              rooms)
        collected))))

;;needa change this
(defun current-dms-from-server (connection &optional (set nil))
  "grabs the m.direct storage key from the server and sets (current-rooms *module*) to this"
  (let ((dms (get-account-data connection (user-id connection) "m.direct")))
    (when set
      (if (eql (first dms) :NO-VALS)
          (setf (current-rooms *module*) nil)
          (setf (current-rooms *module*) (getf dms :|content|))))
    dms))

(defun leave-dm-room (proom connection)
  (with-accessors ((room-id room-id))
      proom
    (log:info "Leaving room ~A" room-id)
    (handler-case
        (progn 
          (leave-room connection room-id)
          (log:info "done"))
      (condition ()
        (log:error "Error leaving dm room ~A" room-id)))
    (remove-dm-room room-id)));;could get the user-id

(defun find-private-room (user-id)
  "Finds the private-room that is associated with user-id ie where (with <private-room>) is
string= to USER-ID."
  (find user-id (current-rooms *module*) :key #'with :test #'string=))

(defun find-room (room-id)
  "returns the private-room that ROOM-ID is within"
  (loop :for private-room :in (current-rooms *module*)
        :if (find room-id (rooms private-room) :test #'string= :key #'room-id)
          :return private-room))

(defun find-proom (room-id)
  (loop :for private-room :in (current-rooms *module*)
          :thereis (find room-id (rooms private-room) :test #'string= :key #'room-id)))

(defun add-dm-room (user-id room-id context)
  "when given a USER-ID and ROOM-ID adds the new room-id to (rooms <private-room>), 
if no private-room can be found that is associated with USER-ID then creates a new one."
  (with-accessors ((current-rooms current-rooms))
      *module*
    (let ((user-entry (find-private-room user-id))
          (proom (make-instance 'proom :room-id room-id
                                       :context context)))
      (if user-entry
          (push proom (rooms user-entry))
          (push (make-instance 'private-room
                               :rooms (list proom)
                               :with user-id)
                current-rooms)))))

(defun remove-empty-private-rooms ()
  "Removes any private-room object that has no (rooms <private-room>) from *module*"
  (setf (current-rooms *module*)
        (remove-if (lambda (private-room)
                     (null (rooms private-room)))
                   (current-rooms *module*))))

(defun remove-dm-room (room-id &optional (user-id nil))
  "Takes in a ROOM-ID and an optional USER-ID. If user-id is a string then checks 
within (current-rooms <module>) to find a private room that has a (with <room>)
 that is string= to that id, then it removes the proom associated with ROOM-ID
 from its list of prooms. If USER-ID isn't provided
then searches through all of the current rooms for the ROOM-ID and then removes it
from the appropriate private-room. After cleans any empty private-rooms"
  (check-type user-id (or null string))
  (let ((room (if user-id (find-private-room user-id) (find-room room-id))))
    (when room
      (setf (rooms room) (remove room-id (rooms room) :test #'string= :key #'room-id))
      (remove-empty-private-rooms))))

(defun upload-current-rooms (community)
  (add-to-account-data (connection community)
                       (user-id (connection community))
                       "m.direct"
                       (private-rooms-to-json)))

(defmethod locate-command ((module direct-message-module) (priv ubermensch-privilege)
                           invoker community)
  "When prefix is 'admin with no privileges search for admin-command"
  (or (type-find 'direct-message-command invoker *commands*
                 :key #'name :test #'string-equal)
      (error 'missing-command)))

(defmethod execute-command ((moonbot moonbot) (priv ubermensch-privilege)
                            (command direct-message-command)
                            community room message rest)
  (if (string-equal (first rest) "help")
      (print-command-information command community room)
      (safe-execution command community room message rest moonbot)))

(defmethod inform-command-is-missing
    (priv (module direct-message-module) community room)
  ""
  nil)

(defmethod inform-command-is-missing
    ((priv admin-privilege) (module direct-message-module) community room)
  "ADMIN & ADMIN command."
  nil)

(command-defining-macro-moonbot new-direct-message-command 'direct-message-command)

(new-direct-message-command help ()
    "attempts an explanation of how commands work"
  (moonhelp 'direct-message-command community room))

(defun message-current-open-rooms-of-context-type (community context message)
  "sends MESSAGE to all of the prooms in current-rooms within *module* whose context-type
matches CONTEXT. Community is used to send the message."
  (check-type context keyword)
  (check-type message string)
  (mapc (lambda (private-room)
          (let* ((prooms (rooms private-room))
                 (user-id (with private-room)))
            (declare (ignorable user-id))
            (mapc (lambda (proom)
                    (let ((context-type (context-type (context proom)))
                          (room-id (room-id proom)))
                      (when (eql context-type context)
                        (moonmat-message community room-id "~A" message))))
                  prooms)))
        (current-rooms *module*)))

(defun start-dm (context user-id connection &rest args)
  "Starts a new DM with the context CONTEXT with USER-ID. Args is passed to the 
function (gen-context ..). Returns t when successful, nil when not."
  (let ((cont (apply #'gen-context context args)))
    (if (and cont (valid-user-p connection user-id))
        (let* ((room (create-private-room connection (list user-id)))
               (room-id (getf room :|room_id|)))
          (add-dm-room user-id room-id cont)
          (moonmat-message (first (communities matrix-moonbot::*luna*))
                           room-id "~A" (initial-message cont))
          t)
        nil)))

