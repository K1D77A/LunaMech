(in-package #:matrix-moonbot)

;;;see events.lisp for events classes

(defun make-event (content etype event-id sender origin-server-ts room-id
                   &optional (unsigned nil))
  (make-instance 'event :content content :etype etype :event-id event-id
                        :origin-server-ts origin-server-ts :room-id room-id
                        :unsigned unsigned :sender sender))

(defclass luna ()
  ())

(defclass moonbot (luna)
  ((communities
    :accessor communities
    :initarg :communities
    :type list
    :documentation "A list of instances of community")
   (ubermensch
    :accessor ubermensch
    :initarg :ubermensch
    :initform nil
    :type list
    :documentation "A list of users who will have ubermensch privilege. These are folks 
who will be able to execute commands that can modify Luna herself.")
   (connections
    :accessor connections
    :initarg :connections
    :type list
    :documentation "A list of connections that have been used to login to the Matrix api.
Its important to note that all testing has only ever been done with a single connection, 
so although most functionality will work with multiple connections as communities are 
associated with a connection, there are a few places where (first (connections <luna>)) 
is used to send data to the server. So best not rely on this functionality.")
   (filters
    :accessor filters
    :initarg :filters
    :documentation "A list of filter objects used for Luna wide filters. Currently unused")
   (cycle-history
    :accessor cycle-history
    :initform nil
    :type list
    :documentation "This is a list of event-ids that is reset every 100 cycles. This makes 
sure that no event is handled twice by the command execution system.")
   (modules
    :accessor modules
    :initarg :modules
    :initform nil
    :type list
    :allocation :class
    :documentation "An alist of symbols to their associated packages.")
   (found-modules
    :accessor found-modules
    :initform nil
    :type list
    :allocation :class
    :documentation "When Luna is started the modules within Luna are searched for within 
the Lisp image, this is a list of all the modules that were found.")
   (uber-room
    :accessor uber-room
    :initarg :uber-room
    :initform ""
    :type string
    :documentation "This room is the room that Luna will always listen in. It will be 
used for starting and stopping Lunas primary thread from Matrix.")
   (stopp
    :accessor stopp
    :initform nil
    :type boolean
    :documentation "A boolean used to check whether Luna should stop.")
   (timestamp
    :accessor timestamp
    :initform (local-time:now)
    :documentation "A timestamp (see local-time library) that is updated after each
complete listening cycle. This can be used to implement timers.")
   (thread
    :accessor thread
    :documentation "This is the primary thread that the entire command system runs on.")
   (parallel-p
    :accessor parallel-p
    :initform t
    :documentation "Set to t and actions will be performed concurrently using lparallel.")
   (controller-thread
    :accessor cotroller-thread
    :documentation "This is a thread that is used to control Luna from matrix, ie to start
stop etc")
   (%locks
    :reader %locks
    :type list
    :documentation "The locks for certain other slots."
    :initform (flet ((nl (&rest names)
                       (loop :for name :in names
                             :appending (list name (bt:make-lock (format nil "~A" name))))))
                (nl :found-modules :cycle-history :ubermensch :thread))))
  (:documentation "Luna (Moonbot here) is the primary class that is used to store all 
information related to the operation and interaction with the Matrix api. It is an 
instance of Luna (Moonbot here) that is created from the communities.lisp config file
and it is that same instance that is backed up to the same file."))

(defmacro quicklock ((object lock-key) &body body)
  (alexandria:with-gensyms (lock)
    `(with-slots (%locks)
         ,object
       (let ((,lock (getf %locks ,lock-key)))
         (bt:with-lock-held (,lock)
           (locally ,@body))))))
      

(defmethod print-object ((luna moonbot) stream)
  (print-unreadable-object (luna stream :type t :identity nil)
    (format stream "~%Community Names: ~{~A ~}~%Modules: ~{~A ~}~%Ubermensch: ~{~A ~}~%~
                   Command count: ~r~%"
            (mapcar #'name (communities luna))
            (mapcar #'prefix (found-modules luna))
            (ubermensch luna)
            (length *commands*))))

(defmethod conn ((luna luna))
  (first (connections luna)))

(defmethod find-community ((community-name symbol) (luna luna))
  (find community-name (communities luna) :key #'name))

(defmethod find-community ((community-name string) (luna luna))
  (find (intern (string-upcase community-name) :keyword) (communities luna) :key #'name))

(defmethod (setf modules) :after (new-val (moonbot moonbot))
  "Remove duplicate modules after changing its value."
  (with-slots (modules)
      moonbot
    (setf modules
          (remove-duplicates modules :test (lambda (ele1 ele2)
                                             (string-equal (car ele1) (car ele2)))
                                     :from-end t))))

(defmethod found-modules :around ((luna luna))
  (quicklock (luna :found-modules)
    (call-next-method)))


(defmethod cycle-history :around ((luna luna))
  (quicklock (luna :cycle-history)
    (call-next-method)))

(defmethod ubermensch :around ((luna luna))
  (quicklock (luna :ubermensch)
    (call-next-method)))

(defmethod thread :around ((luna luna))
  (quicklock (luna :thread)
    (call-next-method)))

(defmethod (setf found-modules) :after (new-val (moonbot moonbot))
  "Remove duplicate found-modules after adding/removing one."
  (with-slots (found-modules)
      moonbot
    (with-slots (modules)
        (make-instance 'module)
      (let ((no-dupes (remove-duplicates found-modules :test #'eq)))
        (setf found-modules no-dupes
              modules no-dupes)))))

(defmethod (setf ubermensch) :before (new-val (moonbot moonbot))
  "Makes sure that the user doesn't attempt to remove the last ubermensch from Luna.
Signals 'cannot-perform-action if they try."
  (unless new-val
    (error 'cannot-perform-action
           :cannot-perform-action-action "Remove the last ubermensch"
           :cannot-perform-action-message "I cannot remove the last ubermensch.")))

(defmethod (setf ubermensch) :after (new-val (moonbot moonbot))
  "Remove the duplicates after the value has been changed."
  (let ((ubermensch (slot-value moonbot 'ubermensch)))
    (setf (slot-value moonbot 'ubermensch)
          (remove-duplicates ubermensch :test #'string=))))

(defclass module ()
  ((command-type
    :accessor command-type
    :initarg :command-type
    :documentation "A type, ie a symbol used to look for commands associated
with this class. The search is done in *commands*.")
   (privilege-required
    :accessor privilege-required
    :initarg :privilege-required
    :type (or ubermensch-privilege admin-privilege normie-privilege)
    :documentation "This is an instance of 'ubermensch-privilege' 'admin-privilege' 
or 'normie-privilege' this determines which level of user can execute this modules
commands")
   (prefix
    :accessor prefix
    :initarg :prefix
    :type symbol
    :documentation "The symbol used to execute commands registered by this module.
For example if the prefix was 'luna then the commands would be executed with
 .luna <command>")
   (modules
    :reader modules;this should not be modifiable by the modules themselves.
    :initarg :modules
    :type list
    :allocation :class
    :documentation "All of the other available modules.")
   (channel
    :accessor channel
    :initform (make-hash-table :synchronized t)
    :type hash-table
    :allocation :class
    :documentation "A means of communicating between modules.")
   (store
    :accessor store
    :initarg :store
    :type list
    :initform nil))
  (:documentation "Modules are used to implement modular behaviour within Luna. The ideal 
for modules is that they will at some point be .fasl files that can be loaded into a 
running instance of Luna, currently they can only be loaded/unloaded while the bot is 
running but they have to have been compiled into the lisp image.
The basic skeleton required to implement modules within Luna. 
The three slots 'command-type' 'privilege-required' and 'prefix' are required for the 
implementation of a new module."))

(defclass command ()
  ((name
    :accessor name
    :initarg :name
    :type symbol
    :documentation "This is the name of the command, this is how it is invoked. For example
if the symbol was 'HELP then the command could be something like .luna help and this 
command would be executed")
   (fun
    :accessor fun
    :initarg :fun
    :type (or symbol function)
    :documentation "The functionality associated with this command. Once the command is 
found within *commands* it is this that is evaluated with the appropriate arguments which
are determined by arg-list")
   (docs
    :accessor docs
    :initarg :docs
    :type string
    :documentation "This is a string that is used to describe what this command does")
   (arg-count
    :accessor arg-count
    :initarg :arg-count
    :documentation "The number of arguments this command requires. When defining a new 
command it is vital that this number is correct as the command execution system 
de-structures the user supplies arguments using this number.")
   (aliases
    :accessor aliases
    :initarg :aliases
    :type list
    :documentation "This is supposed to be a list of user defined aliases for this command
however this is not used.")
   (arg-list
    :accessor arg-list
    :initarg :arg-list
    :type (or null list)
    :documentation "This an alist (first second not car cdr) that associates arg names 
with their validators required to check the validity of user supplies arguments. 
For more information on this see validators.lisp"))
  (:documentation "A command is an object stored that is normally stored within *commands* 
and is used to implement some functionality. The user can normally invoke commands from 
an appropriate room. When the command is found by its name, the arg list is used to 
validate the arguments sent and the arg-count is used to determine how many times to split
the string sent to the command as its arguments. It is vital that both of these are valid"))

(defmethod print-object ((command command) stream)
  (print-unreadable-object (command stream :type t :identity t)
    (when (slot-boundp command 'name)
      (format stream "Name: ~A"
              (name command)))))

(defclass module-command (command)
  ()
  (:documentation "Default subclass for all commands defined within a module."))

(defclass community-command (command)
  ()
  (:documentation "Commands to be used within a community that can be used by normies.
Currently not used"))

(defclass admin-global-command (command)
  ()
  (:documentation "Global commands (those that can be invoked from any room) 
that only an ubermensch can use. Currently not used"))

(defclass admin-community-command (command)
  ()
  (:documentation "Commands used within a community that can be executed by ubermensch 
and that communities administrators."))

(defclass privilege ()
  ((user-id
    :accessor user-id
    :initarg :user-id
    :type string
    :documentation "The user-id of the user associated with this privilege"))
  (:documentation "These are created when the user is determined in the command system.
Determing is done by looking for the userid in a variety of places, and depending on where
it is found the instance of privileged is changed into one of 'ubermensch-privilege', 
'admin-privilege' or 'normie-privilege'"))

(defclass ubermensch-privilege (admin-privilege normie-privilege)
  ()
  (:documentation "The privilege created when the user who sent the command is found 
within (ubermensch luna). This allows the user to invoke any command."))

(defclass admin-privilege (normie-privilege)
  ()
  (:documentation "The privilege created when the user who sent the command is found
within (admins <community>). This will allow the execution of admin level commands."))

(defclass normie-privilege (privilege)
  ()
  (:documentation "The privilege created when the user is not found to be either an 
ubermensch of admin. They can only execute normie commands."))

(defclass community ()
  ((username
    :accessor username
    :initarg :username
    :initform nil
    :type (or null string)
    :documentation "This is the username that this community used to login")
   (connection
    :accessor connection
    :initform nil
    :initarg :connection
    :type (or null connection)
    :documentation "An instance of CONNECTION that is used to login to the server where
this community is located. This should be eq to a connection within (connections LUNA)")
   (top-level-space
    :accessor top-level-space
    :initform ""
    :initarg :top-level-space
    :type string
    :documentation "The top level space for that community.")
   (filters
    :accessor filters
    :initform nil
    :initarg :filters
    :type (or null list)
    :documentation "A list of filter-objects containing filter-ids associated with
nice keywords used for identifying them id. Used for recalling specific filters to /sync")
   (commands
    :accessor commands
    :initform (make-hash-table :test #'equal)
    :type hash-table
    :documentation "A local hash-table of commands within this community. 
This is currently not in use.")
   (commands-admin
    :accessor commands-admin
    :initform (make-hash-table :test #'equal)
    :type hash-table
    :documentation "A local hash-table of administrator commands within this community.
This is currently not in use.")
   (name
    :accessor name
    :initarg :name
    :initform nil
    :type (or null keyword)
    :documentation "A keyword representation of the community name. This is also used to 
invoke community based commands, for example if the name was :MY-COMMUNITY then invoking 
commands related to this community would go something like .my-community <command> <args>")
   (aliases
    :accessor aliases
    :initarg :aliases
    :initform nil
    :type (or null list)
    :documentation "A list of aliases that can be used to invoke commands in that community")
   (url
    :accessor url
    :initarg :url
    :initform nil
    :type (or null string)
    :documentation "The url of the matrix server. This should be something like 
'matrix.<my-domain>.com'")
   (api
    :accessor api
    :initarg :api
    :initform nil
    :type (or null string)
    :documentation "The current API version for the community to use, this will 
normally be '/_matrix/client/r0/'")
   (listen-in
    :accessor listen-in
    :initarg :listen-in
    :initform nil
    :type (or null list)
    :documentation "This is a list of room-ids that Luna will check for commands within.")
   (admins
    :accessor admins
    :initarg :admins
    :initform nil
    :type (or null list)
    :documentation "This is a list of user-ids denoting administrators. An administrator 
can execute admin level community commands.")
   (rooms-spellcheck
    :accessor rooms-spellcheck
    :initform (make-hash-table :test #'equal)
    :type hash-table
    :documentation "This is a hash-table of all the current room names that is used for 
spell checking room names. This currently has no use.")
   (rooms
    :accessor rooms
    :initarg :rooms
    :initform nil
    :type (or null list)
    :documentation "This is a list of plists, each plist contains an :ID key which is the 
room-id of the room within the community and :NAME which is the display name of the room
within the community.")
   (members
    :accessor members
    :initarg :members
    :initform nil
    :type (or null list)
    :documentation "This is a list of user-ids of the folks who are considered 'within' 
the community. This is normally acquired through the populate-community command.")
   (extra
    :accessor extra
    :initarg :extra
    :initform nil
    :type (or null list)
    :documentation "Extra information associated with this community. This is not used.")
   (%locks
    :reader %locks
    :initform
    (flet ((nl (&rest names)
             (loop :for name :in names
                   :appending (list name (bt:make-lock (format nil "~A" name))))))
      (nl :members :rooms :admins :listen-in :aliases)))))

(defmethod members :around ((community community))
  (quicklock (community :members)
    (call-next-method)))

(defmethod rooms :around ((community community))
  (quicklock (community :rooms)
    (call-next-method)))

(defmethod admins :around ((community community))
  (quicklock (community :admins)
    (call-next-method)))

(defmethod listen-in :around ((community community))
  (quicklock (community :listen-in)
    (call-next-method)))

(defmethod rooms-id ((community community))
  "Extracts the room id's of all the rooms within COMMUNITY"
  (mapcar (lambda (room)
            (getf  room :id))
          (slot-value community 'rooms)))

(defmethod rooms-name ((community community))
  "Extracts the room name of all the rooms within COMMUNITY"
  (mapcar (lambda (room)
            (getf room :name))
          (slot-value community 'rooms)))

(defmethod find-room-by-id ((moonbot moonbot) id)
  "Searches all of the communities within MOONBOT for a room that matches ID, then returns
it. If none are found then returns nil"
  (loop :for community :in (communities moonbot)
          :thereis (loop :for room-plist :in (rooms community)
                         :if (string= id (getf room-plist :id))
                           :return room-plist)))

(defmethod find-room-by-name ((moonbot moonbot) name)
  "Searches all of the communities within MOONBOT for a room that matches NAME, then returns
it. If none are found then returns nil"
  (loop :for community :in (communities moonbot)
          :thereis (loop :for room-plist :in (rooms community)
                         :if (string= name (getf room-plist :name))
                           :return room-plist)))

(defun valid-room-p (list)
  "Checks to make sure that the list is a valid room plist"
  (handler-case 
      (destructuring-bind (a b c d)
          list
        (and (eql a :id)
             (stringp b)
             (eql c :name)
             (stringp d)))
    (condition ()
      nil)))

(defmethod add-new-alias (alias (luna moonbot) (community community))
  (check-type alias keyword)
  (unless (loop :for community :in (communities luna)
                  :thereis (find alias (aliases community)))
    (push alias (aliases community))))

(defmethod (setf aliases) :after (new-val (community community))
  (let ((aliases (slot-value community 'aliases)))
    (setf (slot-value community 'aliases)
          (remove-duplicates aliases))))

(defmethod (setf rooms) :after (new-val (community community))
  "When a new room is added from an id get its display name and store that along
side it"
  ;;if the server goes down here this will only end up half complete, but next
  ;;attempt should fix it
  (let* ((rooms (slot-value community 'rooms)))
    (setf (slot-value community 'rooms)
          (remove-duplicates rooms :test #'string= :key (lambda (ele)
                                                          (getf ele :id))))))

(defmethod (setf listen-in) :before (new-val (community community))
  "Checks to make sure that the user isn't removing the last listen-in room. You don't want
the user to lose the ability to invoke commands for their community. If they
try then signals the condition 'cannot-perform-action"
  (unless new-val
    (error 'cannot-perform-action
           :cannot-perform-action-action "Remove the last listen-in"
           :cannot-perform-action-message "I cannot remove the last listen-in.")))

(defmethod (setf listen-in) :after (new-val (community community))
  "Automatically update the filter :listen-in when a new listen-in is added, or one
is removed."
  (let ((rooms (remove-duplicates (slot-value community 'listen-in)
                                  :test #'string=)))
    (setf (slot-value community 'listen-in) rooms)))


(defmethod (setf members) :after (new-val (community community))
  "Remove any duplicates from members after a new one is added."
  (let ((members (slot-value community 'members)))
    (setf (slot-value community 'members)
          (remove-duplicates members :test #'string=))))

(defmethod (setf admins) :before (new-val (community community))
  "Makes sure the user doesn't remove the last administrator from their community. If they 
  did that they would be unable to invoke any administrator level community commands. If they
  try then signals the condition 'cannot-perform-action"
  (unless new-val
    (error 'cannot-perform-action
           :cannot-perform-action-action "Remove the last admin"
           :cannot-perform-action-message "I cannot remove the last admin.")))

(defmethod (setf admins) :after (new-val (community community))
  "Removes the duplicates from (admins COMMUNITY) after a new one is added."
  (let ((admins (slot-value community 'admins)))
    (setf (slot-value community 'admins)
          (remove-duplicates admins :test #'string=))))

(defmethod print-object ((community community) stream)
  (print-unreadable-object (community stream)
    (format stream "Name: ~A. Room count: ~A. Member count: ~A"
            (string-capitalize (name community))
            (length (rooms community))
            (length (members community)))))

(defclass status ()
  ((latest-sync
    :accessor latest-sync)))

(defclass connection ()
  ((logged-in-p
    :initform nil
    :accessor logged-in-p)
   (filters
    :initarg :filters
    :initform nil 
    :accessor filters)
   (status
    :accessor status
    :initform (make-instance 'status))
   (url
    :accessor url
    :initarg :url
    :type string)
   (api
    :accessor api
    :initarg :api
    :type string)
   (username
    :accessor username
    :type string
    :initarg :username)
   (user-id
    :accessor user-id
    :initarg :user-id
    :type string)
   (password
    :accessor password
    :type string
    :initarg :password)
   (auth
    :accessor auth
    :type auth
    :initarg :auth)
   (encryption
    :accessor encryption
    :type encryption
    :documentation "The slot used to store the associated encryption object")
   (device-id
    :accessor device-id
    :type string)))

(defmethod print-object ((connection connection) stream)
  (print-unreadable-object (connection stream :type t :identity t)
    (format
     stream "~&URL: ~S~%Username: ~S~%Logged in: ~S~%Auth: ~S~%Device-id: ~S~%"
     (str:concat (url connection) (api connection))
     (username connection)
     (logged-in-p connection)
     (if (slot-boundp connection 'auth)
         (auth connection)
         "Not authorized yet")
     (if (slot-boundp connection 'device-id)
         (device-id connection)
         "No device ID yet"))))

(defmethod homeserver ((con connection))
  (second (str:split #\: (user-id con))))

(defun make-connection (username password url api)
  (make-instance 'connection :username username :password password
                             :url url :api api))

(defclass encryption ()
  ((olm-account
    :accessor olm-account
    :initarg :olm-account)
   (server-otk
    :accessor server-otk
    :initarg :server-otk)))

(defclass auth ()
  ((token
    :accessor token
    :initarg :token
    :type string)))

(defclass filter ()
  ((key
    :accessor key
    :initarg :key
    :type keyword)
   (id
    :accessor id
    :initarg :id
    :type (or string integer))
   (last-sync-string
    :accessor last-sync-string
    :initarg :last-sync-string
    :type (or null string))
   (next-sync-string
    :accessor next-sync-string
    :type (or null string)))
  (:documentation "Used to store data about a filter and its key"))

