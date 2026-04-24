(in-package #:lunamech)

;;;see events.lisp for events classes

(defun make-event (content etype event-id sender origin-server-ts room-id
                   &optional (unsigned nil))
  (make-instance 'event :content content :etype etype :event-id event-id
                        :origin-server-ts origin-server-ts :room-id room-id
                        :unsigned unsigned :sender sender))

(defclass lunas-ark ()
  ((%lunas
    :accessor lunas
    :initarg :lunas
    :initform ()
    :type list
    :documentation "List of luna instances.")
   (%thread
    :accessor thread
    :initarg :thread    
    :initform nil
    :type (or null bt2:thread)
    :documentation "Background thread to do assorted thinks.")
   (%timestamp
    :accessor timestamp
    :initarg :timestamp
    :initform (local-time:now)
    :type local-time:timestamp)
   (%state
    :accessor state
    :initarg :state
    :initform :idle
    :type (member :starting :started :stopping :stopped :idle :paused))
   (%wanted-modules
    :accessor wanted-modules
    :initarg :wanted-modules
    :initform ()
    :type list)    
   (%global-modules
    :accessor global-modules
    :initarg :global-modules
    :initform ()
    :type list
    :documentation "list global modules that can only be run per single process.")))
   
(defclass lunamech ()
  ((name
    :accessor name
    :initarg :name
    :type string
    :documentation "The name for this instance of Luna")
   (config-path
    :accessor config-path
    :initarg :config-path
    :type pathname
    :documentation "The pathname of the config.")
   (communities
    :accessor communities
    :initarg :communities
    :type list
    :documentation "A list of instances of community")   
   (permissions
    :accessor permissions
    :initarg :permissions
    :initform nil
    :type list
    :documentation "A list structure mapping usernames to their various permissions.")
   (connection
    :accessor connection
    :reader conn
    :initarg :connection
    :type lunamech-matrix-api/v2:connection 
    :documentation "A single connection for this instance of luna")
   (filters
    :accessor filters
    :initarg :filters
    :documentation "A list of filter objects used for Luna wide filters. Currently unused")
   (cycle-history
    :accessor cycle-history
    :initform nil
    :documentation "This is a list of event-ids that is reset every 100 cycles. This makes 
sure that no event is handled twice by the command execution system.")
   (wanted-modules
    :accessor wanted-modules
    :initarg :wanted-modules
    :initform nil
    :type list
    :documentation "List of modules by name (string) that denote the modules we want to load.
                    This list is persistent so we know which modules to load up next start up.")
   (unloaded-modules
    :accessor unloaded-modules
    :initarg :unloaded-modules
    :initform (make-hash-table :test #'equalp)
    :type hash-table
    :documentation "Maintain a hash-table of modules by name that were previously unloaded
                    for whatever reason so they can be loaded once again.")
   (found-modules
    :accessor found-modules
    :initform nil
    :type list    
    :documentation "When Luna is started the modules within Luna are searched for within 
                    the Lisp image, this is a list of all the modules that were found.
                    This is the list of currently running modules.")
   (uber-rooms
    :accessor uber-rooms
    :initarg :uber-rooms
    :initform ()
    :type list
    :documentation "Rooms that Luna will always listen in. Ignores all encapsulation on 
communities")
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
    :initform nil
    :documentation "Set to t and actions will be performed concurrently using lparallel.")
   (timers
    :accessor timers
    :initarg :timers
    :initform (make-timers '(:clear-cycle :backup))
    :documentation "Timers for performing actions after a certain period of time.")
   (module-permissions
    :accessor module-permissions
    :initarg :module-permissions
    :initform ()
    :documentation "Mapping between modules and those who have special permissions 
for that module.")
   (%locks
    :reader %locks
    :type list
    :documentation "The locks for certain other slots."
    :initform (flet ((nl (&rest names)
                       (loop :for name :in names
                             :appending (list name (bt2:make-lock :name (format nil "~A" name))))))
                (nl :found-modules :cycle-history :thread :permissions))))
  (:documentation "Luna (Luna here) is the primary class that is used to store all 
information related to the operation and interaction with the Matrix api. It is an 
instance of Luna (Luna here) that is created from the communities.lisp config file
and it is that same instance that is backed up to the same file."))

(c2mop:ensure-finalized (find-class 'lunamech))

      
(defmethod print-object ((lunamech lunamech) stream)
  (print-unreadable-object (lunamech stream :type t :identity nil)
    (format stream "~%Community Names: ~{~A ~}~%Modules: ~{~A ~}~%Ubermensch: ~{~A ~}~%~
                   Command count: ~r~%"
            (mapcar #'name (communities lunamech))
            (mapcar #'prefix (found-modules lunamech))
            (all-ubermensch lunamech)
            (length *commands*))))



(defclass module ()
  ((command-type
    :accessor command-type
    :initarg :command-type
    :documentation "A type, ie a symbol used to look for commands associated
with this class. The search is done in *commands*.")
   (name
    :accessor name
    :initarg :name
    :type string)
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
For example if the prefix was 'lunamech then the commands would be executed with
 .lunamech <command>")
   (channel
    :accessor channel
    :initform (make-hash-table :synchronized t)
    :type hash-table
    :documentation "A means of communicating between modules.")
   (store
    :accessor store
    :initarg :store
    :type list
    :initform nil))
  (:documentation "Modules are used to implement modular behaviour within Lunamech. The ideal 
for modules is that they will at some point be .fasl files that can be loaded into a 
running instance of Lunamech, currently they can only be loaded/unloaded while the bot is 
running but they have to have been compiled into the lisp image.
The basic skeleton required to implement modules within Lunamech. 
The three slots 'command-type' 'privilege-required' and 'prefix' are required for the 
implementation of a new module."))

(defclass background-module (module)
  ((thread
    :accessor thread
    :initarg :thread
    :type bt2:thread
    :documentation "The currently running thread."))
  (:documentation "A normal module except these ones have certain methods executed 
in a completely separate thread. Currently only on-sync is processed this way.
This can work when modules are completely self contained making no modifications to 
Lunamech. An example is the RSS or Jitsi module."))

(defclass ark-module (module)
  ()
  (:documentation "A module that is run per process rather than per lunamech."))

(defclass command ()
  ((name
    :accessor name
    :initarg :name
    :type symbol
    :documentation "This is the name of the command, this is how it is invoked. For example
if the symbol was 'HELP then the command could be something like .lunamech help and this 
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
within (ubermensch lunamech). This allows the user to invoke any command."))

(defun ubermensch-privilege-p (obj)
  (typep obj 'ubermensch-privilege))

(defclass admin-privilege (normie-privilege)
  ()
  (:documentation "The privilege created when the user who sent the command is found
within (admins <community>). This will allow the execution of admin level commands."))

(defclass module-privilege (ubermensch-privilege)
  ()
  (:documentation "The privilege used to allow per module permissions. For example if you 
want to be let a user execute commands in the RSS module but you dont want them to 
have the perms to power the whole bot then is the privilege they need."))

(defclass normie-privilege (privilege)
  ()
  (:documentation "The privilege created when the user is not found to be either an 
ubermensch of admin. They can only execute normie commands."))

(defclass me-privilege (privilege)
  ()
  (:documentation "Its me! I have no permissions to perform any commands!"))

(defclass community ()
  ((top-level-space
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
                   :appending (list name (bt2:make-lock :name (format nil "~A" name))))))
      (nl :members :rooms :admins :aliases)))))


(defmethod print-object ((community community) stream)
  (print-unreadable-object (community stream)
                           (format stream "Name: ~A. Room count: ~A. Member count: ~A"
                                   (string-capitalize (name community))
                                   (length (rooms community))
                                   (length (members community)))))


