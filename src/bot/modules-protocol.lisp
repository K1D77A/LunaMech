(in-package #:matrix-moonbot)

#||
Okay so this code is used to create 'modular' functionality within Luna (Moonbot).
Each module is its own package and has to export a subclass of 
COMMAND, MODULE and the variable *module*. 
Basically within Luna there is a slot called MODULES which is a list of symbols like so:
'(COMMON-LISP::RSS COMMON-LISP::SITE COMMON-LISP::WEBHOOK ...)
and another slot called FOUND-MODULES which is a list of the values of *module* found within 
each modules package, this is exported by each modules package.
A module package starts with the prefix "mm-module.<name>".
Currently the function FIND-MODULE is used to resolve symbols to module names:
MOONBOT> (find-module 'sticker)
.. some console output ..
#<MM-MODULE.STICKER:STICKER-MODULE {10020E1263}>

Module symbols are stored in a file on disk along with more state information about 
Lunas previous session and when loaded the modules are resolved using (find-module ..)

I am not sure if this is the optimal way to create package based modular functionality. 

At the bottom you can see a variety of methods like ON-SYNC and ON-SAVE which are 
executed at various points within the execution of Luna's main process. 
An example is like so:

(defmethod on-load-up (moonbot (module jitsi-module))
(log:info "Loading Jitsi rooms from jitsi-rooms.lisp")
(results-from-file))

(defmethod on-save (moonbot (module jitsi-module))
(log:info "Saving Jitsi rooms to jitsi-rooms.lisp")
(save-results)
t)


I need a way to have persistent per module extra perms.
I guess we can just store mappings between modules and 

||#


(defgeneric find-module (o sym &optional silent))

(defgeneric find-modules (o))

(defgeneric module-loaded-p (o module))

(defgeneric hotload-module (o sym)
  (:documentation "Attempts to add a module denoted by SYM into Luna as she is running. 
This will start all of the method calls and immediately invoke 'on-module-hotload."))

(defgeneric sym->module-name (o sym))

(defgeneric unload-module (o sym)
  (:documentation "Removes the module denoted by SYM from Luna, meaning all the 
methods that would normally be called during the use of Luna will no longer be called."))


(defun register-module (name package)
  "Registers the module within Luna. NAME is a symbol denoting the name of the module, and
PACKAGE is a symbol denoting the package that the symbols for that module exist. An example
(register-module 'rss 'mm-module.rss). Doing this saves having to keep a 
manual list."
  (pushnew (cons name package) (modules (make-instance 'moonbot))
           :test #'string-equal
           :key #'car))

(defgeneric module-information (module)
  (:documentation "Evaluates to a plist of information within a module. The information 
should at the very least contain the variable *module*, the name of its command subclass
and the name of its module subclass.")
  (:method (m)
    nil))

(defmacro defmodule (name (package prefix privilege-required &rest module-args)
                     command-class command-slots module-class module-slots
                     &key (module-superclass nil))
  `(let ((*package* (find-package ',package)))
     (defclass ,command-class (module-command)
       ,command-slots)
     ,(if module-superclass
          `(defclass ,module-class ,module-superclass
             ,module-slots)
          `(defclass ,module-class (module)
             ,module-slots))
     (register-module ',name ',package)
     (defvar ,(intern (string-upcase "*module*") package)
       (new-module ',module-class ',prefix
                   ',command-class ',privilege-required
                   ,module-args))
     (export (list ',(intern (string-upcase "*module*") package)
                   ',command-class ',module-class))
     (defmethod module-information ((module ,module-class))
       (list :module ,(intern (string-upcase "*module*") package)
             :command ',command-class
             :privilege ',privilege-required))))

(defun check-found-modules (luna prefix)
  "Loops through (found-modules LUNA) looking for a module whose prefix is PREFIX."
  (let ((mods (found-modules luna)))
    (find prefix mods :key #'prefix :test #'string-equal)))

(defun new-module (class prefix command-type privilege-required
                   &rest args)
  (apply #'make-instance class
         (apply #'concatenate 'list 
                (list 
                 :prefix prefix 
                 :command-type (make-instance command-type)
                 :privilege-required (make-instance privilege-required))
                args)))



(defmethod inform-command-is-missing (privilege (module module) community room)
  nil)

(defmethod inform-command-is-missing ((priv ubermensch-privilege)
                                      (module module) community room)
  "UBER default."
  (format t "Admin, that ~A command is missing, try .~A help"
          (string-downcase (symbol-name (type-of module)))
          (string-downcase (prefix module))))

(defmethod locate-command ((module module) priv invoker community)
  "Signal missing command by default."
  (error 'missing-command))

(defmethod locate-command ((module module) (priv ubermensch-privilege)
                           invoker community)
  "When prefix is 'admin with no privileges search for admin-command"
  (or (type-find (type-of (command-type module)) invoker *commands*
                 :key #'name :test #'string-equal)
      (error 'missing-command)))

(defmethod execute-command ((luna luna) (priv ubermensch-privilege)
                            (command command)
                            community room message rest)
  (if (string-equal (first rest) "help")
      (print-command-information command community room)
      (safe-execution command community room message rest)))


(defgeneric pass-to-module (module key &rest args)
  (:documentation "Stores data within lunas module-channel so that it can be accessed 
in a thread safe way for that module. Allows communication between modules."))

(defmethod pass-to-module ((module module) key &rest args)
  "default method for passing information to modules."
  (symbol-macrolet ((%hash (gethash module (channel module))))
    (let ((hash? %hash))
      (unless (hash-table-p hash?)
        (setf %hash (make-hash-table :synchronized t)))
      (unless (typep (gethash key %hash) 'lparallel.queue:queue)
        (setf (gethash key %hash) (lparallel.queue:make-queue)))
      (lparallel.queue:push-queue args (gethash key %hash)))))

(defmacro new-module-hook (name args docstring)
  "Is used to define a new hook that the a module can use. ARGS must contain atleast the 
elements luna and module as symbols. Docstring is simpley a descriptor. 
Defines a generic by NAME, a default method that evals to nil and an :around method that
 when it catches any subclass of condition unloads the module"
  `(progn (defgeneric ,name ,args (:documentation ,docstring))
          (defmethod ,name ,args nil)
          (defmethod ,name :around ,args
            (handler-case
                (call-next-method)
              ((or usocket:socket-condition
                usocket:ns-condition
                usocket:socket-error
                usocket:timeout-error
                usocket:unknown-error
                usocket:ns-error) (c)
                (log:error "Module: ~A attempted some network activity that signalled some 
form of usocket condition. Module is now being unloaded. Condition: ~A" module c)
                nil);just return nil no probs
              ;;chances are that if the condition above is called then Luna will very
              ;;quickly realize that the connection is broken and will simply initiate a
              ;;restart which should fix this.
              (serious-condition (c)
                (log:error "Module: ~A signalled the condition ~A When executing its self ~
       defined '~A' method. Removing the module from Luna. Please fix this."
                           module c ',name)
                (unload-module luna module)
                (report-condition-to-matrix c "Encountered error with module.")
                (trivial-backtrace:print-backtrace c))))))
;;  (setf (found-modules luna)
;;      (remove module (found-modules luna))))))))

(new-module-hook on-load-up (luna module)
                 "This is called when a module is loaded. This is called when Luna starts.")

(new-module-hook on-login (luna module)
                 "This is called when Luna first logs in")

(new-module-hook on-restart (luna module)
                 "This is called when Luna starts up but doesn't login")

(new-module-hook on-module-hotload (luna module)
                 "This is called when a module is hotloaded, this can be used by modules to
    perform operations just after they are loaded.")

(new-module-hook on-module-unload (luna module)
                 "This is called before a module is unloaded from Luna,
 this can be used by
modules to perform operations just before a module is unloaded.")

(new-module-hook on-shutdown (luna module)
                 "This is called before Luna shuts down, is unloaded from Luna,
 this can be used by
modules to perform operations just before Luna goes down.")

(new-module-hook on-save (luna module)
                 "this is called before Luna is saved, this can be used by modules to
perform operations at save time.")

(new-module-hook on-message (luna module community room privilege message text)
                 "This is called when the privilege for a message is determined.")


;;need an on-shutdown
(defgeneric on-sync (luna module sync)
  (:documentation "This hook allows a module to perform some actions based on a sync list"))

(defmethod on-sync (luna module sync) nil)

(defmacro %on-sync-body ()
  `(handler-case (call-next-method)
     ((or usocket:socket-condition usocket:ns-condition
       usocket:socket-error usocket:timeout-error
       usocket:unknown-error usocket:ns-error)
       (c)
       (log:error
        "Module: ~A attempted some network activity that signalled some 
form of usocket condition. Module is not being unloaded. Condition: ~A"
        module c)
       nil)
     (serious-condition (c)
       (log:error
        "Module: ~A signalled the condition ~A When executing its self ~
       defined '~A' method. Removing the module from Luna. Please fix this."
        module c 'on-sync)
       (report-condition-to-matrix c "Encountered error with module.")
       (trivial-backtrace:print-backtrace c)
       (unload-module luna module))))

(defmethod on-sync :around (luna module sync)
  (%on-sync-body))

(defmethod on-sync :around (luna (module background-module) sync)
  "This is the most primitive version I could use. Those modules that subclass from 
background-module will be executed entirely in their own thread."
  (let ((thread (bt2:make-thread (lambda ()
                                  (ignore-errors
                                   (bt:with-timeout (300);force a timeout after 300 seconds
                                     (%on-sync-body))))
                                 :name (format nil "~A-on-sync-background" (name luna))
                                 :initial-bindings `((*package* . ,*package*)
                                                     (*error-output* . ,*error-output*)
                                                     (*standard-output* . ,*standard-output*)
                                                     (dex:*connection-pool* .
                                                                            ,dex:*connection-pool*)
                                                     (*luna* . ,luna)
                                                     ,@bt2:*default-special-bindings*))))
                                 
    (setf (thread module) thread)))
  ;;(%on-sync-body))

(defun execute-all-communications-between-modules (luna module sync)
  (with-accessors ((channel channel))
      module
    (unless (zerop (hash-table-count channel))
      (let ((hash (gethash module channel)))
        (when hash 
          (maphash (lambda (keyword queue)
                     (when keyword
                       (unwind-protect 
                            (apply #'execute-module-communications luna module sync keyword
                                   (lparallel.queue:pop-queue queue))
                         (when (lparallel.queue:queue-empty-p (gethash keyword hash))
                           (remhash keyword hash)))))
                   hash))))))

(new-module-hook execute-module-communications (luna module sync key &rest arguments)
                 "allows for communication between modules. This is called after on-sync.")

