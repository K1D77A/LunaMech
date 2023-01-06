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
and the name of its module subclass."))

(defmethod module-information (module)
  "Default fallback."
  nil)

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

(defmethod module-loaded-p (luna module)
  (and (find module (found-modules luna)) t))

(defgeneric hotload-module (luna sym)
  (:documentation "Attempts to add a module denoted by SYM into Luna as she is running. 
This will start all of the method calls and immediately invoke 'on-module-hotload."))

(defmethod hotload-module ((luna luna) sym)
  "Load a module into Moonbot while Moonbot is running. SYM should be the prefix
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
      (format t "Hotload of ~A~%Successful~%" module)
      (log:info "Hotload of ~A~%Successful" module))))

(defmethod sym->module-name ((luna luna) sym)
  (cdr (assoc sym (modules luna) :test #'string=)))

(defgeneric unload-module (luna sym)
  (:documentation "Removes the module denoted by SYM from Luna, meaning all the 
methods that would normally be called during the use of Luna will no longer be called."))

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

(defmethod execute-command ((moonbot moonbot) (priv ubermensch-privilege)
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
form of usocket condition. Module is not being unloaded. Condition: ~A" module c)
                nil);just return nil no probs
              ;;chances are that if the condition above is called then Luna will very
              ;;quickly realize that the connection is broken and will simply initiate a
              ;;restart which should fix this.
              (serious-condition (c)
                (log:error "Module: ~A signalled the condition ~A When executing its self ~
       defined '~A' method. Removing the module from Moonbot. Please fix this."
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
                 "This is called before a module is unloaded from Moonbot,
 this can be used by
modules to perform operations just before a module is unloaded.")

(new-module-hook on-shutdown (luna module)
                 "This is called before Luna shuts down, is unloaded from Moonbot,
 this can be used by
modules to perform operations just before Luna goes down.")

(new-module-hook on-save (luna module)
                 "this is called before Moonbot is saved, this can be used by modules to
perform operations at save time.")

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
       defined '~A' method. Removing the module from Moonbot. Please fix this."
        module c 'on-sync)
       (report-condition-to-matrix c "Encountered error with module.")
       (trivial-backtrace:print-backtrace c)
       (unless (typep module 'mm-module.rss:rss-module)         
         (unload-module luna module)))))

(defmethod on-sync :around (luna module sync)
  (%on-sync-body))

(defmethod on-sync :around (luna (module background-module) sync)
  "This is the most primitive version I could use. Those modules that subclass from 
background-module will be executed entirely in their own thread."
  (let ((thread (bt:make-thread (lambda ()
                                  (ignore-errors
                                   (bt:with-timeout (300);force a timeout after 300 seconds
                                     (%on-sync-body)))))))
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

