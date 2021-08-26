(in-package #:mm-module.direct-message)

#||
this is the interesting part because we want to be able to program the bot so that
it can handle certain sort of "commands" as responses so for example say you want to
dm everyone in your server and ask them for their X Y scores from spekr (coincidentally
that is the exact point of this module) then we need a way to design a means of calling a 
function that is like the ones used to generate the configs. Say for example you have a convo
where the bot says
"DO NOT IGNORE THIS MESSAGE. 
Hello I am Luna, today I would like you to complete the test at Spekr.org and tell me your 
results. To input your result simply prefix your result with 
X:<your x result> 
and
Y:<your y result>
In each instance you will be asked to verify you results. Simply respond with yes or no.
Once you have entered and confirmed your results I will leave the room and so can you.
If you leave this room without adding your results your name will be recorded.
Thanks, Luna."
Now we would need a way for the DM session to know that the current context is expecting 
exactly two results.
||#

#||
Current description of contexts. Basically when a direct message is initiated by Luna
a context is chosen, a context is associated with a room-id, and the context determines
the actions Luna is taking within that room. A context contains a list of arguments that
Luna may try and fill by asking the user to respond by calling a certain function. 
Contexts are created with only a subset of the functions that are defined within 
room-functions.lisp to make sure that the user cannot act in a way that Luna does not expect

||#


(defclass context ()
  ((results
    :accessor results
    :initarg :results
    :initform nil
    :type list
    :documentation "A list of result objects.")
   (functions-available
    :accessor functions-available
    :initarg :functions-available
    :type list)
   (context-type
    :accessor context-type
    :initarg :context-type
    :type keyword
    :documentation "a keyword denoting the 'type' of context, this is the same as the 
key used to associated the context within (contexts *module*)")
   (help-message
    :accessor help-message
    :initarg :help-message
    :documentation "A function that prints a help message to stdout")
   (initial-message
    :accessor initial-message
    :initarg :initial-message
    :type string
    :documentation "The first string sent to the user")
   (start-time
    :accessor start-time
    :initform (get-universal-time)
    :documentation "the time the context was created")
   (completed-time
    :accessor completed-time
    :documentation "The time the context was set to completed.")
   (completep
    :accessor completep
    :initform nil
    :documentation "Set when the context has accumulated all of its required results"))
  (:documentation "A context is an object that is used to determine the course of action
taken when interacting with a user through a direct message. The purpose of a context is 
to combine a variety of commands found in *room-commands* in a way that means you can 
collect the results you want, through interaction with the user."))

(defclass result ()
  ((value
    :accessor value
    :documentation "The value of result that the user has set.")
   (id
    :accessor id
    :initarg :id
    :documentation "This ID is used to find the result")
   (validator
    :accessor validator
    :initarg :validator
    :documentation "This is used to validate a value before it is finally set, if the 
validator fails then value is not set."))
  (:documentation "result objects are used to store the value of variables that you wish
a user within a DM to set."))

(defmethod print-object ((obj result) stream)
  (print-unreadable-object (obj stream)
    (format stream "ID: ~A. Value: ~A" (id obj) (value obj))))

(define-condition no-context-found (direct-message-condition)
  ((no-context-found-key
    :accessor no-context-found-key
    :initarg :no-context-found-key))
  (:documentation "Signalled when you try to gen a new context with a key but no context is
found")
  (:report
   (lambda (obj stream)
     (format stream "No context found for key: ~A~%"
             (no-context-found-key obj)))))

(defun list->results (list)
  (mapcar (lambda (list)
            (let ((sym (first list))
                  (validator (second list)))
              (get-validator validator)
              (make-instance 'result :id sym :validator validator)))
          list))

(create-new-validator validator-valid-context :valid-context
    "Checks to make sure that entry is a valid context"
    "Your input is not a valid context. Check .direct-message contexts "
  (gethash (intern (string-upcase entry) :keyword) (contexts *module*)))

(defgeneric on-context-creation (context &rest args)
  (:documentation "Executed when a context is created. Use this to hook into the context-executor.lisp generation."))

(defmethod on-context-creation :around (context &rest args)
  (declare (ignore args))
  (handler-case (call-next-method)
    (condition (c)
      (log:error "Error " c))))

(defmethod on-context-creation ((context context) &rest args)
  (declare (ignore args))
  t)

(defgeneric on-context-completion (context &rest args)
  (:documentation "Called when the context is completed."))

(defmethod on-context-completion :around (context &rest args)
  (declare (ignore args))
  (handler-case (call-next-method)
    (condition (c)
      (log:error "Error " c))))

(defmethod on-context-completion ((context context) &rest args)
  (declare (ignore args))    
  t)

(defgeneric on-context-failure (context &rest args)
  (:documentation "Called when the user leaves without finishing the context."))

(defmethod on-context-failure :around (context &rest args)
  (declare (ignore args))
  (handler-case (call-next-method)
    (condition (c)
      (log:error "Error " c))))

(defmethod on-context-failure ((context context) &rest args)
  (declare (ignore args))
  t)

(defgeneric on-room-join (context &rest args)
  (:documentation "Called when the user leaves without finishing the context."))

(defmethod on-room-join :around (context &rest args)
  (declare (ignore args))
  (handler-case (call-next-method)
    (condition (c)
      (log:error "Error " c))))

(defmethod on-room-join ((context context) &rest args)
  (declare (ignore args))
  t)

(defgeneric on-room-leave (context &rest args)
  (:documentation "Called when the user leaves without finishing the context."))

(defmethod on-room-leave :around (context &rest args)
  (declare (ignore args))
  (handler-case (call-next-method)
    (condition (c)
      (log:error "Error " c))))

(defmethod on-room-leave ((context context) &rest args)
  (declare (ignore args))
  t)

(defun make-context (class type initial-message funs help-fun result-vars)
  "Given an INITIAL-MESSAGE (string) a list of function names, these functions 
 have to have be within *room-commands* (see room-commands.lisp for examples of how
to generate these), a HELP-FUN (sym ideally) and an alist of RESULT-VARS
 (result-vars looks something like ((x :valid-compass)(y :valid-compass)) if the 
validator (the keyword argument in position (second ..) is invalid then signals the 
condition 'missing-validator.), 
validates that each symbol in FUNS is a valid  room-command and then creates a new 
 context object with each of these vars as the init args. In the instance that one of 
FUNS is not within *room-commands* then signals the condition 'missing-command"
  (mapc (lambda (fun);verify that all funs exist
          (locate-room-command fun))
        funs)  
  (make-instance class :context-type type 
                       :functions-available funs                 
                       :initial-message initial-message
                       :help-message help-fun
                       :results (list->results result-vars)))

(defun associate-context (key fun)
  "Given a KEY and a FUN, associates FUN with that KEY in (contexts *module*). 
    The fun should be a no argument function that will generate a new context object. 
This is designed to be used with the macro 'register-context'."
  (setf (gethash key (contexts *module*)) fun))

(defun gen-context (key &rest args)
  "Given a KEY which is a symbol, searches (contexts *module*) for a context registered by 
that KEY and generates it, otherwise signals condition 'no-context-found"
  (let ((context (gethash key (contexts *module*))))
    (if context
        (apply context args)
        (error 'no-context-found
               :no-context-found-key key))))

(defmacro register-context (key class result-vars initial-message
                            functions-available helper-function)
  "Simply associates a context generator with a key within (contexts *module*). 
See make-context. The call is wrapped within a function that gets funcalled. This 
function has an &REST ARGS parameter which is passed to the on-context-creation method 
after the context is created, use this with gen-context to perform any post creation
processing."
  `(associate-context ',key
                      (lambda (&rest args)
                        (let ((context (make-context ,class ,key
                                                     ,initial-message ',functions-available
                                                     ',helper-function ',result-vars)))
                          (apply #'on-context-creation context args)
                          context))))

(defun compass-format ()
  (format nil 
          "I would like you to set your political compass score from spekr.org.~%~%~
To set your score use 'set x your_score' and 'set y your_score' to set your x and y ~
value~%set x 100~%set y 100~%~%~
If you want to see your results use 'get your_variable' example:~%get x~%get y~%~%~
When you are happy with your results type finish~%finish~%~%~
I could be unresponsive, if so please give me a little time.~
~%Type ? for help and your_command ? for a description of a command."))

(defclass compass-context (context)
  ())
;;;I will add a 'on-complete' function that is called when the user enters finish
(register-context :COMPASS 'compass-context ((x :valid-compass)(y :valid-compass))
                  (compass-format)
                  (get set finish)
                  context-help-compass)


(defmethod on-context-creation ((context compass-context) &rest args)
  (declare (ignore args))
  (call-next-method))

(defun context-help-compass ()
  (format t
          "To set your score use 'set x your_score' and 'set y your_score' to set ~
your x and y ~
value~%set x 100~%set y 100~%~%~
If you want to see your results use 'get your_variable' example:~%get x~%get y~%~%~
When you are happy with your results type finish~%finish~%~%~
I could be unresponsive, if so please give me a little time.~
~%Type ? for help and your_command ? for a description of a command."))

(defclass otp-context (context)
  ())

(register-context :otp 'otp-context ()
                  (lambda (&rest args)
                    (format nil "Hi there, your otp for LunaMech's stickerpicker signup is ~A. You can leave this room. If you did not signup to use this service please contact @k1d77a:scyldings.com immediately."
                            (first args)))
                  (finish)
                  "You can type 'finish' to get me to leave, but I will leave when you leave.")

(defmethod on-context-creation ((context otp-context) &rest args)
  (with-accessors ((initial-message initial-message))
      context
    (setf initial-message (apply initial-message args))))

(defmethod on-room-join ((context otp-context) &rest args)
  "We want to automatically leave the room as soon as the user joins for this context. 
This is because we dont want any rooms that people havent left clogging up the sync."
  (let ((proom (first args)))
    (leave-dm-room proom (conn *luna*))
    (on-context-completion context args)))




