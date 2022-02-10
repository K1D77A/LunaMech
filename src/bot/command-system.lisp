(in-package #:matrix-moonbot)

(defvar *commands* nil)

(defun make-command (type name docs-string arguments symbol/function)
  (make-instance type :docs docs-string :name name
                      :fun symbol/function :arg-count (length arguments)
                      :arg-list arguments))

(defun add-command (command)
  (let* ((name (name command))
         (type (class-of command)))
    (setf *commands* (type-remove type name *commands* :key #'name :test #'eql))
    (push command *commands*)))

(defun format-command (stream command)
  (format stream
          "The command '~A' has the description ~
            \"~A\" and it requires ~r argument~:p ~A~%"
          (string-capitalize (name command))
          (docs command)
          (arg-count command)                          
          (format nil
                  "~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}"
                  (mapcar #'symbol-name
                          (args-from-validation-lists
                           (arg-list command))))))

(defun print-command-information (command community room)
  (catch-limit-exceeded
    (with-formatted-output-to-room (community room)
      (format-command t command))))

(defun type-find (type item sequence &key from-end (start 0) end key (test #'equal))
  "Just like find except checks the type of ele in the sequence against TYPE
before checking for equality. useful for searching for only certain
instances of a class in a sequence. Because it is checking the type of objects
:key no longer functions like normal, instead :key is called on the object when
it matches the desired type."
  (find item sequence :from-end from-end :start start
                      :end end 
                      :test (lambda (item ele)
                              (when (typep ele type)
                                (funcall test item (if key
                                                       (funcall key ele)
                                                       ele))))))

(defun type-remove (type item sequence &key from-end (start 0) end key
                                         (test #'equal))
  "The same as type-find but removes."
  (remove item sequence :from-end from-end :start start
                        :end end 
                        :test (lambda (item ele)
                                (when (typep ele type)
                                  (funcall test item (if key
                                                         (funcall key ele)
                                                         ele))))))

(defgeneric locate-command (prefix priv invoker community)
  (:documentation "Find the appropriate command given the senders privilege,
the prefix, the command and the community it was sent in."))

(defmethod locate-command (prefix priv invoker community)
  "Default. Just search for community based, non privileged commands."
  (or (type-find 'community-command invoker *commands* '
                 :key #'name :test #'string-equal)
      ;;need to check within the communities local commands
      (error 'missing-command)))

(defmethod locate-command (prefix (priv admin-privilege) invoker community)
  "When community admin with no prefix search for admin-community-commands"
  (or (type-find 'admin-community-command invoker *commands* :key #'name :test #'eql)
      ;;need to check within the communities local commands
      (error 'missing-command)))

(defun %already-processed-message-p (luna event-id)
  "Checks if a message has already been processed within LUNA."
  (and (find event-id (cycle-history luna) :test #'string=) t))

;;;store the message event in a short history to stop repeat executions.
(defmethod initiate-command-execution :before
    (luna priv prefix invoker community room message rest)
  (let ((event-id (gethash "event_id" message)))
    (if (%already-processed-message-p luna event-id)
        (error 'already-processed)
        (sb-ext:atomic-push event-id (slot-value luna 'cycle-history)))))

(defmethod initiate-command-execution
    (luna privilege prefix/module invoker community room message rest)
  (restart-case      
      (let ((command (locate-command prefix/module privilege invoker community)))        
        (execute-command luna privilege command community room message rest))
    (tell-user ()
      :report "Command is missing, inform user?"
      (inform-command-is-missing privilege prefix/module community room))))

(defgeneric inform-command-is-missing (privilege prefix/module community room)
  (:documentation "Decides whether to inform the user that a command is missing"))

(defmethod inform-command-is-missing (privilege prefix/module community room)
  "Default is don't."
  nil)

(defmethod inform-command-is-missing :around (privilege prefix/module community room)
  "Wrap the body in an with-formatted-output-to-room"
  (with-formatted-output-to-room (community room)
    (call-next-method)))

(defmethod inform-command-is-missing
    ((priv ubermensch-privilege) prefix community room)
  "UBER & COMMUNITY command"
  (format t "Admin, that community command is missing, try .<community> help"))

(defmethod inform-command-is-missing
    ((priv admin-privilege) prefix community room)
  "ADMIN & COMMUNITY command."
  (format t "Community admin, that community command is missing, ~
                                try .<community> help"))

(defmethod execute-command ((luna luna) priv command community
                            room message rest)
  (if (string-equal (first rest) "help")
      (print-command-information command community room)
      (progn (log:info "Executing: ~A. Community: ~A. Room: ~A. Caller: ~A. Args: ~A."
                       (name command) (name community) room (user-id priv) rest)
             (safe-execution command community room message rest))))

(defun safe-execution (command community room message args &optional (luna nil))
  "Execute the given COMMAND and catch and format any errors."
  (check-type luna (or null luna))
  (catch-limit-exceeded
    (handler-case
        (let ((connection (connection community))
              (community community)
              (moonbot luna)
              (luna luna))
          (declare (special connection community moonbot luna))
          (execute command community room message args luna)) 
      (api-timeout (c)
        (error c))
      (error (c)
        (format-condition community room c command)))))

(defgeneric format-condition (community room condition command)
  (:documentation "Sends a properly formatted output for CONDITION to COMMUNITY ROOM 
and COMMAND."))

(defmethod format-condition :around (community room condition command)
  (with-formatted-output-to-room (community room)
    (call-next-method)))

(defmethod format-condition (community room (condition invalid-argument-count) command)
  (format t "Incorrect argument count. I expected ~r argument~:p but I received ~r."
          (invalid-argument-count-expected condition)
          (invalid-argument-count-count condition)))

(defmethod format-condition (community room (condition validation-failed) command)
  (format t "Your input is bad: ~A~%~A"
          (validation-failed-entry condition)
          (validation-failed-message condition)))

(defmethod format-condition (community room (condition cannot-perform-action) command)
  (format t "~A" (cannot-perform-action-action condition)))

(defmethod format-condition (community room (condition invalid-arguments) command)
  (format t "Invalid arguments."))

(defmethod format-condition (community room condition command)
  (format t "Unhandled condition signalled Community: ~A~%Room: ~A~%Command: ~A~
             ~%Condition: ~A~%Condition type: ~A"
          (name community) room (name command)
          (str:replace-all #.(string #\Newline) " " (format nil "~A" condition))
          (type-of condition)))

(defgeneric execute (command &rest args)
  (:documentation "Executes the COMMAND using ARGS. ARGS should be a list of 
community room message args and optionally luna. If luna is provided it should 
be in the last position within args."))

(defmethod execute (command &rest args)
  ;; community room message args &optional (luna nil))
  (destructuring-bind (community room message args &optional luna)
      args
    (with-formatted-output-to-room (community room)
      (handler-case
          (if luna
              (apply (fun command) luna community room message args)
              (apply (fun command) community room message args))
        (m-forbidden ()
          (format t "I don't have the privileges to do that."))))))

#||
command-defining-macro-no-moonbot and command-defining-macro-moonbot are both functionally
the same as they both define a macro for defining commands within a module. 
the first argument NAME is the name of a macro you use to define new commands and 
COMMAND-TYPE is a class of subclass COMMAND that each command will be, the default
is 'new-normie-community-command as the macro and 'community-command as the type. 
The macro generated produces instances of those commands and puts them into *commands*.
where they can be looked up. 
The macro itself takes three arguments its name args a doc-string and the body.
The NAME denotes the name of the command (the slot name in a command), 
the args is a list that is best shown by demonstration:
((user-id :valid-user) (reason (:maxlen 40)(:minlen 1)))
it is a list of argument names which can be referenced in body and 
keyword validators which most are listed in validator.lisp, and the 
validators arguments. The last argument BODY is what is executed. 

The fun slot of the generated command is an anonymous function
that takes the arguments:
moonbot (when not no-moonbot) community room message &optional args .
These are the argument passed when executing a function. 
The macro generates a list of functions that are executed to validate that the arguments
provided match their appropriate validators, if they dont then 'validation-failed 
is signalled, this is only done when args are expected. 
If args are expected by not received then 'invalid-argument-count is signalled,
same if an incorrect number of arguments are provided.
If argument validation completes then the body is executed wrapped in the macro 
with-formatted-output-to-room this macro lexically rebinds *standard-output* and 
after body is executed sends any output that was sent to *standard-output* to 
room. This is useful for responding to the invoker in the room they invoked Luna.
||#


(defmacro command-defining-macro-no-moonbot (name command-type)
  "See doc block in command-system.lisp"
  `(defmacro ,name (name args doc-string &body body)
     (alexandria:with-gensyms (command fun lambda split)
       `(let* ((,fun
                 (lambda (community room message &optional args)
                   (declare (ignorable message args))
                   ,(if args
                        `(let ((,lambda ,(length
                                     (args-from-validation-lists args
                                      '(community room message))))
                               (,split (str:split " " args :limit
                                                  ,(length
                                                    (args-from-validation-lists args))
                                                  :omit-nulls t)))
                           (if (/= ,lambda (length ,split))
                               (error 'invalid-argument-count
                                      :invalid-argument-count-expected ,lambda
                                      :invalid-argument-count-count (length ,split))
                               (destructuring-bind
                                   ,(args-from-validation-lists args
                                     '(community room message))
                                   ,split                                 
                                 (progn
                                   ,@(list-of-lists->validators args)
                                   (with-formatted-output-to-room (community room)                                   
                                     ,@body)))))
                        `(with-formatted-output-to-room (community room) ,@body))))
               (,command (make-command ,',command-type ',name
                                       ,doc-string ',args ,fun)))
          (add-command ,command)))))

(command-defining-macro-no-moonbot new-normie-community-command
                                   'community-command)

(command-defining-macro-no-moonbot new-admin-community-command
                                   'admin-community-command)

(defmacro command-defining-macro-moonbot (name command-type)
  "See doc block in command-system.lisp"
  `(defmacro ,name (name args doc-string &body body)
     (alexandria:with-gensyms (command fun lambda split)
       `(let* ((,fun
                 (lambda (moonbot community room message &optional args)
                   (declare (ignorable moonbot message args))
                   ,(if args
                        `(let ((,lambda ,(length
                                     (args-from-validation-lists args
                                      '(community room message))))
                               (,split (str:split " " args :limit
                                                  ,(length
                                                    (args-from-validation-lists args))
                                                  :omit-nulls t)))
                           (if (/= ,lambda (length ,split))
                               (error 'invalid-argument-count
                                      :invalid-argument-count-expected ,lambda
                                      :invalid-argument-count-count (length ,split))
                               (destructuring-bind
                                   ,(args-from-validation-lists args
                                     '(community room message))
                                   ,split                                 
                                 (progn
                                   ,@(list-of-lists->validators args)
                                   (with-formatted-output-to-room (community room)
                                     ,@body)))))
                        `(with-formatted-output-to-room (community room) ,@body))))
               (,command (make-command ,',command-type ',name
                                       ,doc-string ',args ,fun)))
          (add-command ,command)))))
