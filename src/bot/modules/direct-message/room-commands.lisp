(in-package #:mm-module.direct-message)

#|
This file is implementing a similar command system to the main one, it uses the same 
commands class and an almost identitical macro for defining new context based commands.
|#


(defvar *room-commands* nil)

(defun add-room-command (command)
  (setf *room-commands* (remove (name command) *room-commands* :key #'name :test #'string=))
  (push command *room-commands*))

(defun print-room-command-information (command)
  (format-command t command ""))

(defun locate-room-command (command-name)
  (or (find command-name *room-commands* :key #'name :test #'string=)
      (error 'missing-command)))

(defmacro create-room-command (name args doc-string &body body)
  (alexandria:with-gensyms (command fun)
    `(let* ((,fun
              (lambda (proom &optional args)
                (declare (ignorable proom args))
                ,(if args
                     `(destructuring-bind
                          ,(args-from-validation-lists args
                            '(proom))
                          (str:split " " args
                                     :limit
                                     ,(length (args-from-validation-lists args))
                                     :omit-nulls t)
                        (progn
                          ,@(list-of-lists->validators args))                     
                        (locally
                            (unless (completep (context proom))
                              ,@body)))
                     `(locally
                          (unless (completep (context proom)) ,@body)))))
            (,command (make-command 'direct-message-command ',name
                                    ,doc-string ',args ,fun)))       
       (add-room-command ,command))))

(create-room-command set ((id (:maxlen 25)
                              (:minlen 1))
                          (val))
    "Given an ID (an identifier for an object, and the new value. Looks up the validator
associated with the result object found with ID and uses it to validate VAL before setting 
the result objects value to val."
  (let* ((context (context proom))
         (results (results context))
         (result (find id results :key #'id :test #'string-equal)))
    (when result
      (let ((validator (get-validator (validator result))))
        (funcall validator val);if wrong signals validation-failed
        (setf (value result) val)
        (format t "Set ~A to ~A" id val)))))

(create-room-command get ((id (:maxlen 25)
                              (:minlen 1)))
    "Given an ID searches for the result that is denoted by ID and then returns its value."
  (let* ((context (context proom))
         (results (results context))
         (result (find id results :key #'id :test #'string-equal)))
    (when result
      (if (slot-boundp result 'value)
          (format t "~A is ~A~%" id (value result))
          (format t "~A has no value yet~%" id)))))

(create-room-command finish ()
    "Tells Luna to finish up meaning she will leave the room. This will only work if all
expected results are set."
  (let* ((context (context proom))
         (results (results context)))
    (if (loop :for res :in results
              :always (slot-boundp res 'value))
        ;;leave room
        (progn (format t "Thanks! I'm off now, enjoy your day.~%")
               (setf (completep context) t))
        (format t "You haven't set all the variables required.~%"))))
;;tell Luna to clean up this PM







