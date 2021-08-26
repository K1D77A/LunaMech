(in-package #:mm-module.direct-message)

(defmethod execute-context ((proom proom))
  "Called with an instance of PROOM. With this PROOM it then changes the bindings of 
*standard-input* and *standard-output* so that all functions that follow can output 
straight to the direct message room through these streams."
  (with-accessors ((context context)
                   (input input-stream)
                   (output output-stream)
                   (room-id room-id))
      proom
    (let ((*standard-input* (make-string-input-stream (get-output-stream-string input)))
          ;;messages in
          (*standard-output* output));;messages out      
      (let* ((arg (read-line *standard-input* nil nil))
             (fun-and-arg
               (str:split " " (str:trim (str:collapse-whitespaces arg)) :limit 2)))
        (destructuring-bind (invoker &rest args)
            fun-and-arg
          (handle-command context (string-upcase invoker) proom args))))))

(defun handle-command (context invoker proom args)
  (with-accessors ((help-message help-message)
                   (function-stack function-stack)                   
                   (completep completep))
      context
    (if (string= invoker "?")
        (display-help context)
        (let ((fun (and (find invoker (functions-available context) :test #'string=)
                        (locate-room-command invoker))))
          (when fun
            (if (string= (first args) "?")
                (print-room-command-information fun)
                (execute-safely fun proom args)))))))

(defmethod display-help ((context context))
  (with-accessors ((help-message help-message))
      context
    (cond ((stringp help-message)
           (format t help-message))
          ((function help-message)
           (funcall help-message)))))

(defun execute-safely (command proom args)
  "Attempts to execute (fun COMMAND) with PROOM and ARGS, catches a validation failed 
condition and reports that exact condition, validation-failed and reports it and any 
unhandled conditions which are also reported with the exception of api-timeout which is 
allowed to pass."
  (catch-limit-exceeded
    (handler-case (progn
                    (let ((private-room (find-room (room-id proom))))
                      (log:info "with: ~A in: ~A command: ~A args: ~A"
                                (with private-room)
                                (room-id proom)
                                (name command)
                                args))
                    (apply (fun command) proom args)
                    t)
      (validation-failed (c)
        (format t "~S" c)
        nil)
      (invalid-arguments ()
        (format t "Invalid arguments.~%")
        nil)
      ((and condition (not api-timeout)) (c);;let api-timeout pass
        (format t "Unhandled condition signalled ~
                  ~%Command: ~A~
                 ~%Condition: ~A~%"
                command c)
        nil))))

