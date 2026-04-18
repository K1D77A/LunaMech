(in-package #:matrix-moonbot)

(defparameter *hex-colour* "'#cc8aca'")
      
(defun moonmat (control-string &rest args)
  (format nil "[<font color='#cc8aca'>Luna</font>] ~A" (apply #'format nil control-string args)))

(defun %format-strings (strings &key coloured-prefix)
  (if coloured-prefix 
      (format nil "~{[<font color='#cc8aca'>~A</font>] ~A<br>~}"
              (mapcan (lambda (s)
                        (list coloured-prefix s))
                      strings))
      (format nil "~{[<font color='#cc8aca'>Luna</font>] ~A<br>~}" strings)))
;;(format nil "~{[Luna] ~A~%~}" strings)

(defmacro with-formatted-output-to-room ((community room
                                          &key reply-event-id author)
                                         &body body)
  (alexandria:with-gensyms (res string formatted-string)    
    `(let* ((,res nil)
            (,string
              (with-output-to-string (*standard-output*)
                (setf ,res (locally ,@body)))))
       (when (string/= ,string "")
         (let ((,formatted-string (str:split #\Newline ,string :omit-nulls t)))
           (luna-message ,community ,room
                         (%format-strings ,formatted-string :coloured-prefix ,author)
                         :reply-event-id ,reply-event-id)))
       ,res)))

(defun luna-message (community room message &key reply-event-id)
  (moon-message community room message :reply-event-id reply-event-id))


(defun moon-message (community room message &key reply-event-id)
  (let ((event (if reply-event-id
                   (object%event/m-room-message/m-text%reply message message reply-event-id)
                   (object%event/m-room-message/m-text message message))))
    (catch-potential-conditions
      (send-message-event-to-room (connection community) room event))))

(defun module-moonmat-message (connection room control-string &rest args)
  (catch-potential-conditions
    (send-message-to-room connection room (apply #'moonmat control-string args))))

(defun lunamat-message (community room control-string &rest args)
  (moonmat-message community room control-string args))

(defun moonmat-message (community room control-string &rest args)
  (moon-message community room (apply #'moonmat control-string args)))

(defun inform-user-of-error (c community room)
  (moon-message community room (moonmat "~A" c)))

(defun moon-map (fun community room function list)
  (funcall fun (lambda (ele)
                 (handler-bind (((or m-unknown m-forbidden)
                                  (lambda (c)
                                    (invoke-restart 'inform-user-of-error
                                                    c community room))))
                   (restart-case
                       (with-formatted-output-to-room (community room)
                         (funcall function ele))
                     (inform-user-of-error (c community room)
                       :report "Error occurred, inform the user of the error?"
                       (inform-user-of-error c community room)))))
           list))

(defun moon-mapc (community room function list)
  (moon-map #'mapc community room function list))

(defun moon-mapcar (community room function list)
  (moon-map #'mapcar community room function list))

(defun report-condition-to-matrix (condition message)
  (mapc (lambda (uber)
          (module-moonmat-message (conn *luna*) uber "~A ~A" message 
                                  (%emergency-format condition)))
        (uber-rooms *luna*)))

(defun report-to-matrix (message)
  (mapc (lambda (uber)
          (module-moonmat-message (conn *luna*) uber "~A" message))
        (uber-rooms *luna*))
  nil)

(defgeneric %emergency-format (condition))

(defmethod %emergency-format ((condition condition))
  (format nil "Encountered condition ~A" condition))

(defun moon-map-rooms (map-fun community message-room function &optional (exclude nil))
  (let* ((failures nil)
         (res (funcall map-fun community message-room 
                       (lambda (room)
                         (let ((%room-name (getf room :name))
                               (%room-id (getf room :id)))
                           (handler-case
                               (catch-potential-conditions
                                 (funcall function %room-name %room-id))
                             (m-forbidden (c)
                               (moonmat-message community message-room "~A"
                                                (api-error-error c)))
                             (api-error (c)
                               (moonmat-message community message-room "~A"
                                                (api-error-error c))
                               (push (list :room-id %room-id :room-name %room-name :error c)
                                     failures)))))
                       (if exclude
                           (set-difference (rooms community) exclude :test #'string=
                                                                     :key
                                                                     (lambda (x)
                                                                       (typecase x
                                                                         (list (getf x :id))
                                                                         (string x))))
                           (rooms community)))))
    (when failures
      (with-formatted-output-to-room (community message-room)
        (format t "Failures: ~{ ~A ~}"
                (mapcar (lambda (failure)
                          (destructuring-bind
                              (&key room-name room-id error)
                              failure
                            (format nil
                                    "Room-name: ~A. ~
                                    Room-id: ~A. ~
                                    Failure type: ~A.~%"
                                    room-name room-id (type-of error))))
                        failures))))
    res))

(defun moon-mapc-rooms (community room function &optional (exclude nil))
  (moon-map-rooms #'moon-mapc community room function exclude))

(defun moon-mapcar-rooms (community room function &optional (exclude nil))
  (moon-map-rooms #'moon-mapcar community room function exclude))

(defun find-room-either (community id-or-name)
  (find-room community id-or-name))

(defun %moon-map-communities (mapper function)
  "map over all communities in *luna* calling MAPPER and FUNCTION"
  (with-accessors ((communities communities))
      *luna*
    (funcall mapper function communities)))

(defun moon-mapc-communities (function)
  (%moon-map-communities #'mapc function))

(defun moon-mapcar-communities (function)
  (%moon-map-communities #'mapcar function))

(defun moon-some-communities (function)
  (%moon-map-communities #'some function))

(defun find-room-in-communities (id-or-name)
  (moon-mapc-communities
   (lambda (community)
     (let ((room? (find-room community id-or-name)))
       (when room?
         (return-from find-room-in-communities 
           (values room? community)))))))
  

(defun find-room (community id-or-name)
  "Given an string in ID-OR-NAME looks in (rooms COMMUNITY) for a room whose :id
  or :name key matches ID-OR-NAME then returns the room list"
  (loop :for room :in (rooms community)
          :thereis (cond ((string= (getf room :id) id-or-name)
                          room)
                         ((string= (getf room :name) id-or-name)
                          room))))

(defun string-bool-to-bool (community room string)
  "convert any of the follwoing: true false yes no y n t f 1 0 nil  into a bool"
  (cond ((some (lambda (str)
                 (string-equal string str))
               '("true" "t" "1" "yes" "y"))
         t)
        ((some (lambda (str)
                 (string-equal string str))
               '("false" "f" "0" "no" "n" "nil"))
         
         nil)
        (t (moonmat-message
            community room ":string-bool should be any of the following ~
            t nil yes no y f true false 0 1")
           :error)))

(defun moonhelp (type community room)
  (let ((strings))
    (push
     (format
      nil "Type '<font color=~A>command-name help</font>' for a description" *hex-colour*)
     strings)
    ;;(format nil "Type 'command-name help' for a description") strings)
    (moon-mapc community room
               (lambda (command)
                 (push
                  (format nil
                          "The command '<font color=~A>~A</font>' ~
                           requires ~r argument~:p ~A~%"
                          *hex-colour*
                          (string-capitalize (name command))                          
                          (arg-count command)                          
                          (format nil
                                  "~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}"
                                  (mapcar
                                   (lambda (sym)
                                     (let ((name (symbol-name sym)))
                                       (format nil "<font color='#cc8aca'>~A</font>"
                                               name)))
                                   (args-from-validation-lists
                                    (arg-list command)))))

                  strings))
               (remove-if-not (lambda (ele)
                                (eql (type-of ele) type))
                              *commands*))
    (format t "~{~A~}" strings)))

(defun bot-member-id-p (id)
  "Checks if a string is a bot member. This is really just testing for _discord_ in the name"
  (or (str:containsp "_discord_" id)
      (str:containsp "_webhook_" id)))

(defun non-bot-members (community)
  "Attempts to remove members that appear to be bots from the list of (members COMMUNITY)"
  (remove-if #'bot-member-id-p (members community)))

(defmethod add-room ((community community) (new-val list))
  (when (valid-room-p new-val)
    (pushnew new-val (rooms community) :key (lambda (x)
                                              (getf x :id))
                                       :test #'string=)))

(defmethod add-room ((community community) (new-val string))
  (unless (find new-val (rooms community)  :key (lambda (x) (getf x :id))
                                           :test #'string=)
    (let ((room-name
            (catch-potential-conditions
              (get-room-events (connection community)
                               new-val "m.room.name"))))
      (push (list :id new-val :name (second room-name)) (rooms community)))))

(defun clean-user-id (user-id)
  "Removes the @ from the start of USER-ID, hopefully stopping them from getting pinged."
  (if (char= #\@ (aref user-id 0))
      (subseq user-id 1)
      user-id))

(defun same-homeserver-p (connection user-id)
  (string= (homeserver connection)
           (second (str:split #\: user-id))))

