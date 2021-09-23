(defpackage #:mm-module.jitsi
  (:use #:cl #:matrix-moonbot)
  (:export #:jitsi-command
           #:jitsi-module
           #:*module*))

(in-package #:mm-module.jitsi)

(defmodule jitsi (mm-module.jitsi JITSI ubermensch-privilege)
           jitsi-command ()
           jitsi-module
           ((timers
             :accessor timers
             :initform (make-timers '(:check)))
            (rooms
             :accessor rooms
             :type list)
            (previous-counts
             :accessor previous-counts
             :initform (make-hash-table :test #'equal))))

(define-condition jitsi-condition (moonbot-condition)
  ((jitsi-condition-prefix
    :accessor jitsi-condition-prefix
    :initarg :jitsi-condition-prefix)
   (jitsi-condition-domain
    :accessor jitsi-condition-domain
    :initarg :jitsi-condition-domain)
   (jitsi-condition-url
    :accessor jitsi-condition-url
    :initarg :jitsi-condition-url)
   (jitsi-condition-message
    :accessor jitsi-condition-message
    :initarg :jitsi-condition-message
    :documentation "a message")))

;;;few more module specific commands for persistent configs
(defun save-results ()
  (when (rooms *module*);dont write empty results
    (alexandria:write-string-into-file
     (format nil "~S" (rooms *module*)) "config/jitsi-rooms.lisp"
     :if-does-not-exist :create
     :if-exists :supersede)))

(defun results-from-file ()
  (handler-case
      (let ((form (uiop:safe-read-file-form "config/jitsi-rooms.lisp")))
        (setf (rooms *module*) form))
    (file-error ()
      (warn "config/jitsi-rooms.lisp does not exist.")
      nil)))

(defmethod on-load-up (moonbot (module jitsi-module))
  (log:info "Loading Jitsi rooms from jitsi-rooms.lisp")
  (results-from-file))

(defmethod on-save (moonbot (module jitsi-module))
  (log:info "Saving Jitsi rooms to jitsi-rooms.lisp")
  (save-results)
  t)

(defmethod on-module-unload (moonbot (module jitsi-module))
  nil)

;;;main functionality that works on an on-sync

(defun clean-name (name)
  "takes in a room name and attempts to remove any traces of (..) from it."
  (let ((open (position #\( name :test #'char=))
        (close (position #\) name :test #'char=)))
    (if (and open close)
        (let ((start (str:trim-right (subseq name 0 open)))
              (end (str:trim-left (subseq name (1+ close)))))
          (format nil "~A~A" start end))
        name)))

(defmethod on-sync (luna (module jitsi-module) sync)
  (declare (ignorable luna module sync))
  (execute-stamp-n-after-luna ((find-timer (timers *module*) :check)
                               24)
;;    (log:info "Checking if rooms need their names changed.")
    (mapc (lambda (room-plist)
            (handler-case
                (initiate-name-change luna room-plist)
              (jitsi-condition ()
                nil)));in case of signalled condition do nothing
          (rooms *module*))))

(defun initiate-name-change (luna room-plist)
  "Given LUNA and a ROOM-PLIST (see (rooms *module*)) attemps to determine the number of
participants in the room and if it has changed from the previous count then updates the 
room name. Otherwise does nothing. Room names look like '(<participant count>) <name>"  
  (let* ((url (getf room-plist :url))
         (prefix (getf room-plist :prefix))
         (domain (getf room-plist :domain))
         (rooms (getf room-plist :rooms))
         (info (get-room-info url :prefix prefix :domain domain))
         (processed (mapcar (lambda (info)
                              (process-info-list info rooms))
                            info)))
    (mapc (lambda (processed);change names of those with participants
            (perform-name-change luna processed))
          processed)
    (let ((unprocessed-rooms
            (set-difference rooms processed
                            :test #'string-equal
                            :key (lambda (lst) (getf lst :id)))))
      ;;need to change the names of rooms that werent returned by the status request.
      (mapc (lambda (unprocessed)
              (process-unprocessed luna unprocessed))
            unprocessed-rooms))))

(defun process-unprocessed (luna unprocessed)
  "Given LUNA and a list of rooms that no information was returned about from the jitsi
api call then check to see if they are already at part count 0 and then set it. It is 
assumed that if no information is returned from the api call that there are no participants
in the call."
  (with-accessors ((previous-counts previous-counts))
      *module*
    (let ((id (getf unprocessed :id))
          (jitsi-id (string-downcase (getf unprocessed :jitsi-id))))
      (unless (eql (gethash jitsi-id previous-counts) 0)
        (let ((room (find-room-by-id luna id)))
          (when room 
            (change-name (conn luna)
                         id
                         (clean-name (getf room :name)))
            (setf (gethash jitsi-id previous-counts) 0)))))))

(defun process-info-list (list rooms)
  "After making a call to 'get-room-info' this will process those lists into a more useful
list. Currently it'll return a new plist with the keys :room-name :id and :participants. "
  (if (listp (first list))
      (process-info-list (first list) rooms)
      (let ((room-name (getf list :|roomname|))
            (participants (getf list :|NBparticipant|)))
        (when room-name          
          (let ((room (find room-name rooms :test #'string-equal;the strings are downcased
                                            :key (lambda (lst) (getf lst :jitsi-id)))))
            (when room 
              (list :room-name room-name
                    :id (getf room :id)
                    :participants participants)))))))

(defun perform-name-change (luna processed)
  "Given LUNA and a list that is returned by process-info-list attempts to perform a name 
change of the rooms display-name. first it will check to see if the event actually needs 
to be sent, if it does then changes the name to '(<participant count>) <name>'. No name 
change will happen if the room is not registered within a community in Luna."
  (with-accessors ((previous-counts previous-counts))
      *module*
    (let* ((id (getf processed :id))
           (jitsi-id (getf processed :room-name))
           (parts (getf processed :participants))
           (matrix-room (find-room-by-id luna id)))
      (when (and matrix-room (not (eql (gethash jitsi-id previous-counts)
                                       (parse-integer parts))))
        (let ((name (clean-name (getf matrix-room :name))))
          (change-name (conn luna) id (format nil "(~A) ~A" parts name))
          (setf (gethash jitsi-id previous-counts) (parse-integer parts)))))))

;;;module methods

(defmethod locate-command ((module jitsi-module) (priv ubermensch-privilege)
                           invoker community)
  "When prefix is 'jitsi with no privileges search for jitsi-command"
  (or (type-find 'jitsi-command invoker *commands*
                 :key #'name :test #'string-equal)
      (error 'missing-command)))

(defmethod locate-command ((module jitsi-module) priv invoker community)
  "When prefix is jitsi with no privilege just signal 'missing-command"
  (error 'missing-command))

(defmethod execute-command ((moonbot moonbot) (priv ubermensch-privilege)
                            (command jitsi-command)
                            community room message rest)
  (if (string-equal (first rest) "help")
      (print-command-information command community room)
      (safe-execution command community room message rest moonbot)))

(defmethod inform-command-is-missing
    ((priv admin-privilege) (module jitsi-module) community room)
  "ADMIN & JITSI command."
  nil)

(command-defining-macro-moonbot new-jitsi-command 'jitsi-command)

;;;a few helpers
(defun fixurl (urls &key (https nil))
  "Concatenates 'http://' or 'https:// do the start of urls if it does not already exist.
Returns a string."
  (check-type urls list)
  (if (or (str:starts-with-p "http://" (first urls))
          (str:starts-with-p "https://" (first urls)))
      (apply #'str:concat urls)
      (apply #'str:concat (if https "https://" "http://") urls)))

(defun change-name (connection room-id new-name)
  "Changes the name of ROOM-ID to NEW-NAME"
  (let ((name-event (make-instance 'm-room-name :name new-name)))
    ;;;need something to handle a m forbidden
    (send-state-event-to-room connection room-id "m.room.name" name-event)))

(defun get-room-info (url &key (prefix "") (domain "meet.jitsi"))
  (handler-case 
      (let ((res nil))
        (if (string= prefix "")
            (setf res (dex:get (concatenate 'string (fixurl (list url))
                                            "/status?domain=" domain)
                               :use-connection-pool nil))
            (setf res (dex:get
                       (concatenate 'string (fixurl (list url)) "/"
                                    prefix "/" "status?domain=" domain)
                       :use-connection-pool nil)))
        (let ((parsed (jojo:parse (babel:octets-to-string res))))
          (if (null (first parsed))
              nil
              parsed)))
    (dexador.error:http-request-failed ()
      (error 'jitsi-condition :jitsi-condition-url url
                              :jitsi-condition-domain domain
                              :jitsi-condition-prefix prefix
                              :jitsi-condition-message "Dexador errored on request"))))


(defun valid-jitsi-room-p (id)
  (let ((rooms (getf (first (rooms *module*)) :rooms)))    
    (find id rooms :test #'string= :key (lambda (lst)
                                          (getf lst :id)))))

(defun update-room-ids-jitsi-id (room-id new-jitsi-id)
  "Changes ROOM-ID's jitsi-id to NEW-JITSI-ID."
  (let* ((rooms (getf (first (rooms *module*)) :rooms))
         (room (find room-id rooms :test #'string= :key (lambda (lst) (getf lst :id)))))
    (setf (getf room :jitsi-id) new-jitsi-id)))

(defun associate-room-id-with-jitsi-room (room-id jitsi-id)
  "Adds a new association between ROOM-ID and JITSI-ID, meaning that Luna will start 
checking and changing the room name based on the number of people in the jitsi instance."
  (push (list :ID room-id :JITSI-ID jitsi-id)
        (getf (first (rooms *module*)) :rooms)))

(defun remove-room-id-and-jitsi-association (room-id)
  "Remove the association between ROOM-ID and its jitsi id, this means Luna will no longer 
check and change the name of this room depending on the number of people within the room."
  (when (valid-jitsi-room-p room-id)
    (setf (getf (first (rooms *module*)) :rooms)
          (remove room-id (getf (first (rooms *module*)) :rooms)
                  :test #'string=
                  :key (lambda (lst) (getf lst :id))))))

(new-jitsi-command help ()
    "attempts an explanation of how commands work"
  (moonhelp 'jitsi-command community room))

;;;exposed user commands
(new-jitsi-command rooms ()
    "Returns a human readable representation of all of the rooms that Luna is renaming"
  (with-accessors ((rooms rooms))
      *module*
    (when rooms
      (format t "~{~A~%~}"
              (loop :for room-list :in (getf (first rooms) :rooms)
                    :collect
                    (format nil "~A~%  Jitsi ID: ~A~%  Room ID: ~A."
                            (getf 
                             (find-room-by-id moonbot 
                                              (getf room-list :id))
                             :name)
                            (getf room-list :jitsi-id)
                            (getf room-list :id)))))))

(new-jitsi-command change-rooms-jitsi ((room-id (:minlen 1) (:maxlen 50))
                                       (jitsi-id (:minlen 1)(:maxlen 75)))
    "Changes ROOM-ID's associated jitsi room to JITSI-ID."
  (update-room-ids-jitsi-id room-id jitsi-id)
  (format t "Changed ~A's associated Jitsi instance to ~A." room-id jitsi-id))

(new-jitsi-command remove-associated-jitsi ((room-id :valid-jitsi-room))
    "Removes the association between ROOM-ID and its jitsi instance."
  (remove-room-id-and-jitsi-association room-id)
  (format t "I have removed the association between ~A and its Jitsi room. ~
                           I am no longer checking this room for members."
          room-id))

(new-jitsi-command monitor-new-jitsi ((url (:minlen 1) (:maxlen 100))
                                      (room-id (:minlen 1) (:maxlen 50))
                                      (jitsi-id (:minlen 1) (:maxlen 50)))
    "Gets Luna to modify the name of ROOM-ID based on the number of people in the JITSI-ID."
  (let ((list (list :ID room-id :JITSI-ID jitsi-id))
        (rooms (find url (rooms *module*) :test #'string= :key (lambda (ele) (getf ele :url)))))
    (if rooms
        (progn 
          (pushnew list (getf rooms :rooms) :test #'string=
                                            :key (lambda (ele) (getf ele :jitsi-id)))
          (format t "I am now monitoring ~A." jitsi-id))
        (format t "Cannot find rooms for URL: ~A." url))))

(new-jitsi-command all-urls ()
    "Returns the available URL's that Luna already has registered."
  (format t "~{ ~A~%~}" (mapcar (lambda (ele) (getf ele :url)) (rooms *module*))))
  
