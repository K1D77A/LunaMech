(in-package #:lunamech)

(defmethod make-instance :around ((class community) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (let ((instance (call-next-method)))
    (setf (slot-value instance '%locks)
          (apply #'make-locks (locks-for-object instance)))))

(defmethod locks-for-object ((community community))
  '(:members :rooms :admins :aliases))

                             
(defmethod initiate-room-spellchecker ((community community))
  (with-accessors ((rooms-spellcheck rooms-spellcheck)
                   (rooms rooms))
      community
    (spellcheck:train
     (mapcar (lambda (room-plist)
               (string-downcase (pkv room-plist :NAME)))
             rooms))))



(defmethod members :around ((community community))
  (quicklock (community :members :read)
    (call-next-method)))

(defmethod (setf members) :around (newval (community community))
  (quicklock (community :members :write)
    (call-next-method)))


(defmethod rooms :around ((community community))
  (quicklock (community :rooms :read)
    (call-next-method)))

(defmethod (setf rooms) :around (new-val (community community))
  (quicklock (community :rooms :write)
    (call-next-method)))


(defmethod admins :around ((community community))
  (quicklock (community :admins :read)
    (call-next-method)))

(defmethod (setf admins) :around (new-val (community community))
  (quicklock (community :admins :write)
    (call-next-method)))


(defmethod aliases :around ((community community))
  (quicklock (community :aliases :read)
    (call-next-method)))

(defmethod (setf aliases) :around (new-val (community community))
  (quicklock (community :aliases :write)
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

