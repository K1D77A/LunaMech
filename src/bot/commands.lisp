(in-package #:matrix-moonbot)

(new-admin-community-command echo ((to-echo (:maxlen 40) (:minlen 1)))
    "repeats what you say back to you"
  (format t "~A" to-echo))

(new-admin-community-command rate-limit ((count))
    "hits rate limit"
  (dotimes (i (parse-integer count))
    (catch-limit-exceeded 
      (with-formatted-output-to-room (community room)
        (format t"message: ~D" (1+ i))))))

(new-admin-community-command listen-in ()
    "Prints out the rooms that this community is listening in"
  (format t "Listening in:~%~{  ~A~%~}" (listen-in community)))

(new-admin-community-command add-listen-in ((room-id :valid-room))
    "Adds a room that the bot will listen in"
  (push (getf (find-room community room-id) :id) (listen-in community))
  (format t "Now listening in ~A" room-id))

(new-admin-community-command remove-listen-in ((room-id :valid-listen-in))
    "Removes a room that the bot listens in"
  (setf (listen-in community)
        (remove room-id (listen-in community) :test #'string=))
  (format t "No longer listening in ~A" room-id))

(new-admin-community-command message-community ((to-send (:maxlen 40) (:minlen 1)))
    "Sents a message to all rooms in a community"
  (moonmat-message community room "Sending ~A to all rooms in 5 seconds" to-send)
  (sleep 5)
  (let ((fun (lambda (room-name room-id)
               (declare (ignore room-name))
               (moon-message community room-id to-send))))
    (moon-mapc-rooms community room fun (list room))
    (format t "Message sent")))

(new-admin-community-command remove-room ((removing :valid-room))
    "Removes a room from the community"
  (let ((to-remove (getf (find-room community removing) :id)))
    (format t "Removing ~A from community" to-remove)
    (setf (rooms community) (remove to-remove (rooms community)
                                    :test #'string=
                                    :key (lambda (room)
                                           (getf room :id))))))

(new-admin-community-command find-room ((room-name (:minlen 1)))
    "Just finds an id from a name"
  (format t "~A" (getf (find-room community room-name) :id)))

(new-admin-community-command add-room ((room-id (:minlen 1)))
    "Adds a room to the community"
  (let ((to-add (remove #\' room-id :test #'char=)))
    (format t "Adding ~A to community" to-add)
    (handler-case 
        (add-room community to-add)
      (warning ()
        (format t "Bad room ~A, ignoring" to-add)))))

(new-admin-community-command join-room ((room-id :valid-room (:minlen 1)))
    "Join a room in the community"
  (format t "Joining room ~A." room-id)
  (let ((room (getf (find-room community room-id) :id)))
    (join-room (connection community) room)))

(new-admin-community-command add-rooms ((rooms (:maxlen 1000)
                                               (:minlen 20)))
    "Adds a list of rooms to the community. The list must be comma separated." 
  (let ((rooms (str:split "," rooms :omit-nulls t)))
    (mapc (lambda (room)
            (add-room community room))
          rooms)
    (format t "Adding rooms ~A to community" rooms)))

(new-admin-community-command displayname ((user-id :valid-user (:minlen 1)))
    "displays a users displayname"  
  (let ((name (user-display-name (connection community) user-id)))
    (format t  "Display name: ~A" (getf name :|displayname|))))

(new-admin-community-command kick ((user-id :valid-user)
                                   (reason (:maxlen 40)(:minlen 1)))
    "kicks a user from the community"
  (format t "Kicking ~A from ~A" user-id (string-capitalize (name community)))
  (moon-mapc-rooms community room
                   (lambda (room-name room-id)
                     (format t  "Kicking ~A from ~A~%" user-id room-name)
                     (kick-user-from-room (connection community)
                                          room-id user-id reason)))
  (setf (members community) (remove user-id (members community) :test #'string=)))

(new-admin-community-command ban ((user-id :valid-user)
                                  (reason (:maxlen 40) (:minlen 1)))
    "bans a user from the community"
  (format t "Banning ~A from ~A" user-id (string-capitalize (name community)))
  (moon-mapc-rooms community room
                   (lambda (room-name room-id)                     
                     (format t "Banning ~A from ~A~%" user-id room-name)
                     (ban-user-from-room (connection community)
                                         room-id user-id reason)))
  (setf (members community) (remove user-id (members community) :test #'string=)))

(new-admin-community-command unban ((user-id :valid-user))
    "unbans a user from the community"
  (format t "unbanning ~A from ~A" user-id (string-capitalize (name community)))
  (moon-mapc-rooms community room
                   (lambda (room-name room-id)
                     (declare (ignore room-name))
                     (format t "unbanning ~A from ~A~%" user-id room-id)
                     (unban-user-from-room (connection community)
                                           room-id user-id))))

(new-admin-community-command invite ((user-id :valid-user))
    "invites the user to the community"
  (format t "Inviting ~A to ~A" user-id (string-capitalize (name community)))
  (handler-bind ((m-unknown
                   (lambda (c)
                     (invoke-restart 'inform-user-id-of-error
                                     c community room))))
    (let ((fun (lambda (room-name room-id)
                 (format t "Inviting ~A to ~A~%" user-id room-name)
                 (sleep 0.25)
                 (invite-member-to-room (connection community) user-id room-id))))
      (moon-mapc-rooms community room fun)
      (setf (members community) (append (members community) (list user-id))))))

(new-admin-community-command members ()
    "prints out the members in the community"
  (moon-message community room
                (moonmat "~{~A ~}~%"
                         (mapcar (lambda (string)
                                   (string-capitalize 
                                    (first (str:split ":" (subseq string 1)))))
                                 (members community)))))

(new-admin-community-command member-count ()
    "prints out the number of members in the community"
  (format t (format nil "~r member~:p" (length (members community)))))

(new-admin-community-command rooms ()
    "prints out the rooms in the community"
  (let ((fun
          (lambda (room-name room-id)
            (format nil "Name: '~A'. ID: '~A'.~%" room-name room-id))))
    (format t (format nil "~{~A~}~%" (moon-mapcar-rooms community room fun)))))

(new-admin-community-command admins ()
    "prints out the names of the admins in the community"
  (format t "~{Authorized: ~A~%~}"
          (mapcar (lambda (string)
                    (subseq string 1))
                  (admins community))))

(new-admin-community-command add-admin ((user-id :valid-user))
    "Adds an admin to the community"
  (push user-id (admins community))
  (format t "Adding ~A to community admins." user-id))

(new-admin-community-command remove-admin ((user-id :valid-user))
    "Removes an admin from the community"
  (if (= (length (admins community)) 1)
      (format t "Can't remove anymore admins.")
      (progn 
        (setf (admins community)
              (remove user-id (admins community) :test #'string=))
        (format t "Removing ~A from community admins." user-id))))

(new-admin-community-command populate-community ((room-id :valid-room))
    "Populates the community from the member list in a single room"
  (let ((members (getf (members-in-room-ids (connection community)
                                            (getf (find-room community room-id) :id))
                       :|joined|)))
    (when members
      (setf (members community) nil)
      (let ((new-members ()))
        (alexandria:doplist (user userplist members)
          (push (string user) new-members))
        (setf (members community) new-members)
        (format t "Adding ~r member~:p to the community" (length (members community)))))))

(new-admin-community-command create-room ((private :string-bool
                                                   (:maxlen 5) (:minlen 1))
                                          (room-name (:minlen 1)
                                                     (:maxlen 50))
                                          (room-alias (:minlen 1)
                                                      (:maxlen 50))
                                          (invite-community :string-bool
                                                            (:maxlen 5)
                                                            (:minlen 1)))
    "Create a new room in the community."
  (format t "Creating a new room")
  (psetf private (string-bool-to-bool community room private)
         invite-community (string-bool-to-bool community room invite-community))
  (unless (or (eql private :error)
              (eql invite-community :error))
    (let ((id (getf
               (create-room (connection community) room-name room-alias
                            "Im a topic" :private private
                            :invite (when invite-community
                                      (remove (user-id (connection community))
                                              (members community)
                                              :test #'string=)))
               :|room_id|)))
      (sleep 0.1)
      (add-room (rooms community) id)
      (format t "Created room. id: ~A" id))))

(new-admin-community-command aliases ()
    "Lists the aliases associated with this community."
  (format t "Aliases: ~{ ~A ~}" (aliases community)))

(new-admin-community-command help ()
    "Attempts an explanation of how commands work"
  (moonhelp 'admin-community-command community room))

(new-admin-community-command all-spaces ()
    "Returns all the spaces within the top level space for this community."
  (with-accessors ((top-level-space top-level-space)
                   (connection connection))
      community
    (when (string/= top-level-space "")
      (format t "Spaces:~%~{ ~A~%~}"
              (mapcar (lambda (plist)
                        (destructuring-bind (&key |name| |room_id| &allow-other-keys)
                            plist
                          (format nil "NAME: ~A  ID: ~A" |name| |room_id|)))
                      (spaces-in-a-space connection top-level-space))))))

(new-admin-community-command rooms-in-a-space ((space (:minlen 1)
                                                      (:maxlen 50)))
    "Returns all of the room with in a space. Space must be either the top level space or 
a subspace within this community."
  (with-accessors ((top-level-space top-level-space)
                   (connection connection))
      community
    (when (string/= top-level-space "")
      (let* ((spaces (spaces-in-a-space connection top-level-space))
             (space (find space spaces :test #'string-equal
                                       :key (lambda (ele) (getf ele :|name|)))))
        (if space
            (format t "Rooms in that Space: ~%~{ ~A~%~}"
                    (mapcar (lambda (plist)
                              (destructuring-bind (&key name room-id room-type
                                                   &allow-other-keys)
                                  plist
                                (format nil "NAME: ~A. ROOM-ID ~A. TYPE: ~A."
                                        name room-id room-type)))
                            (rooms-in-a-space connection
                                              (getf space :|room_id|))))
            (format t "No subspace by the name ~A" space))))))


(new-admin-community-command populate-rooms ()
    "Fills all of the rooms in the community using the top-level-space."
  (with-accessors ((top-level-space top-level-space)
                   (connection connection))
      community
    (when (string/= top-level-space "")
      (let* ((rooms (rooms-in-a-space connection top-level-space)))
        (when rooms
          (setf (rooms community) rooms)
          (format t "New Rooms:~% ~{ ~A~%~}"
                  (mapcar (lambda (room)
                            (destructuring-bind (&key name id room-type
                                                 &allow-other-keys)
                                room
                              (format nil "ROOM: '~A'. ID: ~A. TYPE: ~A." name id
                                      room-type)))
                          rooms)))))))

(new-admin-community-command update-top-level-space ((space (:minlen 10)(:maxlen 50)))
    "Updates the top level space for this community. Best called 'populate rooms' after."
  (setf (top-level-space community) space)
  (format t "I have set (top-level-space COMMUNITY) to ~A" space))
