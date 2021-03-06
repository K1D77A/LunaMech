(in-package #:matrix-moonbot)

;;;;this file contains code to handle a variety of syncs.

;; (defmethod filter-sync ((connection connection) (filter filter))
;;   (declare (optimize (speed 3) (safety 1)))
;;   (let ((plist (list :|filter| (id filter))))
;;     (if (last-sync-string filter)
;;         (setf plist (nconc plist (list :|since| (last-sync-string filter))))
;;         (when (slot-boundp (status connection) 'latest-sync)
;;           (setf plist (nconc plist (list :|since|
;;                                          (get-key (latest-sync (status connection))
;;                                                   :|next_batch|))))))
;;     (sync 
;;      (setf ;;(latest-sync (status connection)) resp;;just keep this
;;       (last-sync-string filter) (get-key resp :|next_batch|))
;;      (when (slot-boundp connection 'encryption)
;;        (setf (server-otk (encryption connection))
;;              (get-key (get-key resp :|device_one_time_keys_count|) :|signed_curve25519|)))
;;      resp)))

;; (defmethod filter-sync ((community community) filter)
;;   (with-accessors ((connection connection))
;;       community
;;     (filter-sync connection filter)))

;; (defmethod filter-sync ((community community) (filter null))
;;   (log:error "Missing filter"))

;; (defmethod key-sync ((community community) key)
;;   (key-sync (connection community) key))

;; (defmethod key-sync ((connection connection) key)
;;   "Takes a CONNECTION and a KEY, then uses they key as an argument to /sync, and 
;; returns the final value. If KEY is not found within (filters COMMUNITY) then signals
;; 'missing-filter-for-key"
;;   (check-type key keyword)
;;   (let ((filter (find key (filters connection) :key #'key)))
;;     (unless filter
;;       (error 'missing-filter-for-key
;;              :missing-filter-for-key-key key
;;              :missing-filter-for-key-community connection
;;              :missing-filter-for-key-message "Trying a key sync but key is missing"))
;;     (filter-sync connection filter)))

(defun valid-invite-p (connection list)
  "Checks if the :|state_key| is addressed to the connections user-id and that the
:|membership| key of in :|content| is 'invite'."
  (and (string= (get-key list :|state_key|) (user-id connection))
       (string= (get-key (get-key list :|content|) :|membership|) "invite")
       t))

(defmethod process-invites ((luna luna) (connection connection) sync)
  "Takes in a SYNC event and looks for the appropriate events associated with receiving 
an invite. If the invite is valid and from an ubermensch user then joins the room."
  (flet ((valid-senders (events)
           (remove-if-not (lambda (event)                            
                            (let ((sender (get-key event :|sender|)))
                              (will-accept-invite-p luna sender)))
                          events)))
    (let ((invites (room-invite sync)))
      (when invites 
        (maphash (lambda (room rest)
                   (let* ((events (traverse-sync rest '("invite_state" "events")))
                          (from-uber (valid-senders events)))
                     (when from-uber
                       (let ((room.members
                               (extract-events-of-type from-uber '("m.room.member"))))
                         ;;this should hopefully produce a list of two.
                         (when (loop :for event :in room.members
                                     :thereis (valid-invite-p connection event))
                           (log:info "Accepting invite and joining room" room)
                           (join-room connection room))))))
                 invites)))))

(defun find-types-in-rooms-timeline (types rooms sync)
  (let ((join-events (traverse-sync sync '("rooms" "join"))))
    (when join-events
      (loop :for room :being :the :hash-key :in join-events
            :for hash :being :the :hash-value :in join-events 
            :when (find room rooms :test #'string=)
              :appending (list room
                               (remove-if-not 
                                (lambda (event)
                                  (let ((type (get-key event :|type|)))
                                    (when type
                                      (the string type)
                                      (some (lambda (wanted)
                                              (string= type wanted))
                                            types))))
                                (traverse-sync hash
                                               '("timeline" "events"))))))))

(defun find-messages-from-rooms (rooms sync)
  (find-types-in-rooms-timeline '("m.room.message" "m.room.encrypted") rooms sync))

(defun extract-all-relevant-messages (luna community sync)
  (with-accessors ((uber-rooms uber-rooms))
      luna
    (let ((rooms (append uber-rooms
                         (loop :for room-list :in (rooms community)
                               :when (string/= (getf room-list :room-type) "m.space")
                                 :collect (getf room-list :id)))))
      (when rooms 
        (find-messages-from-rooms rooms sync)))))
