;; (in-package #:matrix-moonbot)
;; ;;;;this file contains the code for creating and managing filters within Moonbot.

;; (defun get-filter (instance key)
;;   (find key (filters instance) :key #'key))

;; (defmethod initiate-filters ((connection connection))
;;   (log:info "Uploading filters for connection: ~A" (device-id connection))
;;   (generate-and-upload-junk-removal-filter connection))

;; (defmethod initiate-filters ((moonbot moonbot))
;;   (log:info "Uploading filter for Luna.")
;;   (generate-and-upload-invites-filter moonbot))

;; (defun add-filter-id (instance key new-val &optional (last-sync nil))
;;   (check-type last-sync (or null string))
;;   (let* ((filter (find key (filters instance) :key #'key)))
;;     (if filter 
;;         (setf (id filter) new-val)
;;         (push (make-instance 'filter :id new-val :key key
;;                                      :last-sync-string last-sync)
;;               (filters instance)))))

;; (defun filter-to-remove-junk ()
;;   `(("event_format" "client")
;;     ("presence" ("not_types" ("m.presence")))
;;     ("room" ("ephemeral" ("not_types" ("m.room.*" "m.receipt"
;;                                                   "m.typing" "m.reaction"))))));ignore all ephemeral

;; (defmethod gen-invite-filter ((moonbot moonbot))
;;   (let ((connection (first (connections moonbot)))
;;         (user-id (user-id connection))
;;         (uber (ubermensch moonbot)))
;;     (lmav2:generate-user-room-filter connection user-id
;;                                      :event-format "client"
;;                                      :presence (object%event-filter
;;                                                 :not-types '("m.presence"))
;;                                      :room-filter
;;                                      (list
;;                                       (lmav2:object%state-filter :types '("m.room.member")
;;                                                                  :senders ,uber
;;                                                                  :not-types
;;                                                                  '("m.room.message"
;;                                                                    "m.room.encrypted"))
;;                                       (lmav2:object%time)


;; (defmethod generate-filter-for-invites ((moonbot moonbot))
;;   (let ((uber (ubermensch moonbot)))
;;     `(("event_format" "client")
;;       ("presence" ("not_types" ("m.presence")))
;;       ("room" ("state" ("types" ("m.room.member")) ("sender" ,uber)
;;                        ("not_types" ("m.room.message" "m.room.encrypted")))
;;               ("timeline" ("types" ("m.room.member"))
;;                           ("not_types" ("m.room.message" "m.room.encrypted")))))))

;; (defmethod generate-filter-from-room-list (list)
;;   `(("event_format" "client")
;;     ("presence" ("not_types" ("m.presence")))
;;     ("room" ("rooms" ,list)
;;             ("state" ("types" ("m.room.message" "m.room.encrypted")))
;;             ("timeline" ("rooms" ,list)
;;                         ("types" ("m.room.message" "m.room.encrypted"))))))

;; (defmethod generate-and-upload-junk-removal-filter ((connection connection))
;;   (let* ((filter (filter-to-remove-junk))
;;          (filter-id (pkv (add-user-room-filter connection (user-id connection) filter)
;;                          :|filter_id|)))
;;     (add-filter-id connection :junk-removed filter-id)))
