(in-package #:matrix-moonbot)
;;;;this file contains the code for creating and managing filters within Moonbot.

(defun get-filter (instance key)
  (find key (filters instance) :key #'key))

(defmethod initiate-filters ((connection connection))
  (log:info "Uploading filters for connection: ~A" (device-id connection))
  (generate-and-upload-junk-removal-filter connection))

(defmethod initiate-filters ((moonbot moonbot))
  (log:info "Uploading filter for Luna.")
  (generate-and-upload-invites-filter moonbot))

(defun add-filter-id (instance key new-val &optional (last-sync nil))
  (check-type last-sync (or null string))
  (let* ((filter (find key (filters instance) :key #'key)))
    (if filter 
        (setf (id filter) new-val)
        (push (make-instance 'filter :id new-val :key key
                                     :last-sync-string last-sync)
              (filters instance)))))

(defun filter-to-remove-junk ()
  `(("event_format" "client")
    ("presence" ("not_types" ("m.presence")))
    ("room" ("ephemeral" ("not_types" ("m.room.*" "m.receipt"
                                                  "m.typing" "m.reaction"))))));ignore all ephemeral

(defmethod generate-filter-from-listen-in ((community community))
  "Attempts to generate a valid filter that can be uploaded from the rooms listed
in (listen-in COMMUNITY). The purpose of the filter is to return only messages 
from /sync contained within the rooms the bot is supposed to listen-in"
  (let ((ids  (listen-in community)))
    `(("event_format" "client")
      ("presence" ("not_types" ("m.presence")))
      ("room" ("rooms" ,ids)
              ("state" ("types" ("m.room.message" "m.room.encrypted")))
              ("timeline" ("rooms" ,ids)
                          ("types" ("m.room.message" "m.room.encrypted")))))))

(defmethod generate-filter-for-invites ((moonbot moonbot))
  (let ((uber (ubermensch moonbot)))
    `(("event_format" "client")
      ("presence" ("not_types" ("m.presence")))
      ("room" ("state" ("types" ("m.room.member")) ("sender" ,uber)
                       ("not_types" ("m.room.message" "m.room.encrypted")))
              ("timeline" ("types" ("m.room.member"))
                          ("not_types" ("m.room.message" "m.room.encrypted")))))))

(defmethod generate-filter-from-room-list (list)
  `(("event_format" "client")
    ("presence" ("not_types" ("m.presence")))
    ("room" ("rooms" ,list)
            ("state" ("types" ("m.room.message" "m.room.encrypted")))
            ("timeline" ("rooms" ,list)
                        ("types" ("m.room.message" "m.room.encrypted"))))))

(defmethod generate-filter-from-listen-in ((moonbot moonbot))
  (let ((ids (remove-duplicates 
              (alexandria:flatten (mapcar #'listen-in (communities moonbot)))
              :test #'string=)))
    `(("event_format" "client")
      ("presence" ("not_types" ("m.presence")))
      ("room" ("rooms" ,ids)
              ("state" ("types" ("m.room.message" "m.room.encrypted")))
              ("timeline" ("rooms" ,ids)
                          ("types" ("m.room.message" "m.room.encrypted")))))))

(defmethod generate-and-upload-listen-in-filter ((community community)
                                                 &optional (last-sync nil))
  "Generates a filter from the COMMUNITY that means only message related events 
are received when syncing. First generates the filter, then uploads it, then adds
the filter under the key :listen-in to (filters COMMUNITY). If a value already
exists for that key then updates the already existing one."
  (let* ((filter (generate-filter-from-listen-in community))q
         (connection (connection community))
         (filter-id (add-user-room-filter (connection community)
                                          (user-id connection)
                                          filter)))
    (add-filter-id community :listen-in (pkv filter-id :|filter_id|) last-sync)))

(defmethod generate-and-upload-invites-filter ((moonbot moonbot))
  (let* ((filter (generate-filter-from-listen-in moonbot))
         (conns (connections moonbot))
         (filter-ids (mapcar (lambda (con)
                               (add-user-room-filter con
                                                     (user-id con)
                                                     filter))
                             conns)))
    (mapc (lambda (con filter-id)
            (add-filter-id con :invites filter-id))
          conns (mapcar (lambda (fil)
                          (pkv fil :|filter_id|))
                        filter-ids))
    moonbot))

(defmethod generate-and-upload-junk-removal-filter ((connection connection))
  (let* ((filter (filter-to-remove-junk))
         (filter-id (pkv (add-user-room-filter connection (user-id connection) filter)
                         :|filter_id|)))
    (add-filter-id connection :junk-removed filter-id)))
    


;; (defmethod generate-and-upload-listen-in-filter ((moonbot moonbot))
;;   "";;small problem, right now we only have one connection but if there were multiple
;;   ;;then only some of the rooms would actually be applicable to that connection
;;   ;;so we would have to keep track of filters in moonbot that apply to certain
;;   ;;connections and I cba to do that cos everything currently works.
;;   ;;only if I need to go faster will I consider this
;;   (let* ((filter (generate-filter-from-listen-in moonbot))
;;          (connection (connection community))xb
;;          (filter-id (add-user-room-filter (connection community)
;;                                           (user-id connection)
;;                                           filter)))
;;     (add-filter-id moonbot :listen-in (second filter-id))))
