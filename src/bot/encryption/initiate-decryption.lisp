(in-package #:matrix-moonbot)

;;;;this file contains the code used to initiate encryption/decryption within a room
;;;;in a community

(defun keys-query-basic (connection device-keys &optional (timeout 10000)
                                                  (token nil))
  (let* ((output (make-string-output-stream))
         (*standard-output* output))
    (auth-req (:post-object connection ("keys/query")
               (jojo:to-json device-keys)
               resp)
      resp)))

(defun download-devices (community room)
  "Given a COMMUNITY and a ROOM id, grabs all of the devices within it."
  (let ((members (pkv (members-in-room (connection community) room)
                      :|chunk|)))
    (if members
        (let ((ids (remove-duplicates
                    (mapcar (lambda (lst) (make-instance 'device-keys-basic
                                                    :user-id 
                                                    (pkv lst :|user_id|)))
                            members)
                    :test #'string= :key #'user-id)))
          (keys-query-basic (connection community)
                            (make-instance 'device-keys-basic-holder
                                           :device-keys ids)))
        (error "no ID's")));;need a better condition
  )
  
