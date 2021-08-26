(in-package #:matrix-moonbot)

(defmethod upload-keys (connection (device-keys device-keys))
  (let* ((output (make-string-output-stream))
         (*standard-output* output))
    (jojo:with-object
      (jojo:write-key-value "device_keys"
                            device-keys))
    (auth-req (:post-object connection ("keys/upload")
               (get-output-stream-string output)
               resp)
      resp)))

(defmethod upload-keys (connection (one-time-keys one-time-keys))
  (let* ((output (make-string-output-stream))
         (*standard-output* output))
    (jojo:with-object
      (jojo:write-key-value "one_time_keys"
                            one-time-keys))
    (auth-req (:post-object connection ("keys/upload")
               (get-output-stream-string output)
               resp)
      (setf (server-otk (encryption connection))
            (reduce #'+ (rest resp)))
      (cl-megolm:mark-keys-as-published (olm-account (encryption connection))))))

(defmethod initiate-encryption ((connection connection))
  (let* ((account (cl-megolm:make-account))
         (otk (pkv (latest-sync (status connection)) :|device_one_time_keys_count|)))
    (setf (encryption connection)
          (make-instance 'encryption
                         :olm-account account
                         :server-otk otk))
    (generate-otk connection)
    (let ((device-keys (device-keys-from-connection connection))
          (otks (otk-object-from-one-time-keys-signed
                 connection (connection-otks connection))))
      (log:info "Uploading device-keys for device-id ~A" (device-id connection))
      (upload-keys connection device-keys)
      (log:info "Uploading one-time-keys for device-id ~A" (device-id connection))
      (upload-keys connection otks))))

(defun device-keys-from-connection (connection)
  (with-accessors ((user-id user-id)
                   (device-id device-id)
                   (encryption encryption))
      connection
    (with-accessors ((acc olm-account))
        encryption
      (let* ((keys (cl-megolm:identity-keys acc))
             (ed (pkv keys :|ed25519|))
             (curve (pkv keys :|curve25519|))
             (encryption-keys (make-instance 'encryption-keys
                                             :device-id device-id
                                             :ed25519 ed
                                             :curve curve))
             (device-keys 
               (make-instance 'device-keys :keys encryption-keys
                                           :device-id device-id
                                           :user-id user-id))
             (signature (cl-megolm:sign acc (jojo:to-json device-keys))))
        (setf (signature device-keys)
              (make-instance 'encryption-signature
                             :signature
                             (make-instance 'signature :ed25519 signature
                                                       :device-id device-id)
                             :user-id user-id))
        device-keys))))

(defun max-otk (connection)
  "returns the maximum number of one-time-keys associated with CONNECTION"
  (cl-megolm:max-one-time-keys (olm-account (encryption connection))))

(defun generate-otk (connection)
  (let ((acc (olm-account (encryption connection))))
    (cl-megolm:generate-one-time-keys acc (/ (max-otk connection) 2))))

(defun connection-otks (connection)
  (cl-megolm:one-time-keys (olm-account (encryption connection))))

(defun make-signed-otk (connection key label)
  (with-accessors ((device-id device-id)
                   (user-id user-id)
                   (encryption encryption))
      connection
    (let ((signed (cl-megolm:sign (olm-account encryption)
                                  (jojo:to-json (list label key)))))
      (make-instance 'signed-otk
                     :signature
                     (make-instance 'encryption-signature
                                    :user-id user-id
                                    :signature (make-instance 'signature
                                                              :ed25519 signed
                                                              :device-id device-id))
                     :key key
                     :label label))))

(defun otk-object-from-one-time-keys (otk-list)
  (let ((keys (second otk-list)))
    (make-instance 'one-time-keys :otks 
                   (loop :for (label key) :on keys :by #'cddr
                         :collect (make-instance 'one-time-key :key key
                                                               :label label)))))

(defun otk-object-from-one-time-keys-signed (connection otk-list)
  (let ((keys (second otk-list)))
    (make-instance 'one-time-keys :otks 
                   (loop :for (label key) :on keys :by #'cddr
                         :collect (make-signed-otk connection key label)))))


