(in-package #:matrix-moonbot)

(defmethod initiate-encryption ((connection connection))
  (let* ((account (cl-megolm:make-account))
         (device-keys (cl-megolm:identity-keys account))
         (signature (cl-megolm:sign account "")))
    (setf (encryption connection)
          (make-instance encryption :olm-account account))
    
    
    
