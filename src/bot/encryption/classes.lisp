(in-package #:matrix-moonbot)

(defclass device-keys-basic-holder ()
  ((device-keys
    :initarg :device-keys
    :accessor device-keys)))

(defmethod jojo:%to-json ((obj device-keys-basic-holder))  
  (jojo:with-object
    (jojo:write-key "device_keys")
    (jojo:with-object 
      (dolist (id (slot-value obj 'device-keys))
        (jojo:write-key-value (user-id id) #())))))

(defclass device-keys-basic ()
  ((user-id
    :initarg :user-id
    :accessor user-id)))

(defmethod jojo:%to-json ((obj device-keys-basic))
  (jojo:with-object
    (jojo:write-key-value (slot-value obj 'user-id) #())))

(defclass device-keys ()
  ((user-id
    :initarg :user-id
    :accessor user-id)
   (device-id
    :initarg :device-id
    :accessor device-id
    :type string)
   (algorithms
    :initarg :algorithms
    :accessor algorithms
    :initform '("m.olm.curve25519-aes-sha256" "m.megolm.v1.aes-sha")
    :type list)
   (keys
    :initarg :keys
    :accessor keys
    :type encryption-keys)
   (signature
    :initarg :signature
    :accessor signature
    :type encryption-signature)))

(defmacro wkv (obj key value)
  `(jojo:write-key-value ,key (slot-value ,obj ',value)))

(defmethod jojo:%to-json ((obj device-keys))
  (jojo:with-object
    (wkv obj "user_id" user-id)
    (wkv obj "device_id" device-id)
    (wkv obj "algorithms" algorithms)
    (wkv obj "keys" keys)
    (when (slot-boundp obj 'signature)
      (wkv obj "signatures" signature))))

(defclass encryption-keys ()
  ((ed25519
    :accessor ed25519
    :initarg :ed25519
    :type string)
   (curve25519
    :accessor curve
    :initarg :curve
    :type string)
   (device-id
    :accessor device-id
    :initarg :device-id
    :type string)))

(defmethod jojo:%to-json ((obj encryption-keys))
  (jojo:with-object
    (wkv obj (format nil "curve25519:~A" (device-id obj)) curve25519)
    (wkv obj (format nil "ed25519:~A" (device-id obj)) ed25519)))

(defclass encryption-signature ()
  ((signature
    :initarg :signature
    :type signature)
   (user-id
    :initarg :user-id
    :type string)))

(defclass signature ()
  ((device-id
    :accessor device-id
    :initarg :device-id)
   (ed25519
    :accessor ed25519
    :initarg :ed25519)))

(defmethod jojo:%to-json ((obj encryption-signature))
  (jojo:with-object
    (jojo:write-key-value (slot-value obj 'user-id)
                          (slot-value obj 'signature))))

(defmethod jojo:%to-json ((obj signature))
  (jojo:with-object
    (jojo:write-key-value (format nil "ed25519:~A" (slot-value obj 'device-id))
                          (slot-value obj 'ed25519))))

(defclass one-time-keys ()
  ((otks
    :accessor otks
    :initarg :otks)))

(defmethod jojo:%to-json ((otk one-time-keys))
  (jojo:with-object
    (dolist (key (slot-value otk 'otks) (values))
      (with-slots ((label label)
                   (key-type key-type))
          key
        (jojo:write-key-value
         (format nil "~A:~A" key-type label) key)))))


(defclass one-time-key ()
  ((label
    :accessor label
    :initarg :label
    :type symbol)
   (key
    :accessor key
    :initarg :key
    :type string)
   (key-type
    :accessor key-type
    :initarg :key-type
    :initform "curve25519")))

(defmethod jojo:%to-json ((otk one-time-key))
  (with-slots ((label label)
               (key key)
               (key-type key-type))
      otk
    (let ((gen (intern (format nil "~A:~A" key-type label) :keyword)))
      (jojo:%to-json (list gen key)))))

(defclass signed-otk (one-time-key)
  ((key-type
    :initform "signed_curve25519")
   (signature
    :accessor signature
    :initarg :signature
    :type encryption-signature)))

(defmethod jojo:%to-json ((otk signed-otk))
  (jojo:with-object
    (jojo:write-key-value "key" (slot-value otk 'key))
    (jojo:write-key-value "signatures" (slot-value otk 'signature))))
  




