(in-package #:mm-module.sticker)

(define-condition processor-condition (sticker-condition)
  ())

(define-condition bad-variable (processor-condition)
  ((variable-in-q
    :accessor variable-in-q
    :initarg :variable-in-q))
  (:documentation "Signalled when a value is not what is expected.")
  (:report
   (lambda (obj stream)
     (format stream "Please remove the spaces from the images filename. ~A"
             (variable-in-q obj)))))

(defclass image-event ()
  ((server
    :accessor server
    :initarg :server)
   (user
    :accessor user
    :initarg :user)
   (overwritep
    :accessor overwritep
    :initarg :overwritep)
   (filename
    :accessor filename
    :initarg :filename)
   (bytes
    :accessor bytes
    :initarg :bytes)
   (size
    :accessor size
    :initarg :size)
   (height
    :accessor height
    :initarg :height)
   (width
    :accessor width
    :initarg :width)   
   (url
    :accessor url
    :initarg :url)
   (thumb
    :accessor thumb
    :initarg :thumb)
   (content-type
    :accessor content-type
    :initarg :content-type)))

(defclass sized-image-event (image-event)
  ())

(defclass smol-image-event (sized-image-event)
  ())

(defclass big-image-event (sized-image-event)
  ())


(defgeneric process-image-event (image-event)
  (:documentation "Processes the image-event correctly."))

(defmethod validate-image-event ((image image-event))
  (with-slots (filename)
      image
    (when (find #\Space filename)
      (error 'bad-variable :variable-in-q filename
                           :message "Please remove any spaces from the images filename."))))

(defmethod process-image-event ((image image-event))
  (validate-image-event image)
  (with-slots (server user overwritep filename bytes content-type
               height width url)
      image
    (setf bytes (download-content (conn *luna*) url))
    (let* ((args (list :server server :user user :overwritep overwritep
                       :filename filename :bytes bytes :content-type content-type
                       :height height :width width))
           (big (apply #'make-instance 'big-image-event args))
           (smol (apply #'make-instance 'smol-image-event args)))
      (process-image-event big)
      (process-image-event smol)
      (setf (thumb big) (url smol))
      (list :big big :small smol))))

(defmethod process-image-event :around (image)
  (call-next-method))

(defmethod process-image-event ((image sized-image-event))
  "Resizes, and uploads the a size-image-event."
  (generate-new-image image)
  (upload-image-bytes image))

(defgeneric generate-new-image (image)
  (:documentation "Generates a new image by writing the old bytes to disk and resizing it."))
;;can error if no dimensions known
(defmethod generate-new-image ((image smol-image-event))
  (with-slots (server url height width bytes filename)
      image
    (multiple-value-bind (bigx bigy small-x small-y)
        (destructure-dimensions server)
      (declare (ignore bigx bigy))
      (let ((ran (save-image-to-disk filename bytes)))
        (unwind-protect 
             (let ((resized (process-image ran bytes width height small-x small-y)))
               (destructuring-bind (&key x y length bytes &allow-other-keys)
                   resized
                 (setf (width image) x
                       (height image) y
                       (size image) length
                       (bytes image) bytes)))
          (delete-file ran))))))

(defmethod generate-new-image ((image big-image-event))
  (with-slots (server url height width bytes filename)
      image
    (multiple-value-bind (bigx bigy small-x small-y)
        (destructure-dimensions server)
      (declare (ignore small-x small-y))
      (let ((ran (save-image-to-disk filename bytes)))
        (unwind-protect 
             (let ((resized (process-image ran bytes width height bigx bigy)))
               (destructuring-bind (&key x y length bytes &allow-other-keys)
                   resized
                 (setf (width image) x
                       (height image) y
                       (size image) length
                       (bytes image) bytes)))
          (delete-file ran))))))

(defgeneric upload-image-bytes (image-event)
  (:documentation "Uploads the image-event and stores the mxc in url."))

(defmethod upload-image-bytes ((image sized-image-event))
  (with-accessors ((url url)
                   (bytes bytes)
                   (content-type content-type)
                   (filename filename)
                   (thumb thumb))
      image
    (let* ((resp (upload-content (conn *luna*) filename content-type bytes))
           (mxc (getf resp :|content_uri|)))
      (setf url mxc
            thumb mxc))))

(defgeneric upload-image-event (image)
  (:documentation "Uploads image-event to the stickerpicker"))

(defmethod upload-image-event :around ((image sized-image-event))
  (with-accessors ((server server)
                   (filename filename)
                   (user user)
                   (url url)
                   (size size)
                   (height height)
                   (width width)
                   (content-type content-type)
                   (thumb thumb)
                   (bytes bytes))
      image
    (upload-image (list "server" server
                        "name" (pathname-name filename)
                        "sender" user
                        "filename" filename
                        "url" url
                        "size" size
                        "height" height
                        "width" width
                        "big-or-smol" (call-next-method)
                        "mimetype" content-type
                        "thumbnail-url" thumb
                        "bytes" bytes))))

(defmethod upload-image-event ((image big-image-event))
  "big")

(defmethod upload-image-event ((image smol-image-event))
  "smol")
