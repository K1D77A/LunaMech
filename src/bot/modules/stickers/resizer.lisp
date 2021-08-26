(in-package #:mm-module.sticker)

(define-condition imagemagick-condition (sticker-condition)
  ())

(define-condition imagemagick-image-dimension-condition (imagemagick-condition)
  ()
  (:documentation "Signalled by image-dimension if there is a condition signalled"))

(define-condition imagemagick-resize-condition (imagemagick-condition)
  ()
  (:documentation "Signalled by redimension if there is a condition signalled"))

(defmacro wrap-imagemagick-call (condition-to-call message &body body)
  `(handler-case
       (progn ,@body)
     (uiop/run-program:subprocess-error (c)
       (error ',condition-to-call :message ,message
                                  :uiop-condition c))))

(defun needs-to-be-resized-p (x y wanted-x wanted-y)
  (or (> x wanted-x)
      (> y wanted-y)))

(defun save-image-to-disk (filename bytes)
  (let* ((random-name (format nil "~A/~A~A"
                              *tmp-dir* (random most-positive-fixnum) filename)))
    (ensure-directories-exist random-name)
    (alexandria:write-byte-vector-into-file bytes random-name
                                            :if-exists :supersede
                                            :if-does-not-exist :create)
    random-name))

(defun %resize-image (filename bytes wanted-x wanted-y)
  (let ((random-name (save-image-to-disk filename bytes)))
    (resize random-name wanted-x wanted-y)
    (multiple-value-bind (new-x new-y size)
        (image-dimensions random-name)
      (let ((bytes (alexandria:read-file-into-byte-vector random-name)))
        (delete-file random-name)
        (list :x new-x :y new-y :length size :bytes bytes)))))

(defun process-image (filename bytes x y wanted-x wanted-y)
  (if (needs-to-be-resized-p x y wanted-x wanted-y)
      (%resize-image filename bytes wanted-x wanted-y)
      (list :x x :y y :length (length bytes) :bytes bytes)))

(defun resize (filename wanted-x wanted-y)
  "Given a FILENAME attempts to redimension the image down to a *wanted-size* x *wanted-size* 
 image. If the image doesn't resize perfectly because it is not a 1 to 1 ration then 
will shrink 
its largest (height or width) to *wanted-size*  and the other proportionally"
  (wrap-imagemagick-call imagemagick-resize-condition
      (format nil "Attempted to resize filename ~A" filename)
    ;;need a timeout here...
    (uiop:run-program (format nil "convert ~A -adaptive-resize ~Dx~D  ~A"
                              filename
                              wanted-x
                              wanted-y
                              filename))))

(defun image-dimensions (filename)
  "Returns a list of the X Y and SIZE of the image at FILENAME"
  (let ((res 
          (mapcar #'parse-integer 
                  (str:split
                   " "
                   (wrap-imagemagick-call imagemagick-image-dimension-condition
                       (format nil "Attempted to get the image dimensions for filename ~A"
                               filename)
                     (uiop:run-program
                      (format nil "identify -format \" %w %h %B \" ~A" filename)
                      :output :string))
                   :omit-nulls t))))
    (let ((x (reduce #'max (loop :for n :in res :by #'cdddr :collect n)));max width
          (y (reduce #'max (loop :for n :in (rest res) :by #'cdddr :collect n))));max height
      (values x y (third res)))))
