(in-package #:lunamech)

(defgeneric login (o &optional relog))

(defgeneric initialize (o)
  (:documentation
   "Called with O when the lunamech binary is started. Used to perform
    initializations such as checking for blank configs and creating an initial instances of luna."))

(defgeneric start (o))

(defgeneric stop (o))

(defgeneric lrestart (o))

(defgeneric force-stop (o))

(defgeneric wait-for-stop (o))

(defgeneric find-room-by-name (o name))

(defgeneric find-room-by-id (o id))

(defgeneric find-community (name o))

(defgeneric cleanup (o)
  (:documentation "Perform cleanup on O after restoration"))

(defgeneric dont-serialize-slots (o)
  (:documentation "Return a list of slots that aren't saved to disk."))



