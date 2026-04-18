(in-package #:matrix-moonbot)

(defgeneric login (o &optional relog))

(defgeneric start (o))

(defgeneric stop (o))

(defgeneric force-stop (o))

(defgeneric wait-for-stop (o))


