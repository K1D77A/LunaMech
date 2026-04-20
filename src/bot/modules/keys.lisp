(defpackage #:mm-module.private-keys
  (:use #:cl #:matrix-moonbot)
  (:export #:new-key #:get-key))

(in-package #:mm-module.private-keys)

(defmodule private-keys (mm-module.private-keys PRIVATE-KEYS ubermensch-privilege)
           private-keys-command ()
           private-keys-module
           ((private-keys
             :accessor private-keys
             :initform ()
             :initarg :private-keys
             :documentation "Private keys")))

(defun new-key (luna keyword pkey)
  (setf (getf (private-keys (find-module luna "private-keys")) keyword) pkey))

(defun get-key (luna keyword)
  (getf (private-keys (find-module luna "private-keys")) keyword))

(defmethod on-load-up (luna (module private-keys-module))
  (let ((path (module-persistent-path luna module "private-keys" "lisp")))
    (format t "Loading private-keys~%")
    (log:info "Loading private-keys rooms from: ~S" path)
    (if (probe-file path)
        (setf (private-keys module) (uiop:read-file-form path))
        (progn (format t "Private-keys config file doesn't exist.")
               (log:info "path: ~S didn't exist." path)))))

(defmethod on-module-load (luna (module private-keys-module))
  (on-load-up luna module))

(defmethod on-save (luna (module private-keys-module))
  (let ((path (module-persistent-path luna module "private-keys" "lisp")))
    (log:info "Saving private keys to ~S" path)
    (alexandria:write-string-into-file (format nil "~S" (private-keys module))
                                       path
                                       :if-exists :supersede
                                       :if-does-not-exist :create))
  t)
