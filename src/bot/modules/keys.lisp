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

(defun new-key (keyword pkey)
  (setf (getf (private-keys *module*) keyword) pkey))

(defun get-key (keyword)
  (getf (private-keys *module*) keyword))

(defmethod on-load-up (luna (module private-keys-module))
  (let ((path (module-persistent-path luna module "private-keys" "lisp")))
    (log:info "Loading private-keys rooms from: ~S" path)
    (when (probe-file path)
      (setf (private-keys module) (uiop:read-file-form path)))))

(defmethod on-save (luna (module private-keys-module))
  (let ((path (module-persistent-path luna module "private-keys" "lisp")))
    (log:info "Saving private keys to ~S" path)
    (alexandria:write-string-into-file (format nil "~S" (private-keys module))
                                       path))
  t)
