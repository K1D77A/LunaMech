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
             :documentation "Private keys")))


(defun new-key (keyword pkey)
  (setf (getf (private-keys *module*) keyword) pkey))

(defun get-key (keyword)
  (getf (private-keys *module*) keyword))

(defun save-results ()
  (when (private-keys *module*);dont write empty results
    (alexandria:write-string-into-file
     (format nil "~S" (private-keys *module*)) "config/private-keys.lisp"
     :if-does-not-exist :create
     :if-exists :supersede)))

(defun results-from-file ()
  (handler-case
      (let ((form (uiop:safe-read-file-form "config/private-keys.lisp")))
        (setf (private-keys *module*) form))
    (file-error ()
      (warn "config/private-keys.lisp does not exist.")
      nil)))

(defmethod on-load-up (luna (module private-keys-module))
  (log:info "Loading private-keys rooms from private-keys.lisp")
  (results-from-file))

(defmethod on-save (luna (module private-keys-module))
  (log:info "Saving private keys to private-keys.lisp")
  (save-results)
  t)
