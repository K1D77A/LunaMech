(in-package #:matrix-moonbot)

(defun new-password ()
  (format *query-io* "Enter a new password: ")
  (read-line *query-io*))

(defmethod login ((connection connection) &optional (relog nil))
  "Attempts to login a CONNECTION to its appropriate server with its appropriate username 
and password. In this method the flag RELOG is completely ignored as the 
function (password-login ..) automatically relogs with the previous device-id. 
This method establishes the restart 'new-password' that will prompt the user for a new pass
and recall login."
  (declare (ignore relog))
  (log:info "Attempting login at url ~A with username ~A"
            (url connection) (username connection))
  (restart-case (progn (catch-limit-exceeded ()
                         (password-login connection))
                       (log:info "Successful login"))
    (new-password (pass)
      :report "Password is incorrect, enter another?"
      :interactive new-password
      (setf (password connection) pass)
      (login connection))))

(defparameter *sync-timeout* 30000)

(defmethod first-sync ((connection connection))
  (log:info "Performing initial sync for device-id ~A" (device-id connection))
  (sync connection :timeout *sync-timeout*))
