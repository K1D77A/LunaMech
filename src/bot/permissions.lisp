(in-package #:lunamech)

#||
We can have an list of alists denoting permissions.
(("@k1d77a:scyldings.com" ((:ubermensch . t)))
("@zimmmy:scyldings.com" ((rss . t))))

Now we can use (determine-permissions  <community> sender)
That will determine if they are ubermensch or a community admin. 
Then when it comes to processing modules, we can call 
(determine-permissions <module> <privilege>) 
To see if the privilege can be escalated for each module execution.
This will introduce a new execute-command for modules that want per module perms.
||#

(defmethod find-permissions (luna user-id)
  (second (find user-id (permissions luna) :key #'first :test #'string=)))

(defmethod find-permissions (luna (user privilege))
  (find-permissions luna (user-id user)))

(defun ubermensch-p (luna string)
  (let ((perms (find-permissions luna string)))
    (cdr (assoc :ubermensch perms :test #'eq))))

(defun community-admin-p (community string)
  (and (find string (admins community) :test #'string=) t))

(defgeneric determine-permissions (luna obj sender))

(defmethod determine-permissions (luna (obj community) sender)
  (cond ((ubermensch-p luna sender)
         'ubermensch-privilege)
        ((community-admin-p obj sender)
         'admin-privilege)
        ((is-me-p luna sender)
         'me-privilege)
        (t 'normie-privilege)))

(defmethod determine-permissions (luna (obj module) (sender ubermensch-privilege))
  "Ubermensch overrides all other privileges."
  sender)

(defmethod determine-permissions (luna (obj module) (sender me-privilege))
  "Its me!"
  sender)

(defmethod determine-permissions (luna (obj module) (sender privilege))
  "if the SENDER has per module permissions with MODULE then 
returns a new instance of 'module-privilege."
  (let ((perms (find-permissions luna sender)))
    (if (cdr (assoc (class-name (class-of obj)) perms :test #'string-equal))
        (make-instance 'module-privilege :user-id (user-id sender))
        sender)))

(defun determine-privilege (luna community message)
  "Determines the privilege of a message sender by checking the name first against
(ubermensch moonbot) then by checking if they are in (admins community) and finally
defaulting to normie. Returns either an ubermensch, admin, or normie privilege 
object"
  (let ((sender (message-from? message t)))
    (make-instance (determine-permissions luna community sender)
                   :user-id sender)))

(defmethod can-upgrade-permissions-p (privilege)
  t)

(defmethod can-upgrade-permissions-p ((privilege ubermensch-privilege))
  nil)

(defmacro with-per-module-permissions ((luna module user-priv new-priv) if else)
  "Checks to see if the user can be upgraded to module-privilege and then executes 
the body, then restores the user to their previous permission level after execution."
  `(if (can-upgrade-permissions-p ,user-priv)
       (let* ((,new-priv (determine-permissions ,luna ,module ,user-priv)))
         (locally ,if))
       (locally ,else)))

(defun update-permission (luna user-id key new-val)
  (setf (second (find user-id (permissions luna) :key #'first :test #'string=))
        (acons key new-val
               (second (find user-id (permissions luna) :key #'first :test #'string=)))))

(defun ubermensch-count (luna)
  (length (remove-if-not (lambda (name)
                           (ubermensch-p luna name))
                         (mapcar #'first (permissions luna)))))

(defun can-remove-ubermensch-p (luna)
  (> (ubermensch-count luna) 1))

(defun make-ubermensch (luna user-id)
  (unless (ubermensch-p luna user-id)
    (update-permission luna user-id :ubermensch t)))

(defun remove-ubermensch (luna user-id)
  (when (ubermensch-p luna user-id)
    (update-permission luna user-id :ubermensch nil)))

(defmethod make-module-admin (luna user-id (mod-sym symbol))
  (update-permission luna user-id mod-sym t))

(defmethod make-module-admin (luna user-id (mod-sym string))
  (make-module-admin luna user-id (intern (string-upcase mod-sym))))

(defmethod retract-module-admin (luna user-id (mod-sym symbol))
  (update-permission luna user-id mod-sym nil))

(defmethod retract-module-admin (luna user-id (mod-sym string))
  (retract-module-admin luna user-id (intern (string-upcase mod-sym))))

(defun clean-permissions-tree (luna)
  "Removes duplicates from the permissions tree for each entry."
  (mapcar (lambda (list)
            (list (first list)
                  (remove-duplicates (second list)
                                     :key #'first
                                     :from-end t)))
          (permissions luna)))

(defun all-ubermensch (luna)
  (loop :for id :in (mapcar #'first (permissions luna))
        :when (ubermensch-p luna id)
          :collect id))
