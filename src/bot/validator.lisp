(in-package #:matrix-moonbot)

;;;;this file just contains some functions to validate some arguments


(defvar *validators* (make-hash-table))

(defun new-validator (key fun)
  (setf (gethash key *validators*) fun))

(defun get-validator (key)
  "Attempts to find the validator associated with the key KEY within the 
hash-table *validators*. On success returns the validator, on failure signals the condition 
'missing-key"
  (or (gethash key *validators*)
      (error 'missing-key :missing-key-key key)))

(defmacro create-new-validator (name key docstring on-failure &body body)
  (check-type docstring string)
  (check-type on-failure string)
  `(progn (defun ,name (entry &rest args)
            ,docstring
            (declare (ignorable args))
            (or (locally ,@body)
                (error 'validation-failed
                       :validation-failed-entry entry
                       :validation-failed-key ,key
                       :validation-failed-message ,on-failure)))
          (new-validator ,key ',name)))

(create-new-validator validator-valid-jitsi-room :valid-jitsi-room
    "Checks to make sure that room is listed in the jitsi config file."
    "The Room-id you have entered is not a valid jitsi room, try adding it."
  (valid-jitsi-room-p entry))

(create-new-validator validator-max-len :maxlen
    "Checks to make sure that the entry is shorter than maxlen"
    "Your input is too long"
  (check-type (first args) integer) 
  (<= (length entry) (first args)))

(create-new-validator validator-valid-alias :valid-alias
    "Checks to make sure that the alias you are giving a community wont conflict."
    "That Alias you have provided will conflict with another alias."
  (declare (special luna community))
  (check-type entry string)
  (let ((interned (intern (string-upcase entry) :keyword)))
    (not (loop :for community :in (communities luna)
                 :thereis (find interned  (aliases community))))))

(create-new-validator validator-min-len :minlen
    "Checks to make sure that the entry is longer than minlen"
    "Your input is too short"
  (check-type (first args) integer)
  (<= (first args) (length entry)))

(create-new-validator validator-valid-community :valid-community
    "Checks to make sure that entry denotes a valid-community"
    "Your input is not a valid community."
  (declare (special luna))
  (find-community entry luna))

(create-new-validator validator-compass :valid-compass
    "Checks to make sure that entry is a valid compass result entry"
    "Your input is not a valid compass result. It must be between -100 and 100."
  (and (<= (length entry) 4)
       (not (find #\. entry :test #'char=))
       (let ((parsed (parse-integer entry :junk-allowed t)))
         (when (<= -100 parsed 100)
           t))))

(create-new-validator validator-valid-user :valid-user
    "Checks to make sure that entry is a valid user-id"
    "Your input is not a valid user-id. Maybe they have multiple accounts? Maybe they are not actually in the community?"
  (declare (special community connection))
  (if (find entry (members community) :test #'string=)
      t 
      (valid-user-p connection entry)))

(create-new-validator validator-valid-matrix-id :valid-id
    "Checks to make sure the entry is a valid matrix id."
    "Your input is not a valid matrix ID. A valid matrix ID contains an @ and a :"
  (and (char= (aref entry 0) #\@)
       (find #\: entry :test #'char=)))

(create-new-validator validator-valid-length :length
    "Checks to make sure that entry is a set length"
    "Your input is either too long or too short, it is not just right."
  (check-type (first args) integer)
  (= (length entry) (first args)))

(create-new-validator validator-string-bool :string-bool
    "Checks to make sure that entry is a string-bool"
    "Your input is none of the following: true false yes no y n t f 1 0 nil"
  (some (lambda (string)
          (string-equal entry string))
        '("true" "false" "yes" "no" "y" "n" "t" "f" "1" "0" "nil")))

(create-new-validator validator-validate-room :valid-room
    "Checks to make sure that entry is a valid room"
    "Your input is not a valid room. Check the case if you used a room name not ID."
  (declare (special community))
  (find-room community entry))

(defun args-from-validation-lists (list &optional (check-syms-against nil))
  (let ((syms (mapcar #'first list)))
    (when (listp check-syms-against)
      (loop :for sym :in syms
            :if (find sym check-syms-against)
              :do (error 'bad-symbols-used
                         :bad-symbols-used-symbols check-syms-against
                         :bad-symbols-used-used sym))
      syms)))

(defun list->validators (list)
  (let* ((arg (first list))
         (po-funs (rest list))
         (list-funs (remove-if-not #'listp po-funs))
         (non-list-funs (remove-if #'listp po-funs))
         (res nil))
    (loop :for non :in non-list-funs
          :do (let ((validator (get-validator non)))
                (push `(funcall ',validator ,arg) res)))
    (loop :for list :in list-funs
          :do (let ((validator (get-validator (first list))))
                (push `(funcall ',validator ,arg ,(second list)) res)))
    res))

(defun list-of-lists->validators (list)
  (apply #'append (mapcar #'list->validators list)))

(defun validate-room (entry x)
  (declare (special community))
  (let ((entry (elt entry x)))
    (loop :for room :in (rooms community)
            :thereis (or (string= entry (pkv room :id))
                         (string= entry (pkv room :name))))))

(defun validate-community (entry x)
  (declare (special luna))
  (find (intern (string-upcase (elt entry x)) :keyword)
        (communities luna) :key #'name))
