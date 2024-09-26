(in-package #:mm-module.webhook)
;;;;this file contains a MOP for webhooks

(defparameter *hook-types* (make-hash-table :test #'equal))

(defclass webhook-api-slot (c2mop:slot-definition)
  ((fn
       :accessor fn
     :initarg :fn
     :documentation "The function that is executed when this webhook is found.")
   (validator
    :accessor validator
    :initarg :validator
    :documentation "The argument validation function")
   (expected-args
    :accessor expected-args
    :initarg :expected-args
    :initform nil
    :type (or null list)
    :documentation "A list of arg names")
   (result
    :accessor result
    :initarg :result
    :type t
    :documentation "The result of executing this hook. This can be an actual result or 
a condition.")
   (unique-id
    :accessor unique-id
    :initarg :unique-id
    :documentation "A unique ID assigned by the caller so that the caller can come back
later and check the result.")
   (private-key
    :accessor private-key
    :initarg :private-key
    :documentation "The private key used to execute this webhook."))
  (:documentation "A toplevel class used to define new webhooks."))


(defclass webhook (standard-class)
  ((private-key
    :accessor private-key
    :initarg :private-key)))

(defclass webhook-api-direct (webhook-api-slot c2mop:standard-direct-slot-definition)
  ())

(defclass webhook-api-effective (webhook-api-slot c2mop:standard-effective-slot-definition)
  ())

(defmethod c2mop:validate-superclass ((class webhook) (metaclass standard-class))
  t)

(defmethod c2mop:validate-superclass ((class webhook-api-slot) (metaclass standard-class))
  t)

(defmethod c2mop:effective-slot-definition-class ((class webhook) &rest initargs)
  (declare (ignore initargs))
  (find-class 'webhook-api-effective))

(defmethod c2mop:direct-slot-definition-class ((class webhook) &rest initargs)
  (declare (ignore initargs))
  (find-class 'webhook-api-direct))

(defmethod c2mop:compute-effective-slot-definition ((class webhook) name dslots)
  (call-next-method))

(defmethod c2mop:compute-effective-slot-definition :after ((class webhook) name dslots)
  (with-accessors ((private-key private-key))
      class
    (when (listp private-key)
      (setf private-key (compile nil (first private-key))))
    (mapc (lambda (slot)
            (post-process-slot slot private-key))
          dslots)))

(defmethod c2mop:compute-slots ((class webhook))
  (call-next-method))

(defmethod post-process-slot ((slot webhook-api-direct) pkey)
  (with-slots (fn validator expected-args private-key)
      slot
    ;;really should check that fn and validator are either lists, symbols or functions
    (unless (slot-boundp slot 'private-key)
      (setf private-key pkey))
    (when (listp private-key)
      (when (eql (first private-key) 'lambda)
        (setf private-key (compile nil private-key))))
    (when (listp fn)      
      (if (eql (first fn) 'lambda)
          (setf fn (compile nil fn))
          (setf fn
                (compile nil `(lambda ,expected-args
                                (declare ,(append (list 'ignorable) expected-args))
                                (block fn 
                                  (locally ,fn)))))))
    (when (listp validator)
      (if (eql (first validator) 'lambda)
          (setf validator (compile nil validator))
          (setf validator
                (compile nil
                         `(lambda ,expected-args
                            (declare ,(append (list 'ignorable) expected-args))
                            (block validator
                              (locally ,validator)))))))))

(defmacro def-webhook (name direct-superclasses direct-slots &rest options)
  `(progn (defclass ,name (,@direct-superclasses)
            ,direct-slots
            (:metaclass webhook)
            ,@options)
          (c2mop:finalize-inheritance (find-class ',name))
          (setf (gethash ',name *hook-types*) (find-class ',name))))


;;;inheritance from superclasses isn't creating direct slots

(defmethod execute-validator :before ((slot webhook-api-slot) &rest args)
  (or (= (length args) (length (slot-value slot 'expected-args)))
      (error 'hook-validation-failed)))

(defmethod execute-validator ((slot webhook-api-slot) &rest args)
  (with-slots (validator)
      slot
    (handler-case
        (or (apply validator args)
            (error 'hook-validation-failed))
      ((or error (not hook-validation-failed)) (c)
        (error 'hook-validation-failed :signalled c)))))

(defmethod execute-function ((slot webhook-api-slot) &rest args)
  (with-slots (fn result)
      slot
    (handler-case (setf result (apply fn args))
      (condition (c)
        (setf result c)))))

(defmethod execute-hook :around ((slot webhook-api-slot) &rest args)
  (declare (ignore args))
  (handler-case (call-next-method)
    (hook-validation-failed (c)
      (setf (slot-value slot 'result) c))))

(defmethod execute-hook ((slot webhook-api-slot) &rest args)
  "Calls execute-validator on the SLOT with ARGS and then EXECUTE-FUNCTION. 
Execute-validator has the "
  (apply #'execute-validator slot args)      
  (apply #'execute-function slot args))

(defmethod execution-result ((slot webhook-api-slot))
  (when (slot-boundp slot 'result)
    (slot-value slot 'result)))

(defgeneric find-hook (c slot-name private-key)
  (:documentation
     "Looks for a webhook denoted by slot-name within the class, if found then validates 
the provided PRIVATE-KEY against the one stored within the found webhook, if they are 
the same then returns the slot, otherwise signals 'bad-private-key. If no slot can be 
found by the name SLOT-NAME signals 'webhook-not-found.")
  (:method ((class string) slot-name private-key)
    (find-hook (find-class (intern (string-upcase class) :mm-module.webhook))
               slot-name private-key))
  (:method ((class webhook) (slot-name string) private-key)
    (find-hook class (intern (string-upcase slot-name) :mm-module.webhook) private-key))
  (:method ((class webhook) slot-name pkey)
    (let ((hook (find slot-name (c2mop:class-direct-slots class)
                      :key #'c2mop:slot-definition-name)))
      (unless hook
        (error 'webhook-not-found :webhook slot-name))
      (with-slots (private-key)
          hook
        (if (string= (typecase private-key
                       (string private-key)
                       (function (funcall private-key)))
                     pkey)
            (progn (when (slot-boundp hook 'result)
                     (slot-makunbound hook 'result))
                   hook)
            (error 'bad-private-key :private-key private-key
                                    :webhook slot-name))))))

(defmethod no-applicable-method ((gf (eql #'find-hook)) &rest args)
  (when (every #'null args)
    "I'm alive"))

