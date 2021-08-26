(in-package #:mm-module.direct-message)

#||
this file contains the means to define and control 'collectors'. 
Collectors are various ways to collect all of the 'contexts' from prooms into a 
human-readable/machine readable format, or to perform an action on all of the contexts 
like adding them to the compass results. 
collectors will have to be functions that take two arguments, Luna and a completed context

||#

(defclass collector ()
  ((description
    :accessor description
    :initarg :description
    :type string
    :documentation "A doc string used to describe this collector")
   (context-type
    :accessor context-type
    :initarg :context-type
    :type keyword
    :documentation "the type of context this collector is applicable for.")
   (invoker
    :accessor invoker
    :initarg :invoker
    :type string
    :documentation "The key that this collector is stored under in (collectors *module*)")
   (fun
    :accessor fun
    :initarg :fun
    :type (or symbol function)
    :documentation "The symbol that denotes a two argument function to be called with Luna
and a context. Or a function that takes two arguments."))
  (:documentation "A collect is an object that is used with completed contexts in order
to do something useful"))

(define-condition no-collector-found (direct-message-condition)
  ((no-collector-found-type
    :accessor no-collector-found-type
    :initarg :no-collector-found-type)
   (no-collector-found-invoker
    :accessor no-collector-found-invoker
    :initarg :no-collector-found-invoker))
  (:documentation "Signalled when you try to gen a new collector with a key but no 
collector is found")
  (:report
   (lambda (obj stream)
     (format stream "No collector found for context-type: ~A~%and invoker: ~A~%"
             (no-collector-found-type obj)
             (no-collector-found-invoker obj)))))

(create-new-validator validate-valid-collector :valid-collector
    "Checks to make sure that entry is a valid collector"
    "Your input is not a valid collector. See .direct-message collectors "
  (get-collector (intern (string-upcase (first args)) :keyword) entry))

(defun add-collector (key collector)
  (setf (gethash key (collectors *module*))
        (remove (invoker collector) (gethash key (collectors *module*))
                :key #'invoker :test #'string=))
  (push collector (gethash key (collectors *module*))))

(defun new-collector (context-type description invoker fun)
  (make-instance 'collector :context-type context-type :description description
                            :invoker invoker :fun fun))

(defun get-collector (context-type invoker)
  (let ((val (find invoker (gethash context-type (collectors *module*))
                   :key #'invoker
                   :test #'string=)))
    (if val
        val
        (error 'no-collector-found
               :no-collector-found-type context-type
               :no-collector-found-invoker invoker))))

(defmacro define-collector (invoker context-type description &body body)
  (check-type invoker string)
  (check-type description string)
  `(add-collector ,context-type
                  (new-collector ,context-type ,description ,invoker
                                 (lambda (luna context user-id)
                                   (declare (ignorable luna context user-id))
                                   (with-accessors ((results results))
                                       context
                                     (declare (ignorable results))
                                     (locally ,@body))))))

(defun invoke-collector (luna user-id collector-invoker context)
  (let ((collector (get-collector (context-type context) collector-invoker)))
                                        ;can signal no known collector
    (funcall (fun collector) luna context user-id)))

(define-collector "print-results" :COMPASS
    "prints the results of the context."
  (mapc (lambda (result)
          (with-accessors ((id id)
                           (validator validator)
                           (value value))
              result
            (format t "ID: ~A. Value: ~A. Validator: ~A~%" id value validator))) 
        results))

(define-collector "add-to-compass" :COMPASS
    "Adds the results to the compass module"
  (let ((x (value (find 'X results :key #'id)))
        (y (value (find 'Y results :key #'id))))
    (when (and x y)
      (compass:add-results (parse-integer x) (parse-integer y) user-id))))
