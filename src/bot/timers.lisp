(in-package #:matrix-moonbot)

;;;;The code in this file implements a nice way to perform many various actions at different
;;;;intervals relative to a timestamp.
#|| 
The plan is to have an object that will store a list of various timers, 
you will be able to register a new timer and then check if the timer should fire,
if the timer is ready to fire than its body is executed and the time stamp associated with 
that key is reset
||#

(defclass timer ()
  ((timestamp
    :accessor timestamp
    :initform (local-time:now)
    :type local-time:timestamp
    :documentation "The timestamp used for comparison")
   (key
    :accessor key
    :initarg :key
    :type keyword
    :documentation "The keyword used to find this timer"))
  (:documentation "a key and a timestamp. This is used to find and compare this timestamp
to another and choose whether it is time to execute or not"))

(defclass timers ()
  ((timers
    :accessor timers
    :initform nil
    :type list
    :documentation "A list of timer objects")))

(defmethod find-timer ((timers timers) key)
  (find key (timers timers) :key #'key))

(defun make-timers (keys)
  (check-type keys list)
  (let ((timers (make-instance 'timers)))
    (mapc (lambda (key)
            (make-timer timers key))
          keys)
    timers))

(defmethod make-timer ((timers timers) key)
  (push (make-instance 'timer :key key)
        (timers timers)))

(defmethod reset-timer ((timer timer))
  (setf (timestamp timer) (local-time:now)))

(defmethod find-and-reset-timer ((timers timers) key)
  (let ((timer (find-timer timers key)))
    (when timer
      (reset-timer timer))))

(defun execute-when-timer-difference (dif-in-seconds timer1 timer2 function)
  (when (>= (local-time:timestamp-difference timer2 timer1)
            dif-in-seconds)
    (funcall function)
    (reset-timer timer1)))
;;'((:join <fun-to-execute>)(:check <fun-to-execute>))

;; (defmacro gen-lets (list &body body)
;;   `(let ,(mapcar (lambda (sym)
;;                    `(,sym nil))
;;           list)
;;      ,@body))

;; (defmacro key-fun-list->code (exec-var timers-object start-timestamp key-fun-list
;;                               &key (next-timer-key nil)
;;                                 body)
;;   (destructuring-bind (key time fun)
;;       key-fun-list
;;     `(when (>= (local-time:timestamp-difference ,start-timestamp
;;                                                 (timestamp (find-timer ,timers-object
;;                                                                        ,key)))
;;                ,time)
;;        (if ,exec-var
;;            (progn (setf exec-var t);this body never executes again
;;                   (when ,next-timer-key
;;                     (find-and-reset-timer ,timers-object ,next-timer-key))
;;                                         ;set the next timer to the time that body executed
;;                   (funcall ,fun));execute the body
;;            (progn ,body)))))


;; (defmacro execute-with-timers ((timers-object) start-timestamp key-fun-list)
;;   ""
;;   (let ((to-reset (mapcar #'car key-fun-list)))
;;     (alexandria:with-gensyms (execed1 execed2)
;;       `(gen-lets (,execed1 ,execed2)
;;          (key-fun-list->code ,execed1 ,timers-object ,start-timestamp (:join 5 '+)
;;            :body (key-fun-list->code ,execed2 ,timers-object ,start-timestamp (:check 5 '+))
;;            :next-timer-key :check)))))









