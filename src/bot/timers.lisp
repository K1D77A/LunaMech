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

(defmacro execute-stamp-n-after-luna ((timer time-in-seconds)
                                      &body body)
  `(when (>= (local-time:timestamp-difference (timestamp *luna*)
                                              (timestamp ,timer))
             ,time-in-seconds)
     (unwind-protect
          (locally ,@body)
       (reset-timer ,timer))))
