(in-package #:matrix-moonbot)

(defparameter *ark* "The top level luna ark instance :)")

(defmethod initialize ((ark lunas-ark))
  (setf lparallel:*kernel* (lparallel:make-kernel 2))
  (log:info "Adding exit hooks for LunaMech.")
  (push (lambda ()
          (lparallel:pmapc (lambda (luna)
                  (log:info "Exit hook invoked. Shutting down.")
                  (lparallel:pmapc (lambda (mod) (on-shutdown luna mod)) (found-modules luna))
                  (when (slot-boundp luna 'thread)
                    (stop luna)))
                (lunas *ark*)))
        sb-ext:*exit-hooks*))

(defmethod start ((ark lunas-ark))
  "Attempt to start all instances of luna within ark."
  (with-accessors ((lunas lunas))
      ark
    (log:info "Starting Lunas")
    (mapc #'start lunas)))

(defmethod stop ((ark lunas-ark))
  (with-accessors ((lunas lunas))
      ark
    (log:info "Stopping Lunas")
    (mapc #'stop lunas)))
  
    
