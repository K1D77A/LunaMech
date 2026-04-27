(in-package #:lunamech)

(defparameter *ark* "The top level luna ark instance :)")

(defmethod pause ((ark lunas-ark))
  (sb-concurrency:close-gate (pause-gate ark))
  (setf (state ark) :paused))

(defmethod proceed ((ark lunas-ark))
  (sb-concurrency:wait-on-gate (pause-gate ark)))

(defmethod resume ((ark lunas-ark))
  (sb-concurrency:open-gate (pause-gate ark))
  (setf (state ark) :running))

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
  (with-accessors ((lunas lunas)
                   (state state))
      ark
    (log:info "Starting Lunas")
    (setf state :starting)
    (mapc #'start lunas)
    (setf state :running)))

(defmethod stop ((ark lunas-ark))
  (with-accessors ((lunas lunas)
                   (state state))
      ark
    (log:info "Stopping Lunas")
    (setf state :stopping)
    (mapc #'stop lunas)
    (setf state :stopped)
    (ark->config *ark*)))
  
    
