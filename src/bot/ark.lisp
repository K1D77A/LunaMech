(in-package #:matrix-moonbot)

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
  
    
