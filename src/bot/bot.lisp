(in-package #:lunamech)

(defun front-end-loop ()
  "systemd just restarts Luna if we enter the toplevel so instead we will just loop 
and sleep over and over and over again."
  (log:info "Starting the top level loop.")
  (loop :do (sleep 1000)))

(defun setup-log4cl ()
  (log:info "Starting daily logging to logs/")
  (log:config :daily "logs/lunamech-log"))

(defun setup-and-go (&key (swank t) (toplevel t) (quit t))
  "This is the default entry function for the dumped lisp image."
  (log:info "LunaMech is booting.")
  (setup-log4cl)
  (handler-case
      (progn 
        (when swank
          (log:info "Starting primary Swank server.")
          (swank:create-server :port 54000 :dont-close t);primary sly connection
          (log:info "You can now connect with Swank."))
        (tagbody weee 
           (flet ((initialize-configs ()
                    (log:info "No ark.bin exists @ ~S" *default-config-location*)
                    (log:info "Attempting to build initial ark from templates!")
                    (build-ark-from-blanks)
                    (go weee)))
             (if (ark-exists-p)
                 (progn 
                   (log:info "Restoring Luna from config...")
                   (setf *ark* (config->ark)))
                 (initialize-configs))))
        (log:info "Booting LunaMech's Ark.")
        (initialize *ark*)
        (start *ark*)
        (log:info "Dropping into top level."))
    (condition (c)
      (log:error "~A" c)
      (when quit 
        (sb-ext:quit :unix-status 1))))
  (handler-case
      (when toplevel 
        (front-end-loop))
    (SB-SYS:INTERACTIVE-INTERRUPT ()
        (stop *ark*)
      (sb-ext:quit))))





