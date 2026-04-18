(in-package #:matrix-moonbot)

(defparameter *ark* "The top level luna ark instance :)")

(defun setup-and-go (&key (slynk t))
  "This is the default entry function for the dumped lisp image."
  (setup-log4cl)
  (log:info "LunaMech is booting.")
  (when slynk
    (log:info "Starting primary Swank server.")
    (swank:create-server :port 54000 :dont-close t);primary sly connection
    (log:info "You can now connect with Swank."))
  (log:info "Restoring Luna from config...")
  (handler-bind ((warning
                   (lambda (c)
                     (declare (ignore c))
                     (invoke-restart 'muffle-warning))))
    (setf *ark* (config->ark)))
  (add-exit-hooks *ark*)
  (log:info "Booting LunaMech's Ark.")
  (setf lparallel:*kernel* (lparallel:make-kernel 2))
  (start *ark*)
  (log:info "Dropping into the SBCL Top Level.")
  (handler-case 
      (front-end-loop)
    (SB-SYS:INTERACTIVE-INTERRUPT ()
      (when (slot-boundp *ark* 'lunas)
        (stop *ark*))
      (sb-ext:quit))))

(defun front-end-loop ()
  "systemd just restarts Luna if we enter the toplevel so instead we will just loop 
and sleep over and over and over again."
  (log:info "Starting the top level loop.")
  (loop :do (sleep 1000)))

(defun setup-log4cl ()
  (log:info "Starting daily logging to log/")
  (log:config :daily "logs/lunamech-log"))

(defun add-exit-hooks (ark)
  (log:info "Adding exit hooks for Luna.")
  (push (lambda ()
          (mapc (lambda (luna)
                  (log:info "Exit hook invoked. Shutting down.")
                  (mapc (lambda (mod) (on-shutdown luna mod)) (found-modules luna))
                  (when (slot-boundp luna 'thread)
                    (stop luna)))
                (lunas ark)))
        sb-ext:*exit-hooks*))


