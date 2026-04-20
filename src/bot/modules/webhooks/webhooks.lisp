(in-package #:mm-module.webhook)

(defclass luna-acceptor (tbnl:easy-acceptor)
  ((%luna
    :accessor luna
    :initarg :luna
    :type lunamech
    :documentation "The lunamech that created this acceptor")
   (allow-webhooks-p
    :accessor allow-webhooks-p
    :initform t
    :documentation "if set to nil then all requests will be ignored."))
  (:documentation "The acceptor for lunas webhooks."))

(defmodule webhook (mm-module.webhook WEBHOOK ubermensch-privilege)
           webhook-command ()
           webhook-module
           ((port
             :accessor port
             :initarg :port
             :initform 0 ;;this is used as the marker of a successful load..
             :type fixnum)
            (address
             :accessor address
             :initarg :address
             :initform ""
             :type string)
            (server-name
             :accessor server-name
             :initarg :server-name
             :initform "Luna's webhook"
             :type string)
            (webhook-server
             :accessor webhook-server
             :initform nil
             :type (or luna-acceptor null))))


(defmethod on-load-up (luna (module webhook-module))
  (let ((path (module-persistent-path luna module "webhook" "lisp")))
    (log:info "Loading Webhook config from: ~S" path)
    (format t "Loading Webhook config from webhook.lisp~%")
    (if (probe-file path)
        (progn 
          (destructuring-bind (&key port address name)
              (uiop:read-file-form path)
            (setf (port module) port
                  (address module) address
                  (server-name module) name))
          (log:info "Starting Luna's webhook listener on port ~D." (port module))
          (format t "Starting Luna's webhook listener on port ~D.~%" (port module))
          (handler-case
              (progn
                (unless (webhook-server module)
                  (setf (webhook-server module)
                        (make-instance 'luna-acceptor :name (name module)
                                                      :port (port module)
                                                      :address (address module)
                                                      :luna luna)))
                (tbnl:start (webhook-server module))
                (log:info "Success")
                (format t "Success~%"))
            (hunchentoot::hunchentoot-simple-error (c)
              (format t "Webhook listener already listening. ~A" c)
              (log:warn "Webhook listener already listening. ~A" c))
            (serious-condition (c)
              (log:error "Error starting webhook server on load up. ~A" c))))
        (progn (log:info "webhook path: ~S didn't exist" path)
               (format t "Missing webhook config. Module is loaded but not functional.~
                          Correct missing config and unload then load again.~%")))))


(defmethod on-save (luna (module webhook-module))
  (unless (zerop (port module))
    (let ((path (module-persistent-path luna module "webhook" "lisp")))    
      (log:info "Saving webhook configuration to ~S" path)
      (alexandria:write-string-into-file
       (format nil "~S"
               (list :port (port module)
                     :name (server-name module)
                     :address (address module)))
       path
       :if-exists :supersede
       :if-does-not-exist :create))))

(defmethod on-restart (luna (module webhook-module))
  (log:info "Restarting Luna's webhook listener on port: ~D" (port module))
  (on-shutdown luna module) ;;shutdown the server if possible.
  (handler-case 
      (tbnl:start (webhook-server module))
    (hunchentoot::hunchentoot-simple-error (c)
      (log:warn "Webhook listener already listening. ~A" c))
    (serious-condition (c)
      (log:error "Error restarting webhook-module. ~A" c))))
    

(defmethod on-shutdown (luna (module webhook-module))
  (log:info "Stopping Luna's webhook listener on port ~D." (port module))
  (format t "Stopping Luna's webhook listener on port ~D.~%" (port module))
  (handler-case
      (when (webhook-server module)
        (tbnl:stop (webhook-server module)))
    (unbound-slot (c)
      (format t "Webhook listener was not on while trying to shutdown. ~A~%" c)
      (log:error "Webhook listener was not on while trying to shutdown. ~A" c))
    (serious-condition (c)
      (format t "Error shutting down wehbook-module. ~A~%" c)
      (log:error "Error shutting down wehbook-module. ~A" c))))

(defmethod on-module-load (luna (module webhook-module))
  (on-load-up luna module))

(defmethod on-module-unload (luna (module webhook-module))
  (on-shutdown luna module))

(defmethod locate-command ((module webhook-module) (priv ubermensch-privilege)
                           invoker community)
  "When prefix is 'webhook with no privileges search for webhook-command"
  (or (type-find 'webhook-command invoker *commands*
                 :key #'name :test #'string-equal)
      (error 'missing-command)))

(defmethod locate-command ((module webhook-module) priv invoker community)
  "When prefix is webhook with no privilege just signal 'missing-command"
  (error 'missing-command))

(defmethod execute-command ((luna lunamech) (priv ubermensch-privilege)
                            (command webhook-command)
                            community room message rest)
  (if (string-equal (first rest) "help")
      (print-command-information command community room)
      (safe-execution command community room message rest luna)))

(command-defining-macro-luna new-webhook-command 'webhook-command)

(new-webhook-command help ()
    "attempts an explanation of how commands work"
  (moonhelp 'webhook-command community room))

(new-webhook-command restart ()
    "Attempts to start/restart Luna' webhook listener."
  (ignore-errors (tbnl:start (webhook-server *module*)))
  (format t "Done."))
  
(defmethod hunchentoot:acceptor-dispatch-request ((acceptor luna-acceptor) request)
  (when (allow-webhooks-p tbnl:*acceptor*)
    (call-next-method)))

;; (def-webhook direct-message ()
;;   ((send-otp
;;     :validator
;;     (lambda (username otp)
;;       (declare (ignore username))
;;       (let ((len (length otp)))
;;         (<= 1 len 4)))
;;     :fn (lambda (username otp)
;;           (string (mm-module.direct-message:start-dm :otp username
;;                                                      (luna tbnl:*acceptor*)
;;                                                      otp)))
;;     :expected-args (username otp)))
;;   (:private-key
;;    (lambda ()
;;      (mm-module.private-keys:get-key :webhook/direct-message))))

;;doesnt work. 
(def-webhook send-messages ()
  ((log-room
    :validator (lambda (message room-id)
                 (declare (ignore message room-id))
                 t)
    :fn (lambda (message room-id)
          (module-moonmat-message (conn (luna tbnl:*acceptor*)) room-id "~A" message)
          "t")
    :expected-args (message room-id)))
  (:private-key
   (lambda ()
     (mm-module.private-keys:get-key (luna tbnl:*acceptor*) :webhook/send-messages))))

(def-webhook openid ()
  ((openid
    :validator (lambda () t)
    :fn (lambda ()
          (log:info "Request for Luna's openID made.")
          (jojo:to-json (request-open-id-token (conn (luna tbnl:*acceptor*)))))
    :expected-args nil))
  (:private-key
   (lambda ()
     (mm-module.private-keys:get-key :webhook/openid))))

(def-webhook luna ()
  ((force-restart
    :validator (lambda () t)
    :fn (lambda ()
          (log:warn "Force restarting Luna from webhook")
          (force-stop (luna tbnl:*acceptor*))
          (sleep 1)
          (start (luna tbnl:*acceptor*))
          "t")))
  (:private-key
   (lambda ()
     (mm-module.private-keys:get-key (luna tbnl:*acceptor*) :webhook/force-restart))))

(tbnl:define-easy-handler (uri :uri "/webhook" :default-request-type :POST)
    ()
  (let ((hook-type (tbnl:header-in* :hook-type))
        (hook-name (tbnl:header-in* :hook-name))
        (authorization (tbnl:header-in* :private-key)))
    (handler-case
        (let* ((hook (find-hook hook-type hook-name authorization))
               (raw (tbnl:raw-post-data))
               (parsed (when raw
                         (jojo:parse (babel:octets-to-string raw)))))
          (if (stringp hook)
              hook 
              (with-slots (lock)
                  hook
                (bt2:with-lock-held (lock :timeout 30)
                  (when (slot-boundp hook 'result)
                    (slot-makunbound hook 'result))              
                  (log:info "Executing hook. Type ~S. Name: ~S. Rawlen: ~S. Parsed: ~%~S"
                            hook-type hook-name (if raw (length raw) 0) parsed)
                  (apply #'execute-hook hook parsed)))))
      (condition (c)
        (log:error "Serious condition ~A caught in /webhook." c)
        (setf (tbnl:return-code*) 400)
        (format nil "See you Space Cowboy!")))))

