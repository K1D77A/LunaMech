(in-package #:mm-module.webhook)

(defclass luna-acceptor (tbnl:easy-acceptor)
  ()
  (:documentation "The acceptor for lunas webhooks."))

(defmodule webhook (mm-module.webhook WEBHOOK ubermensch-privilege)
           webhook-command ()
           webhook-module
           ((allow-webhooks-p
             :accessor allow-webhooks-p
             :initform t
             :documentation "if set to nil then all requests will be ignored.")
            (webhook-server
             :accessor webhook-server
             :initform (make-instance 'luna-acceptor :name "Luna's webhook"
                                                     :address "0.0.0.0"
                                                     :port 61111))))

(defmethod on-load-up (luna (module webhook-module))
  (log:info "Starting Luna's webhook listener on port 61111.")
  (handler-case 
      (tbnl:start (webhook-server *module*))
    (hunchentoot::hunchentoot-simple-error (c)
      (log:warn "Webhook listener already listening. ~A" c))
    (serious-condition (c)
      (log:error "Error starting webhook server on load up. ~A" c))))

(defmethod on-restart (luna (module webhook-module))
  (log:info "Restarting Luna's webhook listener on port 61111.")
  (on-shutdown luna module) ;;shutdown the server if possible.
  (handler-case 
      (tbnl:start (webhook-server *module*))
    (hunchentoot::hunchentoot-simple-error (c)
      (log:warn "Webhook listener already listening. ~A" c))
    (serious-condition (c)
      (log:error "Error restarting webhook-module. ~A" c))))
    

(defmethod on-shutdown (luna (module webhook-module))
  (log:info "Stopping Luna's webhook listener on port 61111.")
  (handler-case 
      (tbnl:stop (webhook-server *module*))
    (unbound-slot (c)
      (log:error "Webhook listener was not on while trying to shutdown. ~A" c))
    (serious-condition (c)
      (log:error "Error shutting down wehbook-module. ~A" c))))

(defmethod on-save (moonbot (module webhook-module))
  t)

(defmethod on-module-unload (moonbot (module webhook-module))
  (log:info "Stopping Luna's webhook listener.")
  (handler-case 
      (tbnl:stop (webhook-server *module*))
    (unbound-slot (c)
      (log:error "Webhook listener was not on while trying to unload. ~A" c))
    (serious-condition (c)
      (log:error "Error unloading wehbook-module. ~A" c))))

(defmethod locate-command ((module webhook-module) (priv ubermensch-privilege)
                           invoker community)
  "When prefix is 'webhook with no privileges search for webhook-command"
  (or (type-find 'webhook-command invoker *commands*
                 :key #'name :test #'string-equal)
      (error 'missing-command)))

(defmethod locate-command ((module webhook-module) priv invoker community)
  "When prefix is webhook with no privilege just signal 'missing-command"
  (error 'missing-command))

(defmethod execute-command ((moonbot moonbot) (priv ubermensch-privilege)
                            (command webhook-command)
                            community room message rest)
  (if (string-equal (first rest) "help")
      (print-command-information command community room)
      (safe-execution command community room message rest moonbot)))

(command-defining-macro-moonbot new-webhook-command 'webhook-command)

(new-webhook-command help ()
    "attempts an explanation of how commands work"
  (moonhelp 'webhook-command community room))

(new-webhook-command restart ()
    "Attempts to start/restart Luna' webhook listener."
  (ignore-errors (tbnl:start (webhook-server *module*)))
  (format t "Done."))
  
(defmethod hunchentoot:acceptor-dispatch-request ((acceptor luna-acceptor) request)
  (when (allow-webhooks-p *module*)
    (call-next-method)))

(def-webhook direct-message ()
  ((send-otp
    :validator
    (lambda (username otp)
      (declare (ignore username))
      (let ((len (length otp)))
        (<= 1 len 4)))
    :fn (lambda (username otp)
          (string (mm-module.direct-message:start-dm :otp username (conn *luna*) otp)))
    :expected-args (username otp)))
  (:private-key
   (lambda ()
     (mm-module.private-keys:get-key :webhook/direct-message))))

(def-webhook send-messages ()
  ((log-room
    :validator (lambda (message room-id)
                 (declare (ignore message room-id))
                 t)
    :fn (lambda (message room-id)
          (lunamat-message (first (communities *luna*)) room-id "~{~A~}" message)
          "t")
    :expected-args (message room-id)))
  (:private-key
   (lambda ()
     (mm-module.private-keys:get-key :webhook/send-messages))))

(def-webhook openid ()
  ((openid
    :validator (lambda () t)
    :fn (lambda ()
          (log:info "Request for Luna's openID made.")
          (jojo:to-json (request-open-id-token (conn *luna*))))
    :expected-args nil))
  (:private-key
   (lambda ()
     (mm-module.private-keys:get-key :webhook/openid))))

(def-webhook luna ()
  ((force-restart
    :validator (lambda () t)
    :fn (lambda ()
          (log:warn "Force restarting Luna from webhook")
          (force-stop *luna*)
          (sleep 1)
          (start *luna*)
          "t")))
  (:private-key
   (lambda ()
     (mm-module.private-keys:get-key :webhook/force-restart))))

(defun check-authorized-webhook-request (application private-key)
  "Checks if the APPLICATION provided (string) is valid, checks if a private-key exists
for APPLICATION, and then finally makes sure that PRIVATE-KEY provided is string= to 
the private-key associated with the application found.
If the private-keys do not match signals 'bad-private-key."
  (check-type application string)
  (let* ((upped (intern (string-upcase application) :keyword))
         (app (get-application upped))
         (key (get-private-key upped)))
    (if (string= key private-key)
        (make-instance app)
        (error 'bad-private-key :webhook app :private-key private-key))))

(tbnl:define-easy-handler (uri :uri "/webhook" :default-request-type :POST)
    ()
  (let ((hook-type (tbnl:header-in* :hook-type))
        (hook-name (tbnl:header-in* :hook-name))
        (authorization (tbnl:header-in* :private-key)))
    (handler-case
        (let ((hook (find-hook hook-type hook-name authorization))
              (raw (tbnl:raw-post-data)))
          (log:info "Executing hook. Type ~S. Name: ~S. Rawlen: ~S"
                    hook-type hook-name (if raw (length raw) 0))
          (if (stringp hook)
              hook
              (apply #'execute-hook hook
                     (when raw
                       (jojo:parse (babel:octets-to-string raw))))))
      (serious-condition (c)
        (log:error "Serious condition ~A caught in /webhook."
                   c c)
        (setf (tbnl:return-code*) 400)
        (format nil "~A" c)))))

