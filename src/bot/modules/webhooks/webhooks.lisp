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

(defmethod on-load-up (moonbot (module webhook-module))
  (log:info "Starting Luna's webhook listener on port 54002.")
  (tbnl:start (webhook-server *module*)))

(defmethod on-save (moonbot (module webhook-module))
  t)

(defmethod on-module-unload (moonbot (module webhook-module))
  (log:info "Stopping Luna's webhook listener.")
  (tbnl:stop (webhook-server *module*))
  t)

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
          (mm-module.direct-message:start-dm :otp username (conn *luna*) otp)
          "t")
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

(def-webhook upload-content ()
  ((for-stickerpicker
    :validator (lambda (filename content-type content-bytes)
                 (declare (ignore filename content-type content-bytes))
                 t)
    :fn (lambda (filename content-type content-bytes)
          (log:info filename)
          (let ((uploaded 
                  (upload-content (conn *luna*) filename content-type
                                  (coerce content-bytes '(vector (unsigned-byte 8))))))
            (getf uploaded :|content_uri|)))
    :expected-args (filename content-type content-bytes)))
  (:private-key
   (lambda () (mm-module.private-keys:get-key :webhook/upload-content))))

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
              (args (jojo:parse (babel:octets-to-string (tbnl:raw-post-data)))))
          (apply #'execute-hook hook args))
      (webhook-condition (c)
        (setf (tbnl:return-code*) 400)
        (format nil "~A" c))
      (error (c)
        (setf (tbnl:return-code*) 400)
        (format nil "~A" c)))))
