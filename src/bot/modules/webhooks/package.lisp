(defpackage #:mm-module.webhook
  (:use #:cl #:matrix-moonbot)
  (:export #:webhook-command
           #:webhook-module
           #:register-new-webhook
           #:get-application
           #:*module*))

(in-package #:mm-module.webhook)
