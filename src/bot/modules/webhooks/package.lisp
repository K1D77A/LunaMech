(defpackage #:luna-module.webhook
  (:use #:cl #:lunamech)
  (:export #:webhook-command
           #:webhook-module
           #:register-new-webhook
           #:get-application
           #:*module*))

(in-package #:luna-module.webhook)
