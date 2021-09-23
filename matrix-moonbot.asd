;;;; admin.asd
(asdf:defsystem #:matrix-moonbot
  :description "LunaMech is a featureful bot for use on Matrix."
  :author "K1D77A"
  :license  "GPL-3.0"
  :build-operation "program-op"
  :build-pathname "./lunamech"
  :entry-point "matrix-moonbot:setup-and-go"
  :version "0.0.12"
  :depends-on (#:jonathan
               #:alexandria
               #:str
               #:dexador
               #:flexi-streams
               #:cl-megolm
               #:ironclad
               #:parse-float
               #:do-urlencode
               #:bordeaux-threads
               #:vecto
               #:babel
               #:slynk
               #:lparallel
               #:local-time
               #:ubiquitous-concurrent
               #:chirp
               #:log4cl
               #:trivial-backtrace
               #:cl-feedparser
               #:spellcheck
               #:spinneret
               #:hunchentoot
               #:validate-list)
  :components ((:module "config"
                :components ((:static-file "communities.lisp")))
               (:module "src"
                :components
                ((:module "compass"
                  :components ((:file "compass")))
                 (:file "package")
                 (:file "helpers")
                 (:file "classes" :depends-on ("package"))
                 (:file "conditions")
                 (:module "api"
                  :depends-on ("package" "classes")
                  :serial t
                  :components ((:file "api-helpers")
                               (:file "events")
                               (:file "user-api")
                               (:file "admin-api")
                               (:file "filter-api")
                               (:file "spaces")
                               (:file "sync")))
                 (:module "bot"
                  :depends-on ("package" "classes" "api" "compass")
                  :serial t
                  :components ((:module "encryption"
                                :serial t
                                :components ((:file "classes")
                                             (:file "encryption")))
                               (:file "configparse")
                               (:file "module")
                               (:file "filters")
                               (:file "timers")
                               (:file "syncing")
                               (:file "validator")
                               (:file "command-helpers")
                               (:file "command-system")
                               (:module "modules"
                                :components
                                        ((:file "keys")
                                         (:file "admin")
                                         (:file "compass")
                                         (:file "luna")
                                         (:file "huginn")
                                         (:file "site")
                                         (:file "jitsi")
                                         (:file "rss")
                                         (:file "twitter")
                                         (:module "direct-message"
                                          :serial t
                                          :description "Module that implements the 
construction and execution of direct messages with individual users."
                                          :components
                                                  ((:file "package")
                                                   (:file "direct-message")
                                                   (:file "context-generator")
                                                   (:file "context-executor")
                                                   (:file "collect")
                                                   (:file "room-commands")
                                                   (:file "to-file")
                                                   (:file "commands")
                                                   ))
                                         (:module "webhooks"
                                          :serial t
                                          :components ((:file "package")
                                                       ;; (:file "classes")
                                                       (:file "conditions")
                                                       (:file "webhooks-protocol")
                                                       (:file "webhooks"))) 
                                         (:module "stickers"
                                          :serial t
                                          :description "Module that implements uploading
stickers to Lunamechs Stickerpicker."
                                          :components ((:file "package")
                                                       (:file "classes")
                                                       (:file "resizer")
                                                       (:file "processor")
                                                       (:file "stickers")
                                                       (:file "from-website")))))
                               (:file "commands")
                               (:file "message-processing")
                               (:file "bot")))
                 (:file "config-creation" :depends-on ("bot"))))))
