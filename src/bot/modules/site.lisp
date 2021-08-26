(defpackage #:mm-module.site
  (:use #:cl #:matrix-moonbot))

(in-package #:mm-module.site)

(defmodule site (mm-module.site SITE normie-privilege)
           site-command ()
           site-module ())

(defmethod locate-command ((module site-module) priv invoker community)
  "When prefix is 'site with no privileges search for site-command"
  (or (type-find 'site-command invoker *commands*
                 :key #'name :test #'sym-name-equal)
      ;;site commands created for communities that are for normies
      (error 'missing-command)))

(command-defining-macro-no-moonbot new-normie-site-command
                                   'site-command)


(new-normie-site-command help ()
    "attempts an explanation of how commands work"  
  (moonhelp 'site-command community room))

(new-normie-site-command stickers ()
    ""
  (lunamat-message community room "https://lunamech.com/stickerpicker"))

(new-normie-site-command luna ()
    ""
  (lunamat-message community room "https://lunamech.com/"))

