(defpackage #:luna-module.site
  (:use #:cl #:lunamech))

(in-package #:luna-module.site)

(defmodule site (luna-module.site SITE normie-privilege)
           site-command ()
           site-module ())

(defmethod locate-command ((module site-module) priv invoker community)
  "When prefix is 'site with no privileges search for site-command"
  (or (type-find 'site-command invoker *commands*
                 :key #'name :test #'string-equal)
      ;;site commands created for communities that are for normies
      (error 'missing-command)))

(command-defining-macro-no-luna new-normie-site-command
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

