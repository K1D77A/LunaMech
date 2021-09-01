(defpackage #:mm-module.luna
  (:use #:cl #:matrix-moonbot)
  (:export #:global-command
           #:global-module
           #:*module*))

(in-package #:mm-module.luna)

(defmodule luna (mm-module.luna LUNA normie-privilege)
           luna-command ()
           luna-module ())

(defmethod locate-command ((module luna-module) priv invoker community)
  "When prefix is 'luna with no privileges search for luna-command"
  (or (type-find 'luna-command invoker *commands*
                 :key #'name :test #'sym-name-equal)
      ;;luna commands created for communities that are for normies
      (error 'missing-command)))

(command-defining-macro-no-moonbot new-normie-luna-command
                                   'luna-command)

(new-normie-luna-command help ()
    "attempts an explanation of how commands work"
  (moonhelp 'luna-command community room))

(new-normie-luna-command apologize ()
    "Luna apologizes"
  (format t "My sincerest apologies."))

(new-normie-luna-command hi ()
    "Luna says Hi"
  (let* ((greetings '("Hello" "Hi" "Nice to see you!" "Good to see you!"
                      "How's it going?" "Hi, how's it going?" "Hi, how are you doing?"
                      "What's up?" "Sup" "Heyyy" "Hey, lovely to see you!"
                      "Alright, bud" "Hellow stranger!" "'Ello, gov'nor!"
                      "What's crackin?" "What's up buttercup?" "Howdy!"))
         (len (length greetings)))
    (format t "~A" (elt greetings (random len)))))

(new-normie-luna-command fuck ((arg))
    ""
  (format t "No fuck ~A" arg))

(new-normie-luna-command stickers ()
    ""
  (format t "https://lunamech.com/stickerpicker"))

(new-normie-luna-command site ()
    ""
  (format t "https://lunamech.com/"))

(new-normie-luna-command source ()
    ""
  (format t "My source code is here: https://github.com/K1D77A/LunaMech I am licensed under the GNU General Public License v3.0"))

(new-normie-luna-command modules ()
    "Returns the names of all the modules within Luna"
  (format t "Loaded modules: ~%~{.~:(~A~)~%~}~%Type 'MODULE help' for more information."
          (mapcar (lambda (module)
                    (symbol-name (matrix-moonbot::prefix module)))
                  (found-modules *luna*))))
