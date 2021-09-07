(defpackage #:mm-module.compass
  (:use #:cl #:matrix-moonbot)
  (:export #:compass-command
           #:compass-module
           #:*module*))

(in-package #:mm-module.compass)

(defmodule compass (mm-module.compass COMPASS normie-privilege)
           compass-command ()
           compass-module ())

(defmethod on-load-up (moonbot (module compass-module))
  (log:info "Loading compass results from compass-results.lisp")
  (compass:results-from-file))

(defmethod on-save (moonbot (module COMPASS-MODULE))
  (log:info "Saving compass results to compass-results.lisp")
  (compass:save-results)
  t)

(defmethod locate-command ((module compass-module) priv invoker community)
  "When prefix is 'compass with no privileges search for compass-command"
  (or (type-find 'compass-command invoker *commands*
                 :key #'name :test #'string-equal)
      ;;compass commands created for communities that are for normies
      (error 'missing-command)))

(command-defining-macro-no-moonbot new-compass-command 'compass-command)

(new-compass-command help ()
    "attempts an explanation of how commands work"
  (moonhelp 'compass-command community room))

(new-compass-command count ()
    "tells the user how many entries there are"
  (format t "There are ~r unique compass entr~@:p"
          (hash-table-count compass:*results*)))

(new-compass-command plot-xy ((x :valid-compass) (y :valid-compass))
    "Plots x and y on the compass"
  (let* ((parsedx (parse-integer x))
         (parsedy (parse-integer y))
         (name (first (str:split ":" (pkv message :|sender|) :limit 2)))
         (str (compass:draw-compass t `((,parsedx ,parsedy ,name))))
         (vec (flexi-streams:get-output-stream-sequence str)))
    (send-image-bytes-to-room (connection community) room
                              "compass.png" "image/png" vec)))

(new-compass-command add ((x :valid-compass)(y :valid-compass))
    "Adds your X Y results to the bot. These are the default spekr.org results. Don't divide
by 10."
  (let* ((id (pkv message :|sender|))
         (parsedx (parse-integer x))
         (parsedy (parse-integer y))
         (name (first (str:split ":" id :limit 2)))
         (str (compass:draw-compass t `((,parsedx ,parsedy ,name))))
         (vec (flexi-streams:get-output-stream-sequence str)))
    (compass:add-results parsedx parsedy id)
    (send-image-bytes-to-room (connection community) room
                              "compass.png" "image/png" vec)))

(new-compass-command self ()
    "Receive your own result."
  (let* ((id (pkv message :|sender|))
         (res (gethash id compass:*results*)))
    (if res
        (let* ((x (pkv res :x))
               (y (pkv res :y))
               (name (first (str:split ":" id :limit 2)))
               (str (compass:draw-compass t `((,x ,y ,name))))
               (vec (flexi-streams:get-output-stream-sequence str)))
          (send-image-bytes-to-room (connection community) room
                                    "compass.png" "image/png" vec))
        (format t "Sorry but I couldn't find results for: ~A.~%" id))))

(new-compass-command id ((user-id :valid-user))
    "Receive a result by id"
  (let ((res (gethash user-id compass:*results*)))
    (if res
        (let* ((x (pkv res :x))
               (y (pkv res :y))
               (name (first (str:split ":" user-id :limit 2)))
               (str (compass:draw-compass t `((,x ,y ,name))))
               (vec (flexi-streams:get-output-stream-sequence str)))
          (send-image-bytes-to-room (connection community) room
                                    "compass.png" "image/png" vec))
        (format t "Couldn't find results for: ~A.~%" (subseq user-id 1)))))

(new-compass-command all ()
    "See all results plotted"
  (let ((list nil))
    (maphash (lambda (key val)
               (let ((x (pkv val :x))
                     (y (pkv val :y)))
                 (push (list x y (first (str:split ":" key :limit 2))) list)))
             compass:*results*)
    (let* ((str (compass:draw-compass nil list))
           (vec (flexi-streams:get-output-stream-sequence str)))
      (send-image-bytes-to-room (connection community) room
                                "compass.png" "image/png" vec))))


(new-compass-command test ()
    "The URL for the test"
  (format t "You can find the test at spekr.org"))
