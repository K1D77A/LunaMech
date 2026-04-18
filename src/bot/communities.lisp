(in-package #:matrix-moonbot)

(defmethod initiate-room-spellchecker ((community community))
  (with-accessors ((rooms-spellcheck rooms-spellcheck)
                   (rooms rooms))
      community
    (spellcheck:train
     (mapcar (lambda (room-plist)
               (string-downcase (pkv room-plist :NAME)))
             rooms))))
