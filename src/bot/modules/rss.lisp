(defpackage #:mm-module.rss
  (:use #:cl #:matrix-moonbot)
  (:export #:global-command
           #:global-module
           #:*module*))

(in-package #:mm-module.rss)

(defmodule rss (mm-module.rss RSS ubermensch-privilege)
           rss-command ()
           rss-module ((rooms->feeds
                        :accessor rooms->feeds
                        :initform nil
                        :documentation "A mapping of rooms to feeds.")))

(defmethod locate-command ((module rss-module) priv invoker community)
  "When prefix is 'luna with no privileges search for luna-command"
  (or (type-find 'rss-command invoker *commands*
                 :key #'name :test #'sym-name-equal)      
      (error 'missing-command)))

(defmethod execute-command ((moonbot moonbot) (priv ubermensch-privilege)
                            (command rss-command)
                            community room message rest)
  (if (string-equal (first rest) "help")
      (print-command-information command community room)
      (safe-execution command community room message rest)))

(command-defining-macro-no-moonbot new-admin-rss-command
                                   'rss-command)

(new-admin-rss-command help ()
    "attempts an explanation of how commands work"
  (moonhelp 'rss-command community room))

(defmacro destructure-rss ((rss) &body body)
  `(let* ((entries (gethash :entries ,rss))
          (title (gethash :title ,rss))
          (sub (gethash :subtitle ,rss))
          (link (gethash :link ,rss)))
     (locally ,@body)))

(defmacro destructure-entry ((entry) &body body)
  `(let* ((author (gethash :author ,entry))
          (author-detail (gethash :author-detail ,entry))
          (id (gethash :id ,entry))
          (link (gethash :link ,entry))
          (published (gethash :published ,entry))
          (published-parsed (gethash :published-parsed ,entry))
          (summary (gethash :summary ,entry))
          (summary-detail (gethash :summary-detail ,entry))
          (title (gethash :title ,entry)))
     (locally ,@body)))

(new-admin-rss-command get-information ((url (:minlen 10)))
    "reads the entries from a url"
  (destructure-rss ((feedparser:parse-feed (dex:get url)))
    (format t "Title: ~A~%Subtitle: ~A~%URL: ~A~%Count ~D~%" title sub link
            (length entries))))

(new-admin-rss-command get-entry ((url (:minlen 10))
                                  (n))
    "returns entry n from URL"
  (destructure-rss ((feedparser:parse-feed (dex:get url)))
    (let ((entry (elt entries (parse-integer n :junk-allowed t))))
      (destructure-entry (entry)
        (format t "Title: ~A~%Link: ~A~%" link title)))))

(new-admin-rss-command all-entries ((url (:minlen 10)))
    "returns entry n from URL"
  (destructure-rss ((feedparser:parse-feed (dex:get url)))
    (dolist (entry entries)
      (destructure-entry (entry)
        (format t "Title: ~A~%Link: ~A~%Date: ~A~%"
                title link published)))))


