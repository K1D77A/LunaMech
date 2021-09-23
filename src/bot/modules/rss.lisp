(defpackage #:mm-module.rss
  (:use #:cl #:matrix-moonbot)
  (:export #:global-command
           #:global-module
           #:*module*))

(in-package #:mm-module.rss)

(defmodule rss (mm-module.rss RSS ubermensch-privilege)
           rss-command ()
           rss-module ((timers
                        :accessor timers
                        :initform (make-timers '(:check))
                        :documentation "a timer")
                       (rooms->feeds
                        :accessor rooms->feeds
                        :initform nil
                        :documentation "A mapping of rooms to feeds.")))

(defmethod locate-command ((module rss-module) priv invoker community)
  "When prefix is 'luna with no privileges search for luna-command"
  (or (type-find 'rss-command invoker *commands*
                 :key #'name :test #'string-equal)
      (error 'missing-command)))

(defmethod execute-command ((moonbot moonbot) (priv ubermensch-privilege)
                            (command rss-command)
                            community room message rest)
  (if (string-equal (first rest) "help")
      (print-command-information command community room)
      (safe-execution command community room message rest)))

;;;few more module specific commands for persistent configs
(defun save-results ()
  (when (rooms *module*);dont write empty results
    (alexandria:write-string-into-file
     (format nil "~S" (rooms->feeds *module*)) "config/rss-config.lisp"
     :if-does-not-exist :create
     :if-exists :supersede)))

(defun results-from-file ()
  (handler-case
      (let ((form (uiop:safe-read-file-form "config/rss-config.lisp")))
        (setf (rooms->feeds *module*) form))
    (file-error ()
      (warn "config/rss-config.lisp does not exist.")
      nil)))

(defmethod on-load-up (moonbot (module rss-module))
  (log:info "Loading rss rooms from rss-config.lisp")
  (results-from-file))

(defmethod on-save (moonbot (module rss-module))
  (log:info "Saving rss rooms to rss-config.lisp")
  (save-results)
  t)

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

(defun new-rss (name feed room last-entry)
  (setf (getf (rooms->feeds *module*) name)
        (list :feed feed :room room :last-entry last-entry)))

(defmethod last-entry (name)
  (getf (getf (rooms->feeds *module*) name) :last-entry))

(defun rss-entry (name)
  (getf (rooms->feeds *module*) name))

(defun rss-feed (name)
  (getf (rss-entry name) :feed))

(defun rss-room (name)
  (getf (rss-entry name) :room))

(defmethod (setf last-entry) (new-val name)
  (setf (getf (getf (rooms->feeds *module*) name) :last-entry)
        new-val))

(defun update-last-entry (name last-entry)
  (setf (getf (getf (rooms->feeds *module*) :last-entry) name)
        last-entry))

(defun subscribe-to-rss (name feed room &key (count 10))
  (let ((last-entry nil))
    (destructure-rss ((feedparser:parse-feed (dex:get feed) :max-entries count))
      (destructure-entry ((first entries))
        (setf last-entry published-parsed))
      (dolist (entry entries)
        (destructure-entry (entry)
          (module-moonmat-message (conn *luna*) room
                                  (format-rss-entry entry)))))
    (new-rss name feed room last-entry)))

(defun format-rss-entry (entry)
  (destructure-entry (entry)
    (format nil "Title: ~A~%Link: ~A~%Date: ~A~%" title link published)))

(defun update-rss (name)
  (let ((last-entry (last-entry name)))
    (destructure-rss ((feedparser:parse-feed (dex:get (rss-feed name)
                                                      :use-connection-pool nil)
                                             :max-entries 10))
      (dolist (entry (reverse entries))
        (destructure-entry (entry)
          (when (< last-entry published-parsed)
            (setf (last-entry name) published-parsed)
            (module-moonmat-message (conn *luna*) (rss-room name)
                                    (format-rss-entry entry))))))))

(defmethod on-sync (luna (module rss-module) sync)
  (declare (ignore sync))
  (execute-stamp-n-after-luna ((find-timer (timers *module*) :check)
                               600);600 seconds
    (alexandria:doplist (rss-name rss-feed (rooms->feeds *module*))
      (log:info "Checking for RSS updates for ~A" rss-name)
      (handler-case 
          (update-rss rss-name)
        (error (c)
          (report-condition-to-matrix c (format nil "Trying to update room ~A"
                                                rss-name))
          (log:error "Error ~A when trying to update RSS for room ~A"
                     c (rss-room rss-name)))))))

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

(new-admin-rss-command subscribe-to-rss ((name (:minlen 1)
                                               (:maxlen 50))
                                         (feed (:minlen 5)
                                               (:maxlen 100))
                                         (room-id (:minlen 1)
                                                  (:maxlen 50)))
    "Subscribes to the RSS feed FEED (the url) under the name NAME (unique identifier), and publishes new updates into ROOM-ID. On subscription will post the latest 10 entries."
  (let ((name (intern (string-upcase name) :keyword)))
    (subscribe-to-rss name feed room)
    (module-moonmat-message (conn *luna*) room "Success.")))




