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
                        :documentation "A mapping of rooms to feeds."))
           :module-superclass (background-module))

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
  (when (rooms->feeds *module*);dont write empty results
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

(defmethod rss-room (name)
  (getf (rss-entry name) :room))

(defmethod (setf rss-room) (new-val name)
  (setf (getf (getf (rooms->feeds *module*) name) :room)
        new-val))

(defmethod (setf last-entry) (new-val name)
  (setf (getf (getf (rooms->feeds *module*) name) :last-entry)
        new-val))

(defun latest-published-date (entries)
  (reduce #'max (mapcar (lambda (entry)
                          (destructure-entry (entry)
                            published-parsed))
                        entries)))

(defun subscribe-to-rss (name feed room &key (count 10))
  (let ((last-entry nil))
    (destructure-rss ((feedparser:parse-feed (dex:get feed) :max-entries count))
      (destructure-entry ((first entries))
        (setf last-entry (latest-published-date entries))
        (dolist (entry (reverse entries))
          (destructure-entry (entry)
            (module-moonmat-message (conn *luna*) room
                                    "~A"
                                    (format-rss-entry entry)))))
      (new-rss name feed room last-entry))))

(defun format-rss-entry (entry)
  (destructure-entry (entry)
    (format nil "Title: ~A~%[Luna] Date: ~A~%[Luna] URL: ~A~%" title published link)))

(defun newer-entry-p (last-entry published)
  (< last-entry published))

(defun safe-dex (url)
  (handler-case 
      (let ((retry-request (dex:retry-request 1 :interval 1)))
        (handler-bind ((dex:http-request-failed retry-request))
          (dex:get url :use-connection-pool nil
                       :connect-timeout 3 :read-timeout 3)))
    (error (c)
      (log:error "Safe dex failed with URL: ~A" url)
      nil)))

(defun update-rss (name)
  (let ((last-entry (last-entry name)))
    (ignore-errors 
     (destructure-rss ((feedparser:parse-feed (safe-dex (rss-feed name))
                                              :max-entries 10))
       (let ((newer 
               (loop :for entry :in entries
                     :when (destructure-entry (entry)
                             (newer-entry-p last-entry published-parsed))
                       :collect entry)))
         (when newer
           (log:info "~D new articles for ~A" (length newer) name)
           (let ((latest (latest-published-date newer)))
             (setf (last-entry name) latest)
             (dolist (entry (reverse newer))
               (module-moonmat-message (conn *luna*) (rss-room name)
                                       "~A"
                                       (format-rss-entry entry))))))))))

(defmethod on-sync (luna (module rss-module) sync)
  (declare (ignore sync))
  (execute-stamp-n-after-luna ((find-timer (timers *module*) :check)
                               900
                               :position :before);15 minutes
    (let ((keys (loop :for key :in (rooms->feeds *module*) :by #'cddr :collect key)))
      (lparallel:pmapc
       (lambda (rss-name)
         (log:info "Checking for RSS updates for ~A" rss-name)
         (handler-case 
             (update-rss rss-name)
           (serious-condition (c)
             (report-condition-to-matrix c (format nil "Trying to update room ~A"
                                                   rss-name))
             (log:error "Error ~A when trying to update RSS for room ~A"
                        c (rss-room rss-name)))))
       keys))
    (log:info "Done checking for RSS.")))

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

(new-admin-rss-command subscribe ((name (:minlen 1)
                                        (:maxlen 50))
                                  (feed (:minlen 5)
                                        (:maxlen 100))
                                  (room-id (:minlen 1)
                                           (:maxlen 50)))
    "Subscribes to the RSS feed FEED (the url) under the name NAME (unique identifier), and publishes new updates into ROOM-ID. On subscription will post the latest 10 entries."
  (let ((name (intern (string-upcase name) :keyword)))
    (if (rss-feed name)
        (module-moonmat-message (conn *luna*) room "Already subscribed.")
        (subscribe-to-rss name feed (if (string-equal room-id "here")
                                        room
                                        room-id)))))

(new-admin-rss-command unsubscribe ((name (:minlen 1)
                                          (:maxlen 50)))
    "Unsubscribes from the RSS feed. This will remove its reference from Luna."
  (let ((name (intern (string-upcase name) :keyword)))
    (remf (rooms->feeds *module*) name))
  (module-moonmat-message (conn *luna*) room "Success."))

(new-admin-rss-command update-rss-room ((name (:minlen 1)
                                              (:maxlen 50))
                                        (new-room (:minlen 1)
                                                  (:maxlen 50)))
    "Changes the room that Luna publishes new RSS feed updates to NEW-ROOM."
  (let ((feed (rss-entry (intern (string-upcase name) :keyword))))
    (if feed
        (progn (setf (rss-room name) new-room)
               (module-moonmat-message (conn *luna*) room "Success."))
        (module-moonmat-message (conn *luna*) room
                                "Could not find entry by name: ~A" name))))

(new-admin-rss-command all-feeds ()
    "Returns the name of all the feeds."
  (loop :for key :in (rooms->feeds *module*) :by #'cddr
        :do (format t "~A~%" key)))





