(defpackage #:mm-module.opengraph
  (:use #:cl #:matrix-moonbot)
  (:export #:global-command
           #:global-module
           #:*module*))

(in-package #:mm-module.opengraph)

(defmodule opengraph (mm-module.opengraph og ubermensch-privilege)
           og-command ()
           og-module ())

(defmethod locate-command ((module og-module) priv invoker community)
  "When prefix is 'og with no privileges search for luna-command"
  (or (type-find 'og-command invoker *commands*
                 :key #'name :test #'string-equal)
      ;;luna commands created for communities that are for normies
      (error 'missing-command)))

(command-defining-macro-moonbot new-admin-command 'og-command)



(defun extract-url (text)
  (values (str:starts-with-p "http" text :ignore-case nil)
          (first (cl-ppcre:all-matches-as-strings "https?://\\S+" text))))

(defun extract-domain (url)
  (cl-ppcre:scan-to-strings "https?://([^/]+)" url))

(defparameter *domain->opengraph-getter* (make-hash-table :test #'equal))

(defclass opengraph-getter ()
  ((%domain
    :accessor domain
    :initarg :domain)
   (%url
    :accessor url
    :initarg :url)
   (%info
    :accessor info
    :initarg :info)))

(defclass oembed ()
  ((%oembed-url
    :accessor oembed-url
    :initarg :oembed-url
    :allocation :class)))


(defun map-domain-to-og-getter (class &rest domains)
  (let ((class (find-class class)))
    (mapc (lambda (domain)
            (setf (gethash domain *domain->opengraph-getter*)
                  class))
          domains)))

(defun get-opengraph-getter (domain url)
  (let ((class 
          (or (gethash domain *domain->opengraph-getter*)
              (find-class 'opengraph-getter))))
    (make-instance class :domain domain :url url)))

(defgeneric fetch-opengraph-info (opengraph-getter stream)
  (:method :around (og stream)
    (handler-case
        (call-next-method)
      (serious-condition (c)
        (log:error "Condition: ~S~A" c c)
        nil))))

(defmethod fetch-opengraph-info (o stream)
  nil)
    

(defgeneric extract-oembed-info (o parsed stream)
  (:method :around (o parsed stream)
    (handler-case
        (call-next-method)
      (serious-condition (c)
        (log:error "Condition in extract-oembed-info: ~S~A" c c)))))

(defmethod fetch-opengraph-info ((o oembed) stream)
  (let* ((fetched (dex:get (format nil (oembed-url o) (url o))))
         (parsed (jojo:parse fetched :as :hash-table)))
    (extract-oembed-info o parsed stream)))
  

#||

X

||#

(defclass x (opengraph-getter oembed)
  ((%oembed-url
    :initform "https://publish.twitter.com/oembed?url=~A")))

(map-domain-to-og-getter 'x 
                         "x.com"
                         "www.x.com"
                         "twitter.com"
                         "www.twitter.com"
                         "mobile.twitter.com"
                         "m.twitter.com")

(defparameter *message* nil)

(defmethod extract-oembed-info ((o x) parsed stream)  
  (let* ((html (gethash "html" parsed))
         (plumped (plump:parse html))
         (elements (plump-dom:get-elements-by-tag-name plumped "p"))
         (author (gethash "author_name" parsed)))   
    (when (listp elements)
      (setf *message* elements)
      (let* ((content (first elements))
             (children (plump-dom:children content)))
        (map nil (lambda (element)
                   (typecase element
                     (plump-dom:text-node (format stream "~A" (plump:text element)))
                     (plump-dom:element
                      (let ((tag (plump:tag-name element)))
                      (cond ((string= tag "br")
                             (format stream "~%"))
                            ((string= tag "a")
                             (format stream "~A" (plump:text element)))
                            (t nil))))
                     (otherwise nil)))                                      
             children)
        (format stream " -- ~A" author)))))


(defun extract-opengraph (dom)
  (let* ((nodes (clss:select "meta[property^='og:']" dom))
         (result (make-hash-table :test 'equal)))
    (map nil (lambda (node)
               (let ((prop (plump:attribute node "property"))
                     (content (plump:attribute node "content")))
                 (when (and prop content)
                   (setf (gethash prop result) content))))
         nodes)
    result))
#||

Youtube

||#

(defclass youtube (opengraph-getter oembed)
  ((%oembed-url
    :initform "https://www.youtube.com/oembed?url=~A")))


(map-domain-to-og-getter 'youtube
                         "www.youtube.com"
                         "youtube.com"
                         "youtu.be"
                         "youtube-nocookie.com"
                         "www.youtube-nocookie.com"
                         "music.youtube.com")


(defmethod extract-oembed-info ((o youtube) parsed stream)
  (let ((title (gethash "title" parsed))
        (author (gethash "author_name" parsed)))
    (when (and title author)
      (format stream "~A -- ~A" title author))))


(defmethod on-message (luna (module og-module) community room privilege message text)
  (multiple-value-bind (starts-with-http-p url)
      (extract-url text)
    (when url
      (log:info "Trying to get opengraph info for URL: ~S" url)
      (multiple-value-bind (with-https domain-vec)
          (extract-domain url)
        (declare (ignore with-https))
        (let ((domain? (aref domain-vec 0)))
          (when domain?
            (log:info "Domain: ~S" domain?)
            (let ((og-getter (get-opengraph-getter domain? url))
                  (event-id (gethash "event_id" message)))
              (if (and starts-with-http-p
                       (not (find #\Space text :test #'char=)))
                  (with-formatted-output-to-room (community room :reply-event-id event-id)
                    (fetch-opengraph-info og-getter *standard-output*))
                  (with-formatted-output-to-room (community room)
                    (fetch-opengraph-info og-getter *standard-output*))))))))))
