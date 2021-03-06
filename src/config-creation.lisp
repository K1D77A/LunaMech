(defpackage #:mm.config-generator
  (:use #:cl #:matrix-moonbot)
  (:export #:gen-config))

(in-package #:mm.config-generator)

;;;;this file contains the code for helping a new user setup a brand new
;;;;config.

#|
A config is generated by creating individual functions that return each aspect of 
the config. Each function is then associated with a 'helper', this helper is called
whenever the function 'ask' receives "?" from the user, it will then call the
associated helper, and then recall the function that called it. Once all of the 
config functions have been the results have to be put into the template, this is done
using associations between the functions called that generated the entry and 
functions that turn that entry into something that the template can use. Finally
the template is outputted to the file ./generated-communities.lisp which the user
can then mv to ./config/communities.lisp to start Moonbot.
Doing it this way should make extending the config generation system pretty easy.
|#



(defparameter *all-modules* '(HUGINN COMPASS ADMIN LUNA DIRECT-MESSAGE JITSI))
;;;need another way to determine what all of the modules are.
(defparameter *help-funs* nil)
(defparameter *fun-to-list* nil)

(defun new-help (sym help-fun)
  (pushnew (list :caller sym :fun help-fun) *help-funs* :key #'second))

(defun new-fun-to-list (fun processor)
  (pushnew (list :fun fun :processor processor) *fun-to-list* :key #'second))

(defmacro new-help-fun (sym help-fun)
  `(new-help ,sym ,help-fun))

(defmacro new-fun->list (fun processor)
  `(new-fun-to-list ,fun ,processor))

(defun lookup-processor (fun)
  (pkv (find fun *fun-to-list* :key #'second) :processor))

(defmacro let-when (var-name form &body body)
  `(let ((,var-name ,form))
     (when ,var-name
       ,@body)))

(defun display-help (caller &rest args)
  (let-when fun (find caller *help-funs* :key #'second)
    (funcall (pkv fun :fun)))
  (apply caller args))

(defun out (control-string &rest args)
  (apply #'format *query-io* control-string args))

(defun ask (caller control-string &rest args)
  "Evaluate format with CONTROL-STRING, read in the users response, if the user 
responds with ? then execute the help function associated with CALLER. Otherwise 
returns the value entered by the user. CALLER should be a symbol representing the
name of the function that is calling ask so that CL can call it again."
  (apply #'format *query-io* control-string args)
  (let ((input (str:collapse-whitespaces (read-line *query-io*))))
    (if (string= input "?")
        (display-help caller)
        input)))

(defun ask1 (caller control-string &rest args)
  "Same as ask but splits by space and returns only the first. Useful if you only
want 1 argument."
  (let-when str (apply #'ask caller control-string args)
    (first (str:split " " str))))

(defun get-user-help ()
  (out "A user looks like @<name>:<url or part of url>~%")
  (out "An example would be @luna:luna-matrix.com~%"))

(new-help-fun 'get-user 'get-user-help)

(defun get-url-help ()
  (out "A URL looks like https://matrix.lunamatrix.com~%")
  (out "Make sure you include the 'http://'~%"))

(new-help-fun 'get-url 'get-url-help)

(defun get-url ()
  (let-when url (ask1 'get-url "Enter the URL of the server.~%")
    (and (str:containsp "http" url)
         (str:containsp "//" url)
         url)))

(defun valid-userp (str)
  (and (char= (aref str 0) #\@)
       (str:containsp ":" str)
       (str:containsp "." str)))

(defun get-ubermensch-help ()
  (funcall (pkv (find 'get-user *help-funs* :key #'second) :fun))
  (out "An Ubermensch looks the same as USER.~%An Ubermensch has total control of ")
  (out "the bot and you must define at least one.~%"))

(new-help-fun 'get-ubermensch 'get-ubermensch-help)

(defun get-ubermensch ()
  (let-when ubermensch (ask 'get-ubermensch "Enter a list of Ubermensch.~%")
    (setf ubermensch (str:split " " ubermensch :omit-nulls t))    
    (and (<= 1 (length ubermensch))
         (every #'valid-userp ubermensch)
         ubermensch)))

(defun get-community-name-help ()
  (out "The community name is given to the groups of rooms, users, admins and ~
        rooms listened in.~%It is also used for the prefix to the commands.~%~
        An example would be 'moon-community'~%The prefix for this community ~
        be '#.moon-community <command>'.~%"))

(new-help-fun 'get-community-name 'get-community-name-help)

(defun get-community-name ()
  (let-when
      community (ask1 'get-community-name "Enter a name for your community.~%~
                                          Dont include spaces.~%")
    (intern (string-upcase community) :keyword)))

(defun get-username-help ()
  (out "The username is the account name of the account you want Luna to use.~%"))

(new-help-fun 'get-username 'get-username-help)

(defun get-username ()
  (let-when username (ask1 'get-username "Enter the username used to login.~%")
    username))

(defun get-listen-in-help ()
  (out "Listen in is a room that the bot will read messages from and constantly ~
        check for new commands.~%"))

(new-help-fun 'get-listen-in 'get-listen-in-help)

(defun get-listen-in ()
  (let-when listen-in (ask1 'get-listen-in "Enter a single room id ~
                                            that the bot will listen in.~%")
    listen-in))

(defun get-listen-in-name-help ()
  (out "This name is just used for a shorthand when giving certain commands.~%")
  (out "When the bot is running room names will be resolved automatically, this ~
        is just to keep the config consistent for first run.~%"))

(new-help-fun 'get-listen-in-name 'get-listen-in-name-help)

(defun get-listen-in-name ()
  (let-when name (ask 'get-listen-in-name "Enter the name for the room you just ~
                                            added as your first listened in room~%")
    name))

(defun get-uber-room-help ()
  (out "The uber-room is the room that Luna will always listen in. Its used to turn Luna~%")
  (out "on and off."))

(defun get-uber-room ()
  (let-when uber-room (ask 'get-uber-room "Enter the room-id of your choice ~
                                           for uber-room~%")
    uber-room))

(new-help-fun 'get-uber-room 'get-uber-room-help)

(defun ubermensch->list (ubermensch)
  `(:UBERMENSCH ,ubermensch))

(new-fun->list 'get-ubermensch 'ubermensch->list)

(defun uber-room->list (uber-room)
  `(:UBER-ROOM ,uber-room))

(new-fun->list 'get-uber-room 'uber-room->list)

(defun username->list (username)
  `(:USERNAME ,username))

(new-fun->list 'get-username 'username->list)

(defun url->list (url)
  `(:URL ,url))

(new-fun->list 'get-url 'url->list)

(defun community-name->list (community-name)
  community-name)

(new-fun->list 'get-community-name 'community-name->list)

(defun listen-in->list (listen-in)
  `(:LISTEN-IN (,listen-in)))

(new-fun->list 'get-listen-in 'listen-in->list)

(defun get-listen-in-name->list (listen-in)
  listen-in)

(new-fun->list 'get-listen-in-name 'get-listen-in-name->list)

(defun %gen-config (list-of-functions)
  (format t "Welcome to the config generator.~%Type ? anytime for help.~%~%")
  (labels ((call-fun (fun)
             (let ((res (funcall fun)))
               (if res
                   (if (yes-or-no-p "Is this what you expect? ~S" res)
                       (list :fun fun :res res)
                       (call-fun fun))
                   (progn (out "Sorry that input is invalid~%")
                          (call-fun fun))))))
    (mapcar #'call-fun list-of-functions)))

(defun to-list (reslist)
  (funcall (lookup-processor (pkv reslist :fun)) (pkv reslist :res)))

(defun to-res (key results)
  (to-list (find key results :key #'second)))

(defun fill-config-template (results)
  (macrolet ((tr (sym)
               `(to-res ,sym results)))
    `((:GLOBAL-CONFIG
       ,(tr 'get-ubermensch)
       ,(tr 'get-uber-room)
       (:MODULES
        ,*all-modules*))
      (,(tr 'get-community-name)
       (,(tr 'get-url)
        (:API "/_matrix/client/r0/")
        ,(tr 'get-username)
        ,(tr 'get-listen-in)
        (:ADMINS ,(pkv (find 'get-ubermensch results :key #'second) :res))
        (:ROOMS
         ((:ID ,(pkv (find 'get-listen-in results :key #'second)
                     :res) :NAME ,(tr 'get-listen-in-name))))
        (:MEMBERS ,(pkv (find 'get-ubermensch results :key #'second) :res))
        (:EXTRA NIL))))))

(defun gen-config ()
  "Steps through a series of questions that are asked to the invoker. Each question 
is asked and then verified, the user can type ? at any time to get some information
regarding what is required of them. Once completed the results are used to fill in
a template communities.lisp file which is output to ./generated-communities.lisp. 
This file should be moved to ./config/communities.lisp to be used to start Luna.
It will either do that, error or return nil :P"
  (let-when conf (%gen-config '(get-ubermensch get-url get-uber-room get-community-name
                                get-username get-listen-in get-listen-in-name))
    (alexandria:write-string-into-file  (format nil "~S" (fill-config-template conf))
                                        "./generated-communities.lisp"
                                        :if-exists :overwrite
                                        :if-does-not-exist :create)))












