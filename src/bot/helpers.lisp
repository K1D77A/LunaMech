(in-package #:matrix-moonbot)

(defun mapc-found-modules (luna fun &rest args)
  (mapc (lambda (mod)
          (apply fun luna mod args))
        (found-modules luna)))

(defmacro maybe-pmapc (luna fun seq)
  `(funcall (if (parallel-p ,luna) #'lparallel:pmapc #'mapc) ,fun ,seq))

(defmacro catch-limit-exceeded (() &body body)
  "Evaluates BODY like normal, if m-limit-exceeded is signalled this handles the
   condition by waiting the required time and then attempts to execute
   body again"
  (alexandria:with-gensyms (repeat tag)    
    `(let ((,repeat 3))
       (tagbody ,tag
          (handler-case
              (locally ,@body)
            (m-limit-exceeded (c)
              (let ((arg (api-error-args c)))
                (sleep (/ (+ 10 arg) 1000))
                (unless (zerop ,repeat)
                  (log:info "Retries: ~D" ,repeat)                  
                  (decf ,repeat)
                  (go ,tag)))))))))

(defmacro catch-potential-conditions (&body body)
  "Like catch-limit-exceeded except also attempts to handle api-no-connection by sleeping and
   performing a relog."
  (alexandria:with-gensyms (repeat tag)    
    `(let ((,repeat 3))
       (tagbody ,tag
          (handler-case
              (locally ,@body)
            (api-no-connection (c)
              (log:error "Api no connection: ~A~%Attempting to relog." c)
              (login *luna* t)
              (unless (zerop ,repeat)
                (sleep 5)
                (log:warn "Retries: ~D" ,repeat)
                (decf ,repeat)
                (go ,tag)))
            (m-limit-exceeded (c)
              (let ((arg (api-error-args c)))
                (sleep (/ (+ 10 arg) 1000))
                (unless (zerop ,repeat)
                  (log:warn "Retries: ~D" ,repeat)                  
                  (decf ,repeat)
                  (go ,tag)))))))))

(defun room->id-and-server (room-id)
  "Split a room id like abcd:im-a-server.com by : and return this as a list.
   id is the first, and the server is second"
  (str:split #\: room-id :omit-nulls t :limit 2))

(defun connection-for-room-id (room-id)
  "Attempt to find the connection in LUNA associated with ROOM-ID."
  (destructuring-bind (id server)
      (room->id-and-server room-id)
    (declare (ignore id))
    (find-if (lambda (ele)
               (str:ends-with-p server ele))
             (connections *luna*)
             :key #'url)))

(defun connection-for-user-id (user-id)
  (connection-for-room-id user-id))
                                         
(defun mapc-uber-rooms (function)
  "Mapc calling FUNCTION on (uber-rooms *luna*). FUNCTION must accept 2 args,
   the first is the connection associated with that room, the 2nd is the room-id."
  (mapc (lambda (uber-room)
          (let ((connection (connection-for-room-id uber-room)))
            (funcall function connection uber-room)))
        (uber-rooms *luna*)))

