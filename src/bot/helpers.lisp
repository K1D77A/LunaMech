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

