(in-package #:matrix-moonbot)

(defun mapc-found-modules (luna fun &rest args)
  (mapc (lambda (mod)
          (apply fun luna mod args))
        (found-modules luna)))

(defmacro maybe-pmapc (luna fun seq)
  `(funcall (if (parallel-p ,luna) #'lparallel:pmapc #'mapc) ,fun ,seq))

(defmacro catch-limit-exceeded (() &body body)
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
