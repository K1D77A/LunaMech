(in-package #:matrix-moonbot)

(defun mapc-found-modules (luna fun &rest args)
  (mapc (lambda (mod)
          (apply fun luna mod args))
        (found-modules luna)))

(defmacro maybe-pmapc (luna fun seq)
  `(funcall (if (parallel-p ,luna) #'lparallel:pmapc #'mapc) ,fun ,seq))

