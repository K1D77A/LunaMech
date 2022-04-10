(in-package #:matrix-moonbot)

(defun mapc-found-modules (luna fun &rest args)
  (mapc (lambda (mod)
          (apply fun luna mod args))
        (found-modules luna)))
