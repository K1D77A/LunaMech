(in-package #:matrix-moonbot)

(defmacro get-key (location key)
  `(gethash ,(string key) ,location))

(defun remove-if-not%hash (function hash-table)
  (loop :for val :being :the :hash-value :in hash-table
        :when (funcall function val)
          :collect val))
        

