(in-package #:lunamech)

(defmacro get-key (location key)
  `(gethash ,(string key) ,location))

(defmacro pkv (location key)
  `(get-key ,location ,key))

(defun remove-if-not%hash (function hash-table)
  (loop :for val :being :the :hash-value :in hash-table
        :when (funcall function val)
          :collect val))

(defun make-locks (&rest names)
  (loop :for name :in names
        :appending (list name (sb-concurrency:make-frlock :name (format nil "~A" name)))))

        

