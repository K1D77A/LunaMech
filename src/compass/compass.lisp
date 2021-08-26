(defpackage #:compass
  (:use #:CL #:vecto)
  (:export #:draw-compass
           #:*results*
           #:save-results
           #:add-results
           #:results-from-file
           #:result-set-p))

(in-package #:compass)

(defvar *results* (make-hash-table :test #'equal))

(defun result-set-p (key)
  (multiple-value-bind (ignore val)
      (gethash key *results*)
    (declare (ignore ignore))
    val))

(defun results-to-list ()
  (let ((lst nil))
    (maphash (lambda (key val)
               (declare (ignore key))
               (push val lst))
             *results*)
    lst))

(defun add-results (x y id)
  (setf (gethash id *results*) (list :x x :y y :id id :time (get-universal-time))))

(defun save-results ()
  (unless (zerop (hash-table-count *results*));dont write empty results
    (alexandria:write-string-into-file
     (format nil "~S" (results-to-list)) "config/compass-results.lisp"
     :if-exists :overwrite
     :if-does-not-exist :create)))

(defun results-from-file ()
  (handler-case 
      (let ((form (uiop:safe-read-file-form "config/compass-results.lisp")))
        (mapc (lambda (lst)
                (setf (gethash (sixth lst) *results*) lst))
              form))
    (file-error ()
      (warn "config/compass-results.lisp does not exist.")
      nil)))

(defun range-mapper (input-min input-max output-min output-max)
  (let ((in-magnitude (- input-max input-min))
        (out-magnitude (- output-max output-min)))
    (lambda (value)
      (let ((norm
              (cond ((<= value input-min)
                     0.0)
                    ((<= input-max value)
                     1.0)
                    (t
                     (float (/ (- value input-min) in-magnitude))))))
        (+ output-min (* norm out-magnitude))))))

(defun draw-compass (stringp dots)
  (declare (optimize (speed 3) (safety 1)))
  (let* ((font nil)
         (height 740)
         (hhalf (/ height 2))
         (width 740)
         (whalf (/ width 2))
         (label-size 35)
         (dot-size 10)
         (dot-text 20)
         (border 70)
         (mapper (the function (range-mapper -100 100 border (- width border))))
         (half (/ (- width border) 2))
         (half-half-border (- half (/ border 2)))
         (cx (- width border))
         (cy (- height border)))
    (with-canvas (:width width :height height)
      (setf font (get-font "font.ttf"))
      (with-graphics-state
        (rectangle 0 0 width height)
        (fill-path))
      (set-rgba-fill 0 250 0 0.35);;green    
      (rectangle border border half-half-border half-half-border)
      (fill-path)
      (rectangle (+ (/ border 2) half) border
                 half-half-border half-half-border)
      (set-rgba-fill 250 250 0 0.35);;yellow
      (fill-path)
      (rectangle border hhalf half-half-border half-half-border)
      (set-rgba-fill 250 0 0 0.35);;red
      (fill-path)
      (rectangle hhalf whalf half-half-border half-half-border)
      (set-rgba-fill 0 0 250 0.35);;blue
      (fill-path)
      (with-graphics-state 
        (set-line-width 2)
        (set-rgba-stroke 255 255 255 0.15)
        (loop :for x :from border :to cx :by (/ (- width border border) 20)
              :do (move-to x border)
                  (line-to x cy)
                  (move-to border x)
                  (line-to cx x))
        (stroke))    
      (with-graphics-state
        (set-line-width 5)
        (move-to whalf border)
        (line-to whalf cy)
        (move-to border hhalf)
        (line-to cx hhalf)
        (stroke))
      (set-font font label-size)
      (with-graphics-state
        (set-rgb-fill 255 255 255)
        (draw-centered-string (/ border 2) (- hhalf (/ label-size 2)) "L")
        (draw-centered-string whalf (- border label-size) "Libertarian")
        (draw-centered-string whalf (+ (/ label-size 2) cy) "Authoritarian")
        (draw-centered-string (+ cx label-size)
                              (- hhalf (/ label-size 2))
                              "R"))
      (with-graphics-state
        (set-rgb-fill 255 255 255)
        (set-font font dot-text)
        (loop :for (x y string) :in dots
              :do (let* ((bounds (string-bounding-box string dot-text font))
                         (ysize (aref bounds 3))
                         (xsize (aref bounds 2))
                         (nx (funcall mapper x))
                         (ny (funcall mapper y)))
                    (centered-circle-path nx ny (+ dot-size 2))
                    (when stringp
                      (draw-string
                       (if (> x 0)
                           (- nx xsize dot-size 5)
                           (+ nx (+ dot-size 5)))
                       (- ny (/ ysize 4))
                       string))
                    (set-rgba-fill 0 0 0 1)
                    (fill-path)
                    (stroke)
                    (centered-circle-path nx ny dot-size)
                    (set-rgba-fill 250 0 0 1)
                    (fill-path)
                    ))
        (stroke))
      (let ((stream (flexi-streams:make-in-memory-output-stream)))
        (save-png-stream stream)
        stream))))
