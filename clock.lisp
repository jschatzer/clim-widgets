;;;; clock.lisp

(in-package #:clim-widgets)

(defun hand (f ink l w tr)
  (draw-line* f 200 200 200 l :ink ink :line-thickness w :transformation (make-rotation-transformation tr (make-point 200 200))))

#|
(defun clock ()
  (defparameter f (open-window-stream :label 'clock))
  (multiple-value-bind (s m h) (get-decoded-time)
    (let* ((is (/ pi 30)) (im (/ pi (* 30 60))) (ih (/ pi (* 30 60 6))) ;increment per second
           (as (* (/ pi 30) s)) (am (+ (* (/ pi 30) m) (/ (* (/ pi 30) s) 60))) (ah (+ (* (/ pi 6) (rem h 12)) (/ (* (/ pi 6) m) 60))) ;initial arc
           (xx (updating-output (f)
                 (draw-circle* f 200 200 100 :filled nil)
                 (dotimes (i 12) (draw-line* f 200 100 200 105 :transformation (make-rotation-transformation (* i (/ pi 6)) (make-point 200 200))))
                 (draw-text* f "0" 200 120 :align-x :center :align-y :bottom)
                 (draw-text* f "3" 280 200 :align-x :left :align-y :center)
                 (draw-text* f "6" 200 280 :align-x :center :align-y :top)
                 (draw-text* f "9" 120 200 :align-x :right :align-y :center)
                 (updating-output (f)
                   (hand f +red+   130 3 ah)
                   (hand f +green+ 110 2 am)
                   (hand f +black+ 100 1 as)))))
      (loop
        (sleep 1) 
        (setf as (+ as is) am (+ am im) ah (+ ah ih))
        (redisplay xx f)))))
|#

(defun clock ()
  (let ((f (open-window-stream :label 'clock)))
    (multiple-value-bind (s m h) (get-decoded-time)
      (let* ((is (/ pi 30)) (im (/ pi (* 30 60))) (ih (/ pi (* 30 60 6))) ;increment per second
             (as (* (/ pi 30) s)) (am (+ (* (/ pi 30) m) (/ (* (/ pi 30) s) 60))) (ah (+ (* (/ pi 6) (rem h 12)) (/ (* (/ pi 6) m) 60))) ;initial arc
             (xx (updating-output (f)
                   (draw-circle* f 200 200 100 :filled nil)
                   (dotimes (i 12) (draw-line* f 200 100 200 105 :transformation (make-rotation-transformation (* i (/ pi 6)) (make-point 200 200))))
                   (draw-text* f "0" 200 120 :align-x :center :align-y :bottom)
                   (draw-text* f "3" 280 200 :align-x :left :align-y :center)
                   (draw-text* f "6" 200 280 :align-x :center :align-y :top)
                   (draw-text* f "9" 120 200 :align-x :right :align-y :center)
                   (updating-output (f)
                     (hand f +red+   130 3 ah)
                     (hand f +green+ 110 2 am)
                     (hand f +black+ 100 1 as)))))
        (loop
          (sleep 1) 
          (setf as (+ as is) am (+ am im) ah (+ ah ih))
          (redisplay xx f))))))
