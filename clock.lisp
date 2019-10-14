;;;; clock.lisp

;(in-package #:clim-widgets)
(in-package #:cw-clock)


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
;  (let ((f (open-window-stream :label 'clock)))
  (let ((f (open-window-stream :label "clock")))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 15.10.16 ---- eu design ----

;from clim-analog-clock, cac1.lisp
(defmacro uset (n) ; uset update-with-setf
  "dec 60, doz 72"  ; decimal/docenal
  `(progn
     (setf s (mod (1+ s) ,n))
     (when (zerop s) (setf m (mod (1+ m) ,n)))
     (when (and (zerop m) (zerop s)) (setf h (mod (1+ h) 24)))))

(defun tics (s n l)
  (dotimes (i n) (draw-line* s 200 100 200 l :transformation (make-rotation-transformation (* i (/ pi (/ n 2))) (make-point 200 200)))))

(defun face-dec-eu (s)
  (tics s 12 110))

(defun hand-s (s ink l w tr)
  (draw-line* s 200 110 200 l :ink ink :line-thickness w :transformation (make-rotation-transformation tr (make-point 200 200))))

(define-symbol-macro hand-s
  (updating-output (p)
    (hand-s p +red+ 115 5 as)))

(defun clock-eu% (l n f ts ) ; l label, n number-tics, f face, ts time-of-second, fd format-digital
  (let ((p (open-window-stream :label l)))
    (multiple-value-bind (s m h) (get-decoded-time) ; may be false about 1 mintute
      (let* ((is (/ pi (/ n 2))) (im (/ pi (* (/ n 2) n))) (ih (/ pi (* (/ n 2) n 6))) ;increment per second
             (as (* (/ pi 30) s)) (am (+ (* (/ pi 30) m) (/ (* (/ pi 30) s) 60))) (ah (+ (* (/ pi 6) (rem h 12)) (/ (* (/ pi 6) m) 60))) ;initial arc
             (xx (updating-output (p) (funcall f p) 
                                      hand-s
                                      (updating-output (p) (draw-text* p (format nil "~2,,,'0@a:~2,,,'0@a" h m) 200 200 :align-x :center)))))
        (loop (sleep ts) 
              (setf as (+ as is) am (+ am im) ah (+ ah ih))
              (uset n)
              (redisplay xx p))))))

(defun digital-clock-eu () 
  ;(clock-eu% 'clock-eu-design 60 'face-dec-eu 1))
  (clock-eu% "clock-eu-design" 60 'face-dec-eu 1))
