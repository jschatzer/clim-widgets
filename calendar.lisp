;;;; calendar.lisp
;;; trying incremental redisplay, switch layout not working, hoping to get some help
;;; :incremental-redisplay nil  <-- 

(in-package #:clim-widgets)
;--------------------------------------
; app-logic and  helper
;--------------------------------------
(defparameter days '("Su" "Mo" "Tu" "We" "Th" "Fr" "Sa"))
(defmacro s () `(princ " " p))

(defun start (m y)
  "returns the start-date, i.e. the first of 7x6=42 dates, as an instance
  e.g #<SIMPLE-DATE-TIME:DATE-TIME 2014-07-27 00:00:00.000 {100654EAF3}>"
  (dt:make-date y (1- m) 
    (- (local-time:days-in-month (1- m) y)
       (nth-value 6 (decode-universal-time (encode-universal-time 0 0 0 1 m y))))))

(defmacro disp-day-nr ()
  `(formatting-cell (p :align-x :right)
    (cond ((dt:date= (dt:today) (dt:day+ (start month year) i)) (with-text-face (p :bold) (present (dt:day+ (start month year) i) 'date :stream p)))
          ((/= month (dt:month-of (dt:day+ (start month year) i))) (with-drawing-options (p :ink +gray+ :text-style (make-text-style nil :italic :smaller)) (present (dt:day+ (start month year) i) 'date :stream p)))
          (t (present (dt:day+ (start month year) i) 'date :stream p)))))

(defmacro disp-fn (rc)
  `(with-slots (month year) *application-frame*
     (present #\< 'change-month :stream p) (princ (svref local-time:+short-month-names+ month) p) (present #\> 'change-month :stream p) (s) 
     (present #\< 'change-year :stream p) (princ year p) (present #\> 'change-year :stream p) (s)
     (present "Today" 'string :stream p) (s)
     (present #\x 'character :stream p) (terpri p)
     (formatting-table (p :equalize-column-widths t)
       (,rc (p) (dolist (d days) (formatting-cell (p) (princ d p))))
       (let ((i 0)) (dotimes (w 6) (,rc (p) (dotimes (d 7) (disp-day-nr) (incf i))))))))

(defmacro mp (d-fn)
  `(make-pane 'application-pane :width :compute :height :compute :borders nil :scroll-bars nil :text-style '(:fix nil nil) :display-function ',d-fn :incremental-redisplay nil))
;--------------------------------------
; present-types
;--------------------------------------
(define-presentation-type change-month () :inherit-from 'character)
(define-presentation-type change-year () :inherit-from 'character)
(define-presentation-type date ())
(define-presentation-method present (d (type date) s v &key) (format s "~A" (dt:day-of d)))
;--------------------------------------
; gui
;--------------------------------------
(define-application-frame cal () 
    ((day :accessor d)
     (month :accessor m :initarg :m)
     (year :accessor y :initarg :y))
    (:panes (cal1 (mp layout1)) (cal2 (mp layout2)))
    (:layouts (c1 cal1) (c2 cal2)))

(defun layout1 (f p) (disp-fn formatting-row))
(defun layout2 (f p) (disp-fn formatting-column))
;--------------------------------------
; commands
;--------------------------------------
(define-cal-command switch-layout ((c 'character :gesture :select))
  (with-accessors ((layout frame-current-layout)) *application-frame*
    (setf layout (if (eq layout 'c1) 'c2 'c1))))

(define-cal-command change-y ((c 'change-year :gesture :select))
  (with-application-frame (f)
    (if (char= c #\<) (progn (decf (y f)) (redisplay-frame-panes f)) (progn (incf (y f)) (redisplay-frame-panes f)))))

(define-cal-command change-m ((c 'change-month :gesture :select))
  (with-application-frame (f)
    (if (char= c #\<)
      (typecase (m f) ((integer 2 12) (decf (m f)) (redisplay-frame-panes f)) (t (setf (m f) 12) (decf (y f)) (redisplay-frame-panes f)))
      (typecase (m f) ((integer 1 11) (incf (m f)) (redisplay-frame-panes f)) (t (setf (m f)  1) (incf (y f)) (redisplay-frame-panes f))))))

(define-cal-command get-date ((date 'date :gesture :select))
  (with-application-frame (f)
    (with-accessors ((da d) (ma m) (ya y)) f (setf da (dt:day-of date) ma (dt:month-of date) ya (dt:year-of date)))
    (frame-exit f)))

(define-cal-command todays-date ((c 'string :gesture :select))
  (with-application-frame (f)
    (with-accessors ((da d) (ma m) (ya y)) f (let ((now (dt:now))) (setf da (dt:day-of now) ma (dt:month-of now) ya (dt:year-of now))))
    (frame-exit f)))
;--------------------------------------
; main
;-------------------------------------
(defun calendar (&optional (month (dt:month-of (dt:now))) (year (dt:year-of (dt:now))))
  (let ((frame (make-application-frame 'cal :m month :y year)))
    (run-frame-top-level frame)
    (if (slot-boundp frame 'day) (format nil "~a-~a-~a" (y frame) (m frame) (d frame)))))
