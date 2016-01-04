; tree-view.lisp

; =========================================================================================
; based on
; http://osdir.com/ml/mcclim-devel/2009-08/msg00010.html
; http://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/lisp/gui/clim/clim_2/browsers/
;
;;; Written by Jeff Morrill (jmorrill@bbn.com), December 92.
;;; It is provided "as is" without express or implied warranty.
;;; Thanks to Scott McKay for solving an incremental redisplay problem.
; =========================================================================================

(in-package clim-widgets)

;updating-output with/without :unique-id -- for performance?
;(defmethod print-object ((self directory-display) stream) (format stream "#<~A>" (node-name self))) ;überlegen 

;;; ************************************************************
;;; icons and grid

(defvar *icon* 'plus)     ; 'triangle 'triangle2
(defvar *grid* t)         ; nil
(defvar *dashed-lines* t) ;nil

(define-presentation-type icon ())
(define-presentation-method highlight-presentation ((type icon) r s st) :unhighlight)
(define-presentation-method present (o (type icon) s v &key) (disp-tree o ptype s (indentation o)))

;; general rectangle
(defun rect (s) (draw-rectangle* s 0 0 10 10 :filled nil) (draw-lines* s '(5 -2 5 -5   12 5 15 5)))
;; plus
(defun m% (s) (draw-line* s 2.5 5 7.5 5))    ; m is used in calendar <-----
(defun p (s) (m% s) (with-rotation (s (/ pi -2) (make-point 5 5)) (m% s)))
(defun plus (s x y o) (with-translation (s x y) (with-scaling (s 1) (rect s) (if o (m% s) (p s)))))
;; triangles
(defun tri-m (s) (draw-polygon* s '(0 0 10 0 5 10) :ink +black+)) ; so gehts
(defun tri-p (s) (draw-polygon* s '(0 0 10 5 0 10)))
(defun triangle  (s x y o) (with-translation (s x y) (with-scaling (s 1)          (if o (tri-m s) (tri-p s)))))
(defun triangle2 (s x y o) (with-translation (s x y) (with-scaling (s 1) (rect s) (if o (tri-m s) (tri-p s)))))

(defun draw-icon (s group)
  (with-output-as-presentation (s group 'icon)
    (let ((open-p (disp-inf group)))
      (multiple-value-bind (x y) (stream-cursor-position s)
        (funcall *icon* s x y open-p) (stream-set-cursor-position s (+ 20 x) y)))))

(defun spc (s) (stream-increment-cursor-position s 20 nil))
(defun bar (s) 
  (multiple-value-bind (x y) (stream-cursor-position s)
    (draw-line* s (+ x 5) (- y 5) (+ x 5) (+ y 15) :line-dashes *dashed-lines*) (stream-set-cursor-position s (+ 20 x) y)))      ; use line or text hight   <---
(defun lin (s) 
  (flet ((lin% (s) (draw-lines* s '(0 0 10 0   0 0 0 -10) :line-dashes *dashed-lines*)))
    (multiple-value-bind (x y) (stream-cursor-position s) 
      (with-translation (s (+ x 5) (+ y 8)) (with-scaling (s 1 1.5) (lin% s))) (stream-set-cursor-position s (+ 20 x) y))))

;suppress the line in last child, recursively? <---
(defun grid (s i n)
  (cond ((= i 1))
        ((= i 2) (spc s))
        (t (if *grid*
             (progn (spc s) (dotimes (x (- i 2)) (bar s)) (typecase n (node) (t (lin s))))
             (progn (spc s) (dotimes (x (- i 2)) (spc s)) (typecase n (node) (t (spc s))))))))

;;; ************************************************************
;;; tree display helper methods

(defclass node () 
  ((sup :accessor sup :initarg :sup :documentation "superior, name")
   (inf :accessor inf :documentation "inferiors, list")
   (disp-inf :initform nil :initarg :disp-inf :accessor disp-inf :documentation "boolean")))

(defmethod toggle (n) (setf (disp-inf n) (not (disp-inf n))))
(defmethod node-name (n) n)
(defmethod inf (n) nil)
(defmethod item-ptype (n default) default)
(defmethod indentation (n) 1)

(defmethod disp-tree (item pt s indent)
  "This presents the name of both nodes and leaves"
    (typecase item
      (node (present (node-name item) pt :stream s))
      (t (grid s indent item) (present (node-name item) pt :stream s))) (terpri s))

(defmethod disp-tree :around ((item node) pt s indent)
  "This displays the icon and the children of a node"
    (grid s indent item)
    (draw-icon s item)
    (call-next-method)
    (when (disp-inf item)
      (let ((i (indentation item))
            (type (item-ptype item pt)))
        (dolist (child (inf item))
          (disp-tree child type s (+ indent i))))))

;;;************************************************************
;;; A generic application for viewing a tree
(define-application-frame tree ()
  ((group :accessor group)
   (ptype :accessor ptype))
  (:pane (make-pane 'application-pane :display-function 'display-tree :incremental-redisplay t :end-of-line-action :allow :end-of-page-action :allow)))

(defmethod display-tree ((f tree) p) (disp-tree (group f) (ptype f) p (indentation (group f))))

(define-presentation-action toggle (icon command tree) (object window)
  (toggle object) (redisplay-frame-pane *application-frame* window))

(defun tree-view (gp pt &optional (frame 'tree) &key (left 0) (top 0) (right 400) (bottom 400) &allow-other-keys)
  (let ((f (make-application-frame frame :left left :top top :right right :bottom bottom)))
    (setf (group f) gp (ptype f) pt)
    (run-frame-top-level f)))

(defun tree-view (gp pt &optional (frame 'tree) &key (left 0) (top 0) (right 400) (bottom 400) (title "XXX") &allow-other-keys)
  (let ((f (make-application-frame frame :left left :top top :right right :bottom bottom :title title)))
    (setf (group f) gp (ptype f) pt)
    (run-frame-top-level f)))



;;;************************************************************
;;; helper functions to put tree nodes with their respective values into a hash-table for faster retreeving
(defparameter *nodes* (make-hash-table :test #'equal))

(defun t2h (tree)
  "tree to hash-table, key is a superior, val is a list of inferiors"
  (mapc (lambda (x)
          (cond ((and (atom (car x)) (null (cdr x))))
                (t (setf (gethash (car x) *nodes*) (mapcar #'car (cdr x))) (t2h (cdr x)))))
        tree))

(defmethod node-name ((n node)) (sup n))

(defmethod inf :around ((n node))
  (unless (slot-boundp n 'inf) 
    (setf (inf n) (mapcar (lambda (x) (if (gethash x *nodes*) (make-instance 'node :sup x) x)) (gethash (sup n) *nodes*))))
  (call-next-method))

;;;************************************************************
; form http://www.ic.unicamp.br/~meidanis/courses/problemas-lisp/L-99_Ninety-Nine_Lisp_Problems.html
(defmethod key ((s symbol)) (#~s'-.*''(symbol-name s)))
;(defmethod key ((s string)) (#~s'-.*'' s))

(defun pega (l)
  (cond ((null l) nil)
        ((null (cdr l)) l)
        ((equal (key (car l)) (key (cadr l))) (cons (car l) (pega (cdr l))))
        (t (list (car l)))))
(defun tira (l)
  (cond ((null l) nil)
        ((null (cdr l)) nil)
        ((equal (key (car l)) (key (cadr l))) (tira (cdr l)))
        (t (cdr l))))
(defun pack (l) (if (null l) nil (cons (pega l) (pack (tira l)))))
