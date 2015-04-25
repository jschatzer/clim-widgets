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
; f frame, p pane, s stream string, pt presentation-type, n node, gp group
; use leaf and node  <-----
;updating-output with/without :unique-id -- for performance?
;(defmethod print-object ((self directory-display) stream) (format stream "#<~A>" (node-name self))) ;überlegen 

;;; ************************************************************
;;; icons and grid

(define-presentation-type icon ())
(define-presentation-method highlight-presentation ((type icon) r s st) :unhighlight)
(define-presentation-method present (o (type icon) s v &key) (disp-tree o ptype s (indentation o)))  ; anschauen

(defvar *icon* 'plus) ; 'triangle 'triangle2
(defvar *grid* t)     ; nil

;general rectangle
(defun rect (s) (draw-rectangle* s 0 0 10 10 :filled nil) (draw-lines* s '(5 0 5 -5   10 5 15 5)))
;plus
(defun m% (s) (draw-line* s 2.5 5 7.5 5))    ; m is used in calendar <-----
(defun p (s) (m% s) (with-rotation (s (/ pi -2) (make-point 5 5)) (m% s)))
(defun plus (s x y o) (with-translation (s x y) (with-scaling (s 1) (rect s) (if o (m% s) (p s)))))
;triangles
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
    (draw-line* s (+ x 5) (- y 5) (+ x 5) (+ y 15)) (stream-set-cursor-position s (+ 20 x) y)))      ; use line or text hight   <---
(defun lin (s) 
  (flet ((lin% (s) (draw-lines* s '(0 0 10 0   0 0 0 -10))))
    (multiple-value-bind (x y) (stream-cursor-position s) 
      (with-translation (s (+ x 5) (+ y 8)) (with-scaling (s 1 1.5) (lin% s))) (stream-set-cursor-position s (+ 20 x) y))))

;suppress the line in last child, recursively? <---
(defun grid (s i n)
  (cond ((= i 1))
        ((= i 2) (spc s))
        (t (if *grid*
             (progn (spc s) (dotimes (x (- i 2)) (bar s)) (typecase n (node) (t (lin s))))
             (dotimes (x (- i 1)) (spc s))))))

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

(defmethod disp-tree (group pt s indent)
  "This presents the name part of both groups and nongroups"
    (typecase group
      (node (present (node-name group) pt :stream s))
      (t (grid s indent group) (present (node-name group) pt :stream s))) (terpri s))

(defmethod disp-tree :around ((group node) pt s indent)
  "This displays the icon and the group-contents of a group"
    (grid s indent group)
    (draw-icon s group)
    (call-next-method)
    (when (disp-inf group)
      (let ((i (indentation group))
            (type (item-ptype group pt)))
        (dolist (child (inf group))
          (disp-tree child type s (+ indent i))))))

;;;************************************************************
;;; A generic application for viewing a tree
(define-application-frame tree ()
  ((group :accessor group)
   (ptype :accessor ptype))
  (:pane (make-pane 'application-pane :display-function 'display :incremental-redisplay t :end-of-line-action :allow :end-of-page-action :allow)))

(defun display (f p) (disp-tree (group f) (ptype f) p (indentation (group f))))

(define-presentation-action toggle (icon command tree) (object window)
  (toggle object) (redisplay-frame-pane *application-frame* window))

(defun tree-view (gp pt)
  (let ((f (make-application-frame 'tree :left 0 :top 0 :right 400 :bottom 400)))
    (setf (group f) gp (ptype f) pt)
    (run-frame-top-level f)))

;;;************************************************************
;;; Generic 2-pane-application for viewing a tree with info
(define-application-frame tree-info (tree)
 ((info :accessor info :initform ""))
  (:command-table (tree-info :inherit-from (tree)))
  (:panes 
   (tree :application :display-function 'display :incremental-redisplay t :end-of-line-action :allow :end-of-page-action :allow)
   (info :application :display-function 'disp-info :incremental-redisplay t))
	(:layouts (double (horizontally () tree (make-pane 'clim-extensions:box-adjuster-gadget) info))))

(defgeneric disp-info (f p))

(defun tree-info-view (group ptype)
  (let ((f (make-application-frame 'tree-info :left 0 :top 0 :right 800 :bottom 400)))
    (setf (group f) group (ptype f) ptype)
    (run-frame-top-level f)))

;;;************************************************************
;;; tree to hash-table
; key is a superior-name STRING
; val is a LIST of inferio-names 
(defparameter *nodes* (make-hash-table :test #'equal))

(defun defnode (sup inf)
  (setf (gethash sup *nodes*) inf))

(defun t2h (tree)
  "tree to hash-table"
  (mapc (lambda (x)
          (cond ((and (atom (car x)) (null (cdr x))) (defnode (car x) nil))
                ((and (atom (car x)) (cdr x)) (defnode (car x) (mapcar #'car (cdr x))) (t2h (cdr x)))
                (t (t2h x))))
        tree))
