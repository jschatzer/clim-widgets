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

(defclass node () 
  ((sup :accessor sup :initarg :sup :documentation "superior, name")
   (inf :accessor inf :documentation "inferiors, list")
   (disp-inf :initform nil :initarg :disp-inf :accessor disp-inf :documentation "boolean")))

(defmethod toggle (n) (setf (disp-inf n) (not (disp-inf n))))
(defmethod node-name (n) n)
(defmethod inf (n) nil)
(defmethod item-ptype (n default) default)
(defmethod indentation (n) 1)

;;; ************************************************************
;;; icons and grid

(define-presentation-type icon (&optional pt))

(define-presentation-method accept ((type icon) s (view textual-view) &key)
  (accept pt :stream s :prompt nil))

(define-presentation-method present (object (type icon) s (view textual-view) &key)
  (disp-tree object ptype s (indentation object)))

(defvar *icon* 'plus) ; 'triangle
(defvar *grid* t)     ; nil
(setf climi::*border-default-padding* 0)

(defun triangle (s x y o)
  (flet ((triang (s) (draw-polygon* s '(0 0 1 2 2 0))))
    (with-translation (s x y) 
      (with-scaling (s 5) 
        (surrounding-output-with-border (s)
          (if o (triang s) (with-rotation (s (/ pi -2) (make-point 1 1)) (triang s))))))))

(defun plus (s x y o) (surrounding-output-with-border (s) (if o (princ "--" s) (princ "+" s))))

(defun draw-icon (s group)
  "Draw the opened/closed icon"
  (updating-output (s :cache-value (disp-inf group))
    (with-output-as-presentation (s group 'icon :single-box t)
      (let ((open-p (disp-inf group)))
        (multiple-value-bind (x y) (stream-cursor-position s)
          (funcall *icon* s x y open-p)
          (stream-set-cursor-position s x y))))))

;suppress the line in last child, recursively? <---
(defun grid (s i n)
  (let ((spc "     ") (bar "¦     ") (lin "¦---- "))
    (cond ((= i 1))
          ((= i 2) (princ spc s))
          (t (if *grid*
               (progn (princ spc s) (dotimes (x (- i 2)) (princ bar s)) (typecase n (node) (t (princ lin s))))
               (dotimes (x (- i 1)) (princ spc s)))))))

;;; ************************************************************
(defmethod disp-tree (group pt s indent)
  "This presents the name part of both groups and nongroups"
  ;(updating-output (s :unique-id group :cache-value group)
  (updating-output (s :cache-value group)
    (typecase group
      (node (present (node-name group) pt :stream s))
      (t (grid s indent group) (present (node-name group) pt :stream s)))
    (terpri s)))

(defmethod disp-tree :around ((group node) pt s indent)
  "This displays the icon and the group-contents of a group"
  (updating-output (s)
    (grid s indent group)
    (draw-icon s group) (stream-increment-cursor-position s 10 nil) (princ " - " s)
    (call-next-method)
    (when (disp-inf group)
      (let ((i (indentation group))
            (type (item-ptype group pt)))
        (dolist (child (inf group))
          (disp-tree child type s (+ indent i)))))))

;;;************************************************************
;;; A generic application for viewing a tree
(define-application-frame tree ()
  ((group :accessor group)
   (ptype :accessor ptype))
  (:panes (tree :application :display-function 'display))
  (:layouts (single tree)))

(defun display (f p)
  (with-end-of-line-action (p :allow)
    (with-end-of-page-action (p :allow)
      ;(updating-output (p :unique-id :top-level)
      (updating-output (p) 
        (disp-tree (group f) (ptype f) p (indentation (group f)))))))

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
   (tree :application :display-function 'display :incremental-redisplay t)
   (info :application :display-function 'disp-info :incremental-redisplay t))
	(:layouts (double (horizontally () tree (make-pane 'clim-extensions:box-adjuster-gadget) info))))

(defgeneric disp-info (f p))
;(defgeneric disp-info ((f tree-info) p)    ; ev so <---- ??

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
