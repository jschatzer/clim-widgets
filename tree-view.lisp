; =========================================================================================
; based on:
; http://osdir.com/ml/mcclim-devel/2009-08/msg00010.html
; http://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/lisp/gui/clim/clim_2/browsers/
;
;;; Written by Jeff Morrill (jmorrill@bbn.com), December 92.
;;; It is provided "as is" without express or implied warranty.
;;; Thanks to Scott McKay for solving an incremental redisplay problem.
; =========================================================================================

(in-package :clim-widgets)
; f frame, p pane, s stream, pt presentation-type, gp group
; use leaf and node  <-----
;updating-output with/without :unique-id -- for performance?

(defvar icon 'plus) ; 'triangle
(defvar *bar* t)    ; nil           ev rename grid?

(defclass node () ((display-contents :initform nil :initarg :display-contents :accessor display-contents)))
(defmethod node-name (n) n)
(defmethod node-contents (n) nil) ; ev node-content   singular?
(defmethod item-ptype (n default) default)
(defmethod indentation (n) 1)
(defmethod toggle (n) (setf (display-contents n) (not (display-contents n))))

;suppress the line in last child, recursively? <---
(defun insert-bars (s i)  ; rename grid?
  (let ((spc "     ")
        (bar "¦     "))
    (cond ((= i 1))
          ((= i 2) (princ spc s))
          (t (if *bar*
            (progn (princ spc s) (dotimes (x (- i 2)) (princ bar s)))
            (dotimes (x (- i 1)) (princ spc s)))))))

(defmethod disp-tree (group pt s indent)
  "This presents the name part of both groups and nongroups"
  ;(updating-output (s :unique-id group :cache-value group)
  (updating-output (s :cache-value group)
    (typecase group
      (node (present (node-name group) pt :stream s))
;      (t (insert-bars s indent) (princ "¦---- " s) (present (node-name group) pt :stream s)))
      (t (insert-bars s indent) (if *bar* (princ "¦---- " s) (princ "      " s)) (present (node-name group) pt :stream s)))  ;ev inclue if in insert-bar
    (terpri s)))

(defmethod disp-tree :around ((group node) pt s indent)
  "This displays the icon and the group-contents of a group"
  (updating-output (s)
    (insert-bars s indent)
    (draw-icon s icon group) (stream-increment-cursor-position s 10 nil) (princ " - " s)
    (call-next-method)
    (when (display-contents group)
      (let ((i (indentation group))
            (type (item-ptype group pt)))
        (dolist (child (node-contents group))
          (disp-tree child type s (+ indent i)))))))

;;; ************************************************************
;;; Icons
(setf climi::*border-default-padding* 0)

(defun triangle% (s &optional o) 
  (flet ((triang (s) (surrounding-output-with-border (s) (draw-polygon* s '(0 0 1 2 2 0)))))
  (if o
    (triang s)
    (with-rotation (s (/ pi -2) (make-point 1 1)) (triang s)))))

(defun triangle (s x y o)
  (with-translation (s x y) 
    (with-scaling (s 5) 
      (if o (triangle% s t) (triangle% s)))))

(defun plus% (s &optional o) (surrounding-output-with-border (s) (if o (princ "--" s) (princ "+" s))))
(defun plus (s x y o) (if o (plus% s t) (plus% s)))

(defun draw-icon (s icon group)
  "Draw the opened/closed icon"
  (updating-output (s :cache-value (display-contents group))
    (with-output-as-presentation (s group 'icon :single-box t)
      (let ((open-p (display-contents group)))
        (multiple-value-bind (x y) (stream-cursor-position s)
          (funcall icon s x y open-p)
          (stream-set-cursor-position s x y))))))        ;; ev use advance-cursuor <--------

;;; ************************************************************
;;; A presentation type and an action for open/close operations.

(define-command-table tree)

(define-presentation-type icon (&optional pt))

(define-presentation-method accept ((type icon) s (view textual-view) &key)
  (accept pt :stream s :prompt nil))

(define-presentation-method present (object (type icon) s (view textual-view) &key)
  (disp-tree object ptype s (indentation object)))

(define-presentation-action toggle (icon command tree :gesture :select) (object window)
  (toggle object) (redisplay-frame-pane *application-frame* window))

;;;************************************************************
;;; A generic application for viewing groups.
(define-application-frame group-viewer ()
  ((group :accessor group)
   (ptype :accessor ptype))
  (:command-table (group-viewer :inherit-from (tree)))
  (:panes (tree :application :display-function 'display))
  (:layouts (single tree)))

(defun display (f p)
  (with-end-of-line-action (p :allow)
    (with-end-of-page-action (p :allow)
      ;(updating-output (p :unique-id :top-level)
      (updating-output (p) 
        (disp-tree (group f) (ptype f) p (indentation (group f)))))))

(defun view-group (gp pt)
  (let ((f (make-application-frame 'group-viewer :left 0 :top 0 :right 400 :bottom 400)))
    (setf (group f) gp (ptype f) pt)
    (run-frame-top-level f)))
