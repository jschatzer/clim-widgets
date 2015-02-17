; code adapted from 
; http://osdir.com/ml/mcclim-devel/2009-08/msg00010.html
; and
; http://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/lisp/gui/clim/clim_2/browsers/
; =========================================================================================
;;; Indented lists are a way of displaying hierarchical (tree) structure.
;;; Each non-terminal tree node is prefixed by an icon that shows whether
;;; the item is "open" or "closed".  If open, the inferiors of the node
;;; are displayed directly below it, with indentation accumulating
;;; in relation to tree depth.  For terminal nodes, there is no icon.
;;;
;;; Written by Jeff Morrill (jmorrill@bbn.com), December 92.
;;; It is provided "as is" without express or implied warranty.
;;; Thanks to Scott McKay for solving an incremental redisplay problem.
;;;
;;; The basic protocol.  Non-terminal nodes are called "groups."
;;; Everything that is not a group is considered to be a terminal node.
; =========================================================================================

;updating-output with/without :unique-id -- for performance?

(in-package :clim-widgets)
; f frame, p pane, s stream, pt presentation-type, gp group

(defclass essential-group ()
  ((display-contents :initform nil :initarg :display-contents :accessor display-contents)))

(defmethod group-name ((group t)) group)
(defmethod group-contents ((group t)) nil)
(defmethod item-ptype ((group t) default) default)
(defmethod indentation ((group t)) 2)
(defmethod toggle ((group t)) (setf (display-contents group) (not (display-contents group))))

(defmethod display-indented-list ((group t) pt s indent)
  "This presents the name part of both groups and nongroups"
;  (updating-output (s :unique-id group :cache-value group)
  (updating-output (s :cache-value group)
    (multiple-value-bind (x y) (stream-cursor-position s) (declare (ignore x))
      (stream-set-cursor-position s (+ (* 2.5 (stream-character-width s #\m)) (* (stream-character-width s #\m) indent)) y)
      (present (group-name group) pt :stream s) (terpri s))))

(defmethod display-indented-list :around ((group essential-group) pt s indent)
  "This displays the icon and the group-contents of a group"
;  (updating-output (s :unique-id group)
  (updating-output (s)
    (multiple-value-bind (x y) (stream-cursor-position s) (declare (ignore x))
      (stream-set-cursor-position s (* (stream-character-width s #\m) indent) y)
      (draw-icon group s)
      (call-next-method)
      (when (display-contents group)
        (let ((i (indentation group))
              (type (item-ptype group pt)))
          (dolist (child (group-contents group))
            (display-indented-list child type s (+ indent i))))))))

(defun triangle (s) (surrounding-output-with-border (s) (draw-polygon* s '(0 0 1 2 2 0))))

(defun icon (s x y o)
  (with-translation (s x y) 
    (with-scaling (s 4) 
      (if o 
        (triangle s)
        (with-rotation (s (/ pi -2) (make-point 1 1)) (triangle s))))))

(defun draw-icon (group s)
  "Draw the opened/closed icon"
  (updating-output (s :cache-value (display-contents group))
    (with-output-as-presentation (s group 'icon :single-box t)
      (let ((open-p (display-contents group)))
        (multiple-value-bind (x y) (stream-cursor-position s)
          (icon s x y open-p)
          (stream-set-cursor-position s x y))))))

;;; ************************************************************
;;; A presentation type and an action for open/close operations.

(define-command-table tree)

(define-presentation-type icon (&optional pt))

(define-presentation-method accept ((type icon) s (view textual-view) &key)
  (accept pt :stream s :prompt nil))

(define-presentation-method present (object (type icon) s (view textual-view) &key)
  (display-indented-list object ptype s (indentation object)))

(define-presentation-action toggle (icon command tree :gesture :select) (object window)
  (toggle object) (redisplay-frame-pane *application-frame* window))

;;;************************************************************
;;; A generic application for viewing groups.

(define-application-frame group-viewer ()
  ((group :accessor group)
   (ptype :accessor ptype))
  (:command-table (group-viewer :inherit-from (tree)))
  (:panes (tree :application :display-function 'disp-tree))
  (:layouts (single tree)))

(defun disp-tree (f p)
  (with-end-of-line-action (p :allow)
    (with-end-of-page-action (p :allow)
;      (updating-output (p :unique-id :top-level) 
      (updating-output (p) 
        (display-indented-list (group f) (ptype f) p (indentation (group f)))))))

(defun view-group (gp pt)
  (let ((f (make-application-frame 'group-viewer :left 0 :top 0 :right 400 :bottom 400)))
    (setf (group f) gp (ptype f) pt)
    (run-frame-top-level f)))
