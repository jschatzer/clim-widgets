; code adapted form 
; http://osdir.com/ml/mcclim-devel/2009-08/msg00010.html


;;; -*- Syntax: Common-lisp; Package: clim-user -*-

;;; Indented lists are a way of displaying hierarchical (tree) structure.
;;; Each non-terminal tree node is prefixed by an icon that shows whether
;;; the item is "open" or "closed".  If open, the inferiors of the node
;;; are displayed directly below it, with indentation accumulating
;;; in relation to tree depth.  For terminal nodes, there is no icon.
;;;
;;; Try:
;;;  (view-directory "~") ; unix
;;;  (view-directory "disneyland:>mickey-mouse>*.*.*") ; lispm
;;;
;;; Written by Jeff Morrill (jmorrill@bbn.com), December 92.
;;; It is provided "as is" without express or implied warranty.
;;; Thanks to Scott McKay for solving an incremental redisplay problem.
;;;
;;; ************************************************************

;;; The basic protocol.  Non-terminal nodes are called "groups."
;;; Everything that is not a group is considered to be a terminal node.

(in-package :clim-widgets)


(defclass essential-group ()
  ((display-contents :initform nil :initarg :display-contents :accessor display-contents)))

(defmethod group-p ((self t)) nil)
(defmethod group-p ((self essential-group)) t)
(defmethod group-contents ((group t)) nil)
(defmethod group-name ((group t)) group)

(defmethod toggle-open ((group t))
  (setf (display-contents group) (not (display-contents group))))

(defmethod indented-list-presentation-type ((group t) default) default)
(defmethod indented-list-indentation ((group t)) 3)

(defmethod display-indented-list ((group t) presentation-type stream indentation)
  "This presents the 'name' part of both groups and nongroups"
  (updating-output (stream :unique-id group :cache-value group)
    (multiple-value-bind (x y) (stream-cursor-position stream) (declare (ignore x))
      (stream-set-cursor-position stream (+ (* 2.5 (stream-character-width stream #\m)) (* (stream-character-width stream #\m) indentation)) y)
      (present (group-name group) presentation-type :stream stream)
      (terpri stream))))

(defmethod display-indented-list :around ((group essential-group) presentation-type stream indentation)
  "This displays the icon and the group-contents of a group"
  (updating-output (stream :unique-id group)
    (multiple-value-bind (x y) (stream-cursor-position stream) (declare (ignore x))
      (stream-set-cursor-position stream (* (stream-character-width stream #\m) indentation) y)
      (draw-indented-list-handle group stream)
      (call-next-method)
      (when (display-contents group)
        (let ((i (indented-list-indentation group))
              (type (indented-list-presentation-type group presentation-type)))
          (dolist (child (group-contents group))
            (display-indented-list child type stream (+ indentation i))))))))

(defun draw-indented-list-handle (group stream)
  "Draw the opened/closed icon (a triangle)"
  (updating-output (stream :unique-id 'list-handle :cache-value (display-contents group))
    (with-output-as-presentation (stream group 'indented-list :single-box t)
      (let* ((open-p (display-contents group))
             (h (- (stream-line-height stream) 2))
             (h/2 (truncate h 2)))
        (multiple-value-bind (x y) (stream-cursor-position stream)
          (incf y 1)
          (incf x h/2)
          (let* ((x1 (+ x h/2))
                 (y1 (+ y h/2))
                 (x2 x)
                 (y2 y)
                 (x3 (if open-p (+ x h) x))
                 (y3 (if open-p y (+ y h))))
            (draw-polygon* stream (list x1 y1 x2 y2 x3 y3) :filled t)
            (draw-point* stream (+ x h) (+ y h) :ink +background-ink+)
            (draw-point* stream
                         (if open-p x (+ x h))
                         (if open-p (+ y h) y)
                         :ink +background-ink+))
          (stream-set-cursor-position stream (+ x h h) y))))))

;;; ************************************************************

;;; A presentation type and an action for open/close operations.

(define-presentation-type indented-list (&optional presentation-type))

(define-presentation-method accept ((type indented-list) stream (view textual-view) &key)
  (accept presentation-type :stream stream :prompt nil))

(define-presentation-method describe-presentation-type ((type indented-list) stream plural-count) (declare (ignore plural-count))
  (describe-presentation-type presentation-type stream))

(define-presentation-method present (object (type indented-list) stream (view textual-view) &key)
  (display-indented-list object presentation-type stream (indented-list-indentation object)))

(define-command-table :indented-lists)

(define-presentation-action com-toggle-open
    (indented-list command :indented-lists
		   :gesture :select
		   :documentation "Reveal/Hide Contents"
		   :menu t)
  (object window)
  (progn
    (toggle-open object)
    (redisplay-frame-pane *application-frame* window)))

;;;************************************************************

;;; A generic application for viewing groups.

(define-application-frame group-viewer ()
  ((group-viewer-group :initform nil :accessor group-viewer-group)
   (group-viewer-ptype :initform nil :accessor group-viewer-ptype)
   (displayer :initform nil :accessor group-viewer-displayer))
  (:command-table (group-viewer :inherit-from (:indented-lists)))
  (:panes (display (scrolling () (make-pane 'application-pane :display-function 'display-viewer-group :display-time :no-clear))))
  (:layouts (default (vertically () display))))

(defun display-or-redisplay-group (group presentation-type stream displayer)
  (cond ((not displayer)
         (window-clear stream)
         (with-end-of-line-action (stream :allow)
           (with-end-of-page-action (stream :allow)
             (setq displayer
                   (updating-output (stream :unique-id :top-level)
                     (display-indented-list group presentation-type stream (indented-list-indentation group)))))))
        (t (redisplay displayer stream)))
  displayer)

(defun display-viewer-group (program stream)
  (setf (group-viewer-displayer program)
    (display-or-redisplay-group (group-viewer-group program)
				(group-viewer-ptype program)
				stream
				(group-viewer-displayer program))))

(defvar *viewer* nil)

(defun view-group (group presentation-type)
  (let ((frame (or *viewer*
		   (setq *viewer*
		     (make-application-frame 'group-viewer 
					     :left 0
					     :top 0
					     :right 400
					     :bottom 400)))))
    (setf (group-viewer-group frame) group
	  (group-viewer-ptype frame) presentation-type
	  (group-viewer-displayer frame) nil)
    (run-frame-top-level frame)))


