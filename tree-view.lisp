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

(in-package cw-treeview)

;************************************************************
; 1) ICONS AND GRID
;************************************************************
(defvar icon 'plus)     ; 'triangle 'triangle2
(defvar grid t)         ; nil
(defvar dashed-lines t) ; nil

(define-presentation-type icon ())
(define-presentation-method highlight-presentation ((type icon) r s st) :unhighlight)
(define-presentation-method present (o (type icon) s v &key) (disp-tree o ptype s (indentation o)))

(defmethod toggle (n) (setf (disp-inf n) (not (disp-inf n))))

; when doing all calculationsts with d, there is no need for scaling
(define-symbol-macro d (STREAM-CHARACTER-WIDTH s #\m))

; GRID
(defun spc (s) (stream-increment-cursor-position s (* 1.5 d) nil))
(defun bar% (s) (draw-line* s 0 0 0 d :line-dashes dashed-lines :ink +gray+))  
(defun bar (s) 
  (multiple-value-bind (x y) (stream-cursor-position s) (with-translation (s (+ x (* .5 d))  y) (bar% s)))
  (stream-increment-cursor-position s (* 1.5 d) nil))

(defmethod grid (s i n)
  (let* ((spaces (initial-space n))
         (bars (- i spaces))) 
    ;1) do initial spaces
    (dotimes (x spaces) (spc s))
    ;2) do bars
    (dotimes (x bars) (if grid (bar s) (spc s)))))

; POINTER
(defun pointr% (s) (draw-lines* s (list 0 (* .3 d) 0 d 0 d (* .7 d) d) :line-dashes dashed-lines :ink +gray+))
(defun pointr (s)
  (multiple-value-bind (x y) (stream-cursor-position s)
    (with-translation (s (+ x (* .5 d)) (+ y (* -.5 d)))
;      (pointr% s))))
      (when grid (pointr% s))))) ; to suppress pointer

; ICONS
;;;plus - remove visible background pointer
(defun rect (s) (draw-rectangle* s 0 0 d d :filled nil))
(defun m (s) (draw-line* s (* d .2) (* d .5) (* d .8) (* d .5)))
(defun p (s) (m s) (draw-line* s (* d .5) (* d .2) (* d .5) (* d .8)))
(defun plus (s x y o) (with-translation (s x y) (if o (m s) (p s)) (rect s)))

;;;triangles
(defun tri-m (s) (draw-polygon* s (list 0 0 d 0 (* .5 d) d) :ink +black+))
(defun tri-p (s) (draw-polygon* s (list 0 0 d (* .5 d) 0 d)))   ; with rotation !!
(defun triangle  (s x y o) (with-translation (s x y)          (if o (tri-m s) (tri-p s))))
(defun triangle2 (s x y o) (with-translation (s x y) (rect s) (if o (tri-m s) (tri-p s))))

(defun draw-icon (s group)
  (with-output-as-presentation (s group 'icon)
    (let ((open-p (disp-inf group)))
      (multiple-value-bind (x y) (stream-cursor-position s)
        (funcall icon s x y open-p)))))

;************************************************************
; 2) CREATE CHILDREN INSTANCES
;************************************************************
(defclass item ()
  ((sup :accessor sup :initarg :sup)  ; ev name
   (initial-space :accessor initial-space :initarg :initial-space :initform 0)))

(defclass leaf (item) ())  ; leaf shall/may not be a superclass of node 

(defclass node (item) 
  ((inf :accessor inf :documentation "inferiors, list")
   (youngest-child :accessor youngest-child :initarg :youngest-child :initform t)
   (disp-inf :initform nil :initarg :disp-inf :accessor disp-inf :documentation "boolean")))

(defmethod item-name (n) n)
;(defmethod inf (n) nil)
(defmethod c-nodep (n) nil)   ; test if child is node
(defmethod children (n) nil)

(defmacro inf-meth (&key  ;short keywords
                     ((:nc node-class) 'node) 
                     ((:cc children-class) 'string) 
                     ((:nn item-name-form) '(sup n)) 
                     ((:ln leaf-name-form) '(sup n)) 
                     ((:c children-form) '(gethash (sup n) nodes)) 
                     ((:cp c-node-p-form) '(gethash n nodes)) 
                     ((:cy childnode-is-youngestsibling-form) 'string=))
  "Define variants of node and leaf CLASSES and corresponding METHODS to get the childs of a node.
   All these classes and methods can also be defined, subclassed, etc. in the normal way.
   Keyword arguments:
   :nc node class
   :cc children class
   :nn node name
   :ln leaf name
   :c  supply a form that returns a list of a node's children
   :cp supply a form to test if a child is node or leaf
   :cy supply a function to test for equality o child nodes
   See view directory for an example usage"
   (let ((leaf-class (intern (#~s'NODE'LEAF'i (symbol-name node-class)) *package*)))
     `(progn
        ;classes
        (unless (eql ',node-class 'node)
          (defclass ,node-class (node) ())
          (defclass ,leaf-class (leaf) ()))
        ;methods
        (defmethod item-name ((n ,node-class)) ,item-name-form)
        (defmethod item-name ((n ,leaf-class)) ,leaf-name-form)
        (defmethod children ((n ,node-class)) ,children-form)
        (defmethod c-nodep ((n ,children-class)) ,c-node-p-form) ; the child is a node if it has children
        (defmethod childnode-is-youngestsibling ((n ,children-class) ch) (and (c-nodep n) (,childnode-is-youngestsibling-form n (car (last ch)))))
        (defmethod inf :before ((n ,node-class))
          "create children-instances"
          (unless (slot-boundp n 'inf) 
            (let ((children (children n))
                  (sp (initial-space n)))
              (setf (inf n) (mapcar 
                              (lambda (x) 
                                (if (c-nodep x) 
                                  (if (youngest-child n) ; if this (parent)-node is youngest sibling
                                    (if (childnode-is-youngestsibling x children)
                                      (make-instance ',node-class :sup x :initial-space (1+ sp) :youngest-child t)
                                      (make-instance ',node-class :sup x :initial-space (1+ sp) :youngest-child nil))
                                    (make-instance ',node-class :sup x :initial-space sp :youngest-child nil))
                                  (if (youngest-child n)
                                    (make-instance ',leaf-class :sup x :initial-space (1+ sp))
                                    (make-instance ',leaf-class :sup x :initial-space sp))))
                              children))))))))

;************************************************************
; 3) DISPLAY ITEMS
;************************************************************
;nodes
(defmethod disp-tree :around ((item node) pt s indent)
  "This displays grid, pointer, icon and children of a node-item"
  (with-text-size (s (txtsize *application-frame*))
    (grid s indent item)
    (pointr s)
    (draw-icon s item)
    (call-next-method)  ; <- item-name
    (when (disp-inf item)
        (dolist (child (inf item))
          (disp-tree child pt s (1+ indent))))))

;leaves
(defmethod disp-tree :before ((item leaf) pt s indent)
  "This displays grid and pointer of a leaf-item"
  (grid s indent item)
  (pointr s))

;names
(defmethod disp-tree (item pt s indent)
  "This presents the item's name"
  (stream-increment-cursor-position s (* 1.5 d) nil)
  (present (item-name item) pt :stream s) (terpri s))

;************************************************************
; 4) A GENERIC APPLICATION FOR VIEWING A TREE
;************************************************************
(define-application-frame tree ()
  ((txtsize :accessor txtsize :initform :normal)
   (group :accessor group :initarg :group)
   (ptype :accessor ptype :initarg :ptype))
  (:pane (make-pane 'application-pane :display-function 'display-tree :incremental-redisplay t :end-of-line-action :allow :end-of-page-action :allow)))

(defmethod display-tree ((f tree) p) (disp-tree (group f) (ptype f) p 0))
(define-presentation-action toggle (icon command tree) (object window) (toggle object) (redisplay-frame-pane *application-frame* window))
(define-tree-command (txt-size :menu t) () (setf (txtsize *application-frame*) textsize))

#|
(defun tree-view (gp pt &optional (frame 'tree) &key (left 0) (top 0) (right 400) (bottom 400) pretty-name &allow-other-keys)
    (run-frame-top-level (make-application-frame frame :group gp :ptype pt :left left :top top :right right :bottom bottom :pretty-name pretty-name)))

;;; siehe zuBehalten QQQQ
;; oben bug pretty name without default !!! ev use gp or pt ?? statt empty string, 27.8.2019
;; + problem optional + key !!??
;; pt ~ frame ?? remove one ??
(defun tree-view (gp pt &optional (frame 'tree) &key (left 0) (top 0) (right 400) (bottom 400) (pretty-name "") &allow-other-keys)
    (run-frame-top-level (make-application-frame frame :group gp :ptype pt :left left :top top :right right :bottom bottom :pretty-name pretty-name)))

;; 11.10.19 only keys
(defun tree-view (gp pt &key (frame 'tree) (left 0) (top 0) (right 400) (bottom 400) (pretty-name "") &allow-other-keys)
    (run-frame-top-level (make-application-frame frame :group gp :ptype pt :left left :top top :right right :bottom bottom :pretty-name pretty-name)))
|#

;; 14.10.19
;(defun tree-view (group &optional (frame 'tree) (ptype 'string) &key &allow-other-keys)
;    (run-frame-top-level (make-application-frame frame :group group :ptype ptype :left 0 :top 0 :right 400 :bottom 400)))


;######################################################################################################
#|
;######################################################################################################
; 16.10.19
; so geht pkg-doc ru-accent ru-reader atc cw:list-dir 
; so geht right 800, aber nicht pretty name <--

(defun tree-view (group &optional (frame 'tree) (ptype 'string) &key (left 0) (top 0) (right 400) (bottom 400) &allow-other-keys)
  (run-frame-top-level (make-application-frame frame :group group :ptype ptype :left left :top top :right right :bottom bottom)))
;######################################################################################################
|#
;######################################################################################################


;test pretty name, scheint zu gehen, test (icd10lquery::icd-clim :i)   etc
(defun tree-view (group &optional (frame 'tree) (ptype 'string) &key (left 0) (top 0) (right 400) (bottom 400)(pretty-name (symbol-name frame)) &allow-other-keys)
  (run-frame-top-level (make-application-frame frame :group group :ptype ptype :left left :top top :right right :bottom bottom :pretty-name pretty-name)))

;(defun tree-view (group &optional (frame 'tree) (ptype 'string) &rest options  &key (left 0) (top 0) (right 400) (bottom 400) &allow-other-keys)
;  (run-frame-top-level (make-application-frame frame options :group group :ptype ptype :left left :top top :right right :bottom bottom)))


#|
;;; sieht options in make-application-frame  <--- ???
;;; so geht pretty name und size 800
(defun tree-view (group &optional (frame 'tree) (ptype 'string) &key (left 0) (top 0) (right 400) (bottom 400) (pretty-name frame) &allow-other-keys)
;  (run-frame-top-level (make-application-frame frame :group group :ptype ptype :left left :top top :right right :bottom bottom &allow-other-keys)))
  (run-frame-top-level (make-application-frame frame :group group :ptype ptype :left left :top top :right right :bottom bottom :pretty-name pretty-name)))
|#
;************************************************************
; 5) SOME FUNCTIONS FOR SIMPLE CASES
;************************************************************
; 1) *** VIEW DIRECTORY - from http://osdir.com/ml/mcclim-devel/2009-08/msg00010.html
(inf-meth 
  :nc node-fs
  :cc pathname
  :cy path:=
  :nn (let ((lst (pathname-directory (sup n)))) (when (consp lst) (car (last lst))))
  :ln (file-namestring (sup n))
  :c  (fad:list-directory (sup n))
  :cp (path:-d n))

;(defun list-dir (d) ;initial key
;  (tree-view (make-instance 'node-fs 
;                            :sup (path:dirname d) 
;                            :disp-inf t) 
;             'string))
;
(defun list-dir (d) ;initial key
  (tree-view (make-instance 'node-fs 
                            :sup (path:dirname d) 
                            :disp-inf t)))


#|
;========================================
;---- with uiop
(inf-meth 
  :nc node-fs
  :cc pathname
;  :cy path:=
  :cy uiop:pathname-equal
  :nn (let ((lst (pathname-directory (sup n)))) (when (consp lst) (car (last lst))))
  :ln (file-namestring (sup n))
;  :c  (fad:list-directory (sup n))
;  :c  (uiop:directory* (sup n))
;  :c  (uiop:directory-files (sup n))
  :c  (format t "~s*" (uiop:directory* (sup n)))
;  :cp (path:-d n))
  :cp (uiop:directory-exists-p n))

(defun list-dir (d) ;initial key
  (tree-view (make-instance 'node-fs 
;                            :sup (path:dirname d) 
                            :sup d 
                            :disp-inf t)))
=======================================================
|#

;-----------------
; 2) *** VIEW A STRING- or SYMBOL-TREE of this form (using a hash-table)
; '(("node1"            ; '((node1 
;    ("node11"          ;    (node11 
;     ("leaf111")       ;     (leaf111)
;     ("leaf112"))))    ;     (leaf112))))

; -1- string-items
(inf-meth)

;  (defun treeview-strings (tree key) ;initial key
;    (t2h tree)  ; 1) create hash-table
;    (tree-view (make-instance 'node :sup key :disp-inf t) 'string))            ;; so gehts 9.10.2019
;  ;  (tree-view (make-instance 'node :sup key :disp-inf t) 'string 'string ))

;geht
;(defun treeview-strings (tree key) ;initial key
;  (t2h tree)  ; 1) create hash-table
;  (tree-view (make-instance 'node :sup key :disp-inf t)))

;(defun treeview-strings (tree &optional (key (caar tree)))

;; include autoscroll
(defun treeview (tree &optional (key (caar tree)))
  "view a tree"  ; of stringitems  <--??
  (t2h tree)  ; 1) create hash-table
  (tree-view (make-instance 'node :sup key :disp-inf t)))


;(cw:treeview-strings cw-examples::stgtree)

; -1- symbol-items --- converts symbols to strings
;  (inf-meth
;    :nc node-z)
;  
;  (defun treeview-symbols (tree key)
;    (t2h (sym2stg tree))
;    (tree-view (make-instance 'node-z :sup (string-downcase (symbol-name key)) :disp-inf t) 'string))
;  ;  (tree-view (make-instance 'node-z :sup (string-downcase (symbol-name key)) :disp-inf t) 'string 'string))
;  
;  ;run (cw:treeview-symbols cw-examples::symtree 'icd)
;  ;------------------------------------------------------
