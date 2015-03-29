(in-package :clim-widgets)

;--------------------------------------------------------
;;; An application for viewing file directories, from http://osdir.com/ml/mcclim-devel/2009-08/msg00010.html
;--------------------------------------------------------
(defun directory-p (p) (not (pathname-name p)))
(defun directory-pathname (p) (fad:pathname-directory-pathname p))
(defun list-directory (p) (fad:list-directory p))
(defun directory-name (p) (let ((lst (pathname-directory p))) (when (consp lst) (car (last lst)))))
(defun file-name (p) (file-namestring p))

(defclass directory-display (node)
  ((pathname :initarg :pathname :accessor pn)
   (contents :accessor node-contents)))

(defmethod node-name ((self directory-display)) (directory-name (pn self)))
;(defmethod print-object ((self directory-display) stream) (format stream "#<~A>" (node-name self))) ;not needed?

(defmethod node-contents :around ((self directory-display))
  (unless (slot-boundp self 'contents)
    (let ((stuff (list-directory (pn self))))
      (setf (node-contents self)
            (append (mapcar
                      (lambda (p)
                        (make-instance 'directory-display :pathname (directory-pathname p)))
                      (sort (remove-if-not #'directory-p stuff) #'string-lessp :key #'directory-name))
                    (sort (mapcar #'file-name (remove-if #'directory-p stuff)) #'string-lessp)))))
  (call-next-method))

(defun view-directory (directory)
  ;(view-group (make-instance 'directory-display :pathname (directory-pathname directory) :display-contents t) 'string))
  (view-group (make-instance 'directory-display :pathname (directory-pathname directory)) 'string))

;--------------------------------------------------------
;;; A trivial example to view a tree of nested lists
;--------------------------------------------------------
(defparameter lst '(("icd" ("dgn" ("d1" ("dleaf1") ("dleaf2")) ("d2")) ("int" ("i1") ("i2")))))

;1) create a hash table:
;   key:=stg sup-name
;   val:=lst of inf-names 
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

(defclass tree-view (node)
  ((sup :accessor sup :initarg :sup)
   (inf :accessor node-contents)))

(defmethod node-name ((self tree-view)) (sup self))
;(defmethod print-object ((self tree-view) stream) (format stream "#<~A>" (node-name self))) ;??

(defmethod node-contents :around ((self tree-view))
  (unless (slot-boundp self 'inf)
    (let ((stuff (gethash (sup self) *nodes*)))
      (setf (node-contents self) 
            (mapcar (lambda (x) (if (gethash x *nodes*) (make-instance 'tree-view :sup x) x)) stuff))))
  (call-next-method))

(defun treeview (tree key) ;initial key
  (t2h tree)  ; 1) create hash-table
  (view-group (make-instance 'tree-view :sup key :display-contents t) 'string))  ; 2) create initial display

;--------------------------------------------------------
;;; A small classification example, with instructions
;--------------------------------------------------------
(defparameter icds ;icd small
'(("d|diagnosi|"
  ("d.01|1. MALATTIE INFETTIVE E PARASSITARIE (001-139)|
    Nota: I postumi delle malattie infettive e parassitarie
               sono compresi nelle categorie 137-139.
    Incl.: malattie generalmente riconosciute come con-
               tagiose o trasmissibili ed alcune malattie di
               origine sconosciuta, ma probabilmente infet-
               tiva.
    Escl.: alcune infezioni localizzate
           infezioni respiratorie acute (460-466)
           influenza (487.0-487.8)
           portatori o sospetti portatori di agenti infettivi
              (V02.0-V02.9)"
   ("d.01.001009|(001-009) MALATTIE INFETTIVE INTESTINALI|
            Escl.: elmintiasi (120.0-129)"
    ("d.01.001009.001|001 Colera|" ("d.01.001009.001.0|001.0 Colera da Vibrio cholerae|") ("d.01.001009.001.1|001.1 Colera da Vibrio cholerae el tor|")
     ("d.01.001009.001.9|001.9 Colera non specificato|"))
    ("d.01.001009.002|002 Febbre tifoide e paratifoide|"
     ("d.01.001009.002.0|002.0 Febbre tifoide|
          Tifoide (febbre)(infezione) [qualsiasi sede]")
     ("d.01.001009.002.1|002.1 Paratifo A|") ("d.01.001009.002.2|002.2 Paratifo B|") ("d.01.001009.002.3|002.3 Paratifo C|")
     ("d.01.001009.002.9|002.9 Paratifo non specificato|"))
    ("d.01.001009.003|003 Altre infezioni da Salmonella|
    Incl.: infezione o intossicazione alimentare da Sal-
              monella [qualunque sierotipo]"
     ("d.01.001009.003.0|003.0 Gastroenterite da Salmonella|
            Salmonellosi")))))))

(define-application-frame tree-info (group-viewer)
 ((info :accessor info :initform ""))
  (:command-table (tree-info :inherit-from (tree group-viewer)))
  (:panes 
   (tree :application :display-function 'display)  ; brauchts anscheinend
   (info :application :display-function 'disp-info))
	(:layouts (double (horizontally () tree (make-pane 'clim-extensions:box-adjuster-gadget) info))))

(defun wdo (s stg ink) (with-drawing-options (s :ink ink :text-face :bold) (format s "~a" stg))) ;helper

(defun disp-info (f p)
  (mapc (lambda (x)
          (cond 
            ((string= x "Incl.:") (wdo p x +green+))
            ((string= x "Escl.:") (wdo p x +red+))
            ((#~m'Not[ae]:' x) (wdo p x +foreground-ink+))
            (t (format p "~a" x))))
        (ppcre:split "(Not[ae]:|Escl.:|Incl.:)" (info *application-frame*) :with-registers-p t)))

(defmethod node-contents :around ((self tree-info))
  (unless (slot-boundp self 'inf)
      (setf (node-contents self) 
            (mapcar (lambda (x) 
                      (if (gethash x *nodes*) 
                        (make-instance 'tree-info :sup x :info (#~s'.*\|.+\|(.*)'\1's x)) 
                        (#~s'.*\|(.+)\|.*'\1's x)))
                    (gethash (sup self) *nodes*))))
  (call-next-method))

(define-presentation-type icd () :inherit-from 'string)

(define-presentation-method present (item (type icd) s view &key) (declare (ignore type view))
  (format s "~a" (#~s'.*\|(.+)\|.*'\1's item)))

(define-tree-info-command xx ((item 'icd :gesture :select))   
  (setf (info *application-frame*) (#~s'.*\|.+\|(.*)'\1's item)))

(defun view-group2 (group ptype)
  (let ((f (make-application-frame 'tree-info :left 0 :top 0 :right 800 :bottom 400)))
    (setf (group f) group (ptype f) ptype)
    (run-frame-top-level f)))

(defun icdview (tree key)
  (t2h tree)
  (view-group2 (make-instance 'tree-view :sup key :display-contents t) 'icd))

;--------------------------------------------------------
;;; A class "browser"
;--------------------------------------------------------
(defun class-p (sym) (if (ignore-errors (find-class sym)) sym))
(defparameter classes (remove nil (mapcar #'class-p (loop for s being the external-symbols of :common-lisp collect s))))

(defun l2h (lst)
  "list to hash-table"
  (mapc (lambda (x)
          (cond ((closer-mop:class-direct-subclasses (find-class x)) (defnode x (mapcar #'class-name (closer-mop:class-direct-subclasses (find-class x)))))
                (t (defnode x nil))))
        lst))

(defun class-view (tree key)
  (l2h tree)
  (view-group (make-instance 'tree-view :sup key :display-contents t) 'symbol))

;--------------------------------------------------------
;;; class browser with description
;--------------------------------------------------------
(define-presentation-type item () :inherit-from 'string)
(define-presentation-method present (item (type item) s view &key) (declare (ignore type view))
  (format s "~a" item))

(define-application-frame cond-info (group-viewer)
 ((info :accessor info :initform ""))
  (:command-table (cond-info :inherit-from (tree group-viewer)))
  (:panes 
   (tree :application :display-function 'display :incremental-redisplay t)
   (info :application :display-function 'disp-info-cond :incremental-redisplay t))
	(:layouts (double (horizontally () tree (make-pane 'clim-extensions:box-adjuster-gadget) info))))

(defun disp-info-cond (f p)
  (format p "~a" (describe (ignore-errors (find-class (info *application-frame*))))))

(define-cond-info-command xx ((item 'item :gesture :select))   
  (setf (info *application-frame*) item))

(defun view-group3 (group ptype)
  (let ((f (make-application-frame 'cond-info :left 0 :top 0 :right 800 :bottom 400)))
    (setf (group f) group (ptype f) ptype)
    (run-frame-top-level f)))

(defun condition-view (tree key)
  (l2h tree)
  (view-group3 (make-instance 'tree-view :sup key :display-contents t) 'item))
