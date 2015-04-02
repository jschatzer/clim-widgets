(in-package clim-widgets)
; p path, n node,

;--------------------------------------------------------
;;; An application for viewing file directories, from http://osdir.com/ml/mcclim-devel/2009-08/msg00010.html
;--------------------------------------------------------
(defun directory-p (p) (not (pathname-name p)))
(defun directory-pathname (p) (fad:pathname-directory-pathname p))
(defun list-directory (p) (fad:list-directory p))
(defun directory-name (p) (let ((lst (pathname-directory p))) (when (consp lst) (car (last lst)))))
(defun file-name (p) (file-namestring p))

(defmethod node-name ((n node)) (directory-name (sup n)))

(defmethod inf :around ((n node))
  (unless (slot-boundp n 'inf)
    (let ((stuff (list-directory (sup n))))
      (setf (inf n)
            (append (mapcar
                      (lambda (p)
                        (make-instance 'node :sup (directory-pathname p)))
                      (sort (remove-if-not #'directory-p stuff) #'string-lessp :key #'directory-name))
                    (sort (mapcar #'file-name (remove-if #'directory-p stuff)) #'string-lessp)))))
  (call-next-method))

(defun view-directory (directory)
  (tree-view (make-instance 'node :sup (directory-pathname directory)) 'string))

;--------------------------------------------------------
;;; A trivial example to view a tree of nested lists
;--------------------------------------------------------
(defparameter lst '(("icd" ("dgn" ("d1" ("dleaf1") ("dleaf2")) ("d2")) ("int" ("i1") ("i2")))))

(defclass node1 (node) ())  ; brauchts wegen obigen list-directory
(defmethod node-name ((n node1)) (sup n))

(defmethod inf :around ((n node1))
  (unless (slot-boundp n 'inf)
    (let ((stuff (gethash (sup n) *nodes*)))
      (setf (inf n) 
            (mapcar (lambda (x) (if (gethash x *nodes*) (make-instance 'node1 :sup x) x)) stuff))))
  (call-next-method))

(defun treeview (tree key) ;initial key
  (t2h tree)  ; 1) create hash-table
  (tree-view (make-instance 'node1 :sup key :disp-inf t) 'string))  ; 2) create initial display

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

(defmethod disp-info ((f tree-info) p)
  (mapc (lambda (x)
          (flet ((wdo (s stg ink) (with-drawing-options (s :ink ink :text-face :bold) (format s "~a" stg))))
            (cond 
              ((string= x "Incl.:") (wdo p x +green+))
              ((string= x "Escl.:") (wdo p x +red+))
              ((#~m'Not[ae]:' x) (wdo p x +foreground-ink+))
              (t (format p "~a" x)))))
        (ppcre:split "(Not[ae]:|Escl.:|Incl.:)" (info *application-frame*) :with-registers-p t)))

(define-presentation-type icd () :inherit-from 'string)

(defmethod inf :around ((n tree-info))
  (unless (slot-boundp n 'inf)
      (setf (inf n) 
            (mapcar (lambda (x) 
                      (if (gethash x *nodes*) 
                        (make-instance 'tree-info :sup x :info (#~s'.*\|.+\|(.*)'\1's x)) 
                        (#~s'.*\|(.+)\|.*'\1's x)))
                    (gethash (sup n) *nodes*))))
  (call-next-method))

(define-presentation-method present (item (type icd) s view &key) (declare (ignore type view))
  (format s "~a" (#~s'.*\|(.+)\|.*'\1's item)))

(define-tree-info-command xx ((item 'icd :gesture :select))   
  (setf (info *application-frame*) (#~s'.*\|.+\|(.*)'\1's item)))

(defun icdview (tree key)
  (t2h tree)
  (tree-info-view (make-instance 'node1 :sup key :disp-inf t) 'icd))

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

(defun view-classes (tree key)
  (l2h tree)
  (tree-view (make-instance 'node1 :sup key :disp-inf t) 'symbol))

;--------------------------------------------------------
;;; class browser with description
;--------------------------------------------------------
(define-presentation-type item () :inherit-from 'string)
(define-presentation-method present (item (type item) s view &key) (declare (ignore type view))
  (format s "~a" item))

; copy paste with renaming% because of inheritance- or specialisation problems with disp-info
(define-application-frame tree-info% (tree)
 ((info :accessor info :initform ""))
  (:command-table (tree-info% :inherit-from (tree)))
  (:panes 
   (tree :application :display-function 'display :incremental-redisplay t)
   (info :application :display-function 'disp-info :incremental-redisplay t))
	(:layouts (double (horizontally () tree (make-pane 'clim-extensions:box-adjuster-gadget) info))))

(defmethod disp-info ((f tree-info%) p)
  (format p "~a" (describe (ignore-errors (find-class (info *application-frame*))))))

(define-tree-info%-command xxx ((item 'item :gesture :select))   
  (setf (info *application-frame*) item))

(defun tree-info-view% (group ptype)
  (let ((f (make-application-frame 'tree-info% :left 0 :top 0 :right 800 :bottom 400)))
    (setf (group f) group (ptype f) ptype)
    (run-frame-top-level f)))

(defun view-classes-with-description (tree key)
  (l2h tree)
  (tree-info-view% (make-instance 'node1 :sup key :disp-inf t) 'item))
