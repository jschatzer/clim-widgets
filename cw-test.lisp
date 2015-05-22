;;; cw-test.lisp

(in-package cw-test)
(named-readtables:in-readtable lol:lol-syntax)

;--------------------------------------------------------
;;; An application for viewing file directories, from http://osdir.com/ml/mcclim-devel/2009-08/msg00010.html
;--------------------------------------------------------
(defun d? (p) (not (pathname-name p)))
(defun dp (p) (fad:pathname-directory-pathname p))
(defun ls (p) (fad:list-directory p))
(defun dn (p) (let ((lst (pathname-directory p))) (when (consp lst) (car (last lst)))))
(defun fn (p) (file-namestring p))

(defclass node-fs (cw:node) ())  ; node-filesystem
(defmethod cw::node-name ((n node-fs)) (dn (cw::sup n)))

(defmethod cw::inf :around ((n node-fs))
  (unless (slot-boundp n 'cw::inf)
    (let ((stuff (ls (cw::sup n))))
      (setf (cw::inf n)
            (append (mapcar (lambda (p) (make-instance 'node-fs :sup (dp p))) (sort (remove-if-not #'d? stuff) #'nsort:nstring-lessp :key #'dn))
                    (sort (mapcar #'fn (remove-if #'d? stuff)) #'nsort:nstring-lessp)))))
  (call-next-method))

(defun view-directory (d) (cw:tree-view (make-instance 'node-fs :sup (dp d)) 'string))

;--------------------------------------------------------
;;; A trivial example to view a tree of nested lists
;--------------------------------------------------------
(defparameter lst '(("icd" ("dgn" ("d1" ("dleaf1") ("dleaf2")) ("d2")) ("int" ("i1") ("i2")))))

(defun treeview (tree key) ;initial key
  (cw:t2h tree)  ; 1) create hash-table
  (cw:tree-view (make-instance 'cw:node :sup key :disp-inf t) 'string))  ; 2) create initial display

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

(define-application-frame icd9it (cw:tree)
 ((info :accessor info :initform ""))
  (:command-table (icd9it :inherit-from (cw:tree)))
  (:panes 
   (tree :application :display-function 'cw:display-tree :incremental-redisplay t :end-of-line-action :allow :end-of-page-action :allow)
   (info :application :display-function 'disp-info :incremental-redisplay t))
	(:layouts (double (horizontally () tree (make-pane 'clim-extensions:box-adjuster-gadget) info))))

(defmethod disp-info ((f icd9it) p)
  (mapc (lambda (x)
          (flet ((wdo (s stg ink) (with-drawing-options (s :ink ink :text-face :bold) (format s "~a" stg))))
            (cond 
              ((string= x "Incl.:") (wdo p x +green+))
              ((string= x "Escl.:") (wdo p x +red+))
              ((#~m'Not[ae]:' x) (wdo p x +foreground-ink+))
              (t (format p "~a" x)))))
        (ppcre:split "(Not[ae]:|Escl.:|Incl.:)" (info *application-frame*) :with-registers-p t)))

(defmethod inf :around ((n icd9it))
  (unless (slot-boundp n 'inf)
      (setf (inf n) 
            (mapcar (lambda (x) 
                      (if (gethash x *nodes*) 
                        (make-instance 'icd9it :sup x :info (#~s'.*\|.+\|(.*)'\1's x)) 
                        (#~s'.*\|(.+)\|.*'\1's x)))
                    (gethash (sup n) *nodes*))))
  (call-next-method))

(define-presentation-type icd () :inherit-from 'string)
(define-presentation-method present (item (type icd) s v &key) (format s "~a" (#~s'.*\|(.+)\|.*'\1's item)))
(define-icd9it-command xx ((item 'icd :gesture :select)) (setf (info *application-frame*) (#~s'.*\|.+\|(.*)'\1's item)))

(defun icdview (tree key)
  (cw:t2h tree)
  (cw:tree-view (make-instance 'cw:node :sup key :disp-inf t) 'icd 'icd9it :right 800))

;--------------------------------------------------------
;;; A class "browser"
;--------------------------------------------------------
(defun class-p (sym) (if (ignore-errors (find-class sym)) sym))
(defparameter classes (remove nil (mapcar #'class-p (loop for s being the external-symbols of :common-lisp collect s))))

(defun defnode (sup inf) (setf (gethash sup cw::*nodes*) inf))

(defun l2h (lst)
  "list to hash-table"
  (mapc (lambda (x)
          (cond ((closer-mop:class-direct-subclasses (find-class x)) (defnode x (mapcar #'class-name (closer-mop:class-direct-subclasses (find-class x)))))
                (t (defnode x nil))))
        lst))

(defun view-classes (tree key)
  (l2h tree)
  (cw:tree-view (make-instance 'cw:node :sup key :disp-inf t) 'symbol))

;--------------------------------------------------------
;;; Class browser with description
;--------------------------------------------------------
(define-application-frame class-browser (cw:tree)
 ((info :accessor info :initform ""))
  (:command-table (class-browser :inherit-from (cw:tree)))
  (:panes 
   (tree :application :display-function 'cw:display-tree :incremental-redisplay t)
   (info :application :display-function 'disp-info :incremental-redisplay t))
	(:layouts (double (horizontally () tree (make-pane 'clim-extensions:box-adjuster-gadget) info))))

(defmethod disp-info ((f class-browser) p) (format p "~a" (describe (ignore-errors (find-class (info *application-frame*))))))

(define-presentation-type item () :inherit-from 'string)
(define-presentation-method present (item (type item) s v &key) (format s "~a" item))
(define-class-browser-command xxx ((item 'item :gesture :select)) (setf (info *application-frame*) item))

(defun view-classes-with-description (tree key)
  (l2h tree)
  (cw:tree-view (make-instance 'cw:node :sup key :disp-inf t) 'item 'class-browser :right 800))

;--------------------------------------------------------
;;; Package "browser", a simple draft of a package documentation
;--------------------------------------------------------
; 1) app-logic and helper
; ev todo: remove second common-lisp from pkg-menu, remove special forms from other categories, rm cclim colors from variable
(defparameter pkg-list (cons :common-lisp (sort (mapcar (lambda (x) (intern (package-name x) :keyword)) (list-all-packages)) #'string<)))
; color-names are mixed case strings, without + or -, e.g. "antique white"
(defparameter colors (mapcar (lambda (x) (#~s'.*'+\&+' (#~s' '-'g x))) (mapcar #'fourth clim-internals::*xpm-x11-colors*)))
(defun constant-p (s) (if (or (constantp s) (#~m'^\+.+\+$' (symbol-name s))) s))
(defparameter clim-es (mapcar #'symbol-name (remove-if-not #'constant-p (loop for s being the external-symbols of :clim collect s))))
(defun clim-non-color-contstants () (set-difference clim-es colors :test 'string-equal))
(defun clim-color-contstants () (intersection clim-es colors :test 'string-equal))
(defun clim-colors ()
  "divide clim-constants into colors and other constants"
  (let ((o (mapcar (lambda (x) (find-symbol x :clim)) (sort (clim-non-color-contstants) #'string<)))     ;other constants
        (c (mapcar (lambda (x) (find-symbol x :clim)) (sort (clim-color-contstants) #'nsort:nstring<)))) ;color-name constants
    (cons :constant (cons (cons 'color-names (mktree c)) (mktree o)))))
(defun spec-op () (cons 'special-operator (mktree (sort (remove-if-not #'special-operator-p (loop for s being the external-symbols of :cl collect s)) #'string<))))

; form http://www.ic.unicamp.br/~meidanis/courses/problemas-lisp/L-99_Ninety-Nine_Lisp_Problems.html
(defun key (s) (subseq (symbol-name s) 0 (position #\- (symbol-name s))))
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

;do it recursive, all -
(defun mktree (l) (mapcar (lambda (x) (if (null (cdr x)) x (cons (key (car x)) (mapcar #'list x)))) (pack l)))

#|
(defun mktree (l) (mapcar (lambda (x) 
                            (cond 
                              ((null (cdr x)) x)
                              ((= 2 (length (ppcre:split "-" (car x)))) (cons (key (car x)) (mapcar #'list x)))
                              (t (cons (second (ppcre:split "-" (car x))) (mapcar #'list x)))))
                          (pack l)))
|#

; from Peter Seibel's "manifest" quicklisp package
(defun present-symbols%% (pkg)
  (loop for what in manifest::*categories*
        for names = (manifest::names pkg what)
        when names collect
        (cons what (remove-if 'consp names))))  ; remove "setf functions" e.g. AREF (SETF AREF)

(defun symbol-tree (p)
  "if pkg is clim, divide constants into color-names and other-constants
  if pkg is cl, show special forms"
  (cond ((eql p :clim) (reverse (cons (clim-colors) (mapcar (lambda (x) (cons (car x) (mktree (cdr x)))) (cdr (reverse (present-symbols%% p)))))))
        ((eql p (or :common-lisp :cl)) (cons (spec-op) (mapcar (lambda (x) (cons (car x) (mktree (cdr x)))) (present-symbols%% p))))
        (t (mapcar (lambda (x) (cons (car x) (mktree (cdr x)))) (present-symbols%% p)))))

; 2) gui -- nodes should not be sensible
(define-application-frame pkg-doc (cw:tree)
 ((info :accessor info :initform ""))
  (:command-table (pkg-doc :inherit-from (cw:tree)))
  (:panes 
   (tree :application :display-function 'cw:display-tree :incremental-redisplay t :end-of-line-action :allow :end-of-page-action :allow)
   (info :application :display-function 'disp-info :incremental-redisplay t))
	(:layouts (double (horizontally () tree (make-pane 'clim-extensions:box-adjuster-gadget) info))))

(defmethod disp-info ((f pkg-doc) p) (describe (info *application-frame*) p))

(define-pkg-doc-command show-info ((item 'symbol :gesture :select))   
  (setf (info *application-frame*) item))

(defun tview (tree key)
  (cw:t2h tree)
  (cw:tree-view (make-instance 'cw:node :sup key :disp-inf t) 'symbol 'pkg-doc :right 800))

(defun pkg-doc (&optional (pkg :clim)) (tview (list (cons pkg (symbol-tree pkg))) pkg))

;creates a separtate window for every new package
(define-pkg-doc-command (packages :menu t) ()
  (let ((pkg (menu-choose pkg-list)))
   (tview (list (cons pkg (symbol-tree pkg))) pkg)))
