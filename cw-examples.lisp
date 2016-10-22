;;; cw-examples.lisp

(in-package cw-examples)


(defmacro proc (fn)
  `(progn 
     (clim-sys:make-process (lambda () ,fn))
     (sleep 2)))

(defun run-all-examples ()
  (proc (cw:calendar))
  (proc (cw:digital-clock-eu))
  (proc (cw:clock))
  (proc (cw:treeview-strings stgtree "icd"))
  (proc (cw:treeview-symbols symtree 'icd))
  (proc (cw:list-dir (user-homedir-pathname)))
  (proc (class-browser1 'number))
  (proc (class-browser2 'condition))
  (proc (pkg-doc))
  (proc (icd-test icds "d|diagnosi|")))

;-----------------------------------------
;1) ICD BROWSER
;-----------------------------------------
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

(cw:inf-meth 
  :nc node-88)

(define-application-frame icd9it (tree)
 ((info :accessor info :initform ""))
  (:command-table (icd9it :inherit-from (tree)))
  (:menu-bar cw:tree)
  (:panes 
   (tree :application :display-function 'display-tree :incremental-redisplay t :end-of-line-action :allow :end-of-page-action :allow)
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

(define-presentation-type icd () :inherit-from 'string)
(define-presentation-method present (item (type icd) s v &key) (format s "~a" (#~s'.*\|(.+)\|.*'\1's item)))
(define-icd9it-command xx ((item 'icd :gesture :select)) (setf (info *application-frame*) (#~s'.*\|.+\|(.*)'\1's item)))

(defun icd-test (tree key)
  (cw:t2h tree)
  (cw:tree-view (make-instance 'node-88 :sup key :disp-inf t) 'icd 'icd9it :right 800))

;-----------------------------------------
;2) CLASS-BROWSER
;-----------------------------------------
(cw:inf-meth 
  :nc node-cb
  :cc symbol
  :cy eql
  :nn (class-name (find-class (cw:sup cw:n)))
  :c  (mapcar 'class-name (closer-mop:class-direct-subclasses (find-class (cw:sup cw:n))))
  :cp (closer-mop:class-direct-subclasses (find-class cw:n)))

;;; one-pane
(defun class-browser1 (key) ;initial key
  (cw:tree-view (make-instance 'node-cb 
                            :sup key  ; key is a symbol which names a class <----- 
                            :disp-inf t) 'symbol))

;;; two-panes
;------------------------------------------------------------------------
(define-application-frame class-browser (cw:tree)
 ((info :accessor info :initform ""))
  (:command-table (class-browser :inherit-from (cw:tree)))
  (:menu-bar cw:tree)
  (:panes 
   (tree :application :display-function 'cw:display-tree :incremental-redisplay t :end-of-line-action :allow :end-of-page-action :allow)
   (info :application :display-function 'disp-info :incremental-redisplay t :end-of-line-action :allow :end-of-page-action :allow))
	(:layouts (double (horizontally () tree (make-pane 'clim-extensions:box-adjuster-gadget) info))))

(defmethod disp-info ((f class-browser) p) (format p "~a" (describe (ignore-errors (find-class (info *application-frame*))))))

(define-presentation-type item () :inherit-from 'string)
(define-presentation-method present (item (type item) s v &key) (format s "~a" item))
(define-class-browser-command xxx ((item 'item :gesture :select)) (setf (info *application-frame*) item))

(defun class-browser2 (key)
  (cw:tree-view (make-instance 'node-cb :sup key :disp-inf t) 'item 'class-browser :right 800))

;-----------------------------------------
;3) PACKAGE-BROWSER, a simple draft of a package documentation
;-----------------------------------------
; 1) app-logic and helper
; ev todo: remove second common-lisp from pkg-menu, remove special forms from other categories, rm cclim colors from variable
(defparameter pkg-list (cons :common-lisp (sort (mapcar (lambda (x) (intern (package-name x) :keyword)) (list-all-packages)) #'string<)))
; color-names are mixed case strings, without + or -, e.g. "antique white"
(defparameter colors (mapcar (lambda (x) (#~s'.*'+\&+' (#~s' '-'g x))) (mapcar #'fourth clim-internals::*xpm-x11-colors*)))
(defun constant-p (s) (if (or (constantp s) (#~m'^\+.+\+$' (symbol-name s))) s))
(defparameter clim-es (mapcar #'symbol-name (remove-if-not #'constant-p (loop for s being the external-symbols of :clim collect s))))
(defun clim-non-color-contstants () (set-difference clim-es colors :test 'string-equal))
(defun clim-color-contstants () (intersection clim-es colors :test 'string-equal))

(defun mktree (l) (mapcar (lambda (x) (if (null (cdr x)) x (cons (cw:key (car x)) (mapcar #'list x)))) (cw:pack l)))

(defun clim-colors ()
  "divide clim-constants into colors and other constants"
  (let ((o (mapcar (lambda (x) (find-symbol x :clim)) (sort (clim-non-color-contstants) #'string<)))     ;other constants
        (c (mapcar (lambda (x) (find-symbol x :clim)) (sort (clim-color-contstants) #'nsort:nstring<)))) ;color-name constants
    (cons :constant (cons (cons 'color-names (mktree c)) (mktree o)))))
(defun spec-op () (cons 'special-operator (mktree (sort (remove-if-not #'special-operator-p (loop for s being the external-symbols of :cl collect s)) #'string<))))

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

(cw:inf-meth 
  :nc node-pkg)

; 2) gui -- nodes should not be sensible
(define-application-frame pkg-doc (cw:tree)
 ((info :accessor info :initform ""))
  (:command-table (pkg-doc :inherit-from (cw:tree)))
  ;(:menu-bar cw:tree)
  (:panes 
   (tree :application :display-function 'cw:display-tree :incremental-redisplay t :end-of-line-action :allow :end-of-page-action :allow)
   (info :application :display-function 'disp-info :incremental-redisplay t))
	(:layouts (double (horizontally () tree (make-pane 'clim-extensions:box-adjuster-gadget) info))))

(defmethod disp-info ((f pkg-doc) p) (describe (info *application-frame*) p))

(define-pkg-doc-command show-info ((item 'string :gesture :select))   
  (setf (info *application-frame*) item))

;creates a separtate window for every new package
(define-pkg-doc-command (packages :menu t) ()
  (let ((pkg (menu-choose pkg-list)))
   (tview (list (cons pkg (symbol-tree pkg))) pkg)))

(defun tview (tree key)
  (cw:t2h tree)
  (cw:tree-view (make-instance 'node-pkg :sup key :disp-inf t) 'string 'pkg-doc :right 800))

(defun pkg-doc (&optional (pkg :clim)) 
  (let ((p (string-downcase (package-name pkg))))
    (tview (list (cons p (cw:sym2stg (symbol-tree pkg)))) p)))

;------------------------
; data for trivial-example above
(defparameter stgtree
  '(("icd" 
     ("dgn" 
      ("d1" 
       ("dleaf1") 
       ("d11" 
        ("dleaf11") 
        ("dleaf12"))
       ("dleaf2")) 
      ("d2")) 
     ("int" 
      ("i1") 
      ("i11" 
       ("ileaf11") 
       ("i2" 
        ("ileaf21") 
        ("ileaf22"))
       ("ileaf12"))
      ("i2")))))

; do the same with a symbol-tree
(defparameter symtree
  '((icd 
     (dgn 
      (d1 
       (dleaf1) 
       (d11 
        (dleaf11) 
        (dleaf12))
       (dleaf2)) 
      (d2)) 
     (int 
      (i1) 
      (i11 
       (ileaf11) 
       (i2 
        (ileaf21) 
        (ileaf22))
       (ileaf12))
      (i2)))))

