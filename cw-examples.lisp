;;; cw-examples.lisp

(in-package cw-examples)
(h:rt h:hh)

(defmacro proc (fn)
  `(progn 
     (clim-sys:make-process (lambda () ,fn))
     (sleep 2)))

#|
to do
(cw:treeview cw-examples::stgtree)       ; use treeview with symbols, too

(cw:list-dir (user-homedir-pathname))    ; with vertical scroll bars

(cw-examples::class-browser1 'number)

(cw-examples::class-browser2 'condition) ; info wird in tree pane unten angehängt

(cw-examples::pkg-doc)   ;remove or make it very simple




#;(defun run-all-examples ()
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
|#

(defun run-all-examples ()
  (proc (cw:calendar))
  (proc (cw:digital-clock-eu))
  (proc (cw:clock))

  ;(proc (cw:treeview-strings stgtree "icd"))
  (proc (cw:treeview stgtree))

;  (proc (cw:treeview-symbols symtree 'icd))
  (proc (cw:list-dir (user-homedir-pathname)))

  (proc (class-browser1 'number))
  (proc (class-browser2 'condition))

  (proc (pkg-doc))

  ;(proc (icd-test icds "d|diagnosi|")))
  (proc (icd-test icds)))


;-----------------------------------------
;1) ICD BROWSER
;-----------------------------------------
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
        (#~d'(Not[ae]:|Escl.:|Incl.:)'r (info *application-frame*))))

(define-presentation-type icd () :inherit-from 'string)
(define-presentation-method present (item (type icd) s v &key) (format s "~a" (#~s'.*\|(.+)\|.*'\1's item)))
(define-icd9it-command xx ((item 'icd :gesture :select)) (setf (info *application-frame*) (#~s'.*\|.+\|(.*)'\1's item)))

(defun icd-test (tree &optional (key (caar tree)))
  (cw:t2h tree)
  (cw:tree-view (make-instance 'node-88 :sup key :disp-inf t) 'icd9it 'icd :pretty-name "icd9it" :right 800))


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
                            :disp-inf t) 'tree 'symbol))

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

(defmethod disp-info ((f class-browser) p) (describe (ignore-errors (find-class (info *application-frame*))) p))

(define-presentation-type item () :inherit-from 'string)
(define-presentation-method present (item (type item) s v &key) (format s "~a" item))
(define-class-browser-command xxx ((item 'item :gesture :select)) (setf (info *application-frame*) item))

(defun class-browser2 (key)
;  (cw:tree-view (make-instance 'node-cb :sup key :disp-inf t) 'item 'class-browser :right 800))
;  (cw:tree-view (make-instance 'node-cb :sup key :disp-inf t) 'item :pretty-name "class-browser" :right 800))
  (cw:tree-view (make-instance 'node-cb :sup key :disp-inf t) 'class-browser 'item :right 800))

;-----------------------------------------
;3) PACKAGE-BROWSER, a simple draft of a package documentation
;  remove it or 1) make it very simple and link to pkg-doc  <---
;-----------------------------------------
; 1) app-logic and helper
; ev todo: remove second common-lisp from pkg-menu, remove special forms from other categories, rm cclim colors from variable

;(defparameter pkg-list (cons :common-lisp (sort (mapcar (lambda (x) (intern (package-name x) :keyword)) (list-all-packages)) #'string<)))

;(defparameter pkg-list (sort (mapcar (lambda (x) (intern (package-name x) :keyword)) (list-all-packages)) #'string<))

(defparameter pkg-list (sort (mapcar 'package-name (list-all-packages)) #'string<))



; ; color-names are mixed case strings, without + or -, e.g. "antique white"
; (defparameter colors (mapcar (lambda (x) (#~s'.*'+\&+' (#~s' '-'g x))) (mapcar #'fourth clim-internals::*xpm-x11-colors*)))
; (defun constant-p (s) (if (or (constantp s) (#~m'^\+.+\+$' (symbol-name s))) s))
; (defparameter clim-es (mapcar #'symbol-name (remove-if-not #'constant-p (loop for s being the external-symbols of :clim collect s))))
; (defun clim-non-color-contstants () (set-difference clim-es colors :test 'string-equal))
; (defun clim-color-contstants () (intersection clim-es colors :test 'string-equal))


;(defun mktree (l) (mapcar (lambda (x) (if (null (cdr x)) x (cons (cw:key (car x)) (mapcar #'list x)))) (cw:pack l)))
(defun mktree (l) (mapcar (lambda (x) (if (null (cdr x)) x (cons (cw:key (car x)) x))) (cw:pack l)))

#|
(defun clim-colors ()
  "divide clim-constants into colors and other constants"
  (let ((o (mapcar (lambda (x) (find-symbol x :clim)) (sort (clim-non-color-contstants) #'string<)))     ;other constants
        (c (mapcar (lambda (x) (find-symbol x :clim)) (sort (clim-color-contstants) #'nsort:nstring<)))) ;color-name constants
    (cons :constant (cons (cons 'color-names (mktree c)) (mktree o)))))
(defun spec-op () (cons 'special-operator (mktree (sort (remove-if-not #'special-operator-p (loop for s being the external-symbols of :cl collect s)) #'string<))))
|#

; from Peter Seibel's "manifest" quicklisp package
(defun present-symbols%% (pkg)
  (loop for what in manifest::*categories*
        for names = (manifest::names pkg what)
        when names collect
        (cons what (remove-if 'consp names))))  ; remove "setf functions" e.g. AREF (SETF AREF)

#|
(defun symbol-tree (p)
  "if pkg is clim, divide constants into color-names and other-constants
  if pkg is cl, show special forms"
  (cond ((eql p :clim) (reverse (cons (clim-colors) (mapcar (lambda (x) (cons (car x) (mktree (cdr x)))) (cdr (reverse (present-symbols%% p)))))))
        ((eql p (or :common-lisp :cl)) (cons (spec-op) (mapcar (lambda (x) (cons (car x) (mktree (cdr x)))) (present-symbols%% p))))
        (t (mapcar (lambda (x) (cons (car x) (mktree (cdr x)))) (present-symbols%% p)))))
|#

;simplify
(defun symbol-tree (p) (mapcar (lambda (x) (cons (car x) (mktree (cdr x)))) (present-symbols%% p)))

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

;(defmethod disp-info ((f pkg-doc) p) (describe (info *application-frame*) p))
(defmethod disp-info ((f pkg-doc) p) (describe (h:sym (info *application-frame*) p)))

(define-pkg-doc-command show-info ((item 'string :gesture :select))   
  (setf (info *application-frame*) item))

;creates a separtate window for every new package
(define-pkg-doc-command (packages :menu t) ()
  (let ((pkg (menu-choose pkg-list)))
   (tview (list (cons pkg (symbol-tree pkg))) pkg)))
;   (tview (pkg-doc::pkg-tree pkg) pkg)))



(defun tview (tree key)
  (cw:t2h-r tree)
;  (cw:tree-view (make-instance 'node-pkg :sup key :disp-inf t) 'string 'pkg-doc :right 800))
;  (cw:tree-view (make-instance 'node-pkg :sup key :disp-inf t) 'string :pretty-name "pkg-doc" :right 800))   ; 11.10.19
  (cw:tree-view (make-instance 'node-pkg :sup key :disp-inf t) 'pkg-doc 'string :right 800))

;  (defun tview (tree key)
;  ;  (cw:t2h tree)
;    (cw:t2h-r tree)
;    (cw:tree-view (make-instance 'node-pkg :sup 
;                                 ;key
;                                 (lol:symb key)
;                                 :disp-inf t) 'string 'pkg-doc :right 800))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,,
(define-pkg-doc-command (packages :menu t) ()
  (let ((pkg (menu-choose pkg-list)))
;   (tview (cw:sym2stg (cw-utils::pack-leaves (cons pkg (present-symbols%% (h:kwd pkg))))))))

;(setf (cw:group *application-frame*) (make-instance 'node-pkg :sup (package-name pkg) :disp-inf t))
(setf (cw:group *application-frame*) (make-instance 'node-pkg :sup (h:kwd pkg) :disp-inf t))

    (redisplay-frame-panes *application-frame* :force-p t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,,


;;; geht
  (defun tview (tree &optional (key (caar tree)))
    (cw:t2h tree)
  ;  (cw:t2h-r tree)
    (cw:tree-view (make-instance 'node-pkg :sup key
                                 :disp-inf t) 'pkg-doc 'string :right 800))

;;; geht
  (defun tview (tree key)
    (cw:t2h tree)
  ;  (cw:t2h-r tree)
    (cw:tree-view (make-instance 'node-pkg :sup (string-downcase key)
                                 :disp-inf t) 'pkg-doc 'string :right 800))

;;;;;;;;;;;;;;;;;;;;;;;;;,
(define-pkg-doc-command (packages :menu t) ()
  (let ((pkg (menu-choose pkg-list)))
    (setf (cw:group *application-frame*) (make-instance 'node-pkg :sup 
;                                                        (h:sym pkg) 
                                                        (string-downcase pkg) 

                                                        :disp-inf t))
    (redisplay-frame-panes *application-frame* :force-p t)))



(define-pkg-doc-command (packages :menu t) ()
  (let ((pkg (menu-choose pkg-list)))
    (setf (cw:group *application-frame*) (h:kwd pkg))
    (redisplay-frame-panes *application-frame* :force-p t)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,
;; geht, macht aber neues frame
(define-pkg-doc-command (packages :menu t) ()
  (let ((pkg (menu-choose pkg-list)))
;    (setf (cw:group *application-frame*) (h:kwd pkg))
    (tview (h:kwd pkg))))
;    (redisplay-frame-panes *application-frame* :force-p t)))



;; gehen
;(tview)
;(tview :nsort)
  (defun tview (&optional (key :clim))
    (cw:t2h (cw:sym2stg (cw-utils::pack-leaves (cons key (present-symbols%% key)))))
    (cw:tree-view (make-instance 'node-pkg :sup (string-downcase key)
                                 :disp-inf t) 'pkg-doc 'string :right 800))


#|
(defun pkg-doc (&optional (pkg :clim)) 
  (let ((p (string-downcase (package-name pkg))))
    (tview (list (cons p (cw:sym2stg (symbol-tree pkg)))) p)))
|#

(defun pkg-doc (&optional (pkg "CLIM")) 
  (let ((p (string-downcase pkg)))
    (tview (list (cons p (cw:sym2stg (symbol-tree pkg)))) p)))


#|
;;;; am 27.8.2019 auskommentiert  <----
;from pkg-doc
(defun pkg-doc (&optional (pkg "CLIM"))
   (tview  (pkg-doc::pkg-tree pkg) pkg))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,,,,,,
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,,,,,,
;;;;;;;;;;
(defparameter pkg-list (sort (mapcar 'package-name (list-all-packages)) #'string<))

; from Peter Seibel's "manifest" quicklisp package
(defun present-symbols%% (pkg)
  (loop for what in manifest::*categories*
        for names = (manifest::names pkg what)
        when names collect
        (cons what (remove-if 'consp names))))  ; remove "setf functions" e.g. AREF (SETF AREF)

(cw:inf-meth 
  :nc node-pkg)

; 2) gui -- nodes should not be sensible
(define-application-frame pkg-doc (cw:tree)
 ((info :accessor info :initform ""))
  (:command-table (pkg-doc :inherit-from (cw:tree)))
  ;(:menu-bar cw:tree)
  (:panes 
   (tree :application :display-function 'cw:display-tree :incremental-redisplay t :end-of-line-action :allow :end-of-page-action :allow)
   (info :application :display-function 'disp-pkg-info :incremental-redisplay t))
	(:layouts (double (horizontally () tree (make-pane 'clim-extensions:box-adjuster-gadget) info))))

;(defmethod disp-info ((f pkg-doc) p) (describe (info *application-frame*) p))

(defmethod disp-pkg-info ((f pkg-doc) p) (describe (h:sym (info *application-frame*) p)))


;(defmethod disp-info ((f pkg-doc) p) (inspect (h:sym (info *application-frame*) p)))


(define-pkg-doc-command show-info ((item 'string :gesture :select))   
  (setf (info *application-frame*) item))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,
;; geht, macht aber neues frame
(define-pkg-doc-command (packages :menu t) ()
  (let ((pkg (menu-choose pkg-list)))
    (tview (h:kwd pkg))))

;geht, ohne neues frame
(define-pkg-doc-command (packages :menu t) ()
  (let ((pkg (menu-choose pkg-list)))
    (setf (cw:group *application-frame*) (make-instance 'node-pkg :sup (string-downcase pkg) :disp-inf t))
    (redisplay-frame-panes *application-frame* :force-p t)))

;; gehen
;(tview)
;(tview :nsort)
  (defun tview (&optional (key :clim))
    (cw:t2h (cw:sym2stg (cw-utils::pack-leaves (cons key (present-symbols%% key)))))
    (cw:tree-view (make-instance 'node-pkg :sup (string-downcase key)
                                 :disp-inf t) 'pkg-doc 'string :right 800))
(defun pkg-doc (&optional (pkg :clim))
  (clim-sys:make-process 
    (lambda ()
      (cw:t2h (cw:sym2stg (cw-utils::pack-leaves (cons pkg (present-symbols%% pkg)))))
      (cw:tree-view (make-instance 'node-pkg :sup (string-downcase pkg)
                                   :disp-inf t) 'pkg-doc 'string :right 800))))

;;display info geht noch nicht
;; rename package-doc, pkg-doc
;run
;(tview)
;(tview :nsort)


;------------------------
; example tree data
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
