;;; cw-examples.lisp

(in-package cw-examples)
;(h:rt h:hh)
(set-dispatch-macro-character #\# #\~ 'perlre:|#~-reader|)


(defmacro proc (fn)
  `(progn 
     (clim-sys:make-process (lambda () ,fn))
     (sleep 2)))

#|
(cw:list-dir (user-homedir-pathname))    ; with vertical scroll bars
|#

(defun run-all-examples ()
  (proc (cw:calendar))
  (proc (cw:digital-clock-eu))
  (proc (cw:clock))
  (proc (cw:treeview stgtree))
  (proc (cw:treeview (sym2stg symtree)))
  (proc (cw:list-dir (user-homedir-pathname)))
  ;(proc (cw:list-dir "*"))

  (proc (class-browser1 'number))
  (proc (class-browser2 'condition))
  ;(proc (pkg-doc))
  (pkg-doc) ; creates a separate process itself
  (proc (icd-test icds))
  (proc (view-exe)))

;-----------------------------------------
;1) ICD BROWSER
;-----------------------------------------
(cw:define-node-methods 
  :nc node-88)

(define-application-frame icd9it (tree)
 ((info :accessor info :initform ""))
  (:command-table (icd9it :inherit-from (tree)))
  (:menu-bar cw:tree)
  (:panes 
   (tree :application :display-function 'display-tree :incremental-redisplay t :end-of-line-action :allow :end-of-page-action :allow)
   (info :application :display-function 'show-childreno :incremental-redisplay t))
	(:layouts (double (horizontally () tree (make-pane 'clim-extensions:box-adjuster-gadget) info))))

(defmethod show-childreno ((f icd9it) p)
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
  (cw:tree-view (make-instance 'node-88 :name key :show-children t) 'icd9it 'icd :pretty-name "icd9it" :right 800))


;-----------------------------------------
;2) CLASS-BROWSER
;-----------------------------------------
(cw:define-node-methods 
  :nc node-cb
  :cc symbol
  :cy eql
  :nn (class-name (find-class (cw:name cw:n)))
  :gc  (mapcar 'class-name (closer-mop:class-direct-subclasses (find-class (cw:name cw:n))))
  :cp (closer-mop:class-direct-subclasses (find-class cw:n)))

;;; one-pane
(defun class-browser1 (key) ;initial key
  (cw:tree-view (make-instance 'node-cb 
                            :name key  ; key is a symbol which names a class <----- 
                            :show-children t) 'tree 'symbol))

;;; two-panes
;------------------------------------------------------------------------
(define-application-frame class-browser (cw:tree)
 ((info :accessor info :initform ""))
  (:command-table (class-browser :inherit-from (cw:tree)))
  (:menu-bar cw:tree)
  (:panes 
   (tree :application :display-function 'cw:display-tree :incremental-redisplay t :end-of-line-action :allow :end-of-page-action :allow)
   (info :application :display-function 'show-childreno :incremental-redisplay t :end-of-line-action :allow :end-of-page-action :allow))
	(:layouts (double (horizontally () tree (make-pane 'clim-extensions:box-adjuster-gadget) info))))

(defmethod show-childreno ((f class-browser) p) (describe (ignore-errors (find-class (info *application-frame*))) p))

(define-presentation-type item () :inherit-from 'string)
(define-presentation-method present (item (type item) s v &key) (format s "~a" item))
(define-class-browser-command xxx ((item 'item :gesture :select)) (setf (info *application-frame*) item))

(defun class-browser2 (key)
  (cw:tree-view (make-instance 'node-cb :name key :show-children t) 'class-browser 'item :right 800))

;-----------------------------------------
;3) PACKAGE-BROWSER, a simple draft of a package documentation
;-----------------------------------------
(defparameter pkg-list (sort (mapcar 'package-name (list-all-packages)) #'string<))

; from Peter Seibel's "manifest" quicklisp package
(defun present-symbols%% (pkg)
  (loop for what in manifest::*categories*
        for names = (manifest::names pkg what)
        when names collect
        (cons what (remove-if 'consp names))))  ; remove "setf functions" e.g. AREF (SETF AREF)

(cw:define-node-methods 
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

;(defmethod disp-pkg-info ((f pkg-doc) p) (print (info *application-frame*) p))
  (defmethod disp-pkg-info ((f pkg-doc) p) 
  (let* ((pkg (intern (string-upcase (cw:item-name (cw:group *application-frame*)))))
         (sym (find-symbol (string-upcase (info *application-frame*)) pkg)))
  (describe sym p)))

(define-pkg-doc-command show-info ((item 'string :gesture :select))   
  (setf (info *application-frame*) item))

;insert clrhash
(define-pkg-doc-command (packages :menu t) ()
  (let ((pkg (menu-choose pkg-list)))
    (cw:t2h (cw:sym2stg (cw-utils::pack-leaves (cons (intern pkg) (present-symbols%% pkg)))))
    (setf (cw:group *application-frame*) (make-instance 'node-pkg :name (string-downcase pkg) :show-children t))
    (redisplay-frame-panes *application-frame* :force-p t)))

(defun pkg-doc (&optional (pkg :clim))
  (clim-sys:make-process 
    (lambda ()
      (cw:t2h (cw:sym2stg (cw-utils::pack-leaves (cons pkg (present-symbols%% pkg)))))
      (cw:tree-view (make-instance 'node-pkg :name (string-downcase pkg)
                                   :show-children t) 'pkg-doc 'string :right 800))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; https://rosettacode.org/wiki/Last_letter-first_letter#Common_Lisp

;;; return all the words that start with an initial letter
(defun filter-with-init (words init)
  (remove-if-not (lambda (word) (eql init (aref word 0))) words))
;;; produce a hash table whose key is the initial letter of a word and whose value is
;;; a list of the words that start with that initial letter
(defun group-by-first-letter (words)
  (let ((map_letters (make-hash-table))
        (inits (remove-duplicates (mapcar (lambda (word) (aref word 0)) words))))
    (dolist (init inits map_letters)
      (setf (gethash init map_letters) (filter-with-init words init)))))

; note ON LINUX 
(defparameter ht
  (group-by-first-letter
    (cdr
      (#~d'./'
       (uiop:run-program 
         "find . -type f -executable -maxdepth 1 -printf '%p'" :output 'string)))))

(defun view-exe ()   ; ev dir
  "list executables on a linux computer"
  (cw:treeview 
    (cw-utils::pack-leaves
      (cons "executables"
            (loop for k in (sort (alexandria:hash-table-keys ht) 'string<)
                  collect (cons (lol:mkstr k) (sort (gethash k ht) 'string<)))))))

;-------
(defun view-deb-available ())
(defun view-deb-installed ())

#|
https://askubuntu.com/questions/598384/how-do-i-get-a-list-of-all-available-packages-from-the-repositories-that-are-con
apt-get update
apt-cache dump | grep -oP 'Package: \K.*' | sort -n          # available

https://askubuntu.com/questions/1056448/how-to-get-the-list-of-installed-packages-on-ubuntu-debian-w-o-command-or-w-o-lo
awk -vRS= '/Status: install/ {print $2}' /var/lib/dpkg/status     #installed
|#


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
