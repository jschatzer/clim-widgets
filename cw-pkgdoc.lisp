;;; cw-pkgdoc
(in-package clim-widgets)
(defpackage cw-pkgdoc (:use clim clim-lisp))
(in-package cw-pkgdoc)

;------------------------------------
; app-logic and helper
;------------------------------------
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

(defun mktree (l) (mapcar (lambda (x) (if (null (cdr x)) x (cons (key (car x)) (mapcar #'list x)))) (pack l)))

; from Peter Seibel's "manifest" quicklisp package
(defun present-symbols%% (pkg)
  (loop for what in manifest::*categories*
        for names = (manifest::names pkg what)
        when names collect
        (cons what (remove-if 'consp names))))  ; remove "setf functions" e.g. AREF (SETF AREF)

(defun symbol-tree (p)
  "if pkg is clim, divide constants into color-names and other-constants"
  (if (eql p :clim)
    (reverse (cons (clim-colors) 
                   (mapcar (lambda (x) (cons (car x) (mktree (cdr x))))              
                           (cdr (reverse (present-symbols%% p))))))
    (mapcar (lambda (x) (cons (car x) (mktree (cdr x)))) (present-symbols%% p))))

;------------------------------------
; gui
;------------------------------------
; nodes should not be sensible <-----

(defmethod cw:disp-info ((f cw:tree-info) p) (describe (cw::info *application-frame*) p))

(define-presentation-type sym () :inherit-from 'symbol)

(define-command (show-info :command-table cw:tree-info) ((item 'sym :gesture :select))   
  (setf (cw::info *application-frame*) item))

(defmethod cw::inf :around ((n cw:tree-info))
  (unless (slot-boundp n 'inf)
      (setf (cw::inf n) 
            (mapcar (lambda (x) 
                      (if (gethash x cw::*nodes*) 
                        (make-instance 'cw:tree-info :sup x :info x)
                        x))
                    (gethash (cw::sup n) cw::*nodes*))))
  (call-next-method))

(define-command (packages :command-table cw:tree-info :menu t) ()
  (menu-choose (sort (mapcar 'package-name (list-all-packages)) #'string<)))

(defun tview (tree key)
  (cw::t2h tree)
  (cw:tree-info-view (make-instance 'cw::node1 :sup key :disp-inf t) 'sym))

(defun pkg-doc (pkg) (tview (list (cons pkg (symbol-tree pkg))) pkg))
