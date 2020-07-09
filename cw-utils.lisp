; cw-utils.lisp

(in-package cw-utils)

;;;************************************************************
;;; helper functions to put tree nodes with their respective values into a hash-table for faster retreeving
;;;************************************************************
(defparameter nodes (make-hash-table :test #'equal))

(defun t2h (tree)
  "tree to hash-table, key is a superior, val is a list of inferiors"
  (mapc (lambda (x)
          (cond ((and (atom (car x)) (null (cdr x))))
                (t (setf (gethash (car x) nodes) (mapcar #'car (cdr x))) (t2h (cdr x)))))
        tree))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,,,
;; 16.10,2019 geht
;gehen
;(pack-leaves '("a" "b" ("c" "d")))  ---> (("a" ("b") ("c" ("d")))
;(treeview (pack-leaves '("a" "b" ("c" "d"))))
;(treeview (pack-leaves (pkg-doc:pkg-tree "CLIM")))
(defun pack-leaves (l)
  "recursive pack every leaf-node into parentheses"
  (list
    (cons (car l)
          (mapcar (lambda (x)
                    (cond ((atom x) (list x))
                          (t (if (every 'atom x) 
                               (cons (car x) (mapcar 'list (cdr x)))
                               (cons (car x) (pack-leaves (cdr x)))))))
                  (cdr l)))))

;-------------------------------------------------------------------------

(defun t2h-r (l)
  "insert into an emtpy ht"
  (clrhash nodes)
  (t2h-r%  l))

(defun t2h-r% (l)     ;t2h recursive
  (cond ((null l) nil)
        ((listp l) (setf (gethash (car l) nodes) (mapcar (lambda (x) (if (listp x) (car x) x)) (cdr l))) 
                   (mapc 't2h-r% (cdr l)))))

;;;************************************************************
; form http://www.ic.unicamp.br/~meidanis/courses/problemas-lisp/L-99_Ninety-Nine_Lisp_Problems.html
;(defmethod key ((s symbol)) (#~s'-.*''(symbol-name s)))
(defmethod key ((s symbol)) (lol:symb (#~s'-.*''(symbol-name s))))
(defmethod key ((s string)) (#~s'-.*'' s))

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

;;;************************************************************
;((:FUNCTION ("NSTRING" (NSORT:NSTRING-GREATERP) (NSORT:NSTRING-LESSP) (NSORT:NSTRING-NOT-GREATERP) (NSORT:NSTRING-NOT-LESSP)) (NSORT:NSTRING<)
;  (NSORT:NSTRING<=) (NSORT:NSTRING>) (NSORT:NSTRING>=))
; (:MACRO (NSORT::DEFNSF)))
(defun rmapcar (fn &rest args)
 (if (some #'atom args)
   (apply fn args)
   (apply #'mapcar (lambda (&rest args) (apply #'rmapcar fn args)) args)))

; hh --  use lol:str  <---
(defun stg (sym)
  (with-output-to-string (s) (princ sym s)))

(defun sym2stg (l)
  "transform a symbol-tree into a downcase string-tree"
  (rmapcar (alexandria:compose 'string-downcase 'symbol-name) l))

;************************************************************
; :smaller :larger don't work for now
(define-symbol-macro textsize
  (menu-choose '(:tiny :very-small :small :normal :large :very-large :huge)))

