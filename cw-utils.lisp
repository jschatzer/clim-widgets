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

;;;************************************************************
; form http://www.ic.unicamp.br/~meidanis/courses/problemas-lisp/L-99_Ninety-Nine_Lisp_Problems.html
(defmethod key ((s symbol)) (#~s'-.*''(symbol-name s)))
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

(defun sym2stg (lst)
  "transform a symbol-tree into a string-tree"
  (cond ((null lst) nil)
        ((atom lst) (string-downcase (if (stringp lst) lst (symbol-name lst))))
        (t (cons (sym2stg (car lst)) (sym2stg (cdr lst))))))

;;;************************************************************
; :smaller :larger don't work for now
(define-symbol-macro textsize
  (menu-choose '(:tiny :very-small :small :normal :large :very-large :huge)))

