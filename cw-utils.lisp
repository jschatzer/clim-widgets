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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,,,



#|
16.10.2019
* (pkg-doc:pkg-tree "CL-FAD")

("CL-FAD"
 ("function:-" "function:-canonical-pathname"
  ("function:-copy-" "function:-copy-file" "function:-copy-stream")
  "function:-delete-directory-and-files"
  ("function:-directory-" "function:-directory-exists-p"
   "function:-directory-pathname-p")
  "function:-file-exists-p" "function:-list-directory"
  ("function:-merge-pathnames-"
   ("function:-merge-pathnames-as-" "function:-merge-pathnames-as-directory"
    "function:-merge-pathnames-as-file"))
  "function:-open-temporary"
  ("function:-pathname-" "function:-pathname-absolute-p"
   ("function:-pathname-as-" "function:-pathname-as-directory"
    "function:-pathname-as-file")
   "function:-pathname-directory-pathname" "function:-pathname-equal"
   "function:-pathname-parent-directory" "function:-pathname-relative-p"
   "function:-pathname-root-p")
  "function:-walk-directory")
 ("macro:-"
  ("macro:-with-" "macro:-with-open-temporary-file"
   "macro:-with-output-to-temporary-file"))
 ("variable:-" "variable:-*default-template*")
 ("condition:-" "condition:-cannot-create-temporary-file"
  "condition:-invalid-temporary-pathname-template"))
*

=======
* (clim-pkg-doc::pkg-tree :cl-fad)

  (("CL-FAD"
    ("function:" 
     ("canonical-pathname") 
     ("copy-" 
      ("copy-file") 
      ("copy-stream")) 
     ("delete-directory-and-files")
     ("directory-" 
      ("directory-exists-p") 
      ("directory-pathname-p")) 
     ("file-exists-p") ("list-directory")
     ("merge-" 
      ("merge-pathnames-as-directory") 
      ("merge-pathnames-as-file")) 
     ("open-temporary")
     ("pathname-" 
      ("pathname-absolute-p") 
      ("pathname-as-directory") 
      ("pathname-as-file") 
      ("pathname-directory-pathname") 
      ("pathname-equal")
      ("pathname-parent-directory") 
      ("pathname-relative-p") 
      ("pathname-root-p"))
     ("walk-directory"))
     ("macro:" 
      ("with-" ("with-open-temporary-file") ("with-output-to-temporary-file"))) 
     ("variable:" ("*default-template*"))
     ("condition:" ("cannot-create-temporary-file") ("invalid-temporary-pathname-template"))))
     * 
|#


;-------------------------------------------------------------------------

(defun t2h-r (l)
  "insert into an emtpy ht"
  (clrhash nodes)
  (t2h-r%  l))


#|
;scheint richtig zu gehen, 21.4.2017
(defun t2h-r (l)     ;t2h recursive
  (cond ((null l) nil)
        ((listp l) (setf (gethash (car l) nodes) 
                         (mapcar (lambda (x) (if (listp x) (car x) x)) (cdr l))) 
                   (mapcar 't2h-r (cdr l)))))
|#


; ;;; das bis 25.4.19
; (defun t2h-r% (l)     ;t2h recursive
;   (cond ((null l) nil)
;         ((listp l) (setf (gethash (car l) nodes) (mapcar (lambda (x) (if (listp x) (car x) x)) (cdr l))) 
;                    (mapcar 't2h-r% (cdr l)))))
; ;;;;

;25.4.19
;mit mapc
(defun t2h-r% (l)     ;t2h recursive
  (cond ((null l) nil)
        ((listp l) (setf (gethash (car l) nodes) (mapcar (lambda (x) (if (listp x) (car x) x)) (cdr l))) 
                   (mapc 't2h-r% (cdr l)))))


#|
;;error the value "CLIM" is not of type list
;form oben (defun t2h (tree)
(defun t2h-r% (l) 
  "tree to hash-table, key is a superior, val is a list of inferiors"
  (mapc (lambda (x)
          (cond ((and (atom (car x)) (null (cdr x))))
                (t (setf (gethash (car x) nodes) (mapcar #'car (cdr x))) (t2h-r% (cdr x)))))
        l))
|#


#|
;* (new-pd::pkg-tree :cl-fad)

(o:p l 
 '("CL-FAD"
   ("function:-" 
    "function:-canonical-pathname" 
    ("function:-copy-" 
     "function:-copy-stream" 
     "function:-copy-file") 
    "function:-delete-directory-and-files"
    ("function:-directory-" 
     "function:-directory-pathname-p" 
     "function:-directory-exists-p") 
    "function:-file-exists-p" 
    "function:-list-directory"
    ("function:-merge-" 
     "function:-merge-pathnames-as-directory" 
     "function:-merge-pathnames-as-file") 
    "function:-open-temporary"
    ("function:-pathname-" 
     "function:-pathname-root-p" 
     "function:-pathname-relative-p" 
     "function:-pathname-parent-directory" 
     "function:-pathname-equal"
     "function:-pathname-directory-pathname" 
     ("function:-pathname-as-" 
      "function:-pathname-as-directory" 
      "function:-pathname-as-file")
     "function:-pathname-absolute-p")
    "function:-walk-directory")
    ("macro:-" 
     "macro:-with-output-to-temporary-file" 
     "macro:-with-open-temporary-file") 
    ("variable:-" "variable:-*default-template*")
    ("condition:-" "condition:-cannot-create-temporary-file" "condition:-invalid-temporary-pathname-template")))
|#


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

#|
;;orig, do not remove !! before testing cw:tests <------
;(cw:sym2stg '(a b nil t)) ; ("a" "b" NIL "t")
(defun sym2stg (lst)
  "transform a symbol-tree into a string-tree"
  (cond ((null lst) nil)
        ((atom lst) (string-downcase (if (stringp lst) lst (symbol-name lst))))
        (t (cons (sym2stg (car lst)) (sym2stg (cdr lst))))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
;gehen alle richtig,  what about this: ;((:FUNCTION ("NSTRING" (NSORT ???
(r-sym2stg '(a (nil c)))

(r-sym2stg '(a (b)))

(r-sym2stg '(a b nil t)) ; ("a" "b" NIL "t")

(r-sym2stg '(a b))
|#

;13.10.18 scheint gut zu gehen
;test, ev rename  r- for recursive
;(defun r-sym2stg (l)
(defun sym2stg (l)
  "transform a symbol-tree into a string-tree"
  (mapcar (lambda (x) 
            (cond ((atom x) (string-downcase (symbol-name x)))
                  ((notany 'consp x) (mapcar 'string-downcase (mapcar 'symbol-name x)))
                  (t (mapcar 'r-sym2stg x))))      ; das hatte ich bis 24.4.19 trotz dieses typw r-.. - pkg-doc ging damit gut!!!! <-----
          l))

(defun sym2stg (l)
  "transform a symbol-tree into a string-tree"
  (mapcar (lambda (x) 
            (cond ((atom x) (string-downcase (symbol-name x)))
                  ((notany 'consp x) (mapcar 'string-downcase (mapcar 'symbol-name x)))
                  (t (mapcar 'sym2stg x))))
          l))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 24.4.2019
;("a" ("b" "nil" "d" "t") "c")
;(o:rmapcar (h:compose 'string-downcase 'h:stg)  '(a (b nil d t) c))
;(rmapcar (h:compose 'string-downcase 'stg)  '(a (b nil d t) c))

;from onlisp
(defun rmapcar (fn &rest args)
 (if (some #'atom args)
   (apply fn args)
   (apply #'mapcar (lambda (&rest args) (apply #'rmapcar fn args)) args)))

; hh --  use lol:str  <---
(defun stg (sym)
  (with-output-to-string (s) (princ sym s)))

(defun sym2stg (l)
  "transform a symbol-tree into a string-tree"
  (rmapcar (h:compose 'string-downcase 'stg) l))

(defun sym2stg (l)
  "transform a symbol-tree into a downcase string-tree"
  (rmapcar (h:compose 'string-downcase 'symbol-name) l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





#|
;may be ok, because there are only single element lists?
;(cw:sym2stg '(a b)) -> ("a" "b" . "nil")  should be: ("a" "b")
; ("a" "b" "nil" "t" . "nil")
(defun sym2stg (lst)
  "transform a symbol-tree into a string-tree"
;  (cond ((null lst) nil)
  (cond ((null lst) "nil")
        ((atom lst) (string-downcase (if (stringp lst) lst (symbol-name lst))))
        (t (cons (sym2stg (car lst)) (sym2stg (cdr lst))))))

;(cw:sym2stg '(a b nil t)) ; ("a" "b" NIL "t")   should be ("a" "b" "nil" "t")
(defun sym2stg (lst)
  "transform a symbol-tree into a string-tree"
  (cond ((null lst) "nil")
        ;((atom lst) (string-downcase (if (stringp lst) lst (symbol-name lst))))
        ((atom lst) (string-downcase (prin1 lst)))
        (t (cons (sym2stg (car lst)) (sym2stg (cdr lst))))))
|#


;;;************************************************************
; :smaller :larger don't work for now
(define-symbol-macro textsize
  (menu-choose '(:tiny :very-small :small :normal :large :very-large :huge)))

