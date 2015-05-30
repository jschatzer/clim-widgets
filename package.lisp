;;;; package.lisp

(defpackage clim-widgets
  (:nicknames cw)
  (:use clim clim-lisp)
  (:export calendar

   ;---------------------------------
   ;tree-view  -- export test -- names may change
   ;---------------------------------
   tree node  ; classes
   display-tree  ; 9.5.15
   tree-view
   t2h ; *nodes*
   key pack
;   tree-info tree-info-view disp-info
   ))

(defpackage cw-test 
  (:use clim clim-lisp))
