;;;; package.lisp

#|There is no way to inherit the internal symbols of another package; 
to refer to an internal symbol, the user must either make that symbol's home package current, 
use a qualifier, or import that symbol into the current package.

use-package, unlike import, does not cause any new symbols to be present 
in the current package but only makes them accessible by inheritance.|#

(defpackage cw-utils 
  (:use clim clim-lisp)
  (:export 
    textsize
    t2h nodes    t2h-r
    pack key
    sym2stg))

(defpackage cw-calendar 
  (:use clim clim-lisp)
  (:export calendar))

(defpackage cw-clock 
  (:use clim clim-lisp)
  (:export clock digital-clock-eu))

(defpackage cw-treeview 
  (:use clim clim-lisp cw-utils)
  (:export
    list-dir treeview ; treeview-strings treeview-symbols 
    item-name sup
    children c-nodep childnode-is-youngestsibling
    tree node leaf  ; classes
    tree-view group display-tree
    inf-meth n))

(defpackage clim-widgets
  (:nicknames cw)
  (:use clim clim-lisp cw-utils cw-calendar cw-clock cw-treeview)
  (:export  t2h-r
    calendar 
    clock digital-clock-eu
    ;---------------------------------
    ;tree-view  -- export test -- names may change
    ;---------------------------------
    list-dir treeview ;treeview-strings treeview-symbols
    item-name sup  ; sup ev remove?
    children c-nodep childnode-is-youngestsibling
    tree node leaf  ; classes
    tree-view group display-tree
    inf-meth n
    t2h nodes sym2stg key pack textsize))

(defpackage cw-examples (:use clim clim-lisp clim-widgets))
;(defpackage cw-test (:use clim clim-lisp clim-widgets))
