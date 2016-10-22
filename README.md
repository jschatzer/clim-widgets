Clim-Widgets should/could be a small collection of clim widgets. 


- calendar
- clock
- treeview



Examples:

(ql:quickload :clim-widgets)

;to see some examples in separate processes
(cw-examples::run-all-examples)



- CALENDAR

(cw:calendar)

(cw:calendar 9 1974)


- CLOCK

(cw:clock)

(cw:digital-clock-eu)


- TREEVIEW, some usage example, new api, may change

(cw:treeview-symbols cw-examples::symtree 'icd)

(cw:treeview-strings cw-examples::stgtree "icd")

(cw:list-dir (user-homedir-pathname))

(cw-examples::class-browser1 'number)

(cw-examples::class-browser2 'condition)

(cw-examples::pkg-doc)

(cw-examples::icd-test cw-examples::icds "d|diagnosi|")

-- Grid can be shown or suppressed
(setf cw::grid nil) (setf cw::grid t) 

-- Icons can be changed
(setf cw::icon 'cw::plus) 
(setf cw::icon 'cw::triangle)
(setf cw::icon 'cw::triangle2)

-- Textsize can be changed


;another treeview example
(ql:quickload :clim-pkg-doc)
(clim-pkg-doc:pkg-doc)



;--------------------------------------------------------------
;from old README, deprecatad
;--------------------------------------------------------------

- TREEVIEW, some usage example, definitive api not yet available

(cw-test::view-directory "/")  

(cw-test::treeview cw-test::lst "icd")

(cw-test::icdview cw-test::icds "d|diagnosi|")

(cw-test::view-classes cw-test::classes t)

(cw-test::view-classes-with-description cw-test::classes t) 

(cw-test::view-classes-with-description cw-test::classes 'condition)

(cw-test::pkg-doc)

(cw-test::pkg-doc :cl-fad)

Grid can be shown or suppressed
(setf cw::*grid* nil) (setf cw::*grid* t) 

Icons can be changed
(setf cw::*icon* 'cw::plus) 
(setf cw::*icon* 'cw::triangle)
(setf cw::*icon* 'cw::triangle2)

