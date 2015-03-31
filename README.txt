Clim-Widgets should/could be a small collection of clim widgets. 

For now there is only one widget: a calendar - see x button for an alternative layout

(ql:quickload :clim-widgets)

Run

(clim-widgets:calendar)

or

(cw:calendar 9 1974)

----------------------------------------
25.1.2015, try an example of treewidget
code from http://osdir.com/ml/mcclim-devel/2009-08/msg00010.html

(clim-widgets::view-directory "/")


----------------------------------------
2.2.2015, two more examples displaying a tree


Trivial example to view a tree of nested lists

(cw::treeview cw::lst "icd")


Small classification example, with instructions

(cw::icdview cw::icds "d|diagnosi|")


----------------------------------------
17.2.2015, a class "browser"

(cw::class-view cw::classes t)


----------------------------------------
26.3.2015, class browser #2

(cw::condition-view cw::classes t) or (cw::condition-view cw::classes 'condition) 

;==============
Now there is also a spartane plus-icon and a line or grid system, the last one not yet correctly working

The icons can be changed by
(setf cw::*icon* 'cw::plus) or (setf cw::*icon* 'cw::triangle)

The grid can be shown or suppressed by 
(setf cw::*grid* nil) (setf cw::*grid* t) 
;==============
