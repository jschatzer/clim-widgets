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
