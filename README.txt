Clim-Widgets should/could be a small collection of clim widgets. 

For now there is only one widget: a calendar - see x button for an alternative layout

(ql:quickload :clim-widgets)

Run

(clim-widgets:calendar)

or

(cw:calendar 9 1974)

----------------------------------------
21.1.2015 try an example of treewidget
code from http://osdir.com/ml/mcclim-devel/2009-08/msg00010.html

(clim-widgets::view-directory "/")
