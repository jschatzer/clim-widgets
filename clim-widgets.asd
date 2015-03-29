;;;; clim-widgets.asd

(asdf:defsystem #:clim-widgets
  :version "0.1  27.12.2014"
  :description "small collection of clim widgets"
  :author "<schatzer.johann@gmail>"
  :license "BSD Simplified"
  :depends-on (mcclim 
               simple-date-time local-time ;calendar
            ; for examples only 
               cl-fad perlre closer-mop)
  :serial t
  :components ((:file "package")
               (:file "clim-widgets")
               (:file "calendar")
               (:file "tree-view")
               (:file "cw-test")
               ))

