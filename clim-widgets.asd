;;;; clim-widgets.asd

(asdf:defsystem #:clim-widgets
  ;  :version "0.1  27.12.2014"
  :version "0.2  20.10.2016"
  :description "small collection of clim widgets"
  :author "<schatzer.johann@gmail>"
  :license "BSD Simplified"
  :depends-on 
  (mcclim
    simple-date-time local-time ;calendar
    ; for examples only 
    perlre nsort
    cl-fad closer-mop manifest)
  :serial t
  :components 
  ((:file "package")
   (:file "cw-utils")
   (:file "clim-widgets")
   (:file "calendar")
   (:file "clock")
   (:file "tree-view")
   (:file "cw-examples")
;   (:file "cw-test")
   ))

