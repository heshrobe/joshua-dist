;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: Cl-user -*-

#+Genera
(scl:defsystem joshua-portable-tests
    (:pretty-name "Joshua Portable Tests"
     :short-name "JPT"
     :default-pathname "joshua:portable;tests;"
     :journal-directory "joshua:portable;tests;patch;"
     :bug-reports t
     :advertised-in (:disk-label :herald :finger)
     :source-category (:basic)
     :distribute-sources t
     :distribute-binaries t
     :maintaining-sites (:scrc :jamaica-plain))
  (:serial-definitions
   "joshua-tests"
   "gramps-test"
   "object-modeling-tests"
   "tms-tests")
  (:module for-non-genera ("compile-files") (:type :lisp-example)))

#+allegro
(defsystem joshua-tests
    (:pretty-name "Joshua Portable Tests"
     :default-module-class joshua:joshua-module
     :default-pathname "joshua:tests;")
  (:serial
   "joshua-tests"
   "gramps-test"
   "object-modeling-tests"
   "tms-tests"
))


#+mcl
(clim-defsys:defsystem joshua-tests
    (:default-pathname "joshua:tests;")
   ("joshua-tests" :language :joshua :language :lisp-unix)
   ("gramps-test" :load-before-compile ("joshua-tests") :language :joshua)
   ("object-modeling-tests" :load-before-compile ("gramps-test") :language :joshua)
   ("tms-tests" :load-before-compile ("object-modeling-tests") :language :joshua))
