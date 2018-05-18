;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: Cl-user -*-

(in-package :cl-user)

#+Genera
(scl:defsystem joshua-portable
    (:pretty-name "Joshua Portable"
     :short-name "JP"
     :default-pathname "joshua:portable;code;"
     :journal-directory "joshua:portable;code;patch;"
     :bug-reports t
     :advertised-in (:disk-label :herald :finger)
     :source-category (:basic)
     :distribute-sources t
     :distribute-binaries t
     :maintaining-sites (:scrc :jamaica-plain :MIT))
  (:serial-definitions
    "packaged"
    "borrowin"
    "mapfvars"
    "mapforms"
    "readnnwr"
    "preddefs"
    "predicat"
    "unificat"
    "predprot"
    "clos-heaps"
    "predimpl" 
    "rete" 
    "matcher"
    "discrimi"
    "supplied"
    "rules"
    "ltms"
    "objectmo"
    "cfs"))

#-genera
(eval-when (:execute :load-toplevel)
  (let* ((loading-file *load-truename*)
         (host (pathname-host loading-file))
         (device (pathname-device loading-file))
         (home-dir (butlast (pathname-directory loading-file)))
         (wildcard-dir (append home-dir (list :wild-inferiors))))
    (let ((home (make-pathname :directory home-dir :host host :device device))
	  (wildcard (make-pathname :directory wildcard-dir :host host :device device)))
      (setf (logical-pathname-translations "joshua")
	    `(("home;*.*" ,home)
	      ("**;*.*" ,wildcard)
	       )))))

#+allegro
(defsystem joshua
    (:pretty-name "Joshua"
     :default-module-class separate-destination-module
     :default-pathname "joshua:code;")
  (:serial
    "packaged"
    "borrowin"
    "mapfvars"
    "mapforms"
    "readnnwr"
    "preddefs"
    "predicat"
    "unificat"
    "predprot"
    "clos-heaps"
    "predimpl" 
    "rete" 
    "matcher"
    "discrimi"
    "supplied"
    "rules"
    "ltms"
    "objectmo"
    "cfs"))


#+mcl
(clim-defsys:defsystem joshua
    (:default-pathname "joshua:code;")
  ("packaged" :language :lisp-unix)
  ("borrowin" :load-before-compile ("packaged") :language :lisp-unix)
  ("mapfvars" :load-before-compile ("borrowin") :language :lisp-unix)
  ("mapforms" :load-before-compile ("mapfvars") :language :lisp-unix)
  ("readnnwr" :load-before-compile ("mapforms") :language :lisp-unix)
  ("preddefs" :load-before-compile ("readnnwr") :language :lisp-unix)
  ("predicat" :load-before-compile ("preddefs") :language :lisp-unix)
  ("unificat" :load-before-compile ("predicat") :language :lisp-unix)
  ("predprot" :load-before-compile ("unificat") :language :lisp-unix)
  ("clos-heaps" :load-before-compile ("predprot") :language :lisp-unix)
  ("predimpl" :load-before-compile ("clos-heaps") :language :lisp-unix)
  ("rete" :load-before-compile ("predimpl") :language :lisp-unix)
  ("matcher" :load-before-compile ("rete") :language :lisp-unix)
  ("discrimi" :load-before-compile ("matcher") :language :lisp-unix)
  ("supplied" :load-before-compile ("discrimi") :language :lisp-unix)
  ("rules" :load-before-compile ("supplied") :language :lisp-unix) 
  ("ltms" :load-before-compile ("rules") :language :lisp-unix)
  ("objectmo" :load-before-compile ("ltms") :language :lisp-unix)
  ("cfs" :load-before-compile ("ltms") :language :lisp-unix))
