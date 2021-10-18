;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: Cl-user -*-

(in-package :cl-user)
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

(asdf:defsystem joshua
  :name "Joshua"
  :description "Joshua Inference System"
  :maintainer "Howie Shrobe"
  :pathname "."
  ;; These two are necessary to support the named-readtable
  ;; feature.  They're probably innocuous even in a bare
  ;; bones environnent
  :depends-on (:cl-interpol :rutils) 
  :components (
	       (:file "packaged")
	       (:file "asdf-support" :depends-on ("packaged"))
	       (:file "borrowin" :depends-on ("packaged"))
	       (:file "mapfvars" :depends-on ("borrowin"))
	       (:file "mapforms" :depends-on ("mapfvars"))
	       (:file "readnnwr" :depends-on ("mapforms"))
               ;; Moved to joshua-developer
               ;; (:file "drei-interface" :depends-on ("readnnwr"))
	       (:file "preddefs" :depends-on ("readnnwr"))
	       (:file "predicat" :depends-on ("preddefs"))
	       (:file "unificat" :depends-on ("predicat"))
	       (:file "predprot" :depends-on ("unificat"))
	       (:file "clos-heaps" :depends-on ("predprot"))
	       (:file "predimpl" :depends-on ( "clos-heaps"))
	       (:file "rete" :depends-on ( "predimpl"))
	       (:file "matcher" :depends-on ("rete"))
	       (:file "discrimi" :depends-on ("matcher"))
	       (:file "supplied" :depends-on ("discrimi"))
	       (:file "rules" :depends-on ("supplied"))
	       (:file "ltms" :depends-on ("rules"))
	       (:file "congruence-closure" :depends-on ("ltms"))
	       (:file "objectmo" :depends-on ("congruence-closure"))
	       (:file "cfs" :depends-on ("objectmo"))))
