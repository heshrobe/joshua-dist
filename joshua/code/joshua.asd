;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: Cl-user -*-

(in-package :cl-user)

(asdf:defsystem joshua
  :name "Joshua"
  :description "Joshua Inference System"
  :maintainer "Howie Shrobe"
  :pathname "."
  :components (
	       (:file "packaged")
	       (:file "asdf-support" :depends-on ("packaged"))
	       (:file "borrowin" :depends-on ("packaged"))
	       (:file "mapfvars" :depends-on ("borrowin"))
	       (:file "mapforms" :depends-on ("mapfvars"))
	       (:file "readnnwr" :depends-on ("mapforms"))
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
