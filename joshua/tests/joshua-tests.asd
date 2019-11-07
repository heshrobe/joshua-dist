;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: Cl-user -*-

(asdf:defsystem Joshua-tests
  :name "joshua tests"
  :description "tests for the Joshua Reasoning System"
  :long-description "..."
  :author "howard shrobe"
  :defsystem-depends-on ()
  :components
  ((:module "core"
	    :serial t
	    :pathname #p"./"
	    :components ((:joshua-file "joshua-tests")
			 (:joshua-file "gramps-test")
			 (:joshua-file "object-modeling-tests")
			 (:joshua-file "tms-tests")
			 )
	    )))
