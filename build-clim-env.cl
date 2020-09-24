;;; -*- Mode: Common-lisp; Package: common-lisp-user -*-

(in-package :common-lisp-user)

;;; This seems to cause trouble in Allegro 10.0
(eval-when (:compile-toplevel :execute :load-toplevel)
  (proclaim '(optimize (debug 3) (safety 3) (speed 1) (space 1))))


(require :climxm)

(require :xml-rpc)

(load "~/joshua-dist/driver.lisp")

(handler-bind ((fasl-casemode-mismatch #'(lambda (c) (invoke-restart 'excl::fasl-casemode-mismatch-continue)))) 
  (build-it :xml-server #+MacOSX t #-MacOSX nil :apps t)
  ;;
  ;; I think that loading driver.lisp takes care of all of the loading
  ;;
  ;; (load "~/joshua-dist/xml-parser/xml-parser-defsystem.lisp")
  ;; (load-system 'xml-parser)
  ;; covered by :xml-server switch to build-it
  ;; (load-system 'sample-xml-rpc-server)

  ;; (load "~/joshua-dist/ideal/load-ideal.lisp")
  ;; (load-system 'ideal)

  (push "~/my-logical-pathnames.lisp"
	(logical-pathname-translations-database-pathnames))

  ;; (load "~/Research-Projects/natural-software/code/defsystem.lisp")
  ;; (load-system 'natsoft)
  
  ; (load "~/Research-Projects/awdrat/code/defsystem.lisp")
  ; (load-system 'awdrat)
  
  ; (load "~/Research-Projects/control-system/defsystem.lisp")
  ; (load-system 'controls)
  
  ; (load "~/Research-Projects/attack-planning/code/defsystem.lisp")
  ; (load-system 'aplan)

  )

(pushnew #\_ *additional-logical-pathname-name-chars* :test #'char-equal)

(setq *read-init-files* t)

(setq *restart-init-function* 'clim-env:start-clim-environment)

(dumplisp :name "sys:clim-env-josh.dxl")




		      