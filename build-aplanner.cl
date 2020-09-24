;;; -*- Mode: Common-lisp; Package: common-lisp-user -*-

(in-package :common-lisp-user)

;;; This seems to cause trouble in Allegro 10.0
(eval-when (:compile-toplevel :execute :load-toplevel)
  (proclaim '(optimize (debug 3) (safety 3) (speed 1) (space 1))))


(require :climxm)

(load "~/joshua-dist/driver.lisp")

(handler-bind ((fasl-casemode-mismatch #'(lambda (c) (invoke-restart 'excl::fasl-casemode-mismatch-continue)))) 
  (build-it)

  (push "~/my-logical-pathnames.lisp"
	(logical-pathname-translations-database-pathnames))

  (load "~/Research-projects/attack-planning/defsystem.lisp")
  (load-system 'aplan)
  )

(setq *read-init-files* t)

(setq *restart-init-function* 'clim-env:start-clim-environment)

(dumplisp :name "sys:aplan.dxl")




		      