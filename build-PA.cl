;;; -*- Mode: Common-lisp; Package: common-lisp-user -*-

(in-package :common-lisp-user)

;;; This seems to cause trouble in Allegro 10.0
(eval-when (:compile-toplevel :execute :load-toplevel)
  (proclaim '(optimize (debug 3) (safety 3) (speed 1) (space 1))))

(require :climxm)

(load "~/joshua-dist/driver.lisp")

(handler-bind ((fasl-casemode-mismatch #'(lambda (c) (invoke-restart 'excl::fasl-casemode-mismatch-continue)))) 
  (build-it)
  
  ;; on pascali I have this in a directory under my home
  ;; but on my MAC (and in its parallels linux) its under research projects
  (let* ((stream (run-shell-command "hostname" :wait nil :output :stream))
	 (host (read stream)))
    (format t "~%Host name is ~a" host)
    (cond
     ((eql 'PASCALI host)
      (load "~/natural-software/code/defsystem.lisp")
      (push "~/natural-software/my-logical-pathnames.lisp"
	    (logical-pathname-translations-database-pathnames)))
     (t
      (load "~/Research-projects/natural-software/code/defsystem.lisp")
      
      (push "~/my-logical-pathnames.lisp"
	    (logical-pathname-translations-database-pathnames))))
    (load-system 'natsoft)
    ))

(setq *read-init-files* t)

(required-top-level-binding *package* (find-package :natsoft))
(required-top-level-binding *readtable* ji::*joshua-readtable*)

(setq *restart-init-function* 'clim-env:start-clim-environment)

(dumplisp :name "sys:PA.dxl")




		      