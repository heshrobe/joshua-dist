;;; -*- Mode: LISP; Syntax: Common-lisp; Package: cl-USER -*-

(in-package :cl-user)

;;;;********************************************************
;;;;  Copyright (c) 1989, 1992 Rockwell International -- All rights reserved.
;;;;  Rockwell International Science Center Palo Alto Lab
;;;;********************************************************

#|
File:      translations.lisp
Description:
  Define logical pathnames for IDEAL - useful for
   non-symbolics systems - for symbolics, use standard
   genera translation facility

Notes:
  Assumes that logical-pathname code is loaded
  This file must be in the top-level IDEAL directory - other pathnames
   are based off of its position
  This file assumes that the top-level load file was used to load it - it defines
   the global variable for the load-path.
|#

;; auxiliary translations
;;; for a symbolics installation, define translations
;;;   for each of the logical pathnames defined below
;;;   in the standard >sys>site location.

(defparameter *dir-symbol* #+mcl ":" #+(and unix (not mcl)) "/" #-(or mcl unix) "")

(let ((wild-core (merge-pathnames (pathname "*.*") *ideal-load-path*)))
  (setf (logical-pathname-translations "ideal")
    `(("home;*.*.*"  ,wild-core)
      ("code;*.*.*"  ,(make-pathname 
		       :directory (append (pathname-directory *ideal-load-path*)
					  (list "code"))
		       :defaults wild-core))
      ("ideal-edit;*.*.*" ,(make-pathname 
			    :directory (append (pathname-directory *ideal-load-path*)
					       (list "ideal-edit"))
			    :defaults wild-core))
      ("diagrams;*.*.*"	,(make-pathname 
			  :directory (append (pathname-directory *ideal-load-path*)
					     (list "diagrams"))
			  :defaults wild-core))
      ("**;*.*.*"  ,(make-pathname 
		     :directory (append (pathname-directory *ideal-load-path*)
					(list :wild-inferiors))
		     :defaults wild-core))
      )))

#+Allegro
(with-open-file (F #P"ideal:home;my-logical-pathnames.lisp" :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format f "~%;;; ideal")
      (format f "~2%~s" "ideal")
      (loop for (a b) in (logical-pathname-translations "ideal")
          do (format f "~%'(~s ~s)" (namestring a) (namestring b)))
      (terpri f)
      )

#+Allegro
(pushnew (namestring (truename #P"ideal:home;my-logical-pathnames.lisp"))
	 (logical-pathname-translations-database-pathnames)
	 :test #'string-equal)


