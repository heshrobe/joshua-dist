;;; -*- Mode: LISP; Syntax: Common-lisp; Package: CL-USER -*-

(in-package :cl-user)

;;;;********************************************************
;;;;  Copyright (c) 1989, 1992 Rockwell International -- All rights reserved.
;;;;  Rockwell International Science Center Palo Alto Lab
;;;;********************************************************

#|
File:      load-ideal.lisp
Description:
  Load file for IDEAL system - primarily used for non-symbolics system loads

Notes:
  This file must be in the top-level IDEAL directory with translatations.lisp - other
   pathnames are based off of its position
|#

;;;
;;; set up the load path for the system
;;;   THIS MIGHT NEED TO BE MANUALLY MODIFIED FOR NEW INSTALLATIONS IF THE LISP
;;;   DOESN'T BIND *LOAD-PATHNAME*
;;;

(setq foobar *load-truename*)

(setq *load-pathname* *load-truename*)

#-genera
(defparameter *ideal-load-path* *load-truename*)

;;;
;;; if the lisp has not yet implemented the CLTL2 logical pathname facility, then
;;;  load a portable implementation here
;;;

#+ignore
(unless (find-package :logical-pathname)
  #-(or genera mcl)
  (progn
    #+allegro   ;;; the common-lisp package has locked the logical pathname symbols
    (setf (excl:package-definition-lock (find-package :lisp)) nil)
    (load (format nil "~a~a" *ideal-load-path* "sysdefs/logical-pathnames.lisp"))
    #+allegro
    (setf (excl:package-definition-lock (find-package :lisp)) t)
    )
  #+(or genera mcl)
  nil
  )

;;; make sure that the correct symbols are imported into user package
#+(or lucid allegro)
(progn
  #+allegro   ;;; the common-lisp package has locked the logical pathname symbols
  (setf (excl:package-definition-lock (find-package :lisp)) nil)
  #+lucid
  (shadowing-import '(
		      lp::logical-pathname
		      lp::translate-logical-pathname
		      lp::logical-pathname-translations
		      )
		    (find-package :user)
		    )
  #+allegro
  (setf (excl:package-definition-lock (find-package :lisp)) t)
  )

;;;
;;; load the logical pathname translations for this system
;;;

#-genera
(let ((translations-path (merge-pathnames (pathname "translations.lisp")
					  *ideal-load-path*)))
  (load translations-path
      #+mcl :external-format  #+mcl :unix))


;;;
;;; if the lisp does not have a built-in defsystem facility, load a portable one
;;; 

#-(or allegro genera)
(unless (find-package :clim-defsys)
  (load "ideal:sysdefs;defsystem.lisp")
  )


;;;
;;; load the defsystem for this system
;;;

(load "ideal:home;sysdcl.lisp" #+mcl :external-format #+mcl :unix)


;;;
;;; define load and compile functions for this system
;;; 

(defun compile-ideal (&rest args)
  (apply 'compile-ideal-system args)
  )

(defun load-ideal (&rest args)
  (apply 'load-ideal-system args)
  )


