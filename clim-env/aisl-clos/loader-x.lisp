;;; -*- Mode: LISP; Package: FUTURE-COMMON-LISP-USER; Lowercase: T; Base: 10 -*-
;;; Created 10/10/01 10:01:54 by HES

;;; Copyright 2001, Symbolics, Inc.  All Rights Reserved.

(in-package :cl-user)

;;; ======================================================================================

(eval-when (eval load)
  (let* ((loading-file *load-truename*)
         (host (pathname-host loading-file))
         (device (pathname-device loading-file))
         (loading-dir (pathname-directory loading-file))
         (wildcard-dir (append loading-dir (list :wild-inferiors))))
    (let ((home (make-pathname :directory loading-dir :host host :device device))
	  (wildcard (make-pathname :directory wildcard-dir :host host :device device)))
      (setf (logical-pathname-translations "aisl-clos")
	    `(("source;*.*" ,home)
	      ("**;*.*" ,wildcard))))))


#-Genera
(clim-defsys:defsystem AISL-CLOS
    (:default-pathname "AISL-CLOS:source;"
     :default-binary-pathname (format NIL "AISL-CLOS:~A-binaries;"
				      #+MCL "MCL"
				      #+ALLEGRO "ALLEGRO"
				      #+LISPWORKS "LISPWORKS"))
  ("aisl-clos-pkg")
  ("mop"))

#+Genera
(sct:defsystem aisl-clos
    (:default-pathname "AISL-clos:source;"
     :journal-directory "aisl-clos:genera;patch;"
     :default-destination-pathname "aisl-clos:genera;")
  (:serial
    "aisl-clos-pkg"
    "mop"))
