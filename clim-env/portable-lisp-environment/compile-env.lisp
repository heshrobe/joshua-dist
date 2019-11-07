;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

;;; Copyright (c) 1994-2000, Scott McKay.
;;; Copyright (c) 2001-2003, Scott McKay and Howard Shrobe.
;;; All rights reserved.  No warranty is expressed or implied.
;;; See COPYRIGHT for full copyright and terms of use.

(in-package :cl-user)

;;; Compile the CLIM Environment

(eval-when (:load-toplevel :execute)
  (let* ((loading-file *load-truename*)
	 (host (pathname-host loading-file))
	 (device (pathname-device loading-file))
	 (loading-dir (pathname-directory loading-file))
	 (parent-directory (butlast loading-dir))
	 (clos-directory (append parent-directory (list "aisl-clos"))))
    (setf (logical-pathname-translations "portable-lisp-environment")
          `(("aisl-clos;*.*" ,(make-pathname :directory clos-directory :host host :device device))
	    ("**;*.*" ,(make-pathname :directory loading-dir :host host :device device))))))

(load "portable-lisp-environment:aisl-clos;sysdcl.lisp")
(load "portable-lisp-environment:sysdcl.lisp")

(compile-system 'aisl-clos)
(load-system 'aisl-clos)

;;;(progn
;;;  #+Lispworks (toggle-source-debugging (y-or-n-p "Include source-debugging info? "))
;;;  (if (y-or-n-p "Compile in production mode? ")
;;;    (proclaim '(optimize (speed 3) (safety 1) (debug 1)))
;;;    (proclaim '(optimize (safety 3) (debug 3)))))

#+Lispworks
(sys::without-warning-on-redefinition
 (load "~ext/ncomp/devel")	;useful compiler macros
 (load "~ext/editor/devel"))

(compile-system 'clim-environment)

#+Lispworks
(when (find-package :dylan)
  (when (y-or-n-p "Compile Dylan support? ")
    (compile-system 'clim-dylan-environment)))
 