;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

;;; Copyright (c) 1994-2000, Scott McKay.
;;; Copyright (c) 2001-2003, Scott McKay and Howard Shrobe.
;;; All rights reserved.  No warranty is expressed or implied.
;;; See COPYRIGHT for full copyright and terms of use.

(in-package :cl-user)


#+allegro
(require :loop)
#+allegro
(require :defsys)
#+allegro
(require :compiler)
#+allegro
(require :trace)

;;; Load the CLIM Environment

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

(load "portable-lisp-environment:aisl-clos;sysdcl.lisp" #+mcl :external-format #+mcl :unix)
(load "portable-lisp-environment:sysdcl.lisp" #+mcl :external-format #+mcl :unix)

(defun load-clim-env (&key compile recompile)
  (when compile
    (compile-system 'aisl-clos :recompile recompile))
  (load-system 'aisl-clos)
  (when compile
    (compile-system 'clim-environment :recompile recompile))
  (load-system 'clim-environment))

#+Lispworks
(when (find-package :dylan)
  (when (y-or-n-p "Load Dylan support? ")
    (load-system 'clim-dylan-environment)))
