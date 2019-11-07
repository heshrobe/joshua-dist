;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-ENV; Base: 10; Lowercase: Yes -*-

;;; Copyright (c) 1994-2000, Scott McKay.
;;; Copyright (c) 2001-2003, Scott McKay and Howard Shrobe.
;;; All rights reserved.  No warranty is expressed or implied.
;;; See COPYRIGHT for full copyright and terms of use.

(in-package :clim-env)

;;; Various early parameters

(defparameter *auto-activate-expressions* #+Symbolics t #-Symbolics nil)

(defparameter *prompt-arrow* 
	      (make-pattern #2A((0 0 0 0 0 0 0 0 0 0 0 0)
				(0 0 0 0 0 1 0 0 0 0 0 0)
				(0 0 0 0 0 1 1 0 0 0 0 0)
				(0 1 1 1 1 1 1 1 0 0 0 0)
				(0 1 1 1 1 1 1 1 1 0 0 0)
				(0 0 0 0 0 0 0 1 1 1 0 0)
				(0 0 0 0 0 0 0 0 1 1 1 0)
				(0 0 0 0 0 0 0 1 1 1 0 0)
				(0 1 1 1 1 1 1 1 1 0 0 0)
				(0 1 1 1 1 1 1 1 0 0 0 0)
				(0 0 0 0 0 1 1 0 0 0 0 0)
				(0 0 0 0 0 1 0 0 0 0 0 0))
			    (list +background-ink+ +foreground-ink+)))


