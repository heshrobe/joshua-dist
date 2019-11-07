;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-ENV; Base: 10; Lowercase: Yes -*-

;;; Copyright (c) 1994-2000, Scott McKay.
;;; Copyright (c) 2001-2003, Scott McKay and Howard Shrobe.
;;; All rights reserved.  No warranty is expressed or implied.
;;; See COPYRIGHT for full copyright and terms of use.

(in-package :clim-env)

;;; General command table structure

;; Commands local to a single activity
(define-command-table activity)

;; Conversation and mail reading commands
(define-command-table conversation)
(define-command-table mail-reading)
(define-command-table communication :inherit-from (conversation mail-reading))

;; Commands to access demos
(define-command-table demonstration)

;; Documentation commands
(define-command-table documentation)

;; General editing commands
(define-command-table editing)

;; Commands for dealing with files and directories
(define-command-table directories)
(define-command-table files)
(define-command-table file-system :inherit-from (directories files))

;; Commands for printing files, etc.
(define-command-table printers)

;; General commands for hacking programs under development
(define-command-table breakpoints)
(define-command-table presentations)
(define-command-table processes)
(define-command-table tracing)
(define-command-table debugging :inherit-from (breakpoints presentations processes tracing))
(define-command-table clos)
(define-command-table inspection)
(define-command-table callers)
(define-command-table lisp :inherit-from (clos inspection callers))
(define-command-table systems)
(define-command-table comtabs)
(define-command-table programming-tools :inherit-from (debugging lisp systems comtabs))

;; Commands for managing the entire session
(define-command-table activities)
(define-command-table gc)
(define-command-table windows)
(define-command-table session :inherit-from (activities gc windows))

;; Commands for reading and writing tapes
(define-command-table tapes)

;; A catch-all for other useful stuff
(define-command-table utilities)

;; The command table that catches them all
(define-command-table global
  :inherit-from (activity
		 communication
		 demonstration
		 documentation
		 editing 
		 file-system
		 printers
		 programming-tools
		 session
		 tapes 
		 utilities))

