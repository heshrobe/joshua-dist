;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: input-editor-commands.lisp,v 1.24 92/12/03 10:26:54 cer Exp $

(in-package :clim-internals)

(define-input-editor-command (com-ie-show-arglist :rescan nil :type information)
			     (stream input-buffer)
  "Show function arglist"
  (let* ((symbol (symbol-at-position stream input-buffer '(#\( )))
	 (function (and symbol (fboundp symbol) (symbol-function symbol))))
    (if function
	(multiple-value-bind (arglist found-p)
	    #-mcl (function-arglist function)
            #+mcl (ccl:arglist symbol)
	  (when found-p
	    (with-input-editor-typeout (stream)
	      #-Cloe-Runtime
	      (format stream "~S: (~{~A~^ ~})"
		symbol arglist)
	      #+Cloe-Runtime
	      (format stream "~S (~A): (~{~:A~^ ~})"
		symbol found-p arglist))))
	(beep stream))))