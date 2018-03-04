;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: standard-types.lisp,v 1.25 92/12/16 16:46:58 cer Exp $

(in-package :clim-internals)

"Copyright (c) 1990, 1991, 1992, 1993 Symbolics, Inc.  All rights reserved."

;;; This file contains the standard, canned presentation types
(defun print-recursive (object stream
			&key (make-presentation t)
			     (length *print-length*)
			     (level *print-level*)
			     (readably *print-readably*)
                             (array *print-array*)
			     #+ignore (radix *print-radix*)
			     #+ignore (base *print-base*)
			     #+ignore (escape *print-escape*)
			     #+ignore (case *print-case*)
			     #+ignore (gensym *print-gensym*))
  (let ((*print-length* length)
	(*print-level* level)
	(*print-readably* readably)
	(*print-array* array)
        (*print-pretty* t)
	#+ignore (*print-radix* radix)
	#+ignore (*print-base* base)
	#+ignore (*print-escape* escape)
	#+ignore (*print-case* case)
	#+ignore (*print-gensym* gensym))
    (if (and make-presentation
	     (stream-recording-p stream))	;default method returns NIL
      (with-output-as-presentation (stream object 'expression
				      ;; Better highlighting performance!
				      :single-box :highlightingq=
				      :type-expanded-p t)
        (write object :stream stream 
               :pretty t
               :level *print-level* 
               :length *print-length* 
               :readably *print-readably*
               :array *print-array*))
      (write object :stream stream 
             :pretty t
             :level *print-level* :length *print-length*
             :readably *print-readably*
             :array *print-array*))))