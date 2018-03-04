;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2005 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2002-2007 Franz Inc, Oakland, CA - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: drag-and-drop.lisp,v 2.7 2007/04/17 21:45:49 layer Exp $

(in-package :clim-internals)

(defmacro define-drag-and-drop-translator
	  (name
	   (from-type to-type destination-type command-table
	    &key (gesture ':select) tester destination-tester
		 documentation pointer-documentation
		 drag-documentation (menu t)
		 priority
		 (feedback 'frame-drag-and-drop-feedback)
		 (highlighting 'frame-drag-and-drop-highlighting)
		 pointer-cursor (finish-on-release *drag-and-drop-finish-on-release*))
	   arglist
	   &body body)
  #+Genera (declare (zwei:indentation 1 3 3 1))
  (with-warnings-for-definition name define-presentation-translator
    (flet ((translator-function-and-name (clause clause-name
                                          &optional args
                                                    (use-default-args t)
                                                    string-ok)
             (when clause
               (cond ((or (functionp clause)
                          (symbolp clause)
                          (and string-ok (stringp clause)))
                      (values nil clause))
                     (t (write-translator-function clause name clause-name args
                                                   use-default-args))))))
      (multiple-value-bind (body-function body-name)
	  (write-translator-function (cons arglist body) name 'real-body
				     '(destination-object destination-presentation))
	(multiple-value-bind (destination-tester-function destination-tester-name)
	    (translator-function-and-name
	     destination-tester 'destination-tester
	     '(destination-object destination-presentation))
	  (multiple-value-bind (drag-documentation-function drag-documentation-name)
	      (translator-function-and-name
	       drag-documentation 'drag-documenation
	       '(destination-object destination-presentation stream) t t)
	    (multiple-value-bind (feedback-function feedback-name)
		(translator-function-and-name
		 feedback 'feedback
		 '(frame presentation stream initial-x initial-y new-x new-y state) nil)
	      (multiple-value-bind (highlighting-function highlighting-name)
		(translator-function-and-name
		 highlighting 'highlighting
		 '(frame presentation stream state) nil)
		`(progn
		   (define-presentation-translator-1 ,name
		       (,from-type ,to-type ,command-table
				   :gesture ,gesture
				   :tester ,tester
				   :tester-definitive t
				   :documentation ,(or documentation
						       (format nil "~a" name))
				   :pointer-documentation ,pointer-documentation
				   :menu ,menu
				   :priority ,priority
				   :translator-class drag-and-drop-translator
				   :destination-type ',(expand-presentation-type-abbreviation destination-type)
				   :real-body ',body-name
				   :feedback ',feedback-name
				   :highlighting ',highlighting-name
				   :pointer-cursor ',pointer-cursor
				   :finish-on-release ',finish-on-release
				   :destination-tester ',destination-tester-name
				   :drag-documentation ',drag-documentation-name)
		     ,*translator-function-arglist*
		     (invoke-drag-and-drop-translator ,@*translator-function-arglist*))
		   ,body-function
		   ,destination-tester-function
		   ,drag-documentation-function
		   ,feedback-function
		   ,highlighting-function)))))))))