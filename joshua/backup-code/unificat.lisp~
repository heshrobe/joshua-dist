;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: JOSHUA-INTERNALS -*-
;;;>
;;;> *****************************************************************************************
;;;> ** (c) Copyright 1989, 1988 Symbolics, Inc.  All rights reserved.
;;;> ** Portions of font library Copyright (c) 1984 Bitstream, Inc.  All Rights Reserved.
;;;>
;;;>    The software, data, and information contained herein are proprietary 
;;;> to, and comprise valuable trade secrets of, Symbolics, Inc., which intends 
;;;> to keep such software, data, and information confidential and to preserve 
;;;> them as trade secrets.  They are given in confidence by Symbolics pursuant 
;;;> to a written license agreement, and may be used, copied, transmitted, and 
;;;> stored only in accordance with the terms of such license.
;;;> 
;;;> Symbolics, Symbolics 3600, Symbolics 3670 (R), Symbolics 3675 (R), Symbolics 3630,
;;;> Symbolics 3640, Symbolics 3645 (R), Symbolics 3650 (R), Symbolics 3653, Symbolics
;;;> 3620 (R), Symbolics 3610 (R), Symbolics XL400, Symbolics Common Lisp (R),
;;;> Symbolics-Lisp (R), Zetalisp (R), Genera (R), Wheels (R), Dynamic Windows (R), Showcase,
;;;> SmartStore (R), Semanticue (R), Frame-Up (R), Firewall (R), MACSYMA (R), COMMON LISP
;;;> MACSYMA (R), CL-MACSYMA (R), LISP MACHINE MACSYMA (R), MACSYMA Newsletter (R), Document
;;;> Examiner (R), S-DYNAMICS (R), S-GEOMETRY (R), S-PAINT (R), S-RENDER (R), "Your Next
;;;> Step in Computing" (R), Ivory, MacIvory, Symbolics C, Symbolics Pascal, Symbolics Prolog,
;;;> Symbolics Fortran, CLOE, Joshua, Concordia, and Statice are trademarks of Symbolics, Inc.
;;;> 
;;;> RESTRICTED RIGHTS LEGEND
;;;>    Use, duplication, and disclosure by the Government are subject to restrictions 
;;;> as set forth in subdivision (c)(1)(ii) of the Rights in Trademark Data and Computer 
;;;> Software Clause at FAR 52.227-7013.
;;;> 
;;;>      Symbolics, Inc.
;;;>      8 New England Executive Park, East
;;;>      Burlington, Massachusetts  01803
;;;>      United States of America
;;;>      617-221-1000
;;;> *****************************************************************************************
;;;>

;;; Support for unification and backtracking

(in-package "JI")

(defvar *variable-binding-trail*)

(defun unwind-trail ()
  (loop for locative in *variable-binding-trail*
	doing (joshua-logic-variable-makunbound locative)))

(defmacro with-unification (&body body)
  `(let ((*variable-binding-trail* nil))
     (unwind-protect
	 (catch 'failure-exit ,@body)
       (unwind-trail))))

(eval-when (compile load eval)
  (proclaim '(inline unify-fail)))
(defun unify-fail ()
  ;; throw out of the extension computation, returning NIL
  ;; the unwind-protect in the caller ensures the trail gets unwound.
  (throw 'failure-exit nil))
;;;
;;; Unification "instructions", or what would be microcoded if we were up to that.
;;;

(defun %unify-variable (from to)
  ;; From is always an unbound logic variable.
  ;; push that variable onto the trail
  (push from *variable-binding-trail*)
  ;; then clobber the variable to make it point at to
  (setf (joshua-logic-variable-value-cell from) to)
  ;; always return T even if value is nil
  t)

(defun variable-occurs-in (variable value)
  ;; Returns T if the variable occurs somewhere in the value
  ;; for example, unifying [foo =x (f =x)] with [foo =y =y] should fail,
  ;; 'cause you can't unify =x with (f =x) and stay sane.
  (typecase value ;don't know this type at compile time.  Sigh.
    (unbound-logic-variable
      ;; if value is a logic variable, this is just a simple EQ-test
      (eq variable value))
    (joshua-logic-variable
      ;; Dereference...
      (variable-occurs-in variable (joshua-logic-variable-value value)))
    (cons
      ;; if value is a list, we've gotta grovel over it
      (loop for value-cons = value then (cdr value-cons)
	    while (consp value-cons)
	    thereis (variable-occurs-in variable (car value-cons))
	    finally (return (variable-occurs-in variable value-cons))))
    (predication
      (loop for p-cons = (predication-statement value) then (cdr p-cons)
	    while (consp p-cons)
	    thereis (variable-occurs-in variable (car p-cons))
	    finally (return (variable-occurs-in variable p-cons))))
    (otherwise
      ;; anything else can't possibly contain variable
      nil)))

(defun unify-variable-with-occur-check (from to)
  ;; like unify-variable, but does an occur-check first.
  (when (variable-occurs-in from to) (unify-fail))
  (%unify-variable from to))

(defun unify-constant (data pattern)
  ;; unification of constants (or structures which have no variables)
  (cond ((eql data pattern))
	((unbound-logic-variable-p data)
	 ;; if there's a variable in the data, clobber it
	 (%unify-variable data pattern))
	(t (unify-fail))))

(defun unify-string (data pattern)
  ;; unification of strings
  (cond ((and (stringp data) (string= data pattern)))
	((unbound-logic-variable-p data)
	 ;; if there's a variable in the data, clobber it
	 (%unify-variable data pattern))
	(t (unify-fail))))

;;;
;;; Structures that usually have to be copied.  (Unless they're variable-free.)
;;;

(defun unify-list (list1 list2)
  ;; unify two lists component-wise
  (loop for list1-cons = list1 then (cdr list1-cons)
	for list2-cons = list2 then (cdr list2-cons)
	while (and (consp list1-cons) (consp list2-cons))
	;; above is a fancy way of iterating over dotted lists.
	do (unify (car list1-cons) (car list2-cons))
	finally
	  (unify list1-cons list2-cons)
	  ;; success
	  (return t)))

(eval-when (compile load eval)
  (proclaim '(inline unify-predication)))
(defun unify-predication (predication1 predication2)
  ;; unify predications by taking their argument map and unifying the lists
  (unify-list (predication-statement predication1)
	      (predication-statement predication2)))

;;;
;;; Main entrypoint to the unifier
;;;

(defun unify (object1 object2)
  ;; unify two objects or fail trying -- this is basically a fancy type-dispatch!
  (setq object1 (joshua-logic-variable-value object1)
	object2 (joshua-logic-variable-value object2))
  (cond ;; if they're eql, they unify with no special provision
	((eql object1 object2))
	((unbound-logic-variable-p object1)
	 (cond ((unbound-logic-variable-p object2)
		;; both object1 and object2 are variables, so make object2 point to object1 & trail.
		(%unify-variable object2 object1))
	       (t
		 ;; object1 is a variable & object2 is a value, so make object1 point to value & trail it.
		 (unify-variable-with-occur-check object1 object2))))
	((unbound-logic-variable-p object2)
	 ;; object1 is a value & object2 is a variable, so make object2 point to the value & trail it.
	 (unify-variable-with-occur-check object2 object1))
	((consp object1)
	 (if (consp object2)
	     ;; both are lists, so treat them component-wise
	     (unify-list object1 object2)
	     (unify-fail)))
	((predicationp object1)
	 (if (predicationp object2)
	     ;; both are predications, so treat them component-wise
	     (unify-predication object1 object2)
	     (unify-fail)))
	((stringp object1)
	 ;; check if they match as strings, comparing with string=
	 (unless (and (stringp object2) (string= object1 object2))
	   (unify-fail)))
	(t
	  ;; can't win for losing
	  (unify-fail))))

(defun unify-constants-p (object1 object2)
  ;; unify two objects or return nil -- this is basically a fancy type-dispatch!
  ;; neither object contains any variables
  ;; this is called by the semi-matchers
  (cond ;; if they're eql, they unify with no special provision
	((eql object1 object2))
	((consp object1)
	 (and (consp object2)
	      ;; both are lists, so treat them component-wise
	      (loop for list1-cons = object1 then (cdr list1-cons)
		    for list2-cons = object2 then (cdr list2-cons)
		    while (and (consp list1-cons) (consp list2-cons))
		    always (unify-constants-p (car list1-cons) (car list2-cons))
		    finally (return (unify-constants-p list1-cons list2-cons)))))
	((predicationp object1)
	 (and (predicationp object2)
	      ;; both are predications, so treat them component-wise
	      (loop for list1-cons = (predication-statement object1) then (cdr list1-cons)
		    for list2-cons = (predication-statement object2) then (cdr list2-cons)
		    while list1-cons
		    always (unify-constants-p (car list1-cons) (car list2-cons)))))
	((stringp object1)
	 (and (stringp object2) (string= object1 object2)))
	(t
	 nil)))


(defun variant (object1 object2)
  ;; the overhead of unification (including unwinding the bindings)
  ;; makes this faster than the old method furthermore it doesn't
  ;; side-effect anything so it doesn't cause problems for parallel
  ;; Joshua
  ;;  Dereference vars by hand right now...
  (let ((variables nil))
    (labels ((variant-internal (object1 object2)
	       (when (joshua-logic-variable-p object1)
		     (setq object1 (joshua-logic-variable-value object1)))
	       (when (joshua-logic-variable-p object2)
		     (setq object2 (joshua-logic-variable-value object2)))
	       (typecase object1
		 (unbound-logic-variable
		   (typecase object2
		     (unbound-logic-variable 
		       (let ((entry (loop for entry in variables
					  ;; assoc can't handle logic variables without help so this is faster
					  do (when (eql object1 (car entry))
					       (return entry)))))
					  
			 (if entry
			     (eql  object2 (cdr entry))
			     (and (loop for entry in variables
					never (eql object2 
						   (cdr entry)))
				  (progn (push (cons object1 object2) variables)
					 t)))))
		     (otherwise nil)))
		 (otherwise
		   (or (eql object1 object2)
		       (typecase object1
			 (cons
			   (when (consp object2)
			     (loop for list1-cons = object1 then (cdr list1-cons)
				   for list2-cons = object2 then (cdr list2-cons)
				   while (and (consp list1-cons) (consp list2-cons))
				   ;; above is a fancy way of iterating over dotted lists.
				   always (variant-internal (car list1-cons) (car list2-cons))
				   finally
				     ;; check the thing in the tails of both lists
				     (return (or (and (null list1-cons)
						      (null list2-cons))
						 (variant-internal list1-cons list2-cons))))))
			 (predication
			   (and (predicationp object2)
				(variant-internal (predication-statement object1)
						  (predication-statement object2))))
			 (string
			   (and (stringp object2)
				(string= object1 object2)))
			 (otherwise nil)))))))
      (variant-internal object1 object2))))



;;; I think neither of these is actually used in portable joshua because
;;; of the difference between CL hash-tables and Genera Hash tables

(defun matcher-variant (object1 object2)
  (let ((variables nil))
  (labels ((variant-internal (object1 object2)
	     ;;  Dereference vars by hand right now...
	     (progn
	       (setq object1 (joshua-logic-variable-value object1))
	       (setq object2 (joshua-logic-variable-value object2)))
	     (typecase object1
	       (logic-variable-maker
	        (typecase object2
		  (logic-variable-maker
		    (let ((entry (assoc object1 variables :test #'(lambda (a b)
								    (eql (logic-variable-maker-name a)
									 (logic-variable-maker-name b))))))
		      (if entry
			  (eql (logic-variable-maker-name object2)
			       (logic-variable-maker-name (cdr entry)))
			(and (loop for entry in variables
				   never (eql (logic-variable-maker-name object2)
					      (logic-variable-maker-name (cdr entry))))
			     (progn (push (cons object1 object2) variables)
				    t)))))
		  (otherwise nil)))
		(otherwise
		 (or (eql object1 object2)
		     (typecase object1
		       (predication-maker
			(and (predication-maker-p object2)
			     (variant-internal (predication-maker-statement object1)
					       (predication-maker-statement object2))))
		       (cons
			(when (consp object2)
			  (loop for list1-cons = object1 then (cdr list1-cons)
				for list2-cons = object2 then (cdr list2-cons)
				while (and (consp list1-cons) (consp list2-cons)
					   (not (logic-variable-maker-p list1-cons))
					   (not (logic-variable-maker-p list2-cons)))
				;; above is a fancy way of iterating over dotted lists.
				always (variant-internal (car list1-cons) (car list2-cons))
				finally
				;; check the thing in the tails of both lists
				(return (or (and (null list1-cons)
						 (null list2-cons))
					    (variant-internal list1-cons list2-cons))))))
		       (string
			(and (stringp object2)
			     (string= object1 object2)))
		       (otherwise nil)))))))
	   (variant-internal object1 object2))))

(defun semi-matcher-variant (object1 object2)
  ;; the overhead of unification (including unwinding the bindings) makes this
  ;; faster than the old method
  ;; furthermore it doesn't side-effect anything so it doesn't cause problems for || Joshua
  (let ((variables nil)
	(skip1 (positions-forward-rule-matcher-can-skip object1))
	(skip2 (positions-forward-rule-matcher-can-skip object2)))
    (labels ((variant-internal (object1 object2)
	       (if (and (member object1 skip1) (member object2 skip2))
		   (variant-internal (cdr object1) (cdr object2))
		   (typecase object1
		     (logic-variable-maker
		       (typecase object2
			 (logic-variable-maker
			   (let ((entry (assoc object1 variables :test #'(lambda (a b)
									   (eql (logic-variable-maker-name a)
										(logic-variable-maker-name b))))))
			     (if entry
				 (eql (logic-variable-maker-name object2)
				      (logic-variable-maker-name (cdr entry)))
				 (and (loop for entry in variables
					    never (eql (logic-variable-maker-name object2)
						       (logic-variable-maker-name (cdr entry))))
				      (progn (push (cons object1 object2) variables)
					     t)))))
			 (otherwise nil)))
		     (otherwise
		       (or (eql object1 object2)
			   (typecase object1
			     ;; Predication Maker has to come first since it's sub-typep cons
			     (predication-maker
			       (and (predication-maker-p object2)
				    (variant-internal (predication-maker-statement object1)
						      (predication-maker-statement object2))))
			     (cons
			       (when (consp object2)
				 (loop for list1-cons = object1 then (cdr list1-cons)
				       for list2-cons = object2 then (cdr list2-cons)
				       while (and (consp list1-cons) (consp list2-cons)
						  (not (logic-variable-maker-p list1-cons))
						  (not (logic-variable-maker-p list2-cons)))
				       ;; above is a fancy way of iterating over dotted lists.
				       always (or (and (member list1-cons skip1) (member list2-cons skip2))
						  (variant-internal (car list1-cons) (car list2-cons)))
				       finally
					 ;; check the thing in the tails of both lists
					 (return (or (and (null list1-cons)
							  (null list2-cons))
						     (variant-internal list1-cons list2-cons))))))
			     (string
			       (and (stringp object2)
				    (string= object1 object2)))
			     (otherwise nil))))))))
      (variant-internal object1 object2))))