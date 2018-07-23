;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: joshua-internals -*-
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

;;; "Borrowings" from Genera, useful things that Joshua can't (or at
;;; least, is too spoiled to) live without.

(in-package :ji)

;;; Make all Lisps at least tolerate values declarations
;;; SBCL has its own values declaration which is a type declaration
;;; incompatible with this one which tells you the names of the return
;;; values.  The other three already have a version mimicing the genera one
#-(or genera cloe lucid sbcl)
(eval-when (:compile-toplevel :execute :load-toplevel)
  (Proclaim '(declaration values))
  )

;;; From EVAL.LISP...
(DEFUN FIND-BODY-DECLARATIONS (BODY ENV &OPTIONAL (LAMBDA-LIST NIL LAMBDA-LIST-P))
  #-SBCL (DECLARE (VALUES DECLARATIONS REAL-BODY FIRST-FORM-ALREADY-MACRO-EXPANDED))
  (DECLARE (IGNORE LAMBDA-LIST ENV))
  "Separate the declarations from the body."
  (LOOP FOR REAL-BODY ON BODY
	FOR FORM = (FIRST REAL-BODY)
	;; don't macroexpand looking for declarations
	WHILE (TYPECASE FORM
		(LIST
		  (EQ (FIRST FORM) 'DECLARE))
		(STRING
		  (AND LAMBDA-LIST-P
		       (NOT (NULL (cdr REAL-BODY))))))
	COLLECT FORM INTO DECLARATIONS
	FINALLY (RETURN (VALUES DECLARATIONS REAL-BODY (AND REAL-BODY FORM)))))

;;; The code below uses CLTL1 versions
;;; Allegro, Genera have compatability versions of these
;;; but SBCL doesn't.  So I've provided them here.
;;; Fix: Probably just include these in the Joshua code universally
;;; Or better yet, change the references to the ANSI version.
;;; Note: get-setf-method in cltl1 goes to get-setf-expansion in ansi
;;;       define-setf-method               define-setf-expander


#+sbcl
(defun get-setf-method (form &optional env)
  (get-setf-expansion form env))

#+sbcl
(defun get-setf-method-multiple-value (form &optional env)
  ;; see get-setf-method for a description of the env arg.
  ;; Like get-setf-method, but may return multiple new-value variables.
  (get-setf-expansion form env))

#+sbcl
(defmacro define-setf-method (access-fn lambda-list &body body)
  `(define-setf-expander ,access-fn ,lambda-list ,@body))

;;;  DEFBITFIELDS...  replacement for FIXNUM type defstruct...

(defun defbitfields-setf-expander (x bytespec accessor-name)
  (multiple-value-bind (temps vals stores sf af)
      (get-setf-method x)
    (let ((store (gensym)))
      (values temps vals (list store)
	      `(let ((,(first stores) (dpb ,store ,bytespec ,af)))
		 ,sf
		 ,store)
	      `(,accessor-name ,af)))))

(defmacro defbitfields (conc-name &body clauses)
  (loop with init = 0
	for (field-name initform . rest) in clauses
	as bytespec = (getf rest ':byte)
	and accessor-name = (intern (concatenate 'string (string conc-name)
						 "-"  (string field-name)))
	collect field-name into field-names
	collect accessor-name into accessor-names
	collect `(defun ,accessor-name (.num.) (ldb ,bytespec .num.))
	into accessor-defs
	collect `(define-setf-method ,accessor-name (x)
		   (defbitfields-setf-expander x ',bytespec ',accessor-name))
	into setfs
	do
	(setf (ldb (eval bytespec) init) (eval initform))
	finally
	(return
	  `(eval-when (:compile-toplevel :execute :load-toplevel)
	     (proclaim '(inline ,@accessor-names))
	     ,@accessor-defs
	     ,@setfs
	     (defun ,(intern (concatenate 'string "MAKE-" (string conc-name)))
		    (&key ,@field-names &aux (value ,init))
	       ,@(mapcar #'(lambda (fn an) `(when ,fn (setf (,an value) ,fn)))
			 field-names accessor-names)
	       value)))))

;;; Resources
;;;  We only need a very simple resource system.

(defvar *all-resources* nil)

(defmacro defresource (name parameters &rest options
		       &key constructor deinitializer)
  (declare (ignore constructor deinitializer))	;for checking only.
  `(defresource-1 ',name ',parameters ',options))

(defstruct (resource-array-entry (:type list))
  object
  parameters
  in-use)

(defun defresource-1 (name parameters options)
  (let ((found (assoc name *all-resources*))
	(plist (list* ':parameters parameters
		     ':package *package*
		     options)))
    (if found (setf (cdr found) plist)
	(push (cons name plist) *all-resources*))))

(defun allocate-resource (resource-name &rest parameters)
  (let* ((entry (assoc resource-name *all-resources*))
	 (array (getf (cdr entry) :resource-array))
	 (constructor (getf (cdr entry) :constructor))
	 (parameter-names (getf (cdr entry) :parameters)))
    (unless entry (error "Resource ~a not found." resource-name))
    (unless array
      (setq array (make-array 20 :fill-pointer 0 :adjustable t))
      (setf (cdr entry) (list* ':resource-array array (cdr entry))))
    (or (and (plusp (fill-pointer array))
	     ;; This should account for calling MATCHER someday
	     (let ((entry (find-if #'not array :key #'resource-array-entry-in-use)))
	       (when entry
		 (setf (resource-array-entry-in-use entry) t)
		 (resource-array-entry-object entry))))
	(and constructor
	     (let ((item (if (and (listp constructor)
				  (not (eq (car constructor) 'lambda)))
                           (progv parameter-names parameters
                             (eval constructor))
                           (apply constructor parameters))))
	       (vector-push-extend (make-resource-array-entry
                                    :object item
                                    :parameters parameters
                                    :in-use t) array)
	       item))
	(error "Resource ~a exhausted and no constructor found." resource-name))))

(defun deallocate-resource (resource-name object)
  (let* ((entry (assoc resource-name *all-resources*))
	 (array (getf (cdr entry) :resource-array))
	 (deinitializer (getf (cdr entry) :deinitializer))
	 (package (getf (cdr entry) :package)))
    (unless entry (error "Resource ~a not found." resource-name))
    (unless array (error "No objects in resource ~a; can't deallocate one." resource-name))
    (let ((rae (find object array :test 'eq :key #'resource-array-entry-object)))
      (unless rae (error "Object ~s not found in resource ~a." object resource-name))
      (setf (resource-array-entry-in-use rae) nil))
    (when deinitializer
      (if (and (listp deinitializer)
	       (not (eq (car deinitializer) 'lambda)))
	  (progv (list (intern "OBJECT" package)) (list object)
	    (eval deinitializer))
	  (funcall deinitializer object)))))


(defmacro using-resource ((variable resource-name &rest parameters) &body body)
  `(let ((,variable :resource-not-allocated))
     (unwind-protect
	 (progn
	   ;; without-interrupts around this form?
	   (setq ,variable (allocate-resource ',resource-name ,@parameters))
	   ,@body)
       (unless (eq ,variable :resource-not-allocated)
	 (deallocate-resource ',resource-name ,variable)))))

(defun clear-resource (resource-name)
  (let* ((entry (assoc resource-name *all-resources*))
	 (array (getf (cdr entry) :resource-array)))
    (unless entry (error "Resource ~a not found." resource-name))
    (when array
      (fill array nil)
      (setf (fill-pointer array) 0))))

(defun deallocate-whole-resource (resource-name)
  (let* ((entry (assoc resource-name *all-resources*))
	 (array (getf (cdr entry) :resource-array)))
    (unless entry (error "Resource ~a not found." resource-name))
    (when array
      (map nil #'(lambda (rae)
		   (when (resource-array-entry-in-use rae)
			 (deallocate-resource resource-name (resource-array-entry-object rae))))
	   array))))


;;;  Stack lists elsewhere are gotten using dynamic extent, easy to fake the syntax...

#-(or genera cloe allegro)
(defmacro with-stack-list ((var &rest elements) &body body)
  `(let ((,var (list ,.elements)))
     (declare (dynamic-extent ,var))
     ,@body))

#-(or genera cloe allegro)
(defmacro with-stack-list* ((var &rest elements) &body body)
  `(let ((,var (list* ,.elements)))
     (declare (dynamic-extent ,var))
     ,@body))

#-(or genera cloe)
(defmacro stack-let (bindings &body body)
  `(let ,bindings
     (declare (dynamic-extent ,@(mapcar #'car bindings)))
     ,@body))

;;; Def-Defining-Form.   Not really a borrowing, but it's in
;;; GENERA-PATCHES for some reason, so goes here.
(defmacro def-defining-form (name &body keys &key
			     definer killer shower parser finder
			     style-checker
			     type-name fcn-spec-type)
  (declare (ignore #-allegro keys  shower parser finder style-checker
		    fcn-spec-type))
  #-(or genera cloe-developer)
  (declare (ignore type-name killer))
  ;; how to define a defining macro, Cadillac style.
  ;; definer is either a function spec or  (args &rest body).  killer, shower, parser, finder,
  ;;   and style-checker are similar.
  ;; type-name is a string or NIL
  ;; fcn-spec-type is a fcn-spec type or NIL (seldom needed)
  (check-type name (or nil symbol))
  (check-type definer (or nil list) "a definition of the form (<args> . <body>)")
  `(progn
     #+(or genera cloe-developer)
     (record-source-file-name ',name 'def-defining-form)
     #+(or genera cloe-developer)
     ,@(when killer `((setf (get ',name 'zl:::zwei:kill-definition) ',killer)))
     #+(or genera cloe-developer)
     ,@(when type-name
	 ;; pretty name that appears in patch comments, typein line, etc.
	 `((setf (get ',name 'zl:::si:definition-type-name) ',type-name)))
     ,(cond ((and definer (listp definer))
	     `(defmacro ,name ,(car definer)
		;; this is the thing that actually expands the defining macro
		,@(cdr definer)))
	    (t
	      (error "Mal-formed definer.  Expected fcn-spec or (<args> . <body>): ~S"
		     definer)))
     ;; return the name
     ',name))

#-mcl
(defun class-instance-slots (class)
  #+(Or allegro sbcl) (finalize-inheritance class)
  (loop for slot-definition in (class-slots class)
      for allocation = (slot-definition-allocation slot-definition)
      when (eql allocation :instance)
      collect slot-definition))


;;; This might be provided by CLIM by now.

(defun format-textual-list (list printer &key conjunction stream)
  (loop for x on list
        as elt = (car x)
	do
    (when (null (cdr x))
      (unless (null (cdr list))
	(write-char #\space stream)
	(write-string conjunction stream))
      (write-char #\space stream))
    (funcall printer elt stream)
    (unless (null (cdr x))
      (write-string ", " stream))))
