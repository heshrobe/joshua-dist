;;; -*- Mode: Lisp; Package: JI; Syntax: Ansi-common-lisp -*-
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

(in-package :ji)

(defparameter default-heap-size 100.)

(defclass heap ()
  ((size :initform default-heap-size :reader heap-size :initarg :size)
   (active-size :initform 0 :reader heap-active-size)
   (heap-array :initform nil :reader heap-array)	;contains (key . item)
   (predicate :initform #'< :reader heap-predicate :initarg :predicate)
   (growth-factor :initform 1.5 :reader heap-growth-factor :initarg :growth-factor)))

;; Call this to make a new heap
(defun make-heap (&rest init-options &key interlocking &allow-other-keys)
 (declare #+genera
	  (scl:arglist (&key (size 100.) (predicate #'<) (growth-factor 1.5) interlocking))
	  (ignore interlocking))
 (apply 'make-instance 'heap init-options))

(defmethod initialize-instance :after ((self heap) &rest ignore)
  (declare (ignore ignore))
  ;; Make the growth-factor reasonable
  (with-slots (growth-factor size heap-array) self
    (let ((gf growth-factor))
      (when gf
	(typecase gf
	  (fixnum
	    (cond ((< gf 2) (setq gf 2))
		  ((> gf size) (setq gf size))))
	  ((or single-float double-float)
	   (cond ((<= gf (/ (float (1+ size)) size)) (setq gf 1.5))
		 ((> gf 10.0) (setq gf 10.0))))
	  (otherwise (setq gf 1.5)))
	(setf growth-factor gf)))
    (setf heap-array (make-array size))))

(defmacro heap-empty-p-macro ()			;for my convenience, not yours...
  `(zerop active-size))

(defmethod heap-empty-p ((self heap))
  (with-slots (active-size) self
    (heap-empty-p-macro)))

(defmethod heap-clear ((self heap))
  (with-slots (active-size) self
    (setq active-size 0)))

;; For internal use only...
(defun adjust-heap (heap i)
  (let ((heap-array (slot-value heap 'heap-array))
	(active-size (slot-value heap 'active-size))
	(predicate (slot-value heap 'predicate)))  
    (loop with key-and-item = (aref heap-array i)
	  for j = (* 2 i) then (* 2 j)
	  while (<= j active-size)
	  if (and (< j active-size)
		  (funcall predicate
			   (car (aref heap-array (1+ j)))
			   (car (aref heap-array j))))
	  do (incf j)
	  while (funcall predicate
			 (car (aref heap-array j))
			 (car key-and-item))
	  do (setf (aref heap-array (floor j 2)) (aref heap-array j))
	  finally (setf (aref heap-array (floor j 2)) key-and-item))))

;; If for some reason the heap is no longer in sorted order (the user
;; could have bashed an entry), rebuild-heap will make it sorted again.
;; For internal use only...
(defun rebuild-heap (heap)
  (loop for i from (floor (slot-value heap 'active-size) 2) downto 1
	do (adjust-heap heap i)))

(defmethod heap-insert ((self heap) item key)
  (with-slots (active-size heap-array size growth-factor predicate) self
    (incf active-size)
    ;; Copy the heap if it's full up
    (when (>= active-size size)
      (if (null growth-factor)
	  (error "The heap ~s has overflowed." self)
	  ;; (signal 'heap-overflow :heap self)
	  (let* ((new-size (if (integerp growth-factor)
			       (+ size growth-factor)
			       (floor (* size growth-factor))))
		 (new-heap-array (make-array new-size)))
	    (replace new-heap-array heap-array)
	    (setq heap-array new-heap-array)
	    (setq size new-size))))
	      (loop with j = active-size
		    with i = (floor active-size 2)
		    while (and (> i 0)
			       (funcall predicate
					key (car (aref heap-array i))))
		    do (setf (aref heap-array j) (aref heap-array i))
		    (setq j i)
		    (setq i (floor i 2))
		    finally (setf (aref heap-array j) (cons key item)))
	      (values item key)))

(defmacro with-top-key-and-item ((key item) &body body)
  `(let* ((.key-and-item. (aref heap-array 1))
	  (,key (car .key-and-item.))
	  (,item (cdr .key-and-item.)))
     ,@body))

(defmethod heap-top ((self heap))
  (with-slots (heap-array active-size) self
  (if (heap-empty-p-macro)
      (values nil nil nil)
    (with-top-key-and-item (key item)
       (values item key t)))))

(defmethod heap-remove ((self heap))
  (with-slots (heap-array active-size) self
  (if (heap-empty-p-macro)
      (values nil nil nil)
    (with-top-key-and-item (key item)
      (setf (aref heap-array 1) (aref heap-array active-size))
      (decf active-size)
      (adjust-heap self 1)
      (values item key t)))))

;;; I've removed the need for the loop path

;;;(define-loop-path heap-elements loop-heap-elements-path (of with-key with-index))
;;;
;;;#+lucid 
;;;(defun loop-tequal (x1 x2)
;;;  (and (symbolp x1) (string= x1 x2)))
;;;
;;;(defun loop-heap-elements-path (path variable data-type prep-phrases
;;;				inclusive? allowed-preps data
;;;				&aux (heap nil) (key-var nil) (index-var nil))
;;;  path data-type allowed-preps data		;ignored
;;;  (when inclusive?
;;;    (error "Inclusive stepping is not supported by the HEAP-ELEMENTS path"))
;;;  (loop for (prep expr) in prep-phrases		;parse prep-phrases
;;;	do (cond ((loop-tequal prep 'of) (setq heap expr))
;;;		 ((loop-tequal prep 'with-key) (setq key-var expr))
;;;		 ((loop-tequal prep 'with-index) (setq index-var expr))
;;;		 (t (error "~S is not a known preposition for the HEAP-ELEMENTS path" prep))
;;;		 ))
;;;  (when (null heap)
;;;    (error "The heap must be specified (the OF clause was missing)."))
;;;  (let* ((step-var (gensym))
;;;	 (heap-var (gensym))
;;;	 (heap-array-var (gensym))
;;;	 (active-size-var (gensym))
;;;	 (bindings `((,heap-var ,heap)
;;;		     (,heap-array-var)
;;;		     (,active-size-var)
;;;		     (,step-var)
;;;		     ,@(when index-var `((,index-var)))
;;;		     ,@(when key-var `((,key-var)))
;;;		     (,variable)))
;;;	 (prologue `((setq ,step-var 1		;first element is in 1, not 0
;;;			   ,heap-array-var (slot-value ,heap-var 'heap-array)
;;;			   ,active-size-var (slot-value ,heap-var 'active-size))))
;;;	 (iterspec `((> ,step-var ,active-size-var)
;;;		     (,variable (let ((key-and-item
;;;					(aref ,heap-array-var ,step-var)))
;;;				  ,@(when index-var `((setq ,index-var ,step-var)))
;;;				  (incf ,step-var)
;;;				  ,@(when key-var `((setq ,key-var (car key-and-item))))
;;;				  (cdr key-and-item)))
;;;		     ()
;;;		     ())))
;;;    (list* bindings prologue iterspec)))
;;;)

#-(or genera lucid mcl allegro)
(defun y-or-n-p (&rest format-args)
  (terpri *query-io*)
  (apply #'format *query-io* format-args)
  (loop for l = (read-line *query-io*)
	do
	(when (plusp (length l))
	  (case (char-upcase (char (string-left-trim '(#\space #\tab) l) 0))
	    (#\Y (return t))
	    (#\N (return nil))))))

(defmethod for-all-items ((h heap) continuation)
  (declare (dynamic-extent continuation))
  (with-slots (heap-array active-size) h
    (loop for index from 1 upto active-size
	  for (key . value) = (aref heap-array index)
	  do (funcall continuation key value index))))

(defmethod describe-object :around ((self heap) stream)
  (call-next-method)
  (with-slots (active-size) self
    (when (and (not (heap-empty-p-macro))
	       (let ((*query-io* stream))
		 (y-or-n-p "Do you want to see the contents of the heap? ")))
      (flet ((print-an-entry (key item index)
	       (declare (ignore index))
	       (format stream "~&Key ~S~20T~S" key item)))
	(declare (dynamic-extent print-an-entry))
	(for-all-items self #'print-an-entry)
	(terpri stream)))))

;; Here come the fancy, non-canonical heap methods
(defmethod delete-by-item ((self heap) item &optional (equal-predicate #'=) no-error-p)
  (with-slots (SIZE ACTIVE-SIZE HEAP-ARRAY PREDICATE GROWTH-FACTOR) self
    (flet ((do-it (this-key this-item i)
	     (when (funcall equal-predicate item this-item)
	       (setf (aref heap-array i) (aref heap-array active-size))
	       (setf (aref heap-array active-size) nil)
	       (decf active-size)
	       (rebuild-heap self)
	       (return-from delete-by-item (values this-item this-key)))))
      (declare (dynamic-extent do-it))
      (for-all-items self #'do-it)
      (if no-error-p
	  (values nil nil)
	  (error "Heap item not found: ~s in ~s." item self)))))

(defmethod delete-by-key ((self heap) key &optional (equal-predicate #'=) no-error-p)
  (with-slots (SIZE ACTIVE-SIZE HEAP-ARRAY PREDICATE GROWTH-FACTOR)  self
    (flet ((do-it (this-key this-item i)
	     (when (funcall equal-predicate key this-key)
	     (setf (aref heap-array i) (aref heap-array active-size))
	       (setf (aref heap-array active-size) nil)
	       (decf active-size)
	       (rebuild-heap self)
	       (return-from delete-by-key (values this-item this-key)))))
      (declare (dynamic-extent do-it))
      (for-all-items self #'do-it)
      (if no-error-p
	  (values nil nil)
	  (error "Heap item not found: ~s in ~s." key self)))))

(defmethod find-by-item ((self heap) item &optional (equal-predicate #'=) no-error-p)
  (flet ((do-it (this-key this-item index)
           (declare (ignore index))
           (when (funcall equal-predicate item this-item)
             (return-from find-by-item (values this-item this-key)))))
    (declare (dynamic-extent do-it))
    (for-all-items self #'do-it)
    (if no-error-p
      (values nil nil)
      (error "Heap item not found: ~s in ~s." item self))))

(defmethod find-by-key ((self heap) key &optional (equal-predicate #'=) no-error-p) 
  (flet ((do-it (this-key this-item index)
	     (declare (ignore index))
	     (when  (funcall equal-predicate key this-key)
	       (return-from find-by-key (values this-item this-key)))))
      (declare (dynamic-extent do-it))
      (for-all-items self #'do-it)
      (if no-error-p
	  (values nil nil)
	  (error "Heap item not found: ~s in ~s." key self))))