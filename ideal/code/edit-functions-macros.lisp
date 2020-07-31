;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: Ideal;  Base: 10 -*-

(in-package :ideal)

;;;;********************************************************
;;;;  Copyright (c) 1989, 1992 Rockwell International -- All rights reserved.
;;;;  Rockwell International Science Center Palo Alto Lab
;;;;********************************************************


;;;;;;;;;;;;;;;;;;;;;;;; Sampath ;;;;;;;;;;;;;;;;;;;;



(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '()))

;--------------------------------------------------------

; Adjusts the label id numbers of <node> in view of the fact that the
; state <state> is going to be trashed. That means that within the
; environment in which <body> is executed, any state labels that have id
; number greater than that of <state> have their id-numbers decremented
; by 1. Whatever u do, u are guaranteed that on exit from the macro the
; state label structures of n and their id-numbers will be the same as
; on entry.


(defmacro with-adjusted-label-ids ((node state) &body body)
  (let ((n (gentemp "node")) (s (gentemp "state"))
	(labels&old-ids (gentemp "old-labels"))
	(state.id (gentemp "state.id"))) 
    `(let* ((,n ,node)(,s ,state)
	    (,labels&old-ids (mapcar #'(lambda (temp)(cons temp (label-id-number temp)))
				     (state-labels ,n))))
       (unwind-protect
	   (progn
	     (dolist (temp (state-labels ,n))
	       (if (> (label-id-number temp)(label-id-number ,s))
		   (decf (label-id-number temp))))
	     ,@body)
	 (setf (state-labels ,n)
	       (mapcar #'(lambda (,state.id)
			   (setf (label-id-number (car ,state.id)) (cdr ,state.id))
			   (values (car ,state.id))) ,labels&old-ids))))))

;---- Two hacks needed to cope with adding and deleteing states under the new
; get-key-for-conditioning-case implementation. In the environment of the macro
; the number of states of the node is incremented/decremented by 1. The number
; is reset after the macro is done.

(defmacro with-decremented-number-of-states ((node) &body body)
  (let ((node-var (gentemp "node"))(old-number (gentemp "old-number")))
    `(let* ((,node-var ,node)(,old-number (number-of-states ,node-var)))
       (unwind-protect
	   (progn (decf (number-of-states ,node-var)),@body)
	 (setf (number-of-states ,node-var) ,old-number)))))


(defmacro with-incremented-number-of-states ((node) &body body)
  (let ((node-var (gentemp "node"))(old-number (gentemp "old-number")))
    `(let* ((,node-var ,node)(,old-number (number-of-states ,node-var)))
       (unwind-protect
	   (progn (incf (number-of-states ,node-var)),@body)
	 (setf (number-of-states ,node-var) ,old-number)))))
