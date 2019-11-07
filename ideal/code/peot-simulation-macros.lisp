11110;;; -*- Base: 10; Syntax: Common-Lisp; Default-character-style: (:FIX :ROMAN :NORMAL); Package: Ideal;  -*-					       

(in-package :ideal)

;;;;********************************************************
;;;;  Copyright (c) 1989, 1992 Rockwell International -- All rights reserved.
;;;;  Rockwell International Science Center Palo Alto Lab
;;;;********************************************************

;;;;;;;;;;;;;;;;;;;;;;;; Mark Peot ;;;;;;;;;;;;;;;;;;;;


(export '(IMPORTANCE-PROB-OF))

;--------------------------------------------------------


(defstruct (diag (:print-function print-simulation-diag))
  algorithm
  initializations
  node-list
  sampling-list
  evidence-list
  conditioning-case
  error-node)

(defun print-simulation-diag (struct stream depth)
  (declare (ignore struct depth))
  (format stream "#<simulation-diag>"))

(defstruct sim-node
  importance-sampling-array
  case
  belief
  diag)


; The following accessors are now replaced by a sim-node slot in the
; node structure. The use of the unused slot was interfering with user
; code which assumed that the slot was really unused (17 Oct 90)
;
;(defsetf node-sim-node (node) (object)
;  `(setf (node-unused-slot ,node) ,object))
;
;(defmacro node-sim-node (node) 
;  `(node-unused-slot ,node))
						
(defun read-importance-array (node-case cond-case)
   (read-probability-array (node-importance-sampling-array (node-in node-case))
			   cond-case
			   (node-predecessors (node-in node-case))
			   :main-node-case node-case))

(defun write-importance-array (node-case cond-case value)
  (write-probability-array (node-importance-sampling-array (node-in node-case))
			   cond-case
			   (node-predecessors (node-in node-case))
			   value
			   :main-node-case node-case))

(defun importance-prob-of (node-case &optional cond-case)
  (read-importance-array node-case cond-case))

(defsetf importance-prob-of (node-case cond-case) 
	                    (new-value)  ;for setting the importance probabilities
  `(write-importance-array ,node-case ,cond-case ,new-value))

(defun initialized-for (diag initialization)
  (member initialization (diag-initializations diag)))

(defsetf initialized-for (diag) (initialization)
  (let ((diag-sym (gensym))
	(initialization-sym (gensym)))
    `(let ((,diag-sym ,diag)
	   (,initialization-sym ,initialization))
       (pushnew ,initialization-sym (diag-initializations ,diag-sym)))))

(defun reset-inits (diag)
  (setf (diag-initializations diag) nil))

(defun node-importance-sampling-array (node)
  (sim-node-importance-sampling-array (node-sim-node node)))

(defsetf node-importance-sampling-array (node) (new-value)
  `(setf (sim-node-importance-sampling-array (node-sim-node ,node))
	 ,new-value))


(defun simulation-node-case (node)
  (sim-node-case (node-sim-node node)))

(defsetf simulation-node-case (node) (new-state)
  `(setf (sim-node-case (node-sim-node ,node)) ,new-state))
	 
(defun simulation-node-state (node)
  (state-in (sim-node-case (node-sim-node node))))

(defsetf simulation-node-state (node) (state)
  `(setf (state-in (sim-node-case (node-sim-node ,node))) ,state))

(defun running-belief-of (node-case)
  (aref (sim-node-belief (node-sim-node (node-in node-case)))
	(label-id-number (state-in node-case))))

(defsetf running-belief-of (node-case) (value)
  (let ((node-sym (gensym)))
    `(let ((,node-sym ,node-case))
       (setf (aref (sim-node-belief (node-sim-node (node-in ,node-sym)))
		   (label-id-number (state-in ,node-sym)))
	     ,value))))


;;;functions to write (error-node 

(defun error-node (diag)
  (diag-error-node diag))

(defsetf error-node (diag) (node)   ;declares that node is to be used as the error 
				       ;measurement node.
  `(setf (diag-error-node ,diag) ,node))


(defun make-arbitrary-node-case (node)
  (list (cons node (first (state-labels node)))))
