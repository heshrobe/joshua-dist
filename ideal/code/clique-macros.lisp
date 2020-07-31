;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: Ideal;  Base: 10 -*-

(in-package :ideal)

;;;;********************************************************
;;;;  Copyright (c) 1989, 1992 Rockwell International -- All rights reserved.
;;;;  Rockwell International Science Center Palo Alto Lab
;;;;********************************************************


;;;;;;;;;;;;;;;;;;;;;;;; Sampath ;;;;;;;;;;;;;;;;;;;;


(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(CLIQUE-NODE-PREDECESSORS
            FOR-EACH-CLIQUE-PROB-DIST-LOCATION
            CLIQUE-NODE-PREDECESSORS
            FOR-ALL-CLIQUE-NODE-CASES
            CLIQUE-NODE-SUCCESSORS 
            PARENT)))

;--------------------------------------------------------

; Redefined access functions. 

(defun clique-node-predecessors (c-node)(clique-node-predecessors-* c-node))
(defsetf clique-node-predecessors (c-node)(value)
  `(setf (clique-node-predecessors-* ,c-node) ,value))

(defun clique-node-successors (c-node)(clique-node-successors-* c-node))
(defsetf clique-node-successors (c-node)(value)
  `(setf (clique-node-successors-* ,c-node) ,value))

(defun clique-node-type (c-node)(clique-node-type-* c-node))
(defsetf clique-node-type (c-node)(value)
  `(setf (clique-node-type-* ,c-node) ,value))

; ********************* Mapping macros

(defmacro for-each-clique-prob-dist-location ((node-case-var node-pred-var node)
					      &body body)
  (let ((node-var (gentemp "node-var")))
    `(let ((,node-var ,node))			
       (for-all-cond-cases (,node-pred-var (clique-node-seperator-nodes ,node-var))
	 (for-all-cond-cases (,node-case-var (clique-node-residual-nodes ,node-var))
	   ,@body)))))

(defmacro for-all-clique-node-cases ((case clique-node) &body body)
  (let ((node-var (gentemp "node-var")))
    `(let ((,node-var ,clique-node))
       (for-all-cond-cases
	 (,case (when ,node-var (clique-node-component-nodes ,node-var)))
			    ,@body))))


;****** Utilities *****************************************


(defmacro parent (c-node)`(first (node-predecessors ,c-node)))

(defmacro locate-array-in-assoc-list (qty assoc-list)
  `(cdr (assoc ,qty ,assoc-list)))

(defmacro  make-dummy-assoc-list (object)
  `(list (cons nil ,object)))



