;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: Ideal;  Base: 10 -*-

(in-package :ideal)



;;;;********************************************************
;;;;  Copyright (c) 1989, 1992 Rockwell International -- All rights reserved.
;;;;  Rockwell International Science Center Palo Alto Lab
;;;;********************************************************


;;;;;;;;;;;;;;;;;;;;;;;; Sampath ;;;;;;;;;;;;;;;;;;;;



(export '(SET-UP-FOR-POLY-TREE-INFER INITIALIZE-POLY-TREE))

;---- Setting up for poly tree infer ----------------------------------------

(defun set-up-for-poly-tree-infer (&optional (poly-tree *diagram*))
  (cond
    ((not (consistent-p (unlink-all-dummy-nodes poly-tree)))
     (error "The input diagram is not consistent"))
    ((not (belief-net-p poly-tree))
     (error "The input diagram is not a belief net"))
    ((not (singly-connected-belief-net-p poly-tree :error-mode nil))
     (error "The input diagram is not a singly connected belief net (poly-tree)"))
    (t  (prog1
	  (set-up-and-initialize-poly-tree poly-tree)
	  (reset-evidence poly-tree )
	  (set-diagram-initialization poly-tree :algorithm-type :POLY-TREE)))))

(defun set-up-and-initialize-poly-tree (poly-tree)
  (initialize-poly-tree (set-up-poly-tree-algorithm-data-structures poly-tree)))

; Initializing the diagram for the belief net update algorithm. It is
; essential that the diagram be ordered before each node is set coz when
; setting the initial pi-msgs on each node the pies of preceding nodes
; need to be already set.

(defun initialize-poly-tree (poly-tree)
  (dolist (node (order poly-tree))
    (reset-node-evidence node)
    (unlink-dummy-nodes-attached-to node)
    (set-lambda-msgs node)
    (set-pi-msgs&initial-beliefs node))
  (values poly-tree))

; Sets up the data structure and also sets the initial lambda messages
; to unit vectors as required at start up.

; There is an extra array associated with <node> itself on
; node-lambda-msg and node-pi-msg. These arrays are used to cache the
; overall lambdas and pies after calculating them.

(defun set-up-poly-tree-algorithm-data-structures (poly-tree)
  (mapc #'set-up-poly-tree-algorithm-data-structures-for-node poly-tree))

(defun set-up-poly-tree-algorithm-data-structures-for-node (node)
  (setf (node-bel node)(make-probability-array node))
  (setf (node-lambda-msg node)
	(mapcar #'(lambda (pred)
		    (cons pred (make-vanilla-msg-array pred)))
		(cons node (node-predecessors node))))
  (setf (node-pi-msg node)
	(mapcar #'(lambda (succ)
		    (cons succ (make-vanilla-msg-array node)))
		(cons node (node-successors node))))
  (values))

; Initializes overall lambda and the lambda msgs sent by <node>

(defun set-lambda-msgs (node)
  (for-all-cond-cases (node-case node)
    (setf (lambda-of node-case) 1))
  (dolist (parent (node-predecessors node))
    (for-all-cond-cases (parent-case parent)
      (setf (lambda-msg-of parent-case node) 1))))

; Sets the pi-msgs to the intial values which happen to be just the
; marginals of the node sending the message. The beliefs are also the
; marginals (obviously). The initial overall pies for each node are just
; the marginals.

(defun set-pi-msgs&initial-beliefs (node)
  (let ((total-belief 0))
    (for-all-cond-cases (node-case (list node))
      (let ((marginal-probability (calculate-pi-of node-case)))
	(incf total-belief
	      (setf (belief-of node-case) marginal-probability))
	(setf (pi-of node-case) marginal-probability)))
    (for-all-cond-cases (node-case (list node))
      (setf (belief-of node-case) (/ (belief-of node-case) total-belief))))
  (dolist (succ (node-successors node))
    (for-all-cond-cases (node-case (list node))
      (setf (pi-msg-of succ node-case) (belief-of node-case)))))

;---- Data structures for poly tree algorithm --------------

(defstruct internal-pt layer-number parent children activated-p)

(defun pt-layer-number (n)(internal-pt-layer-number (node-actual-bel n)))

(defsetf pt-layer-number (n)(value)
  `(setf (internal-pt-layer-number (node-actual-bel ,n)) ,value))

(defun pt-parent (n)(internal-pt-parent (node-actual-bel n)))

(defsetf pt-parent (n)(value)
  `(setf (internal-pt-parent (node-actual-bel ,n)) ,value))

(defun pt-children (n)
  (internal-pt-children (node-actual-bel n)))

(defsetf pt-children (n)(value)
  `(setf (internal-pt-children (node-actual-bel ,n)) ,value))

(defun pt-activated-p (n)(internal-pt-activated-p (node-actual-bel n)))

(defsetf pt-activated-p (n)(value)
  `(setf (internal-pt-activated-p (node-actual-bel ,n)) ,value))

(defun initialize-pt-data-structures  (poly-tree)
  (dolist (n poly-tree)
    (setf (node-actual-bel n) (make-internal-pt))))
