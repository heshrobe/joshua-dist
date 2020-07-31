;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: Ideal;  Base: 10 -*-

(in-package :ideal)


;;;
;;;  Copyright 1990 Robert P. Goldman, All rights reserved.
;;; 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '( )))

;----------------------------------------------------------------------------------

;;;  This file contains functions necessary to apply Jensen and Kjaerulff's
;;;  elimination ordering heuristic to find a clustering scheme.  The algorithm
;;;  works as follows:

;;; 1) find the elimination ordering per heuristic [see below]
;;; 2) construct a fill-in corresponding to this elimination order.
;;; 3) order the resulting filled-in graph by maximum cardinality search.  This
;;; last step is necessary so that the resulting cliques will have the running
;;; intersection property.

;;;An elimination ordering is an ordering for fill-in.  The heuristic used here
;;;for finding a good elimination ordering is a simple greedy method:  if there
;;;are any nodes that can be eliminated without adding any edges (i.e., whose
;;;neighbors are already a clique), eliminate them, else eliminate the node
;;;with the minimum-cost of elimination.  Elimination cost is defined as the
;;;state space of the eliminated node and all its neighbors.
						
;;;The following variable is used because we have a version of the
;;;junction-tree algorithm that puts the evidence in the network BEFORE
;;;building the tree.  This can yield significant savings, but only works for
;;;special purposes.  You can safely regard it as a constant bound to nil.
(defvar *take-evidence-into-account* nil
  "This variable is used as a switch when we are computing the cost of adding a node to the network.")

;;;Safe interface to a destructive operation.
(defmacro safe-construct-elimination-order (m-network)
  `(construct-elimination-order (copy-list ,m-network)))


; This function does a search as per the note from Jensen and
; assigns nodes appropriate indices.

(defun order-for-clustering-jensen (m-network)
  (when m-network
    (let (elimination-order)
      ;;save the markov-node-neighbours field because it is destroyed in the
      ;;process of constructing the elimination-order.  In this function the
      ;;chordal-parents field of the m-nodes is just used as a backup.  Maybe
      ;;we should create a special field for this purpose, or push it onto a
      ;;list instead...
      (mapc #'(lambda (markov-node)
		(setf (markov-node-chordal-parents markov-node)
		      (copy-list (markov-node-neighbours markov-node)))
		(unindex markov-node)) m-network)
      (safe-construct-elimination-order m-network)
      ;;restore the markov-node-neighbours field...
      (mapc #'(lambda (markov-node)
		(setf (markov-node-neighbours markov-node)
		      (markov-node-chordal-parents markov-node))
		(setf (markov-node-chordal-parents markov-node) nil)) m-network)
      (setf elimination-order (sort m-network #'> :key #'markov-node-index))
      (fill-in elimination-order)
      (maximum-cardinality-search elimination-order))))


#||
;;;An obsolete function, I can't help thinking it might be useful again some day...
(defun copy-markov-network (mn)
  (let ((network (mapcar #'copy-markov-node mn)))
    (loop for m-node in network
	  do (setf (markov-node-neighbours m-node)
		   (find-m-nodes (mapcar #'markov-node-ref-node (markov-node-neighbours m-node))
				 network)))
    network))
||#

(defun construct-elimination-order (m-network)
  "Constructs an elimination order per Jensen & Kjaerulff's heuristic.
Destroys its argument -- pass it a copy of the network you want ordered."
  (eliminate-a-node m-network (list-length m-network)))

;;;finds a node in the markov-network to eliminate, and does so
;;;Does not return anything of interest
;;;Chooses a node per Jensen's greedy algorithm:
;;;1) eliminates a node with a complete neighbor set, or
;;;2) eliminates the node with the minimum cost to eliminate
(defun eliminate-a-node (markov-network index)
  (if (> (list-length markov-network) 1)
      (let ((node-to-eliminate (find-if #'neighbors-a-clique-p markov-network)))
	(unless node-to-eliminate 
	  (setf node-to-eliminate (minimum-cost-to-eliminate markov-network)))
	(eliminate-node node-to-eliminate index)
	(eliminate-a-node (delete node-to-eliminate markov-network) (1- index)))
      (eliminate-node (first markov-network) index))
  (values))

;;;to eliminate a node, interconnect its parents, then remove it and all its
;;;edges.
(defun eliminate-node (mnode index)
  (inter-connect (markov-node-neighbours mnode))
  (mapc #'(lambda (on) (setf (markov-node-neighbours on)
		 (delete mnode (markov-node-neighbours on) :test #'eq)))
	(markov-node-neighbours mnode))
  (setf (markov-node-index mnode) index
	(markov-node-neighbours mnode) nil)
  (values))

;;;a predicate
#||
(defun neighbors-a-clique (node)
  (loop for nnl on (markov-node-neighbours node)
	for n1 = (first nnl)
	for others = (cdr nnl)
	while others
	unless (every #'(lambda (x) (member n1 (markov-node-neighbours x))) others)
	  return nil
	finally (return t)))
||#

(defun neighbors-a-clique-p (node)
    (neighbors-a-clique-1 (markov-node-neighbours node)))

(defun neighbors-a-clique-1 (nodelist)
  (let ((n1 (first nodelist))
	(others (cdr nodelist)))
    (cond ((null others) t)
	  ((every #'(lambda (x) (member n1 (markov-node-neighbours x))) others)
	   (neighbors-a-clique-1 (cdr nodelist)))
	  (t nil))))

  
;;;finds the node in the markov-network with the minimum elimination-cost
(defun minimum-cost-to-eliminate (m-net)
  (let (cost
	(min (elimination-cost (car m-net)))
	(bestnode (first m-net)))
    (dolist (node (cdr m-net))
      (setf cost (elimination-cost node))
      (when (< cost min)
	(setf min cost
	      bestnode node)))
    bestnode))

; Note: Robert didnt provide the code for stm-evidence, therefore this dummy.
; See the documentation for *take-evidence-into-account* above. The variable
; is effectively a constant bound to nil and so stm-evidence is never called
; anyway. I have put in this dummy definition to avoid compiler warnings.
; --- Sampath

(defun stm-evidence (arg)(declare (ignore arg)))

;;;elimination cost is the size of the state space of the node and its
;;;neighbors.
(defun elimination-cost (mnode)
  (let ((cost (if (and *take-evidence-into-account*
		       (stm-evidence (markov-node-ref-node mnode)))
		  1
		  (number-of-states (markov-node-ref-node mnode)))))
    (dolist (id-node (mapcar #'markov-node-ref-node (markov-node-neighbours mnode)))
      (setf cost (* cost (if (and *take-evidence-into-account*
				  (stm-evidence id-node))
			     1
			     (number-of-states id-node)))))
    cost))


(defun fill-in (ordered-graph)
  (if  (ordered-by-index-number ordered-graph #'>)
       (fill-in-1 ordered-graph)
       (error "input to fill-in-edges not properly sorted.")))

(defun fill-in-1 (ordered-graph)
  (cond
    ((null ordered-graph) nil)
    (t (let* ((top-node (first ordered-graph))
	      (top-node-index (markov-node-index top-node))
	      (parents (remove-if-not
			 #'(lambda (n)(< (markov-node-index n) top-node-index))
			 (markov-node-neighbours top-node))))
	 (inter-connect parents)
	 (fill-in-1 (rest ordered-graph))))))
