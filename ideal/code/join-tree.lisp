;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: Ideal;  Base: 10 -*-

(in-package :ideal)



;;;;********************************************************
;;;;  Copyright (c) 1989, 1992 Rockwell International -- All rights reserved.
;;;;  Rockwell International Science Center Palo Alto Lab
;;;;********************************************************


;;;;;;;;;;;;;;;;;;;;;;;; Sampath ;;;;;;;;;;;;;;;;;;;;



(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '( )))


;----- Assembling a join tree structure -------------------

(defstruct (jt-node (:print-function print-jt-node))
  clique (children nil) (parent nil))

(defun print-jt-node (n s d)
  (declare (ignore d))
  (format s "#<JTN ~A>" (mapcar #'node-name (jt-node-clique n))))

; This fn's return value is the input to the propagation part of the
; clustering alogrithm set (clustering-infer and jensen-infer). This fn
; is meant to be the interface between various possible triangulation
; schemes and various possible clustering inference schemes.

; the return value is a join tree (with each clique represented as a
; JT-NODE) with the leaves appearing first and root appearing last.

(defun find-join-tree-structure (belief-net)
  (collapse-cliques
    (find-cliques				
      (triangulate-graph
	(convert-into-markov-network belief-net)))))

; Note that even if the cliques came from a belief net consisting of
; multiple subnets you will get a single tree (even though the
; representation could be in terms of multiple trees). The parent the
; first node added from a new possible sub-tree will just be any node in
; the existing tree since the parent is chosen on the basis of max nodes
; shared and nodes shared will be zero for all the cliques in the other
; subtree.

(defun collapse-cliques (clique-list)
  (let ((parent-jt-node nil)
	(join-tree (list (make-jt-node :clique (first clique-list)))))
    (dolist (clique (rest clique-list))
      (cond
	((null (setq parent-jt-node
		     (find-max join-tree :key #'(lambda (jtn)(n-common-nodes jtn clique)))))
	 (error "Didnt find a parent for clique ~A in join-tree ~A" clique join-tree))
	((subsetp (jt-node-clique parent-jt-node) clique)
	 (setf (jt-node-clique parent-jt-node) clique))
	(t
	 (push (make-jt-node :clique clique :parent parent-jt-node) join-tree))))
    (values join-tree)))

(defun n-common-nodes (jt-node clique)
  (length (intersection (jt-node-clique jt-node) clique)))

; Returns list of cliques.  Each clique is a list of belief netnodes
; nodes.  The cliques in the list returned are in increasing order of
; maximum index number of markov node found in a clique.

(defun find-cliques (ordered-chordal-graph)
  (when (ordered-by-index-number ordered-chordal-graph #'<)
    (mapcar #'(lambda (m-node)
		(mapcar #'markov-node-ref-node
			(cons m-node (markov-node-chordal-parents m-node))))
	    ordered-chordal-graph)))

;--------------------------------------------------------
      	
; When converting the diagram to a Markov network, the node-successors field id now
; used to store the neighbours. Therefore the following access definition for node
; neighbours

(defstruct (markov-node (:print-function print-markov-node))
   index
   neighbours
   chordal-parents
   ref-node
   (number-of-ordered-neighbours 0))

(defun print-markov-node (m-node st depth)
  (declare (ignore depth))
  (format st "#<M-node ~A ~A>"
	  (when (markov-node-ref-node m-node)
	    (node-name (markov-node-ref-node m-node)))
	  (markov-node-index m-node)))

;;; *** Functions used by convert-into-markov-network

; Was hacked to drop dummy nodes in the input id-nodes list. The hack
; has been dropped and the calling fns strip the dummy nodes before
; passing the list now (9 Feb 90).

(defun find-m-nodes (id-nodes m-network)
  (mapcar #'(lambda (id-node)(find-m-node id-node m-network)) id-nodes))

(defun find-m-node (id-node m-network)
  (find id-node m-network :key #'markov-node-ref-node))

(defun inter-connect (markov-node-list)
  (let (first-node)
    (loop
      (if (null markov-node-list)(return))
      (setq first-node (pop markov-node-list))
      (dolist (rest-node markov-node-list)
	(connect first-node rest-node :mode :SILENT)))))
    
(defun connect (m-node-1 m-node-2 &key (mode :ERROR))
  (cond
    ((eq m-node-1 m-node-2)
     (when (eq mode :ERROR)
       (error "Trying to connect node ~A to itself" m-node-1)))
    ((find m-node-1 (markov-node-neighbours m-node-2))
     (when (eq mode :ERROR)
       (ideal-warning "Node ~A and Node ~A are already connected" m-node-1 m-node-2))
     (values nil))
    (t (push m-node-1 (markov-node-neighbours m-node-2))
       (push m-node-2 (markov-node-neighbours m-node-1))
       (values t))))

;-- Converts influence diagram into and returns a markov network ------------------------
						
(defun convert-into-markov-network (inf-diagram)
  (let ((network (mapcar #'(lambda (id-node)
			     (make-markov-node :ref-node id-node)) inf-diagram)))
    (dolist (m-node network)
      (setf (markov-node-neighbours m-node)
	    (nconc (find-m-nodes
		     (node-predecessors (markov-node-ref-node m-node))
		     network)
		   (find-m-nodes
		     (remove-if #'dummy-node-p (node-successors (markov-node-ref-node m-node)))
		     network))))
    (dolist (child network)
      (inter-connect
	(find-m-nodes (node-predecessors (markov-node-ref-node child)) network)))
    (values  network)))

;---  Top level function for triangulating graph-----------------------------------------

; NOTE: Returns markov network sorted such that nodes appear in increasing
; order of the max cardinality search index.

; Can handle a belief net that consists of multiple disjoint subnets.

(defun triangulate-graph (markov-net)
  (let ((ordered-graph (order-for-clustering (nreverse markov-net))))
    (fill-in-edges ordered-graph)
    (values (nreverse ordered-graph))))

; Incorporated this function 13 Mar 90. Code by Robert Goldman in jensen-search.lisp  
;(defun order-for-clustering-jensen (m-network)
;  (declare (ignore m-network))
;  (error "This fn has not been implemented yet"))

; This function does a maximum cardinality search (pg 112, Pearl) and
; assigns nodes appropriate indices.

; NOTE: Returns markov network sorted such that nodes appear in decreasing
; order of the max cardinality search index.


(defun order-for-clustering (m-network)
  (cond
    (*jensen-search* (order-for-clustering-jensen m-network))
    (t (maximum-cardinality-search m-network))))

(defun maximum-cardinality-search (m-network)
  (when m-network
    (let ((sorted-m-network nil)(current-index 1)(unindexed-node nil)(indexed-subgraph nil))
      (map nil #'unindex m-network)
      (loop
	(setf unindexed-node (choose-arbitrary-unindexed-node m-network))
	(if (null unindexed-node)(return sorted-m-network))
	(multiple-value-setq (indexed-subgraph current-index)
	  (m-c-s (list unindexed-node) current-index))
	(setq sorted-m-network (nconc indexed-subgraph sorted-m-network))))))

(defun m-c-s (candidates current-index &optional sorted-m-network)
  (cond
    ((null candidates) (values sorted-m-network current-index))
    (t  (let* ((best-candidate
		 (find-max candidates :key #'markov-node-number-of-ordered-neighbours)))
	   (setf (markov-node-index best-candidate) current-index)
	   (setf candidates (delete best-candidate candidates))
	   (dolist (node (markov-node-neighbours best-candidate))
	     (when (not (indexed-p node))
	       (incf (markov-node-number-of-ordered-neighbours node))
	       (if (not (member node candidates)) (push node candidates))))
	   (m-c-s
	     candidates (incf current-index) (push best-candidate sorted-m-network))))))

(defun choose-arbitrary-unindexed-node (m-node-list)
  (find-if-not #'indexed-p m-node-list))

(defun indexed-p (m-node)
  (not (null (markov-node-index m-node))))

(defun unindex (m-node)
  (setf (markov-node-index m-node) nil))

; Recursive link addition algorithm that makes the ordered undirect graph that is input
; a chordal graph

(defun fill-in-edges (ordered-graph)
  (when (ordered-by-index-number ordered-graph #'>)
    (fill-in-edges-1 ordered-graph)))

(defun fill-in-edges-1 (ordered-graph)
  (cond
    ((null ordered-graph) nil)
    (t (let* ((top-node (first ordered-graph))
	      (top-node-index (markov-node-index top-node))
	      (parents (remove-if-not
			 #'(lambda (n)(< (markov-node-index n) top-node-index))
			 (markov-node-neighbours top-node))))
	 (inter-connect parents)
	 (setf (markov-node-chordal-parents top-node) (copy-list parents))
	 (fill-in-edges-1 (rest ordered-graph))))))

; This predicate checks whether the input markov network mentions nodes
; in decreasing order of max cardinality search index.

(defun ordered-by-index-number (graph predicate &optional prev-index-number)
  (let (new-index-number)
    (cond
      ((null graph) t)
      ((progn (setq new-index-number (markov-node-index (first graph)))
	      (or (null prev-index-number)
		  (funcall predicate prev-index-number new-index-number)))
       (ordered-by-index-number (rest graph) predicate new-index-number))
      ( t (error "The input graph is not ordered by node-index with fn ~A" predicate)))))



;------------ Utilities.

; This predicate is useful for debugging. Returns t if the join tree
; satisfies the running intersection property as it is supposed to.

(defun has-running-intersection-property-p (join-tree-structure &optional (error t))
  (and (all-unique-sets (mapcar #'jt-node-clique join-tree-structure) error)
       (properly-ordered-p join-tree-structure error)
       (running-intersection-property-p join-tree-structure error)))

(defun properly-ordered-p (join-tree-structure &optional (error t))
  (or (null (jt-node-parent (first join-tree-structure)))
      (and (member (jt-node-parent (first join-tree-structure))
		   (rest join-tree-structure))
	   (properly-ordered-p (rest join-tree-structure) error))
      (if error
	  (error "The node ~A's parent ~A is not in ordered after it in the join tree"
	     (first join-tree-structure)(jt-node-parent (first join-tree-structure)))
	  nil)))

; Either returns t or breaks if error arg is T

(defun running-intersection-property-p (join-tree-structure &optional (error t))
  (cond
    ((null join-tree-structure)(values t nil))
    (t (multiple-value-bind (t-val union-set)
	   (running-intersection-property-p (rest join-tree-structure) error)
	 (let* ((jt-node (first join-tree-structure))
		(jt-node-parent (jt-node-parent jt-node)))
	   (cond
	     ((not (or (null jt-node-parent)
		       (subsetp (intersection union-set (jt-node-clique jt-node))
				(jt-node-clique jt-node-parent))))
	      (if error
		  (error "The jt node ~A's intersection with its ancestors is not contained
                    in the parent ~A" jt-node jt-node-parent))
	      nil)
	     (t
	      (values t-val (union union-set (jt-node-clique jt-node))))))))))

(defun all-unique-sets (sets &optional (error t))
  (cond
    ((null sets) t)
    (t (let ((first-set (first sets)) subset superset)
	 (cond
	   ((some #'(lambda (other-set)
		      (or (subsetp (setq subset other-set)(setq superset first-set))
			  (subsetp (setq subset first-set)(setq superset other-set))))
		  (rest sets))
	    (if error (error " ~A is a subset of ~A" subset superset))
	    nil)
	   (t
	    (all-unique-sets (rest sets) error)))))))
