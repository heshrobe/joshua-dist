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

; This uses a simple (O n**2) algorithm from Cooper (in the inf diag
; wkshop stuff).

(defun get-cycle-cutset (belief-net)
  (replace-with-nodes-from
    belief-net 
    (find-cycle-cutset-1 (make-bare-bones-copy belief-net))))

; Makes new node and copies predecessor and successors fields
; appropriately. All other fields STILL HAVE the contents of the same
; field of the ORIGINAL node. Dummy nodes are not present in the
; returned copy.

(defun make-bare-bones-copy (belief-net)
  (let ((copied-bel-net (mapcar #'copy-node belief-net)))
    (dolist (n copied-bel-net)
      (setf (node-successors n)
	    (replace-with-nodes-from copied-bel-net
				     (remove-if #'dummy-node-p (node-successors n))))
      (setf (node-predecessors n)
	    (replace-with-nodes-from copied-bel-net
				     (node-predecessors n))))
    (values copied-bel-net)))

; Replaces each node in input list with a node of the same name from
; replacement-list.

(defun replace-with-nodes-from (replacement-list input-list)
  (mapcar #'(lambda (n)(find n replacement-list :test #'same-name)) input-list))

; The actual cutset algorithm

(defun find-cycle-cutset-1 (directed-graph)
  (let (cycle-cutset candidate-cutset-nodes winning-candidate)
    (loop
	; Recursively delete all nodes which are not part of a loop
      (setq directed-graph (delete-all-nodes-which-are-not-in-loops directed-graph))
      (if (null directed-graph) (return cycle-cutset))
	; Find all candidates that satisfy the Loop cutset condition and the max neighbour
	; heuristic
      (setq candidate-cutset-nodes
	    (remove-if-less-than-max-neighbours
	      (remove-if-not #'(lambda (n)(less-than-two (node-predecessors n)))
			     directed-graph)))
	; Find winner by number-of-states heuristic
      (setq winning-candidate
	    (find-min candidate-cutset-nodes :key #'number-of-states))
	; delete the winner from graph and ...
      (setq directed-graph (delete-links&node winning-candidate directed-graph))
	; add winner to cycle-cutset
      (push winning-candidate cycle-cutset))))

(defun delete-links&node (node diagram)
  ; Deletes links and trashes node from diagram. Does NOT fool around with node fields other
  ; than predecessors and successors
  (mapc #'(lambda (pred)(delete-link node pred)) (node-predecessors node))
  (mapc #'(lambda (succ)(delete-link succ node)) (node-successors node))
  (delete node diagram))

; Reursively deletes all nodes that are not part of a loop and then returns bel-net

(defun delete-all-nodes-which-are-not-in-loops (bel-net)
  (let (candidate)
    (loop
      (setq candidate (find-if #'less-than-two-neighbours-p bel-net))
      (if (null candidate)(return bel-net))
      (setq bel-net (delete-links&node candidate bel-net)))))

(defun less-than-two-neighbours-p (node)
  (cond
    ((null (node-predecessors node)) (less-than-two (node-successors node)))
    ((null (node-successors node))(less-than-two (node-predecessors node)))))

; Removes all members of candidate-cutset-nodes which have less
; neighbours than any other member of candidate-cutset-nodes and returns
; the result.

(defun remove-if-less-than-max-neighbours (candidate-cutset-nodes)
  (labels ((number-of-neighbours (node)(+ (length (node-predecessors node))
					  (length (node-successors node)))))
    (multiple-value-bind (ignore max-number-of-neighbours)
	(find-max candidate-cutset-nodes :key #'number-of-neighbours)
      (declare (ignore ignore))
      (remove-if-not #'(lambda (n)
			 (= (number-of-neighbours n) max-number-of-neighbours))
		     candidate-cutset-nodes))))

