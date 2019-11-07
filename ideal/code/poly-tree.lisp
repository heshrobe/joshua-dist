;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: Ideal;  Base: 10 -*-

(in-package :ideal)



;;;;********************************************************
;;;;  Copyright (c) 1989, 1992 Rockwell International -- All rights reserved.
;;;;  Rockwell International Science Center Palo Alto Lab
;;;;********************************************************


;;;;;;;;;;;;;;;;;;;;;;;; Sampath ;;;;;;;;;;;;;;;;;;;;



(export '(POLY-TREE-INFER))


;----- Poly tree algorithm -----------------------------------------

(defun poly-tree-infer (&optional (poly-tree *diagram*))
  (when (and (initialized-diagram poly-tree :algorithm-type :POLY-TREE)
	     (check-evidence poly-tree))
	; Find evidence
    (let ((evidence-list (create-evidence-list poly-tree)))
	; Propagate evidence
      (propagate-evidence-set evidence-list (poly-tree-order poly-tree))
	; Mark evidence as propagated.
      (dolist (e evidence-list)
	(setf (node-old-state (car e))(cdr e)))
	; Mark diagram as unchanged
      (mark-diagram-as-unchanged poly-tree))
    (values poly-tree)))


(defun poly-tree-order (diagram)
  ; Initialize run time data structures
  (initialize-pt-data-structures diagram)
  (let (unmarked-root-node)
    (loop
      (if (null (setq unmarked-root-node (find-if #'unmarked-root-node diagram))) (return))
      (poly-tree-order-1 unmarked-root-node))
    (sort (copy-list diagram) #'> :key #'pt-layer-number)))

(defun unmarked-root-node (n)
  (and (null (node-predecessors n))(not (numberp (pt-layer-number n)))))
     
(defun poly-tree-order-1 (root-node)
  (setf (pt-layer-number root-node) 0 (pt-parent root-node) nil)
  (let ((queue (list root-node)))
    (loop
      (if (null queue)(return)) 
      (let ((current-node (pop queue)))
	(dolist (neighbour (neighbours current-node))
	  (when (not (numberp (pt-layer-number neighbour)))
	    (setf (pt-layer-number neighbour)(+ 1 (pt-layer-number current-node)))
	    (setf (pt-parent neighbour) current-node)
	    (push neighbour (pt-children current-node))
	    (push neighbour queue)))))))


(defun propagate-evidence-set (evidence-list ordered-poly-tree)
  (dolist (evidence evidence-list)
    (set-up-dummy-inf-node-for-evidence evidence))
  (pt-propagate ordered-poly-tree)
  (calculate-and-normalize-beliefs ordered-poly-tree))

(defun pt-propagate (ordered-poly-tree)
  (dolist (n ordered-poly-tree)
    (pt-upward-propagate n))
  (dolist (n (reverse ordered-poly-tree))
    (pt-downward-propagate n)))

(defun pt-upward-propagate (node)
  (update-overall-lambda node)
  (update-overall-pi node)
  (cond
    ((null (pt-parent node)))
    ((member (pt-parent node) (node-predecessors node))
     (update-lambda-msg-to (pt-parent node) node))
    ((member (pt-parent node)(node-successors node))
     (update-pi-msg-to (pt-parent node) node :normalize :NO))
    (t (error "pt-parent of ~A is neither a dag child nor a dag parent of node" node))))

(defun pt-downward-propagate (node)
  (cond
    ((null (pt-parent node))
     nil)
    ((member (pt-parent node) (node-predecessors node))
     (update-overall-pi node))
    ((member (pt-parent node) (node-successors node))
     (update-overall-lambda node))
    (t (error "pt-parent of ~A is neither a dag child nor a dag parent of node" node)))
  (dolist (c (pt-children node))
    (cond
      ((member c (node-predecessors node))(update-lambda-msg-to c node))
      ((member c (node-successors node))(update-pi-msg-to c node :normalize :NO))
      (t (error "Pt-child ~A of node ~A is neither a dag parent nor dag child of node"
		c node)))))



; -------- Estimator function ------------------------------------------------------

;This function makes estimates for a complete sweep of the poly-tree
;where no activation-type optimizations are taken into account.

(defun poly-tree-infer-est (poly-tree)
  (let ((ordered-poly-tree (poly-tree-order poly-tree)))
    (+ (sum-over (n ordered-poly-tree)(message-update-est n))
       (sum-over (n ordered-poly-tree)
	 (+(dummy-node-creation-est n)
	   (belief-normalization-est n))))))

; Formulae: Sp = State space size of Parent set of node
;           Sn = State space size of node
; Operation counts:
; Updating a lambda msg to a parent:  Sp * Sn
; Updating overall lambda          :  Sn
; Updating pi msg to child         :  Sn
; Updating overall Pi              :  Sp * Sn
; Fn message update est calculates what is explained below:
; Each node updates every outgoing message (to children or to parents once)
; Overall Pi and Overall Lambda: Each is updated once for each node during
; the upward sweep. On the downward sweep the overall pi is updated if the
; PT-parent of node is a predecessor node and the ovreall lambda is updated
; if the pt-parent of node is a successor (this feature is taken care
; of by pt-parent-factor.


; Other stuff:
; Each dummy node goes has complexity Sn to set up.
; Each node is normalized in the end with a complexity 2*Sn.

(defun message-update-est (n)
  (let ((parent-space-size (product-over (n (node-predecessors n))(number-of-states n))))
    (+  (* (+ 1 (number-of-children n))(number-of-states n))
	(* (+ 1 (number-of-parents n)) parent-space-size (number-of-states n))
	(pt-parent-factor n parent-space-size))))

(defun pt-parent-factor (n parent-space-size)
  (cond
    ((member (pt-parent n)(node-predecessors n)) (* parent-space-size (number-of-states n)))
    ((member (pt-parent n)(node-successors n))(number-of-states n))
    ((null (pt-parent n)) 0)
    (t (error "Pt-parent ~A is neither a parent of ~A, child of ~A or nil"
	      n n (pt-parent n)))))

(defun dummy-node-creation-est (n)
  (if (unchanged-evidence-p n) 0 (number-of-states n)))

(defun belief-normalization-est (n)
  (* 2 (number-of-states n)))

(defun number-of-parents (n)(length (node-predecessors n)))

(defun number-of-children (n)(length (remove-if #'dummy-node-p (node-successors n))))
