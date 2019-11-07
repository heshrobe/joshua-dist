;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: Ideal;  Base: 10 -*-

(in-package :ideal)



;;;;********************************************************
;;;;  Copyright (c) 1989, 1992 Rockwell International -- All rights reserved.
;;;;  Rockwell International Science Center Palo Alto Lab
;;;;********************************************************


;;;;;;;;;;;;;;;;;;;;;;;; Sampath ;;;;;;;;;;;;;;;;;;;;



(export '(SHAC-EVAL
	   SHAC-INFER
	   IDES-EVAL
	   IDES-INFER
	   NUMBER-OF-PREDECESSORS
	   NUMBER-OF-SUCCESSORS
	   FIND-MIN
	   FIND-MAX
	   PROBABILISTIC-ID-P))

;--------------------------------------------------------

;*******************************Shachter's decision algorithm ***************************

; Evaluting an influence diagram that contains a value-node. The
; algorithm returns an ordered list of decision-nodes. Each of the nodes
; has the policy set. The order is such that chronologically first node is
; first in the list and so on.

(defun shac-eval (&optional (diag *diagram*))
  (when (oriented&regular-p diag)
    (add-decision-ordering-arcs diag)
    (do ((value-node (find-if #'value-node-p diag))
	 (diagram (remove-all-barren-nodes
		    (reduce-all-deterministic-nodes
		      (include-instantiated-evidence diag))))
	 policy-list node-policy candidate  (index 1 (1+ index)))
	; when no predecessors for value node return the policy list
	((null (node-predecessors value-node))
	 (values (cons value-node policy-list)))
      (ideal-debug-msg "~2%Iteration-number: ~A" index)
      (cond
	; Remove chance node directly preceding value node and nothing else
	((setq candidate
	       (find-if #'(lambda (node)
			    (and (chance-node-p node)(precedes-only-value-node node)))
			(node-predecessors value-node))) 
	 (setq diagram (absorb-chance-node candidate diagram)))
	; Else: Remove decision node which has Preds(value-node) as a subset of
	; Preds(decision-node)
	((setq candidate
	       (find-if #'(lambda(node)(and (decision-node-p node)
					    (subsetp (node-predecessors value-node)
						     (informational-predecessors node))))
			(node-predecessors value-node)))
	 (multiple-value-setq (diagram node-policy)
	   (remove-decision-node candidate diagram))
	 (setq diagram (remove-all-barren-nodes diagram))
	 (push node-policy policy-list))
	; Else: find a chance node that precedes the value node but does not precede any
	; decison nodes. Make the node barren by successive arc reversals and then
	; eliminate from diagram.
	((setq candidate
	       (find-if #'(lambda (node)(and (chance-node-p node)
					     (every #'(lambda (n)(not (decision-node-p n)))
						    (node-successors node))))
			(node-predecessors value-node)))
	 (setq diagram (reduce-probabilistic-node candidate diagram :last-node value-node)))
	( t (error "Cant take any action. This should not happen. ~@
                    Debugging of code required"))))))

;**************************Shachter's inference algorithm *****************************

; New version that can take influence diagrams as input.

; This version abosorbs nodes with a single predecessor in addition to deleting
; barren nodes in the pre-processing stage. Suggested by Jack (10 Aug 89)

(defun shac-infer (goal-node cond-node-set)
  (cond
    ((value-node-p goal-node)
     (error "The goal node ~A is a value node. This is not permissible." goal-node))
    ((find-if #'value-node-p cond-node-set)
     (error "The conditioning set ~A contains a value node. This is not permissible"
			    cond-node-set))
    ( t (shac-infer-1 goal-node cond-node-set))))

(defun shac-infer-1 (goal-node input-cond-node-set)
  (let ((diagram (generate-diagram goal-node))
	(cond-node-set input-cond-node-set)
	(candidate nil))
    (remove-informational-predecessor-links&value-node diagram)
    (remove-all-nodes-with-less-than-two-succ 
      (remove-if #'(lambda (n)(or (eq n goal-node)(member n cond-node-set))) diagram))
    (make-node-barren goal-node diagram)
    (loop
      (setq candidate (find-shac-reduction-candidate goal-node cond-node-set))
      (cond
	((null candidate) (return))
	((decision-node-p candidate)
	 (ideal-debug-msg
	   "~%The decision node ~A is also required to answer this query. ~
             ~% It is being added to the list of conditioning nodes" candidate)
	 (push candidate cond-node-set))
	(t (reduce-chance-node candidate diagram :last-node goal-node))))
    (ideal-debug-actions
      (let ((irrelevant-nodes (set-difference cond-node-set (node-predecessors goal-node)))
	    (new-cond-nodes (set-difference (node-predecessors goal-node)
					    input-cond-node-set)))
	(if irrelevant-nodes
	    (format t "~2% The following conditioning nodes are irrelevant: ~A"
		    irrelevant-nodes))
	(if new-cond-nodes
	    (format t "~% The following nodes which were not in the original cond-set ~
                         are required to answer this query: ~A" new-cond-nodes)))))
  (values goal-node))

(defun find-shac-reduction-candidate (goal-node cond-node-set)
  (find-if-not #'(lambda (n)(member n cond-node-set)) (node-predecessors goal-node)))

;************************* Agogino's IDES decision algorithm **************************

(defun ides-eval (&optional (diagram *diagram*))
  (when (oriented&regular-p diagram)
    (add-decision-ordering-arcs diagram)
    (let ((value-node (find-if #'value-node-p diagram)))
      (setf diagram (remove-all-barren-nodes
		      (reduce-all-deterministic-nodes
			(include-instantiated-evidence diagram))))
      (do (policy-list node-policy candidate candidate-list (index 1 (1+ index)))
     ; when no predecessors for value node return the policy list
	  ((null (node-predecessors value-node))
	   (values (cons value-node policy-list)))
	(ideal-debug-msg "~%Iteration-number: ~A" index)
	(cond
	  ((setq candidate-list (remove-if-not #'absorption-candidate-node-p diagram))
	   (setq candidate (find-min candidate-list :key #'number-of-predecessors))
	   (setq diagram (absorb-chance-node candidate diagram)))
     ; Else: Remove decision node  such that Preds(value-node) are a subset of
     ; Preds(decision-node)
	  ((setq candidate (find-if #'(lambda(node)
					(and (decision-node-p node)
					     (subsetp (node-predecessors value-node)
						      (informational-predecessors node))))
				    (node-predecessors value-node)))
	   (multiple-value-setq (diagram node-policy)
	     (remove-decision-node candidate diagram))
	   (setq diagram (remove-all-barren-nodes diagram))
	   (push node-policy policy-list))
     ; Candidate list consists of nodes that are chance nodes and have no decision
     ; node successors
	  ((setq candidate-list (remove-if-not #'(lambda (n)
						   (and (chance-node-p n)
							(not (some #'decision-node-p
								   (node-successors n)))))
					       diagram))
	   (setq candidate (find-min candidate-list :key #'number-of-non-decision-node-succs))
	   (setq diagram (reduce-probabilistic-node candidate diagram
						    :last-node value-node)))
	  ( t (error "Cant take any action. This should not happen. ~@
                    Debugging of code required")))))))

;********************************* Agogino IDES inference algorithm

(defun ides-infer (goal-node cond-node-set)
  (cond
    ((value-node-p goal-node)
     (error "The goal node ~A is a value node. This is not permissible." goal-node))
    ((find-if #'value-node-p cond-node-set)
     (error "The conditioning set ~A contains a value node. This is not permissible"
	    cond-node-set))
    ( t (ides-infer-1 goal-node cond-node-set))))

(defun ides-infer-1 (goal-node input-cond-node-set)
  (let ((diagram (generate-diagram goal-node))
	(cond-node-set input-cond-node-set)(candidate nil))
    (remove-informational-predecessor-links&value-node diagram)
	; Removing barren nodes (This was done just after the "Making goal node barren" stage
	; before. Changed 9 Mar).
    (remove-all-barren-nodes
      (remove-if #'(lambda (n)(or (eq n goal-node)(member n cond-node-set))) diagram))
	; Making goal-node barren
    (do (succ-cand)
	((null (node-successors goal-node)))
      (setq succ-cand (find-min (remove-if-not
				  #'(lambda (s)(single-directed-path-p goal-node s))
				  (node-successors goal-node))
				:key #'number-of-predecessors))
      (reverse-arc goal-node succ-cand diagram))
	; Removing all nodes in node-predecessors of goal-node that are not cond-nodes
    (loop
      (setq candidate (find-ides-reduction-candidate goal-node cond-node-set))
      (cond
	((null candidate) (return))
	((decision-node-p candidate)
	 (ideal-debug-msg "~%The decision node ~A is also required to answer this ~
                     query.~% It is being added to the list of conditioning nodes" candidate)
	 (push candidate cond-node-set))
	(t (reduce-chance-node candidate diagram :last-node goal-node))))
	; The rest is just debugging info stuff
    (ideal-debug-actions
      (let ((irrelevant-nodes (set-difference cond-node-set (node-predecessors goal-node)))
	    (new-cond-nodes
	      (set-difference (node-predecessors goal-node) input-cond-node-set)))
	(if irrelevant-nodes
	    (format t "~2% The following conditioning nodes are irrelevant: ~A"
		    irrelevant-nodes))
	(if new-cond-nodes
	    (format t "~% The following nodes which were not in the original cond-set ~
                         are required to answer this query: ~A" new-cond-nodes)))))
  (values goal-node))

(defun find-ides-reduction-candidate (goal-node cond-node-set)
  (let* ((removal-possibilities
	   (remove-if #'(lambda (n)(eq n goal-node)(member n cond-node-set))
		      (node-predecessors goal-node)))
	 (absorption-possibilities
	   (remove-if-not #'absorption-candidate-node-p removal-possibilities)))
    (cond
      (absorption-possibilities
       (find-min absorption-possibilities :key #'number-of-predecessors))
      (removal-possibilities
       (find-min removal-possibilities :key #'number-of-successors))
      (t nil))))

;**** Utilitiey fns

(defun absorption-candidate-node-p (node)
  "This returns t if node is a chance node with a lone non-chance node successor"
  (and (chance-node-p node)
       (eql (length (node-successors node)) 1)
       (not (some #'decision-node-p (node-successors node)))))

(defun number-of-predecessors (node)(length (node-predecessors node)))

(defun number-of-successors (node)(length (node-successors node)))

(defun number-of-non-decision-node-succs (node)
  (let ((number 0))
    (dolist (n (node-successors node))(if (not (decision-node-p n))(incf number)))
    (values number)))

; The function <key> is applied to each element of list. Each
; application is expected to yield a number. These numbers are compared
; to each other using the fn <comparison> and the winner is returned.
		
(defun find-extremum (list key comparison)
  (cond
    ((null list)(error "find-extremum was handed a null list"))
    (t (let* ((start-flag (gensym)) (extreme-value start-flag) extreme-elt temp-value)
	 (dolist (elt list)
	   (setq temp-value (funcall key elt))
	   (when (or (eq extreme-value start-flag)
		     (funcall comparison temp-value extreme-value))
	     (setq extreme-value  temp-value extreme-elt elt)))
	 (values extreme-elt extreme-value)))))

; See find-extremum above.
(defun find-min (list &key (key #'identity))
  (declare (inline find-extremum))
  (find-extremum list key #'<))

(defun find-max (list &key (key #'identity))
  (declare (inline find-extremum))
  (find-extremum list key #'>))


(defun probabilistic-id-p (diagram)
  (every #'chance-node-p diagram))
