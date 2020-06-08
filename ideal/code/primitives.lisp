;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: Ideal;  Base: 10 -*-

(in-package :ideal)



;;;;********************************************************
;;;;  Copyright (c) 1989, 1992 Rockwell International -- All rights reserved.
;;;;  Rockwell International Science Center Palo Alto Lab
;;;;********************************************************


;;;;;;;;;;;;;;;;;;;;;;;; Sampath ;;;;;;;;;;;;;;;;;;;;



(export '(REMOVE-BARREN-NODE
	   REMOVE-ALL-BARREN-NODES
	   ABSORB-CHANCE-NODE
	   REMOVE-DECISION-NODE
	   REVERSE-ARC
	   PROPOGATE-DETERMINISTIC-NODE
	   CONVERT-DET-NODE-TO-PROB
	   REDUCE-CHANCE-NODE
	   MAKE-NODE-BARREN
	   REDUCE-DETERMINISTIC-NODE
	   REDUCE-ALL-DETERMINISTIC-NODES
	   REDUCE-PROBABILISTIC-NODE))

;--------------------------------------------------------

; Influence diagram manipulation primitives.

; ******************* REMOVING BARREN NODES *********************************

; Removes node and returns updated diagram without the node

(defun remove-barren-node (node &optional (diagram *diagram*))
  (cond
    ((barren-node-p node)
     (ideal-debug-msg "~%Removing barren node ~A" node)
     (delete-links  node)(remove node diagram))
    (t (error "Node ~A is not barren. Cannot remove-barren-node it." node))))

(defun remove-all-barren-nodes (diagram)
  (let (b-node)
    (loop
      (setq b-node (find-if #'barren-node-p diagram))
      (if (null b-node) (return diagram))
      (setq diagram (remove-barren-node b-node diagram)))))

;************ A primitive used by new-shac-infer

; Recursively removes all nodes from the diagram that have less than two
; successor.

(defun remove-all-nodes-with-less-than-two-succ (diagram)
  (labels ((less-than-two-successors-p (n)(null (cdr (node-successors n)))))
    (let (candidate)
      (loop
	(setq candidate (find-if #'less-than-two-successors-p diagram))
	(if (null candidate)(return diagram))
	(setq diagram (absorb-chance-node candidate diagram))))))

; ***************** REMOVING CHANCE NODES****************************************

(defun absorb-chance-node (node &optional (diagram *diagram*))
  (cond
    ((not (chance-node-p node))
     (error "Non chance node ~A given as input to ABSORB-CHANCE-NODE" node))
    ((not (<= (length (node-successors node)) 1))
     (error "The node ~A has more than one successor. Cannot remove" node))
    ((some #'decision-node-p (node-successors node))
     (error "The successor of ~A is a decision node or deterministic chance. ~@
             Cannot remove" node))
    ((not (discrete-dist-node-p node))
     (error "Non discrete node ~A encountered. ABSORB-CHANCE-NODE cannot handle this" node))
    (t (cond
	 ((null (node-successors node)) (remove-barren-node node diagram))
	 (t  (ideal-debug-msg "~%Removing probablistic chance node ~A by absorbing into ~
                               its lone successor ~A" node (first (node-successors node)))
	     (discrete-absorb-chance-node node diagram))))))

(defun discrete-absorb-chance-node (node diagram)
  (let ((succ-node (first (node-successors node))))
	;Convert nodes if necessary
    (if (deterministic-node-p node)(convert-det-node-to-prob node))
    (when (chance-node-p succ-node)
      (cond
	((deterministic-node-p succ-node)(convert-det-node-to-prob succ-node))
	((noisy-or-node-p succ-node)(convert-noisy-or-node-to-chance-node succ-node))))
    (let* ((old-succ-array (distribution-repn succ-node))
	   (old-succ-preds (node-predecessors succ-node)))
	; Delete node from succ-node's predecessors
      (delete-link succ-node node)
	; Add all node's predecessors as predecessors of succ node
      (dolist (pred (node-predecessors node))
	(add-link succ-node pred))
	; Create a new empty distribution for succ-node of the appropriate size.
      (create-empty-distribution succ-node )
	; For all cond cases of the (latest) pred set of succ node  ....
      (for-all-cond-cases (succ-case succ-node)
	(for-all-cond-cases  (cond-case (node-predecessors succ-node))
	; Let the expected value be 0
	  (let ((expected-value 0))
	; For all cond-cases of node
	    (for-all-cond-cases (node-case (list node))
	; Mutiply (value/(cond-case U node-case)) and P(node/cond-case) and add
	; to expected valuec. (Dont forget that irrelevant preds are ignored when
	; looking up probs and succs).
	      (incf expected-value (* (contents-of-dist-array-location
					old-succ-array succ-case
					(combine-cond-cases node-case cond-case)
					old-succ-preds)
				      (prob-of node-case cond-case))))
	; Now set the appropriate loc in the new succ dist to the expected succ
	    (setf (contents-of succ-case cond-case) expected-value))))
	; Remove mention of node from all its predecessors' successor lists
      (delete-links node)
	; Remove node from diagram and then return the diagram
      (remove node diagram))))

; *************************** REMOVING DECISION NODE ********************************

; Removes a decision node that just precedes value node if it also
; satisfies other pre-reqs. (See Schacter86). Returns updated diagram
; without node.

(defun remove-decision-node (dec-node &optional (diagram *diagram*))
  (case (node-type dec-node)
    (:DECISION
      (let ((value-node (find-value-node (node-successors dec-node))))
	(cond
	  ((not value-node) (error "Decision node ~A is not an immediate ~@
                                             predecessor of the value node. remove-~@
                                             decision node cannot remove" dec-node))
	  ((and (every #'discrete-dist-node-p (node-predecessors dec-node))
		(every #'discrete-dist-node-p (node-predecessors value-node)))
	   (cond
	     ((subsetp (node-predecessors value-node)
		       (informational-predecessors dec-node))
	      (ideal-debug-msg "~%Removing decision node ~A" dec-node)
	      (remove-discrete-dec-node dec-node value-node diagram))
	     (t (error "Decision node ~A does not satisfy removal prereqs"
		       dec-node))))
	  ((not (precedes-only-value-node dec-node))
	   (error "The decision node ~A cannot be removed since it does not
                            have a lone value node successor" dec-node))
	  ( t (error "Non discrete nodes involved in the removal of decision node ~@
                              ~A. Remove-decision-node doesnt know how to handle this"
		     dec-node)))))
    (t (error "Node ~A, a non-decision node, was given as input to Remove-decision node"
	      dec-node))))

(defun remove-discrete-dec-node (dec-node value-node diagram)
  (let* ((old-value-array (distribution-repn value-node))
	 (old-value-preds (node-predecessors value-node)))
    (delete-link value-node dec-node)
    (delete-links dec-node)
	; The preds of dec-node are being set to new preds of value-node. These will
	; be the only nodes relevant to the policy of dec-node.
    (setf (node-predecessors dec-node)(copy-list (node-predecessors value-node)))
    (create-empty-distribution value-node)
    (create-empty-distribution dec-node)
    (for-all-cond-cases (combined-cond-case (node-predecessors dec-node))
      (let (max-utility best-decision)
	(for-all-cond-cases (dec-node-case (list dec-node))
	  (multiple-value-setq (max-utility best-decision)
	    (find-better-decision max-utility best-decision
				  (contents-of-det-array-location
				    old-value-array
				    (combine-cond-cases dec-node-case combined-cond-case)
				    old-value-preds)
				  (state-in dec-node-case))))
	(setf (deterministic-state-of value-node combined-cond-case) max-utility)
	(setf (deterministic-state-of dec-node combined-cond-case)
	      (cons max-utility (label-name best-decision)))))
    (values (remove dec-node diagram) dec-node)))

;**************************************** ARC REVERSAL **********************

; Reverses arc from pred to succ if pre-reqs are satisfied (Schacter
; 86). Returns updated diagram.

(defun reverse-arc (pred succ &optional (diagram *diagram*))
  (cond
    ((not (chance-node-p pred))
     (error "Node ~A is not a chance node. Cannot reverse arc from ~A to ~A" pred pred succ))
    ((not (chance-node-p succ))
     (error "Node ~A is not a chance node. Cannot reverse arc from ~A to ~A" succ pred succ))
    ((not (member pred (node-predecessors succ)))
     (error "Node ~A is not a direct predecessor of ~A. Cannot reverse arc." pred succ))
    ((not (single-directed-path-p pred succ))
     (error "There is more or less than one directed path from ~A to ~A. Cannot reverse arc"
	    pred succ))
    ((not (probabilistic-node-p pred))
     (error "The predecessor node ~A in the proposed arc reversal is not probabilistic"
	    pred))
    ((not (and (discrete-dist-node-p pred)(discrete-dist-node-p succ)
	       (every #'discrete-dist-node-p (node-predecessors pred))
	       (every #'discrete-dist-node-p (node-predecessors succ))))
     (error "Non discrete nodes involved in reversing arc from ~A to ~A. Cannot handle"
	    pred succ))
    ( t (if (deterministic-node-p succ)(convert-det-node-to-prob succ))
	; Convert noisy or nodes if necessary
     (if (noisy-or-node-p pred)(convert-noisy-or-node-to-chance-node pred))
     (if (noisy-or-node-p succ)(convert-noisy-or-node-to-chance-node succ))
     (ideal-debug-msg "~%Reversing arc from node ~A to node ~A" pred succ)
     (discrete-reverse-arc pred succ diagram))))

(defun discrete-reverse-arc (pred succ diagram)
	; Save some quantities that will be lost elsewise
  (let ((preds-old-dist-array (distribution-repn pred))
	(preds-old-predecessors (node-predecessors pred))
	(succs-old-dist-array (distribution-repn succ))
	(succs-old-predecessors (node-predecessors succ)))
	; Add pred's predecessors to node and vice versa.
    (dolist (s-o-p succs-old-predecessors)
      (when (not (eq pred s-o-p))(add-link pred s-o-p)))
    (dolist (p-o-p preds-old-predecessors)
      (add-link succ p-o-p))
	; Reverse the actual arc we are interested in
    (delete-link succ pred)(add-link pred succ)
	; Create new prob distributions for the two nodes involved
    (create-empty-distribution pred)
    (create-empty-distribution succ)
	; For all cond cases of succ's present preds ...
    (for-all-cond-cases (new-case (node-predecessors succ))
      (for-all-cond-cases (succ-case succ)	; For all cases of succ ...
	(let ((marginal-w.r.t-pred 0) (preds-cond-case
					(combine-cond-cases succ-case new-case)))
	; Set a running total to 0
	  (for-all-cond-cases (pred-case pred)	; For all cases of pred
	    (incf marginal-w.r.t-pred
	; Increase marginal by product of appropriate probs and set
	; appropriate prob-of of pred to this joint prob.
		  (setf (prob-of pred-case preds-cond-case)
			(* (contents-of-dist-array-location
			     succs-old-dist-array  succ-case
			     (combine-cond-cases pred-case new-case) succs-old-predecessors)
			   (contents-of-dist-array-location
			     preds-old-dist-array  pred-case  new-case
			     preds-old-predecessors)))))
	; Set prob of (succ-case / new-case ) to marginal
	  (setf (prob-of succ-case new-case) marginal-w.r.t-pred)
	  (let ((default (if (zerop marginal-w.r.t-pred) (/ 1 (number-of-states pred)))))
	    (for-all-cond-cases (pred-case (list pred))
	      (setf (prob-of pred-case  preds-cond-case)
		    (or default
			(/ (prob-of pred-case preds-cond-case) marginal-w.r.t-pred))))))))
    (values diagram)))



;********** Propogating a determinsitic node *****************************

(defun propogate-deterministic-node (det-node succ-node &optional (diagram *diagram*))
  (cond
    ((not (deterministic-node-p det-node))
     (error "Node ~A is not a deterministic node" det-node))
    ((not (member succ-node (node-successors det-node)))
     (error "Node ~A is not a successor of det-node ~A" succ-node det-node))
    (t (ideal-debug-msg "Propogating  det node ~A into node ~A" det-node succ-node)
       (if (noisy-or-node-p succ-node)(convert-noisy-or-node-to-chance-node succ-node))
       (propogate-det-node-1 det-node succ-node diagram))))

(defun propogate-det-node-1 (det-node succ-node diagram )
  (let ((old-succ-array (distribution-repn succ-node))
	(old-succ-preds (node-predecessors succ-node)))
    (delete-link succ-node det-node)
    (dolist (pred (node-predecessors det-node))(add-link succ-node pred))
    (when (not (decision-node-p succ-node))
;      (setf (distribution-repn succ-node))
      (create-empty-distribution succ-node)
      (for-all-cond-cases (new-cond-case (node-predecessors succ-node))
	(for-all-cond-cases (new-succ-case succ-node)
	  (setf (contents-of new-succ-case new-cond-case)
		(contents-of-dist-array-location
		  old-succ-array new-succ-case
		  (combine-cond-cases
		    new-cond-case
		    (make-conditioning-case
		      (list (cons det-node (deterministic-state-of det-node new-cond-case)))))
		  old-succ-preds)))))
    (values diagram)))


(defun convert-det-node-to-prob (det-node)
  (cond
	; Do nothing in the case of value and decision nodes
    ((not (chance-node-p det-node)) det-node)
    ((not(deterministic-node-p det-node))
     (ideal-warning "Node ~A is not a deterministic chance node. No need to convert"
		    det-node))
    (t (let ((old-array (distribution-repn det-node)) mapped-state)
	 (ideal-debug-msg
	   "~%Converting deterministic-node ~A into a probabilistic node" det-node)
	 (setf (relation-type det-node) :PROB)
	 (create-empty-distribution det-node)
	 (for-all-cond-cases (cond-case (node-predecessors det-node))
	   (for-all-cond-cases (node-case (list det-node))
	     (setf (prob-of node-case cond-case) 0.0))
	   (setf mapped-state (contents-of-det-array-location
				old-array cond-case (node-predecessors det-node)))
	   (setf (prob-of (make-conditioning-case (list (cons det-node mapped-state)))
			  cond-case) 1.0)))
       (values det-node))))


; Propogate det node into all its succs and remove from diagram

(defun reduce-deterministic-node (node diagram)
  (cond
    ((not (chance-node-p node))
     (error "Node ~A is not a chance node" node))
    ((not (deterministic-node-p node))
     (error "Node ~A is not a deterministic node" node))
    (t (ideal-debug-msg "~%Reducing deterministic node ~A" node)
       (loop
	 (if (null (node-successors node)) (return (remove-barren-node node diagram)))
	 (propogate-deterministic-node node (first (node-successors node)) diagram)))))

; Make prob node barren by successive arc reversals and eliminate from
; diagram

(defun reduce-probabilistic-node (node diagram &key (last-node nil))
  (cond
    ((not (chance-node-p node))
     (error "Node ~A is not a chance node" node))
    ((not (probabilistic-node-p node))
     (error "Node ~A is not a probabilistic node" node))
    ((some #'decision-node-p  (node-successors node))
     (error "Node ~A has decision node successors" node))
    (t (ideal-debug-msg "~%Reducing probabilistic-node ~A" node)
       (do ((succs (order (node-successors node) :last-node last-node) (rest succs)))
	   ((less-than-two succs) (absorb-chance-node node diagram))
	 (reverse-arc node (first succs) diagram)))))

; The cdr is nil if length of list < 2
(defun less-than-two (list)(null (cdr list)))

(defun exactly-two (list)(and (first list)(second list)(null (third list))))

(defun more-than-one (list)(cdr list))

; Reduces a chance node out of the diagram

(defun reduce-chance-node (node diagram &key (last-node nil))
  (case (relation-type node)
    (:PROB (reduce-probabilistic-node node diagram :last-node last-node))
    (:DET (reduce-deterministic-node node diagram))))

; Makes node barren by reversing arcs into successors after ordering
; them and Returns diagram.

(defun make-node-barren (node diagram &key (last-node nil))
  (ideal-debug-msg "~% Making node ~A barren" node)
  (cond
    ((not (chance-node-p node))
     (error "Node ~A is not a chance node" node))
    ((not (probabilistic-node-p node))
     (error "Node ~A is not a probabilistic node" node))
    ((some #'decision-node-p  (node-successors node))
     (error "Node ~A has decision node successors" node))
    (t (do ((succs (order (node-successors node) :last-node last-node) (rest succs)))
	   ((null succs)diagram)
	 (reverse-arc node (first succs) diagram)))))


; Reduce all the det nodes in the diagram

(defun reduce-all-deterministic-nodes (&optional (diagram *diagram*))
  (ideal-debug-msg "~%Reducing all deterministic nodes in diagram")
  (let ((det-nodes (remove-if-not #'(lambda (n)
				      (and (chance-node-p n)(deterministic-node-p n)))
				  diagram)))
    (do ((d-nodes det-nodes (cdr d-nodes)))
	((null d-nodes) diagram)
      (setq diagram (reduce-deterministic-node (first d-nodes) diagram)))))



; Preprocess to include instantiated evidence nodes

(defun include-instantiated-evidence (diagram)
  (let ((first-decision (first (order (remove-if-not #'decision-node-p diagram))))
	(instantiated-nodes (remove-if-not #'instantiated-p diagram)))
    (when  (check-instantiated-evidence instantiated-nodes first-decision)
      (dolist (i-node instantiated-nodes)
	(ideal-debug-msg"~%Adding link from ~A to ~A and reducing ~A ~% to contain only ~
                               state ~A" i-node first-decision i-node (node-state i-node))
	(if (noisy-or-node-p i-node)(convert-noisy-or-node-to-chance-node i-node))
	(add-link first-decision i-node)
	(let ((old-dist-array (distribution-repn i-node)))
	  (setf (state-labels i-node)(list (node-state i-node)))
	  (for-all-cond-cases (pred-case (node-predecessors i-node))
	    (for-all-cond-cases (i-node-case (list i-node))
	      (setf (prob-of i-node-case pred-case)
		    (contents-of-dist-array-location
		      old-dist-array  i-node-case pred-case (node-predecessors i-node))))))))
    (values diagram)))

(defun check-instantiated-evidence (instantiated-nodes first-decision)
  (let (bad-node)
    (cond
      ((and instantiated-nodes (null first-decision))
       (error "The diagram has no decision nodes. Including instantiated evidence ~@
              not permitted"))
      ((setq bad-node
	     (find-if #'(lambda (i-node)
			  (ancestor-p i-node first-decision)) instantiated-nodes))
       (error "The instantiated node ~A comes after the first decision ~A in ~@
               informational sequence. This is not premitted"
	      bad-node first-decision))
      ( t t))))


; Removes links from decision nodes to their predecessors. Needed for
; the extended version of shac-infer.

(defun remove-informational-predecessor-links&value-node (diagram)
  (labels ((delete-predecessor-links (node)
	     (dolist (pred (node-predecessors node))
	       (delete-link node pred))))
    (dolist (n diagram)
      (when (or (decision-node-p n)(value-node-p n))
	(delete-predecessor-links n)))))
