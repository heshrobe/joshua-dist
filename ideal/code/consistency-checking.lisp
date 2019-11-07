;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: Ideal;  Base: 10 -*-
				    

(in-package :ideal)

;;;;********************************************************
;;;;  Copyright (c) 1989, 1992 Rockwell International -- All rights reserved.
;;;;  Rockwell International Science Center Palo Alto Lab
;;;;********************************************************


;;;;;;;;;;;;;;;;;;;;;;;; Sampath ;;;;;;;;;;;;;;;;;;;;


(export '(CONSISTENT-P
	   STRICTLY-POSITIVE-DISTRIBUTIONS-P
	   BELIEF-NET-P
	   CHANGED-DIAGRAM-P
	   SINGLY-CONNECTED-BELIEF-NET-P))

;--------------------------------------------------------

; Checking the consistency of a diagram

(defun consistent-p (diagram &key (tolerance 1e-6))
  (and (consistent-top-level-nodes-p diagram)
       (consistent-implementation-details diagram)
       (consistent-topology-p diagram)
       (consistent-distributions diagram tolerance)))

;----------------------------------------------------------------------------------------

; Checks to see that all the nodes that are connected to nodes in the
; diagram are also  members of the diagram.

(defun consistent-top-level-nodes-p (diagram)
  (let ((hidden-nodes (find-hidden-nodes diagram)))
    (cond ((null hidden-nodes) t)
	  ((every #'dummy-node-p hidden-nodes) t)
	  (t (ideal-warning "The nodes in the list ~A are linked to nodes in the ~
                     diagram but are not mentioned in the diagram" hidden-nodes)))))

(defun find-hidden-nodes (diagram)
  (nset-difference (generate-diagram diagram) diagram))
;----------------------------------------------------------------------------------------
; Checks whether topology of diagram is OK, i.e checks:-
; 1) Consistency of links in diagram
; 2) whether diagram is acyclic
; 3) whether decisions have a complete ordering
; 4) whether diagram is oriented and regular

(defun consistent-topology-p (diagram)
  (and  (consistent-link-refs diagram)
	(acyclic-p diagram)
	(completely-ordered-decisions diagram)
	(check-value-node diagram)))

(defun consistent-link-refs (diagram)
  ; For every node in the diagram
  (every #'(lambda (node)
	     (consistent-link-refs-for-node node diagram)) diagram))

(defun consistent-link-refs-for-node (node diagram)
  (and (all-are-nodes (node-predecessors node) node "predecessors")
       (all-are-nodes (node-successors node) node "successors")
    ; There are no repeats in preds
	      (all-unique-p (node-predecessors node))
	       ; There are no repeats in succs
	      (all-unique-p (node-successors node))
	      ; For every other-node in the diagram
	      (every #'(lambda (other-node)
			 (cond
			   ; If node and other node are the same its ok
			   ((eq node other-node) t)
			   ; if node is in other node's preds there should be reverse link
			   ((find node (node-predecessors other-node))
			    (if (find other-node (node-successors node)) t
				(ideal-warning "~%~A mentions ~A in predecessors but~@
                                             there is no reverse successor link"
					  other-node node)))
			   ; if node is in other node's succs there should be reverse link
			   ((find node (node-successors other-node))
			    (if (find other-node (node-predecessors node)) t
				(ideal-warning "~%~A mentions ~A in successors but~@
                                              there is no reverse predecessor link"
					  other-node node)))
			   ; If all above are not true its ok
			   ( t  t))) diagram)))

(defun all-are-nodes (node-list main-node &optional format-arg)
  (every #'(lambda (node)
	     (cond
	       ((node-p node) t)
	       (t (format t "The entry  ~A in the node ~A list of the node ~A is invalid"
			  node (or format-arg node-list) main-node)))) node-list))



(defun completely-ordered-decisions (diagram)
  (let ((decisions (remove-if-not #'decision-node-p  diagram)))
    (multiple-value-bind (ignore ordered?)
	(order decisions :check-complete-ordering t :mode :WARN)
      (declare (ignore ignore))
      (values ordered?))))

(defun check-value-node (diagram)
  (if (not (find-if #'value-node-p diagram)) t
      (and (oriented diagram)(barren-value-node-p diagram))))

;----------------------------------------------------------------------------------------
;Checks whether diagram implementation details (other than topology) are consistent.
; Specifically, if:-
; 1) All the elements of the diagram list are nodes and they are all unique and
; have unique names.
; 2) ALl the nodes in the  diagram are of valid types.
; 3) All the nodes have unique id numbers.
; 4) The state labels of each node are consistent with the node type.
; 5) The state labels themselves are consistent, i.e they are unique, have unique names
; and have unique id-numbers etc.

; Checks if node numbers are unique and that the label is numbers are valid

(defun consistent-implementation-details (diagram)
  (let (temp)
  (cond
    ((setq temp (find-if-not #'node-p diagram))
     (ideal-warning "Diagram contains ~A which is not a node" temp))
    ((not (all-unique-p diagram))
     (ideal-warning "Nodes repeat in the diagram ~A" diagram))
    ((setq temp (find-if-not #'symbolp diagram :key #'node-name))
     (ideal-warning "The node ~A has a name ~A that is not a symbol" temp (node-name temp)))
    ((member nil diagram :key #'node-name)
     (ideal-warning "A node in the diagram has node name NIL. This is not a valid name"))
    ((not (all-unique-p diagram :key #'node-name))
     (ideal-warning "Two or more nodes in the diagram have the same name"))
    ((setq temp (find-if-not #'(lambda (type)(member type '(:CHANCE :DECISION :VALUE)))
			     diagram :key #'node-type))
     (ideal-warning "Node ~A has a type spec ~A which is not :CHANCE, :DECISION or :VALUE"
	       temp (node-type temp)))
    ((setq temp (find-if-not #'numberp diagram :key #'node-id-number))
     (ideal-warning "The id number of node ~A is ~A, which is not a number"
	       temp (node-id-number temp)))
    ((not (all-unique-p diagram :key #'node-id-number))
     (ideal-warning "The id numbers of all the nodes in the diagram ~A are not unique"
		    diagram))
     (t  (every #'check-node-labels diagram)))))

(defun check-node-labels (node)
  (let (temp)
    (labels ((increasing-order (list)
	       (cond
		 ((null (rest list)) t)
		 ((= (second list)(1+ (first list))) (increasing-order (rest list)))
		 ( t nil))))
      (case (node-type node)
	(:VALUE
	  (cond
	    ((not (null (state-labels node)))
	     (ideal-warning "The value node ~A has labels defined" node))
	    ( t t)))
	((:CHANCE :DECISION)
	 (cond
	   ((setq temp (find-if-not #'label-p (state-labels node)))
	    (ideal-warning "The state labels list  of node ~A contains ~A which is not ~
                                a label-p"
		      node temp))
	   ((setq temp (find-if-not #'(lambda (lab)(eq (label-node lab) node))
				    (state-labels node)))
	    (ideal-warning "The state-label ~A does not belong to node ~A, i.e the ~
                             label-node field of the label does not contain the node"
			   temp node))
	   ((not (all-unique-p (state-labels node) :key #'label-name))
	    (ideal-warning "All the state labels names of node ~A are not unique" node))
	   ( t  (let ((numb-list (sort (mapcar #'label-id-number (state-labels node)) #'<)))
		  (cond
		    ((null numb-list) (ideal-warning "The node ~A has no states defined" node))
		    ((not (and (= (first numb-list) *lowest-label-id-number*)
			       (increasing-order numb-list)))
		     (ideal-warning "The state label id numbers of node ~A are not ordered ~@
                                properly ~% i.e they dont increase in steps of one from ~@
                                *lowest-label-id-number*" node))
		    ((not (= (number-of-states node)(length (state-labels node))))
		     (ideal-warning "The number-of-states field of the node ~A is inconsistent"
			       node))
		    ( t t ))))))))))

;----------------------------------------------------------------------------------------
; Checks to see whether the distributions of each node are consistent.

(defun consistent-distributions (diagram tolerance)
  (every #'(lambda (node)
	     (consistent-node-distribution node tolerance)) diagram))

(defun consistent-node-distribution (node tolerance)
  (cond
    ((null (distribution-repn node))
     (cond
       ((decision-node-p node) t)
       ( t (ideal-warning "Node ~A has no distribution array" node))))
    (t (case (node-type node)
	 (:DECISION (ideal-warning "The policy of decision node ~A will not be checked" node)
	  t)
	 (:VALUE (consistent-value-node-dist node))
	 (:CHANCE (case (relation-type node)
		    (:PROB (consistent-chance-prob-node-dist node tolerance))
		    (:DET (consistent-chance-det-node-dist node))))))))

(defun consistent-value-node-dist (node)
  (block START
    (for-all-cond-cases (cond-case (node-predecessors node))
      (let ((value (deterministic-state-of node cond-case)))
	(if (not (numberp value))
	    (progn
	      (ideal-warning "The cond-case ~A of the value-node ~A has the ~%~@
                                 invalid contents ~A" cond-case node value)
	      (return-from START nil)))))
    (values t)))

(defun consistent-chance-prob-node-dist (node tolerance)
  (and
    (or (not (noisy-or-node-p node))
	(consistent-noisy-or-info node tolerance))
    (consistent-chance-node-probability-table node tolerance)))

(defun consistent-chance-node-probability-table (node tolerance)
  (block START
    (for-all-cond-cases (cond-case (node-predecessors node))
      (let ((remainder 1))
	(for-all-cond-cases (node-case (list node))
	  (let ((prob (prob-of node-case cond-case)))
	    (cond
	      ((not (numberp prob))
	       (ideal-warning  "The contents of P[ ~A /~A] is ~A which is invalid"
			  node-case cond-case prob)(return-from START nil))
	      ((not (<= 0 prob 1))
	       (ideal-warning "P[~A /~A] is ~A which is not between 0 and 1"
			      node-case cond-case prob)
	       (return-from START nil))
	      ( t (decf remainder prob)))))
	(when (>= (abs remainder)(abs tolerance))
	  (ideal-warning "The probabilities of node ~A do not sum to 1 for the ~@
                                 conditioning case ~A" node cond-case)
	  (return-from START nil))))
    (values t)))

(defun consistent-noisy-or-info (node tolerance)
  (and (check-noisy-or-inhibitor-probs node)
       (check-noisy-or-det-fn node)
       (check-noisy-or-prob-table node tolerance)))
  
(defun check-noisy-or-inhibitor-probs (node)
  (block START
    (dolist (p (node-predecessors node))
      (let ((total 0))
	(for-all-cond-cases (pred-case p)
	  (let ((ip (inhibitor-prob-of node pred-case)))
	    (cond
	      ((not (numberp ip))
	       (ideal-warning "Inhibitor prob of node ~A for case ~A is ~A which is not a ~
                            number" node pred-case ip)
	       (return-from START nil))
	      ((not (<= 0 ip 1))
	       (ideal-warning "Inhibitor prob of ~A for ~A is ~A. Not in the range [0,1]"
			      node pred-case ip)
	       (return-from START nil))
	      ((and (eq (noisy-or-subtype node) :BINARY)
		    (not (noisy-or-false-case-p pred-case))
		    (not (zerop ip)))
	       (ideal-warning "~A is a binary noisy-or node. Yet, the inhibitor prob of ~A
                          , which is not the false case is ~A when it should actually be 0 .~
                             This is allowable (will cause no errors) but can be misleading."
			      node pred-case ip))
	      (t (incf total ip)))))
	(when (> total 1)
	  (ideal-warning "The sum of the inhibitor probs for node ~A for pred ~A is ~A ~
                       which is greater than 1" node p total)
	  (return-from START nil))))
    (return-from START t)))

(defun check-noisy-or-det-fn (node)
  (block START
    (let (calculated-value actual-value)
      (ecase (noisy-or-subtype node)
	((:BINARY :NARY)
	 (for-all-cond-cases (pred-case (node-predecessors node))
	   (when (not (eq (setq actual-value (noisy-or-det-fn-of node pred-case))
			  (setq calculated-value
				(generalized-nary-or-function node pred-case))))
	     (ideal-warning "Node ~A has a det fn value of ~A at location ~A when it should ~
                   actually be ~A" node actual-value pred-case calculated-value)
	     (return-from START nil))))
	(:GENERIC
	  (for-all-cond-cases (pred-case (node-predecessors node))
	    (when (not (member (setq actual-value (noisy-or-det-fn-of node pred-case))
			       (state-labels node)))
	      (ideal-warning "Node ~A has det fn value ~A at location ~A and this is ~
                            not eq to any of the state labels"
			     node actual-value pred-case)
	      (return-from START nil)))))
      (return-from START t))))

(defun check-noisy-or-prob-table (node tolerance)
  (block START 
    (let (table-value calc-value diff)
      (for-all-cond-cases (pred-case (node-predecessors node))
	(for-all-cond-cases (node-case node)
	  (setq table-value (prob-of node-case pred-case))
	  (setq calc-value (calc-noisy-or-prob-of node-case pred-case))
	  (setq diff (abs (- calc-value table-value)))
	  (when (> diff tolerance)
	    (ideal-warning "For  (Prob-of ~A ~A): Calculated noisy or value: ~A ~
                          Compiled Table value ~A: The difference ~A is ~
                          greater than the tolerance ~A"
			   node-case pred-case calc-value table-value diff tolerance)
	    (return-from START nil))))
      (return-from START t))))

(defun consistent-chance-det-node-dist (node)
  (block START
    (for-all-cond-cases (cond-case (node-predecessors node))
      (let ((value (deterministic-state-of node cond-case)))
	(cond
	  ((not (label-p value))
	   (ideal-warning "The contents of the dist  of deterministic node ~A ~@
                     for cond case ~A is ~A which is not a state label"
		     node cond-case value) (return-from START nil))
	  ((not (find value (state-labels node)))
	   (ideal-warning "The value ~A for the cond case ~A in the distribution of ~@
                    ~%deterministic node ~A is not eq to any of the state labels ~@
                     of the node" value cond-case node) (return-from START nil)))))
    (values t)))

;----------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------
; Miscellaneous functions

;----------------------------------------------------------------------------------------
; Checks whether the probability distributions of the chance nodes in the diagram are
; strictly positive

(defun strictly-positive-distributions-p (diagram)
  (every #'strictly-positive-node-distribution-p diagram))

(defun strictly-positive-node-distribution-p (node)
  (cond ; Decision and value nodes return adefault of t
    ((or (decision-node-p node)(value-node-p node)) t)
    ((deterministic-node-p node)
     (ideal-warning "~%The node ~A is determinsitic and consequently has a non-strictly ~
                    positive distribution" node))
    (t (block START
	 (for-all-cond-cases (pred-case (node-predecessors node))
	   (for-all-cond-cases (node-case node)
	     (when (zerop (prob-of node-case pred-case))
	       (ideal-warning "The probability P(~A | ~A) is zero. ~
                                            The distribution is hence not strictly positive"
			      (prob-string-of-cond-case node-case)
			      (prob-string-of-cond-case pred-case))
	       (return-from START nil))))
	 (values t)))))
;----------------------------------------------------------------------------------------
;Checks whether a diagram is oriented and regular

(defun oriented&regular-p (diagram)
  (and (oriented diagram)
       (acyclic-p diagram)
       (barren-value-node-p diagram)
       (completely-ordered-decisions diagram)))

(defun oriented (diagram)
  (= 1 (length (remove-if-not #'value-node-p diagram))))

(defun barren-value-node-p (diagram)
  (let ((v-node (find-if #'value-node-p diagram)))
    (if (null (node-successors v-node))
	(values t)
	(progn
	  (ideal-warning "The value node ~A has successors. This is not permissible" v-node)
	  (values nil)))))

;----------------------------------------------------------------------------------------
; Checks whether the diagram is a belief net

(defun belief-net-p (bel-net)
  (every #'(lambda (n)
	     (cond
	       ((not (chance-node-p n))
		(ideal-warning "Node ~A is not a chance node" n))
	       ((not (probabilistic-node-p n))
		(ideal-warning "Node ~A is not a probabilistic node" n))
	       ( t t))) bel-net))

;;----------------------------------------------------------------------------------------
; Checks to see whether the diagram has changed

(defun initialized-diagram (diagram &key (error-mode t) algorithm-type)
  (if (not (member algorithm-type *algorithm-types*))
      (error "~A is not a valid IDEAL algorithm" algorithm-type))
  (cond
    ((not (consistent-top-level-nodes-p diagram))
     (when error-mode
       (error "The top level input list does not contain all the nodes in the diagram")))
    ((changed-diagram-p diagram)
     (when error-mode
       (error "The diagram has changed after the last initialization. Reinitialization reqd")))
    ((some #'(lambda (n)(not (initialized-node n algorithm-type))) diagram)
     (when error-mode
       (error "The diagram has not been initialized for ~A" algorithm-type)))
    ( t t)))

(defun changed-diagram-p (diagram)
  (some #'node-changed-p diagram))

(defun initialized-node (node alg-type)
  (eq (node-initialization node) alg-type))

(defun set-diagram-initialization (diagram &key algorithm-type)
  (if (not (member algorithm-type *algorithm-types*))
      (error "~A is not a valid IDEAL algorithm" algorithm-type))
  (mapc #'(lambda (n)(setf (node-initialization n) algorithm-type
			   (node-changed-p n) nil))
	diagram))

; Many operations carried out by inference algorithms result in the
; changed-p flag of the nodes being set even though there is no real
; functional change in the diagram. In such situations the following fn
; is used during clean up operations.

(defun mark-diagram-as-unchanged (diagram)
  (mapc #'(lambda (n)(setf (node-changed-p n) nil)) diagram))

;----------------------------------------------------------------------------------------
; Checking singly connectedness

; Input checking for the function belnets

(defun singly-connected-belief-net-p (diagram &key (error-mode t))
  (let (temp)
    (cond
      ((setf temp (find-if-not #'chance-node-p diagram))
       (when error-mode
	 (error " The node ~A is not chance node. Belnets can't handle this" temp)))
      ((setf temp (find-if-not #'probabilistic-node-p diagram))
       (when error-mode
	 (error "The node ~A is not a probabilistic node. Belnets can't handle this" temp)))
      ((not (singly-connected-graph-p diagram :error-mode error-mode))
       (when error-mode
	 (error "The input diagram is not singly connected. Belnets can't handle this")))
      ( t t ))))


; Can handle the case where diagram has multiple disconnected
; components.

(defun singly-connected-graph-p (diagram &key (error-mode nil))
  (let ((regenerated-graph nil) next-component-graph unconnected-node exists-p)
    (loop
      (setq unconnected-node
	    (find-if-not #'(lambda (n)(member n regenerated-graph)) diagram))
      (when (null unconnected-node)(return t))
      (multiple-value-setq (next-component-graph exists-p)
	(generate-singly-connected-graph unconnected-node))
      (when (not exists-p)
	(if error-mode (error "The input diagram is not singly connected"))
	(return nil))
      (setq regenerated-graph (nconc regenerated-graph next-component-graph)))))


; If <node> is a member of a singly connected graph it returns the graph
; as a first value and t and a second value. If not, i.e when the graph
; to which <node> belongs is *not* singly connected it returns nil and
; nil.

(defun generate-singly-connected-graph (node)
  (labels ((gen-singly-connected-graph (node activator-node reachable-nodes)
	     (let ((addl-reachable-nodes
		     (safe-append (node-successors node) (node-predecessors node))))
	       (dolist (addl-node addl-reachable-nodes)
		 (when (not (eq addl-node activator-node))
		   (if (member addl-node reachable-nodes)
		       (return-from GENERATE-SINGLY-CONNECTED-GRAPH (values nil nil)))
		   (setq reachable-nodes
			 (gen-singly-connected-graph
			   addl-node node (cons addl-node reachable-nodes)))))
	       (values reachable-nodes))))
    (values (gen-singly-connected-graph node nil (list node)) t)))
