;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: Ideal;  Base: 10 -*-

(in-package :ideal)

;;;;********************************************************
;;;;  Copyright (c) 1989, 1992 Rockwell International -- All rights reserved.
;;;;  Rockwell International Science Center Palo Alto Lab
;;;;********************************************************


;;;;;;;;;;;;;;;;;;;;;;;; Sampath ;;;;;;;;;;;;;;;;;;;;


(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(ADD-NODE DELETE-NODE
            ADD-ARCS DELETE-ARCS
            ADD-STATE DELETE-STATE)))

;--------------------------------------------------------

;;;***********************************ADD NODE ************************************

; Adds a new node to the diagram. The node has no preds, no succs and
 ; has a uniform marginal if its a chance node. All values 0 if it is a
; value node (there is only one value of course, considering there are
; no preds.)

; Specifying noisy-or supersedes all other specs except name
; (i.e type becomes :CHANCE, relation-type :PROB, and state-labels
; '(:TRUE :FALSE)

(defun add-node (diagram &key name type relation-type state-labels noisy-or noisy-or-subtype)
  ; Noisy or nodes are automatically :Chance nodes of rel type :prob
  (when noisy-or
    (if (or (not (null type))(eq type :CHANCE))
       (ideal-warning
	 "You have specified type ~A for a noisy-or node. They are automatically of type
               :CHANCE" type))
    (if (or (not (null relation-type))(eq relation-type :PROB))
	(ideal-warning
	  "You have specified relation-type ~A for a noisy-or node. They are automatically
               of relation-type :PROB" relation-type))
    (setq type :CHANCE relation-type :PROB)
    (when (eq noisy-or-subtype :binary)
      (cond
	((not (and (exactly-two state-labels)
	      (eq (first state-labels) :FALSE)(eq (second state-labels) :TRUE)))
	 (ideal-warning "You have specified Noisy or subtype as :BINARY but the~
                         state label names you have specified ~A are not :FALSE and :TRUE~
                         (**in that order**). Your state label names are being ignored -- ~
                         This node ~
                         will have two states, State 0  of this node will be called :FALSE,~
                         State 1 will be called :TRUE. You can edit these names later if~
                         you wish." state-labels)
	 (setq state-labels '(:FALSE :TRUE))))))
  (cond
    ((null name)
     (error "Nil is not allowed a node-name"))
    ((member nil state-labels)
     (error "Nil is not allowed as the name of a state label"))
    ((not (symbolp name))
     (error "The name ~A for the new node is not a symbol" name))
    ((find name diagram :key #'node-name)
     (error "A node with new nodes name ~A already exists in the diagram" name))
    ((not (member type '(:CHANCE :DECISION :VALUE)))
     (error "The node type ~A of the new node is not :chance, :decision or :value" type))
    ((and (eq type :VALUE ) (not (null state-labels)))
     (error "You have specified state labels ~A for the new value node ~A. A value node  ~
             should have no state labels" state-labels name))
    ((and (not (eq type :VALUE))(null state-labels))
     (error "You have not specified state labels for the ~A node ~A" type name))
    ((and (eq type :VALUE)(find-if #'value-node-p diagram))
     (error "The new node is to be a value node. But a value node already exists in diagram"
	    ))
    ((and (eql type :CHANCE)(not (member relation-type '(:PROB :DET))))
     (error "The node type  cant be ~A. It has to be :PROB or :DET" relation-type))
    ((not (every #'(lambda(x) (or (symbolp x) (numberp x))) state-labels))
     (error "All the suggested state labels in the list ~A are not symbols" state-labels))
    ((not (all-unique-p state-labels))
     (error "All the suggested state labels for the new node ~A in the list ~A are
             not unique" name state-labels))
    ((and noisy-or (not (member noisy-or-subtype '(:BINARY :NARY :GENERIC)))
	(error "Noisy or subtype ~A is not :BINARY, :NARY or :GENERIC" noisy-or-subtype)))
    (t (let* ((rel-type (ecase type
			 (:CHANCE relation-type)
			 ((:DECISION :VALUE) :DET)))) ; Decision and value nodes are :det.
	 (add-node-1 diagram name type rel-type state-labels noisy-or noisy-or-subtype)))))

(defun add-node-1 (diagram name type relation-type state-labels noisy-or noisy-or-subtype)
; Hopefully one day, the distribution type will be an arg instead of a fixed.
  (let ((new-node (create-empty-node :distribution-type :DISCRETE)))
    (setf (node-name new-node)  name
	  (node-id-number new-node)
	  (1+ (apply #'max (cons (1+ *node-lowest-index*) (mapcar #'node-id-number diagram))))
	  (node-type new-node) type
	  (relation-type new-node) relation-type
	  (state-labels new-node)
	  (let ((index (- *lowest-label-id-number* 1)))
	    (mapcar #'(lambda (name)(make-label :name name :id-number (incf index)))
		    state-labels))
	  (number-of-states new-node)(length state-labels))
    (dolist (lab (state-labels new-node))(setf (label-node lab) new-node))
    (unless (eq type :DECISION) ; No distribution needed set for decision nodes
      (when noisy-or
	(set-noisy-or-flag new-node :subtype noisy-or-subtype))
      (create-empty-distribution new-node)
      (cond
	; Have to set the det fn randomly in the :GENERIC case. Note
	; that no inhibitor probs are necessary since there are no
	; predecessors.
	(noisy-or
	 (ecase (noisy-or-subtype new-node)
	   ((:BINARY :NARY)
	    (set-noisy-or-det-fn-to-standard-nary-or-fn new-node))
	   (:GENERIC
	     (set-noisy-or-det-fn-randomly new-node)))
	 (compile-noisy-or-distribution new-node))
	(t (let ((default (case type
			    (:VALUE 0)
			    (:CHANCE (ecase relation-type
				       (:prob (/ 1 (length state-labels)))
				       (:det (car (state-labels new-node))))))))
	     (for-all-cond-cases (cond-case (node-predecessors new-node))
	       (for-all-cond-cases (node-case new-node)
		 (setf (contents-of node-case cond-case)
		       default))))))
      )
    (values (cons new-node diagram) new-node)
    ))


;****************************** DELETE NODE ************************************************

; Deletes a node from a diagram

(defun delete-node (node &optional (diagram *diagram*))
  (let (prev-decision succ-decision)
	; When the node is a decision node keep track of prev and next
	; decisions
    (when (decision-node-p node)
      (setq succ-decision (find-if #'decision-node-p (node-successors node)))
      (setq prev-decision  (find-if #'decision-node-p (node-predecessors node))))
	; Check whether default reqd
	; Delete arcs from node to each succ and set default dist for succ.
    (dolist (succ (node-successors node))(delete-arcs succ (list node)))
	; Delete all predecessors of node (we can use low level delete-link
        ; coz we no longer care about consistency of the node being deleted.
    (dolist (pred (node-predecessors node))(delete-link node pred))
	; If the node is a decision node and prev and next decisions exist then
	; add-arc from prev to next decision (to preserve chronological
	; ordering).
    (if (and (decision-node-p node) succ-decision  prev-decision)
	(add-arcs succ-decision (list prev-decision)))
	; Drop node from the diagram and return diagram
    (delete node diagram)))



; *********************** ADDING ARCS*********************************************

; Adds arcs from each of the nodes in pred-list to node. Will just drop
; those nodes that cause cycles. Returns the node (which has been
; updated).


(defun add-arcs (node pred-list &key (error-mode nil))
  (cond
    ((and (discrete-dist-node-p node)(every #'discrete-dist-node-p pred-list))
     (discrete-add-arcs node pred-list :error-mode error-mode))
    (t (error "Some non-discrete node in either NODE: ~A or PREDLIST: ~A. ~@
                ~%Dont know how to add arcs in this case" node pred-list))))

(defun discrete-add-arcs (node pred-list &key (error-mode nil))
  (cond
    ((noisy-or-node-p node)
     (noisy-or-add-arcs node pred-list error-mode))
    (t (let* ((old-preds (node-predecessors node))
	      (old-dist-array (distribution-repn node))
	      (new-additional-preds (set-difference pred-list (node-predecessors node))))
	; This is basically a non-sequiter coz u wont want to add arcs in
	; a solved diagram and only decision nodes in a solved diagram
	; have non nil distribution representations.
	 (when
	   (and (decision-node-p node)(distribution-repn node))
	   (ideal-warning "When adding arcs to the decision node ~A the policy of ~:*~A has~
                 become inconsistent" node))
	 (dolist (new-pred new-additional-preds)
	   (add-link node new-pred :error-mode error-mode))
	 (when (not (decision-node-p node))
	   (create-empty-distribution node)
	   (for-all-cond-cases (node-case node)
	     (for-all-cond-cases (old-cond-case old-preds)
	       (let ((old-content (contents-of-dist-array-location
				    old-dist-array  node-case old-cond-case old-preds)))
		 (for-all-cond-cases (incremental-cond-case new-additional-preds)
		   (setf (contents-of node-case
				      (combine-cond-cases incremental-cond-case
							  old-cond-case))
			 old-content)))))))))
  (values node))

(defun noisy-or-add-arcs (node pred-list error-mode)
  (let* ((old-predecessors (node-predecessors node))
	 (new-preds (set-difference pred-list old-predecessors))
	 (old-noisy-or-info (get-noisy-or-info node)))
	; Add in links from new predecessors ....
    (dolist (new-pred new-preds)
      (add-link node new-pred :error-mode error-mode))
	; Create a new distribution data structure ......
    (create-empty-distribution node)
	; Fill in available inhibitor probs from previous table.
    (copy-inhibitor-probs node old-noisy-or-info :default 0)
	; Extend the det fn as necessary
    (ecase (noisy-or-subtype node)
      ((:BINARY :NARY)
	; Recompute and cache the standard function
       (set-noisy-or-det-fn-to-standard-nary-or-fn node))
      (:GENERIC
	; Define new function such that the value is the same as the old
	; disregarding the state of the new predecessors
	(for-all-cond-cases (old-pred-case old-predecessors)
	  (let ((old-det-fn-value
		  (get-det-fn-value-from-noisy-or-info
		    old-noisy-or-info old-predecessors old-pred-case)))
	    (for-all-cond-cases (new-pred-case new-preds)
	      (setf (noisy-or-det-fn-of node (combine-cond-cases old-pred-case new-pred-case))
		    old-det-fn-value))))))
	; Recompile the noisy or function.
    (compile-noisy-or-distribution node)
    (values node)))

;******************************** DELETING ARCS *****************************************

; The `centre-most' state of each of the preds in pred-list is chosen.
; When setting the distribution of succ after deleting the predecessors
; in pred-list the probability assigned to each conditioning case is
; that assigned in the old distribution to this cond case augmented
; with all the deleted preds being in their respective `centre-most' states.

(defun delete-arcs (node pred-list)
  (cond
    ((noisy-or-node-p node)
     (noisy-or-delete-arcs node pred-list))
	; This is the general case
    (t (let ((old-preds (node-predecessors node))
	     (old-array (distribution-repn node))
	     (incremental-c-case
	       (make-conditioning-case
		 (mapcar #'(lambda (pred)(cons pred (centre-most-state-of pred)))
			 pred-list))))
	 (dolist (pred pred-list)(delete-link node pred :silent nil))
	 (cond
	; In the case of a dummy-node no need to do anything about distributions.
	   ((dummy-node-p node) nil)
	; If node is a decision node ....
	   ((decision-node-p node)
	; and has a policy say it is going to become inconsistent ...
	    (when (distribution-repn node)
	      (ideal-warning "During deletion of node arc to the decision  ~
                          node ~A  its policy has become inconsistent" node)))
	;If node is not a decision node adjust the distributions accordingly
      (t (create-empty-distribution node)
       (for-all-cond-cases (node-case node)
	 (for-all-cond-cases (new-cond-case (node-predecessors node))
	   (setf (contents-of node-case new-cond-case)
	; Note that contents-of and contents-of-d.. handle both chance and value nodes.
		 (contents-of-dist-array-location
		   old-array node-case (combine-cond-cases incremental-c-case new-cond-case)
		   old-preds)))))))))
  (values node))

(defun noisy-or-delete-arcs (node pred-list)
  (let* ((old-predecessors (node-predecessors node))
	 (old-noisy-or-info (get-noisy-or-info node))
	 (preds-to-be-deleted (intersection pred-list old-predecessors)))
	; Delete links ..
    (dolist (p pred-list)
      (delete-link node p :silent nil))
	; Create a new distribution ....(not that all inhibitor probs are made 0 by default)
    (create-empty-distribution node)
	; Set known probs for new distribution from old distribution
    (copy-inhibitor-probs node old-noisy-or-info)
    (ecase (noisy-or-subtype node)
      ((:BINARY :NARY)
       (set-noisy-or-det-fn-to-standard-nary-or-fn node))
      (:GENERIC
	; Chooses a random instantiation of the nodes to be deleted,
	; New fn is given by NewF(pred-case) =
	; OldF(pred-case concated with random instantiaton of deletd nodes)
	(labels ((make-random-case (node-list)
		   (make-conditioning-case
		     (mapcar #'(lambda (n)(cons n (first (state-labels n)))) node-list))))
	  (let ((random-old-preds-case (make-random-case preds-to-be-deleted)))
	    (for-all-cond-cases (pred-case (node-predecessors node))
	      (setf (noisy-or-det-fn-of node pred-case)
		    (get-det-fn-value-from-noisy-or-info
		      old-noisy-or-info old-predecessors
		      (combine-cond-cases pred-case random-old-preds-case))))))))
	; Recompile distribution
    (compile-noisy-or-distribution node)))

;******************** Overall edit function for state label list *************************

; Takes a new set of label names for node. Leaves the labels that are already labels
; of node INTACT, i.e distributions conditioned on them or probabilities associated with
; that state remain same. Deletes existing labels of node that are not in new label list
; and adds new labels for nodes that are in the new label list and not in the old label
; list.

(defun edit-states (node edited-label-name-list)
  (let ((states-to-be-deleted
	  (set-difference (state-labels node) edited-label-name-list
			  :test #'(lambda (l n)(eq (label-name l) n))))
	(names-of-states-to-be-added
	  (set-difference edited-label-name-list (state-labels node)
			  :test #'(lambda (n l)(eq (label-name l) n)))))
    (dolist (s states-to-be-deleted)
      (delete-state node s))
    (dolist (name names-of-states-to-be-added)
      (add-state node name))
    (values node)))

;*********************************ADD STATE************************************************

; Adds a state to the state list of a node. Sets all the new
; probabilities that are needed to 0 in the distribution of <node>. In
; the distributions of successors of node: GiIven each new cond-case the
; probability distribution over the states of the successor is made
; uniform. If the successor is a value node the values for the new cases
; are set to 0.

(defun add-state (node &optional name)
  (cond
    ((value-node-p node)
	; Adding states is meaningless for value nodes
     (error "Cannot add states to the VALUE node ~A" node))
	; If not value node create new state ..
    (t (let ((new-state (make-label
			  :name (or (check-if-valid-label name node) (query-new-state node))
			  :id-number (1+  (max-label-id-number node))
			  :node node)))
	 (push new-state (state-labels node))
	 (incf (number-of-states node))
	 (case (node-type node)
	; If its a decision node dont do anything more to node
	   (:DECISION (if (distribution-repn node)
			  (warn "Added a state to the decision  node ~A. The policy ~% ~@
                           has been left unchanged is now inconsistent" node)))
	   ; If chance prob node make new dist for node with new p's = 0 and old p's
	    ; preserved.
	   (:CHANCE
	    (case (relation-type node)
		     (:PROB
		       (cond
			 ((noisy-or-node-p node)
			  (noisy-or-add-new-state node new-state))
			 (t (generic-prob-node-add-new-state node new-state))))
		      ; Dont do anything in this case.
		      (:DET  nil))))
	 ; Adjust successors to account for this change
	 (dolist (succ (node-successors node))
	    (adjust-succ-for-new-state new-state node succ)))
	 (values node))))

; In the case of :GENERIC nodes we assume that the old function is
; retained unchanged, i.e, the heuristic for constructing the new
; function from the old function is:  "The new function is the same as
; the old function at all points" (the new state is never mapped to, in
; other words).

(defun noisy-or-add-new-state (node new-state)
  (when (eq (noisy-or-subtype node) :BINARY)
    (ideal-warning "Adding state ~A to binary noisy or node ~A.~
                     Changing it to an nary node" new-state node)
    (setf (noisy-or-subtype node) :NARY))
	; Save old noisy or info
  (let ((old-noisy-or-info (get-noisy-or-info node)))
	; Create an empty distribution
    (create-empty-distribution node)
	; Copy the old inhibitor probs
    (copy-inhibitor-probs node old-noisy-or-info)
    (ecase (noisy-or-subtype node)
      (:NARY	; The det fn has to be recomputed.
	(set-noisy-or-det-fn-to-standard-nary-or-fn node))
      (:GENERIC	; Just copy the old fn values
	(for-all-cond-cases (pred-case (node-predecessors node))
	  (setf (noisy-or-det-fn-of node pred-case)
		(get-det-fn-value-from-noisy-or-info
		  old-noisy-or-info (node-predecessors node) pred-case)))))
	; Recompile the distribution
    (compile-noisy-or-distribution node)))

(defun generic-prob-node-add-new-state (node new-state)
  (let ((old-array (distribution-repn node)))
    (create-empty-distribution node)
    (for-all-cond-cases (cond-case (node-predecessors node))
      (for-all-cond-cases (node-case (list node))
	(setf (prob-of node-case cond-case)
	      (if (eq (state-in node-case) new-state) 0
		  (contents-of-dist-array-location old-array node-case cond-case
		    (node-predecessors node))))))))

(defun check-if-valid-label (label-name node)
  (when label-name
    (cond
      ((not (symbolp label-name))
       (error "The suggested new label ~A  for node ~A is not a symbol" label-name node))
      ((member label-name (state-labels node) :key #'label-name)
       (error "The suggested new label ~A for node ~A is already the name of label of ~A."
	      label-name node node))
      ( t label-name))))

(defun query-new-state (node)
  (query `(and SYMBOL (not (MEMBER ,@(mapcar #'label-name  (state-labels node)))))
	 "What is the name of the new state of node ~A ?" node))

(defun max-label-id-number (node)
  (apply #'max (mapcar #'label-id-number (state-labels node))))

(defun adjust-succ-for-new-state (new-state node succ)
  (cond
    ((dummy-node-p succ) nil)
    ((decision-node-p succ)
     (if (distribution-repn succ)
	 (warn "Addition of state ~A to node ~A has left the policy of ~% ~@
                          decision node ~A inconsistent" new-state node succ)))
    ((noisy-or-node-p succ)
     (adjust-noisy-or-succ-for-new-state new-state node succ))
    (t (let ((old-array (distribution-repn succ))
	     (default-value (ecase (node-type succ)
			      (:VALUE 0)
			      (:CHANCE (ecase (relation-type succ)
					 (:PROB (/ 1 (number-of-states succ)))
					 (:DET (centre-most-state-of succ)))))))
	 (create-empty-distribution succ)
	 (for-all-cond-cases (partial-cond-case (remove-node node (node-predecessors succ)))
	   (for-all-cond-cases (node-case (list node))
	     (for-all-cond-cases (succ-case succ)
	       (setf (contents-of succ-case (combine-cond-cases partial-cond-case node-case))
		     (cond
		       ((eq (state-in node-case) new-state) default-value)
		       (t (with-decremented-number-of-states (node)
			    (contents-of-dist-array-location
			      old-array succ-case
			      (combine-cond-cases partial-cond-case node-case)
			      (node-predecessors succ))))))))))))
  (values succ))

(defun adjust-noisy-or-succ-for-new-state (new-state node succ)
  (let ((old-noisy-or-info (get-noisy-or-info succ)))
    (create-empty-distribution succ)
	; Copy the inhibitor probs
    (copy-inhibitor-probs node old-noisy-or-info :default 0)
    (ecase (noisy-or-subtype node)
      ((:BINARY :NARY)
	; Recompute the det fn
       (set-noisy-or-det-fn-to-standard-nary-or-fn node))
      (:GENERIC
	; The new det fn is the same as the old one except for cases
	; where <node> is in the new state <new-state>. In those cases
	; the fn value is set randomly.
	(let ((random-state-of-succ (first (state-labels succ))))
	  (for-all-cond-cases (other-preds-case (remove node (node-predecessors succ)))
	    (for-all-cond-cases (node-case node)
	      (let ((pred-case (combine-cond-cases node-case other-preds-case)))
		(setf (noisy-or-det-fn-of succ pred-case)
		      (cond
			((eq (state-in node-case) new-state)
			 random-state-of-succ)
			(t (with-decremented-number-of-states (node)
			     (get-det-fn-value-from-noisy-or-info
			       old-noisy-or-info (node-predecessors succ) pred-case)))))))))))
	; Compile the distribution
    (compile-noisy-or-distribution succ)))

;********************************DELETE STATE*******************************************

; Deletes a state from the state list of node

(defun delete-state (node &optional state)
  (cond
    ((value-node-p node)
     (error "Cannot remove a state from the (non-existent) state list of ~@
                       value node ~A" node))
    (t (let ((del-state (or state (get-target-state node))))
	; Remove the state from the labels and decrease the number of states of the node
	 (setf (state-labels node) (delete del-state (state-labels node)))
	 (decf (number-of-states node))
	; Adjust the node's distribution for the deletion of the state
	 (case (node-type node)
	   (:DECISION (if (distribution-repn node)
			  (ideal-warning "~%Have removed state ~A ~
                               from the states of the decision node ~A. ~
                               The node policy is now inconsistent" del-state node)))
	   (:CHANCE (case (relation-type node)
		      (:PROB (adjust-chance-prob-dist-for-del-state node del-state))
		      (:DET (adjust-chance-det-dist-for-del-state node del-state)))))
	; Adjust all the successor node's distributions
	 (dolist (succ (node-successors node))
	   (adjust-succ-for-deletion-of-state del-state node succ))
	; Change the id number of states that require the change permanently
	 (dolist (state (state-labels node))
	   (when (> (label-id-number state)(label-id-number del-state))
	     (decf (label-id-number state))))
	 (values node)))))

(defun adjust-chance-prob-dist-for-del-state (node del-state)
  (cond
    ((noisy-or-node-p node)
     (adjust-noisy-or-node-dist-for-del-state node del-state))
    (t (let ((old-array (distribution-repn node))
	     (node-del-case (make-conditioning-case (list (cons node del-state)))))
	 (create-empty-distribution node)
	; for all cond cases
	 (for-all-cond-cases (cond-case (node-predecessors node))
	; Set the increment to: prob of deleted case / the new number of states
	   (let ((prob-increment (/ (contents-of-dist-array-location
				      old-array node-del-case cond-case
				      (node-predecessors node))
				    (number-of-states node))))
	; for all node cases : Get the old value and ...
	     (for-all-cond-cases (node-case (list node))
	       (let ((old-value (contents-of-dist-array-location
				  old-array node-case cond-case (node-predecessors node))))
	; switch context of label id numbers and set new distribution.
		 (with-adjusted-label-ids (node del-state)
		   (setf (contents-of node-case cond-case)
			 (+ old-value prob-increment)))))))))))

; Deleting a state has no effect on the inhibitor probs but it does affect
; the det function ...

(defun adjust-noisy-or-node-dist-for-del-state (node del-state)
  (when (eq (noisy-or-subtype node) :BINARY)
    (ideal-warning "Deleting state ~A of binary noisy or node ~A. Converting to :NARY node"
		   del-state node)
    (setf (noisy-or-subtype node) :NARY))
  (let ((old-noisy-or-info (get-noisy-or-info node)))
	; Create a new distribution
    (create-empty-distribution node)
	; Do all the rest of the stuff in the context of the new state of
	; the node, i.e with label ids adjusted. Note that the
	; (number-of-states node) has already been decfed in the function
	; delete-state.
    (with-adjusted-label-ids (node del-state)
      (ecase (noisy-or-subtype node)
	(:NARY	; Recompute the det fn
	  (set-noisy-or-det-fn-to-standard-nary-or-fn node))
	(:GENERIC
	; The new det fn is the same as the old one except that in locations where the
	; old one mapped to <del-state> the new one maps to some other state, randomly
	; chosen.
	  (let ((random-other-state (find del-state (state-labels node) :test-not #'eq)))
	    (labels ((switch-if-deleted-state (s)
		       (if (eq s del-state) random-other-state s)))
	      (for-all-cond-cases (pred-case (node-predecessors node))
		(let ((existing-value
			(switch-if-deleted-state
			  (get-det-fn-value-from-noisy-or-info
			    old-noisy-or-info (node-predecessors node) pred-case))))
		  (setf (noisy-or-det-fn-of node pred-case) existing-value)))))))
	; Recompile the distribution
      (compile-noisy-or-distribution node))))

; Set any references to the deleted state in the dist to some arbitrary
; non-deleted state.

(defun adjust-chance-det-dist-for-del-state (node del-state)
  (labels ((other-state (s)(not (eq del-state s))))
    (for-all-cond-cases (ccase (node-predecessors node))
      (when (eq (deterministic-state-of node ccase) del-state)
	(with-adjusted-label-ids (node del-state)
	  (setf (deterministic-state-of node ccase)
		(find-if #'other-state (state-labels node))))))))

(defun get-target-state (node)
  (let* ((label-list (state-labels node))
	 (label-name-list (mapcar #'label-name label-list)))
    (ideal-warning "~%The label-list is of node ~A is : ~% ~{~15A~}" node label-name-list)
    (find (query (cons 'MEMBER label-name-list) "Which state do you want removed ?")
      label-list :key #'label-name)))

(defun adjust-succ-for-deletion-of-state (del-state node succ)
  (cond
    ((dummy-node-p succ) nil)
    ((decision-node-p succ)
     (if (distribution-repn succ)(ideal-warning "The removal of state ~A from ~@
                   the states of node ~A has left the node policy of the decision node ~@
                   inconsistent" del-state node succ)))
    ((noisy-or-node-p succ)
     (adjust-noisy-or-succ-for-deletion-of-state del-state node succ))
    ((or (value-node-p succ)(chance-node-p succ))
	; Works for both det and prob chance nodes.
     (let ((old-array (distribution-repn succ)))
       (create-empty-distribution succ)
       (for-all-cond-cases (partial-cond-case (remove-node node (node-predecessors succ)))
	 (for-all-cond-cases (node-case (list node))
	   (when (not (eq (state-in node-case) del-state))
	     (for-all-cond-cases (succ-case succ)
	       (let ((old-contents
		       (with-incremented-number-of-states (node)
			 (contents-of-dist-array-location
			   old-array succ-case
			   (combine-cond-cases node-case partial-cond-case)
			   (node-predecessors succ)))))
		 (with-adjusted-label-ids (node del-state)
		   (setf
		     (contents-of succ-case (combine-cond-cases node-case partial-cond-case))
		     old-contents)))))))))))

(defun adjust-noisy-or-succ-for-deletion-of-state (del-state node succ)
  (let ((old-noisy-or-info (get-noisy-or-info succ)))
	; Create an empty distribution
    (create-empty-distribution succ)
	; Copy relevant inhibitor probabilities
    (copy-inhibitor-probs succ old-noisy-or-info)
    (ecase (noisy-or-subtype succ)
      ((:BINARY :NARY)
       (with-adjusted-label-ids (node del-state)
	; Recompute the det fn
	 (set-noisy-or-det-fn-to-standard-nary-or-fn succ)))
      (:GENERIC
	; The new det fn is just the same as the old one except that it is smaller
	; since one state of the node <node> doesnt exist.
	(for-all-cond-cases (pred-case (node-predecessors succ))
	  (let ((existing-value
		  (get-det-fn-value-from-noisy-or-info
		    old-noisy-or-info (node-predecessors succ) pred-case)))
	    (with-adjusted-label-ids (node del-state)
	      (setf (noisy-or-det-fn-of succ pred-case) existing-value))))))
    (compile-noisy-or-distribution succ)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Add arcs to geNERATE a complete, direct ordering
;;; of the decision nodes in a diagram


(defun add-decision-ordering-arcs (diagram)
  (let* ((decisions (remove-if-not #'decision-node-p  diagram))
	 (ordered-decisions (order decisions :check-complete-ordering t :mode :WARN))
	 (previous-node nil)
	 (remaining-nodes (rest ordered-decisions))
	 (present-node (first ordered-decisions)))
    (loop
      (when (null present-node)(return))
      (when (not (null previous-node))
	(add-arcs present-node (list previous-node)))
      (setq previous-node present-node)
      (setq present-node (pop remaining-nodes)))))
