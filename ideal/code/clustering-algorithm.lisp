;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: Ideal;  Base: 10 -*-

(in-package :ideal)



;;;;********************************************************
;;;;  Copyright (c) 1989, 1992 Rockwell International -- All rights reserved.
;;;;  Rockwell International Science Center Palo Alto Lab
;;;;********************************************************


;;;;;;;;;;;;;;;;;;;;;;;; Sampath ;;;;;;;;;;;;;;;;;;;;


(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(CREATE-CLIQUE-DIAGRAM CLUSTERING-INFER DUMMY-CLIQUE-NODE-P )))

;--------------------------------------------------------

; ---- Top level stuff --------------------------------

(defun create-clique-diagram (&optional (belief-net *diagram*))
  (initialize-belnet-for-clustering-algorithm belief-net)
  (prog1
    (initialize-clique-diagram
      (set-up-clique-prob-dists
	(set-up-clique-diagram-data-structures
	  (make-clustering-join-tree
	    (find-join-tree-structure belief-net)))))
    (set-diagram-initialization belief-net :algorithm-type :CLUSTERING)))

(defun clustering-infer (clique-diagram belief-net)
  (when (initialized-diagram belief-net :algorithm-type :CLUSTERING)
    (ideal-debug-msg "~% It is assumed that the clique diagram provided by you is valid")
    (multiple-value-bind  (evidence-list evidence-nodes)(create-evidence-list belief-net)
	; Propagate the  evidence
      (update-clique-beliefs-for-evidence clique-diagram evidence-nodes evidence-list)
      (ideal-debug-msg "~%--------- Done with evidence propogation")
	; Marginalize and update the belief vector of each influence diagram node
      (marginalize-to-find-inf-node-beliefs clique-diagram belief-net)
	; Record  evidence in the belief-net's node's old-state field
      (dolist (evidence evidence-list)
	(setf (node-old-state (car evidence))(cdr evidence)))
	; Mark the diagram as unchanged
      (mark-diagram-as-unchanged belief-net)
	; Return the influence diagram
       (values belief-net))))

;--- This fn does the real work --------------------------

; This is a new implementation that updates every message sent by a node
; at most once. (No update may be required somtimes). Is dramatically
; faster than the old "parallel" style implementation (25 Sep). I have
; junked the old implementation.

(defun update-clique-beliefs-for-evidence (clique-diagram evidence-nodes evidence-list)
        ; Initialization
  (initialize-clique-node-activations clique-diagram)
    	; Setting up dummy clique nodes for evidence.
  (set-up-dummy-clique-nodes-for-evidence clique-diagram evidence-nodes evidence-list)
	; Propagating the effects of the evidence on affected nodes ..
  (let ((sorted-clique-diagram (order clique-diagram)))
	; Propagating lambda messages up the tree.
    (clique-lambda-propagate (reverse sorted-clique-diagram))
	; Propagating pi messages down the tree.
    (clique-pi-propagate sorted-clique-diagram)))


(defun clique-lambda-propagate (ordered-clique-nodes)
  (ideal-debug-msg "~%Lambda propagation:")
  (dolist (c-node ordered-clique-nodes)
	; When the c-node has been lambda activated by some node ...
    (when (clique-node-lambda-activator-nodes c-node)
      (update-clique-lambdas c-node)
      (update-clique-lambda-messages c-node))))

(defun clique-pi-propagate (ordered-clique-nodes)
  (ideal-debug-msg "~%Pi propagation:")
  (dolist (c-node ordered-clique-nodes)
    (cond
      ((clique-node-pi-activated-p c-node)
       (update-clique-pies c-node)
       (update-clique-pi-messages c-node))
	; The c-node has been lambda activated ...
      ((clique-node-lambda-activator-nodes c-node)
       (cond
	 ((more-than-one (clique-node-lambda-activator-nodes c-node))
	  (update-clique-pi-messages c-node))
	 (t (dolist (child (clique-node-successors c-node))
	      (when (not (eq child (first (clique-node-lambda-activator-nodes c-node))))
		(update-clique-pi-msg-of child c-node)))))))))

;--- Lambda processing --------------------------------

(defun update-clique-lambdas (c-node)
  (for-all-clique-node-cases (node-case c-node)
    (setf (clique-lambda-of c-node node-case)
	  (calculate-clique-lambda-of c-node node-case))))

(defun calculate-clique-lambda-of (c-node node-case)
  (product-over (child-node (node-successors c-node))
    (clique-lambda-msg-of c-node node-case child-node)))

(defun update-clique-lambda-messages (c-node)
  (when (parent c-node)
    (ideal-debug-msg "~% Lambda msg; ~A --> ~A"
		     (clique-node-name c-node)(clique-node-name (parent c-node)))
    (for-all-clique-node-cases (parent-case (parent c-node))
      (setf (clique-lambda-msg-of (parent c-node) parent-case c-node)
	    (calculate-clique-lambda-msg-of parent-case c-node)))
    (push c-node (clique-node-lambda-activator-nodes (parent c-node)))))

(defun calculate-clique-lambda-msg-of (parent-case child-node)
  (let ((total 0))
    (for-all-clique-node-cases (child-node-case child-node)
      (incf total
	    (* (clique-lambda-of child-node child-node-case)
	       (clique-prob-of child-node child-node-case parent-case))))
    (values total)))

;----- Pi Processing -------------------------------------------------

(defun update-clique-pies (c-node)
  (for-all-clique-node-cases (node-case c-node)
    (setf (clique-pi-of c-node node-case)
	  (calculate-clique-pi-of c-node node-case))))

; When c-node is a root node the existing pi is returned without an
; update.

(defun calculate-clique-pi-of (c-node node-case)
  (cond
    ((null (parent c-node)) (clique-pi-of c-node node-case))
    ( t (let ((total 0))
	  (for-all-clique-node-cases (parent-case (parent c-node))
	    (incf total
		  (* (clique-prob-of c-node node-case parent-case)
		     (clique-pi-msg-of c-node parent-case))))
	  (values total)))))

; The math does not required normalizawtion of pi-msgs but it is
; desirable to do to avoid numerical errors caused by underflow.

(defun update-clique-pi-messages (c-node)
  (dolist (child (node-successors c-node))
    (update-clique-pi-msg-of child c-node)))


(defun update-clique-pi-msg-of (child c-node)
  (when (not (dummy-clique-node-p child))
    (ideal-debug-msg "~% Pi msg: ~A --> ~A"
		     (clique-node-name c-node)(clique-node-name child))
    (let ((total 0))
      (for-all-clique-node-cases (node-case c-node)
	(incf total 
	      (setf (clique-pi-msg-of child node-case)
		    (calculate-clique-pi-msg-of child node-case c-node))))
      (for-all-clique-node-cases (node-case c-node)
	(setf (clique-pi-msg-of child node-case)
	      (/ (clique-pi-msg-of child node-case) total))))
    (setf (clique-node-pi-activated-p child) t)))

(defun calculate-clique-pi-msg-of (child-node parent-node-case parent-node)
  (* (clique-pi-of parent-node parent-node-case)
   (product-over (other-child (remove-node child-node (clique-node-successors parent-node)))
     (clique-lambda-msg-of parent-node parent-node-case other-child))))

;-------- Clique node activation -----------------------------------------

(defun initialize-clique-node-activations (clique-diagram)
  (dolist (c-node clique-diagram)
    (setf (clique-node-pi-activated-p c-node) nil
	  (clique-node-lambda-activator-nodes c-node) nil)))

;----- Dummy nodes and evidence ---------------------------------------------------------

(defun dummy-clique-node-p (c-node)
  (eq (clique-node-type c-node) :DUMMY-CLIQUE-NODE))

(defun set-up-dummy-clique-nodes-for-evidence (clique-diagram evidence-nodes evidence-list)
  (let (evidence-nodes-in-clique)
	; Setting up dummy clique nodes for nodes that need them ..
    (dolist (c-node clique-diagram)
      (setq evidence-nodes-in-clique
	    (intersection evidence-nodes (clique-node-component-nodes c-node)))
	; When the clique node has some evidence nodes as component
	; nodes create a dummy node for the clique node AND activate
	; clique node (implemented as activation of
	; parent of dummy node)
      (when evidence-nodes-in-clique
	(push
	  (set-up-dummy-node-for-clique
	    c-node (relevant-evidence evidence-list evidence-nodes-in-clique))
	  (clique-node-lambda-activator-nodes c-node))))))

(defun relevant-evidence (evidence-list evidence-nodes-in-clique)
  (labels ((evidence-pertinent-to-clique-p (evidence-item)
	     (member (car evidence-item) evidence-nodes-in-clique)))
    (remove-if-not #'evidence-pertinent-to-clique-p evidence-list)))

(defun set-up-dummy-node-for-clique (affected-c-node relevant-evidence-list)
  (let ((dummy-c-node (make-dummy-clique-node-for affected-c-node)))
    (for-all-clique-node-cases (a-node-case affected-c-node)
      (setf (clique-lambda-msg-of affected-c-node a-node-case dummy-c-node)
	    (calculate-dummy-node-lambda-msg a-node-case relevant-evidence-list)))
    (values dummy-c-node)))

(defun make-dummy-clique-node-for (affected-c-node)
  (let (dummy-c-node)
    (cond
      ((setq dummy-c-node (find-if #'dummy-clique-node-p (node-successors affected-c-node)))
       nil)
      (t (setf dummy-c-node (make-clique-node)
	       (clique-node-name dummy-c-node)(make-name-for-dummy-clique-node affected-c-node)
	       (clique-node-type dummy-c-node) :DUMMY-CLIQUE-NODE)
	; Make the affected node its predecessor
	 (add-link dummy-c-node affected-c-node)
	; Set up lambda msg data structure for the affected node
	 (setf (clique-node-lambda-msg dummy-c-node)
	       (list (cons affected-c-node (make-vanilla-clique-array affected-c-node))))))
	; return the dummy node
    (values dummy-c-node)))

(defun make-name-for-dummy-clique-node (affected-c-node)
  (gentemp (string-upcase
	     (format nil "DUMMY-FOR-~A" (symbol-name (clique-node-name affected-c-node))))))

; For the case a-node-case, combines all the evidence in the evidence
; list of dummy-node and returns appropriate weight.

(defun calculate-dummy-node-lambda-msg (a-node-case  relevant-evidence-list)
  (let ((answer 1))
    (dolist (evidence relevant-evidence-list)
      (let* ((node (car evidence))
	     (label-associated-with-node-in-case (cdr (assoc node a-node-case)))
	     (weight-list (cdr evidence))
	     (weight (cond
	; Evidence retraction: 
	; If there is no other evidence a lambda vector of all 1's will be
	; set up (as it should be).If there is other evidence the lambda
	; msg will be set up as if there is no obeserved evidence for the
	; node in the retracted evidence item, as it should be.
		       ((null weight-list) 1)
		       ((label-p weight-list)
			(if (eq weight-list label-associated-with-node-in-case) 1 0))
		       (t (cdr (assoc label-associated-with-node-in-case weight-list))))))
	(setq answer (* answer weight))
	; Local speedup hack
	(if (zerop answer)(return))))
    (values answer)))

;---- Marginalizing beliefs from the clique diagram -------------------------------

; Marginalizes over smallest clique that contains the node of interest.

(defun marginalize-to-find-inf-node-beliefs (clique-diagram belief-net)
  (ideal-debug-msg "~2%---Marginalizing clique node beliefs to find influence diagram~@
                            Node beliefs")
  (let ((ordered-clique-diagram (order-clique-diagram clique-diagram)))
    (dolist (inf-node belief-net)
      (let* ((chosen-clique-node
	       (find-smallest-clique-containing-node
		 inf-node ordered-clique-diagram))
	     (nodes-to-sum-over
	       (find-nodes-to-sum-over inf-node chosen-clique-node)))
	(let ((total 0))
	  (for-all-cond-cases (inf-node-case inf-node)
	    (incf total
		  (setf (belief-of inf-node-case)
			(sum-belief-of-clique-over-nodes-to-sum-over
			  inf-node-case chosen-clique-node nodes-to-sum-over))))
	  (when (zerop total)
	    (error "Marginal of ~A is zero. The input evidence is impossible  ~
                    , i.e., has a joint probability of zero" inf-node))
	  (for-all-cond-cases (inf-node-case inf-node)
	    (setf (belief-of inf-node-case)(/ (belief-of inf-node-case) total))))))))

(defun find-smallest-clique-containing-node (inf-node ordered-clique-diagram)
  (find-if #'(lambda (c-node)
	       (member inf-node (clique-node-component-nodes c-node)))
	   ordered-clique-diagram))

(defun find-nodes-to-sum-over (inf-node clique-node)
  (remove-node inf-node (clique-node-component-nodes clique-node)))

(defun order-clique-diagram (clique-diagram)
  (labels ((number-of-component-nodes (cn)(length (clique-node-component-nodes cn))))
    (sort (copy-list clique-diagram) #'< :key #'number-of-component-nodes)))

(defun sum-belief-of-clique-over-nodes-to-sum-over (inf-node-case c-node nodes-to-sum-over)
  (let ((total 0) clique-case)
    (for-all-cond-cases (sum-nodes-case nodes-to-sum-over)
      (setq clique-case  (combine-cond-cases inf-node-case sum-nodes-case))
      (incf total
	    (* (clique-lambda-of c-node clique-case)(clique-pi-of c-node clique-case))))
    (values total)))



;----  Returns a join tree.

; This fn basically converts the "join-tree-structure" type data
; structure for a join tree to an equivalent data structure that
; clustering-infer uses.

(defun make-clustering-join-tree (join-tree-structure)
  (let* ((index 0)
	 (assoc-list
	   (mapcar #'(lambda (jtn)
		       (cons jtn (make-clique-node-for-clique
				   (jt-node-clique jtn)(incf index))))
		   join-tree-structure))
	 jt-node jt-parent-node)
    (dolist (jtn.clique-node assoc-list)
      (setq jt-node (car jtn.clique-node) jt-parent-node (jt-node-parent jt-node))
      (if jt-parent-node
	  (add-link (cdr jtn.clique-node)(cdr (assoc jt-parent-node assoc-list)))))
    (values (mapcar #'cdr assoc-list))))
