;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: Ideal;  Base: 10 -*-

(in-package :ideal)


;;;;********************************************************
;;;;  Copyright (c) 1989, 1992 Rockwell International -- All rights reserved.
;;;;  Rockwell International Science Center Palo Alto Lab
;;;;********************************************************

;;;;;;;;;;;;;;;;;;;;;;;; Sampath ;;;;;;;;;;;;;;;;;;;;



(export '(CLUSTERING-INFER-EST))

;--------------------------------------------------------

; Returns the number of combinations of node states that will be
; expanded when clustering-infer is given the same inputs as this
; function. Should be a reliable run time estimator.

; The fn does a partial simulation of what clustering-infer does.
; Nevertheless, it runs in time linear in the size of the input.  The fn
; is written to be debuggable and so is not as fast it can be made.

; When *ideal-debug* is non-nil the function prints a break up of which
; steps take how many operations.

(defun clustering-infer-EST (clique-diagram diagram)
  (let ((count 0))
    (multiple-value-bind (evidence-list evidence-nodes)(create-evidence-list diagram)
      (incf count
	    (update-clique-beliefs-for-evidence-EST
	      clique-diagram evidence-nodes evidence-list))
      (incf count
	    (marginalize-to-find-inf-node-beliefs-EST clique-diagram diagram)))
    (ideal-debug-msg "~%-----------------------------------------~
                      ~%Total Operations:        ~15D~2%" count)
    (values count)))

(defun update-clique-beliefs-for-evidence-EST (clique-diagram evidence-nodes evidence-list)
  (initialize-clique-node-activations clique-diagram)
  (let ((count 0))
    (incf count
	  (set-up-dummy-clique-nodes-for-evidence-EST
	    clique-diagram evidence-nodes evidence-list))
    (let ((sorted-clique-diagram (order clique-diagram)))
      (incf count (clique-lambda-propagate-EST (reverse sorted-clique-diagram)))
      (incf count (clique-pi-propagate-EST sorted-clique-diagram)))
    ; The calc and set beliefs step has been dropped
    ;(incf count (calculate-and-set-clique-diagram-beliefs-EST clique-diagram))
    (values count)))

(defun set-up-dummy-clique-nodes-for-evidence-EST (clique-diagram evidence-nodes evidence-list)
  (declare (ignore evidence-list))
  (let ((count 0)
	(evidence-nodes-in-clique nil)
	(simulation-dummy-node (make-clique-node :type-* :DUMMY-CLIQUE-NODE)))
    (dolist (c-node clique-diagram)
      (setq evidence-nodes-in-clique
	    (intersection evidence-nodes (clique-node-component-nodes c-node)))
      (when evidence-nodes-in-clique
	(incf count (number-of-states c-node))
	(push simulation-dummy-node (clique-node-lambda-activator-nodes c-node))))
    (ideal-debug-msg "~3%Setting up dummy nodes:  ~15D" count)
    (values count)))

(defun clique-lambda-propagate-EST (ordered-clique-nodes)
  (let ((count 0))
    (dolist (c-node ordered-clique-nodes)
      (when (clique-node-lambda-activator-nodes c-node)
	(incf count (update-clique-lambdas-EST c-node))
	(incf count (update-clique-lambda-messages-EST c-node))))
    (ideal-debug-msg "~%Lambda propagation:      ~15D" count)
    (values count)))


(defun update-clique-lambdas-EST (c-node)
  (number-of-states c-node))

(defun update-clique-lambda-messages-EST (c-node)
  (cond
    ((parent c-node)
     (push c-node (clique-node-lambda-activator-nodes (parent c-node)))
     (values (* (number-of-states (parent c-node))(number-of-states c-node))))
    (t (values 0))))

(defun clique-pi-propagate-EST (ordered-clique-nodes)
  (let ((count 0))
    (dolist (c-node ordered-clique-nodes)
      (cond
	((clique-node-pi-activated-p c-node)
	 (incf count (update-clique-pies-EST c-node))
	 (incf count (update-clique-pi-messages-EST c-node)))
	((clique-node-lambda-activator-nodes c-node)
	 (cond
	   ((more-than-one (clique-node-lambda-activator-nodes c-node))
	    (incf count (update-clique-pi-messages-EST c-node)))
	   (t (dolist (child (clique-node-successors c-node))
		(when (not (eq child (first (clique-node-lambda-activator-nodes c-node))))
		  (incf count (update-clique-pi-msg-of-EST child c-node)))))))))
    (ideal-debug-msg "~%Pi Propagation:          ~15D" count)
    (values count)))

(defun update-clique-pies-EST (c-node)
  (cond
    ((parent c-node)(* (number-of-states c-node)(number-of-states (parent c-node))))
    (t (number-of-states c-node))))

(defun update-clique-pi-messages-EST (c-node)
  (let ((count 0))
    (dolist (child (node-successors c-node))
      (incf count (update-clique-pi-msg-of-EST child c-node)))
    (values count)))

(defun update-clique-pi-msg-of-EST (child c-node)
  (cond
    ((dummy-clique-node-p child) 0)
    (t (setf (clique-node-pi-activated-p child) t)
       (* 2 (number-of-states c-node)))))

; The calculate and set beliefs step has been dropped (9 Feb 90)
;(defun calculate-and-set-clique-diagram-beliefs-EST (clique-diagram)
;  (let ((count 0))
;    (dolist (c-node clique-diagram)
;      (incf count (calculate-and-set-beliefs-EST c-node)))
;    (ideal-debug-msg "~%Calculating beliefs:     ~15D" count)
;    (values count)))
;
;(defun calculate-and-set-beliefs-EST (c-node)
;  (number-of-states c-node))


(defun marginalize-to-find-inf-node-beliefs-EST (clique-diagram diagram)
  (let ((count 0)
	(ordered-clique-diagram (order-clique-diagram clique-diagram)))
    (dolist (inf-node diagram)
      (incf count
	    (+	; For normalization ..
	      (number-of-states inf-node)
	      ; For marginalization ..
	      (product-over (n (clique-node-component-nodes
				 (find-smallest-clique-containing-node
				   inf-node ordered-clique-diagram)))
		(number-of-states n)))))
    (ideal-debug-msg "~%Marginalizing:           ~15D" count)
    (values count)))

;;; creates cliqu diagram without installing numbers for use in 
;;; generating estimates for alternative cliques

(defun create-clique-diagram-EST (belief-net)
  (initialize-belnet-for-clustering-algorithm belief-net)
  (prog1
    (set-up-clique-diagram-data-structures
      (make-clustering-join-tree
	(find-join-tree-structure belief-net)))
    (set-diagram-initialization belief-net :algorithm-type :CLUSTERING)))
