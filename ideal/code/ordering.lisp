;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: Ideal;  Base: 10 -*-

(in-package :ideal)



;;;;********************************************************
;;;;  Copyright (c) 1989, 1992 Rockwell International -- All rights reserved.
;;;;  Rockwell International Science Center Palo Alto Lab
;;;;********************************************************


;;;;;;;;;;;;;;;;;;;;;;;; Sampath ;;;;;;;;;;;;;;;;;;;;


(export '(ORDER ACYCLIC-P))

;------------------------------------------------------------

; The following three functions are used by the "order" function below.
; They are listed here since they are compiled in-line by order.

(defun last-node-w.r.t-node-list-p (node node-list)
  (labels ((descendant-is-a-member-of-node-list-p (node)
	     (or (member node node-list)
		 (some #'descendant-is-a-member-of-node-list-p
		       (node-successors node)))))
    (not (some #'descendant-is-a-member-of-node-list-p (node-successors node)))))

; We do not use a remove-if since we need to guarantee that the answer
; does not share structure with the input n-list.

(defun find-last-nodes (n-list)
  (declare (inline last-node-w.r.t-node-list-p))
  (let ((last-nodes nil))
    (dolist (n n-list)
      (when (last-node-w.r.t-node-list-p n n-list) (push n last-nodes)))
    (values last-nodes)))


(defun more-than-one-element (list)
  (not (null (cdr list))))


; Orders node-list such that weak predecessors of a node always precede
; a node in the returned node-list. When :mode is :warn it returns a
; second value which is t if the ordering was successful and nil if not.
; When :mode is :error an error is signalled describing why the ordering
; was not successful.  When the second value is nil the first value has
; no meaning.

; If the :last-node argument is specified:  1. If the last-node is not a
; member of the input node-list it is ignored.  2. Else last-node is
; ordered last in the returned list, if it is possible. If this is not
; possible it is an error/warning.

; If a value node is present in the node-list it is ordered last unless
; a last-node argument is specified which is a member of the input
; node-list.

; If :check-complete-ordering is t and the nodes in node-list do not
; have a complete-ordering w.r.t each other, it is an error/warning.

; The semantics are slightly messed up coz this should be a function
; that knows only about dags, but knowledge about last-nodes and value
; nodes are forced into it. This is not the most efficient possible
; implementation but it is not very hairy, which is important coz a lot
; of early bugs were caused by subtle bugs in this fn. Nevertheless, it
; is 3 times faster than the old version (21 Apr).

(defun order (node-list &key (check-complete-ordering nil)
	      (last-node nil)(mode :error))
  "Weak predecessors of a node always precede node in returned list"
  (declare (inline find-last-nodes more-than-one-element))
  (let ((n-list (copy-list node-list)) (ordered-node-list nil)
	(actual-last-node (or (find last-node node-list)(find-if #'value-node-p node-list)))
	last-nodes)
    (loop
      (setq last-nodes (find-last-nodes n-list))
      (cond
						; Termination condition
	((null n-list)(return (values ordered-node-list t)))
						; Check for complete ordering if necessary.
	((and check-complete-ordering (more-than-one-element last-nodes))
	 (ideal-action mode "The ordering of nodes ~A is not complete w.r.t each other"
		       last-nodes) (return (values nil nil)))
						; Check for existence of a loop
	((null last-nodes)
	 (ideal-action mode "There is a loop in the diagram involving a subset of the nodes ~
               in the node set ~A" n-list))
	; This particular branch can be taken only in the 1st pass thru the loop coz
	; actual-last-node is set to nil in this branch and so the branch can never
	; be reentered.
	(actual-last-node (cond
			    ((not (member actual-last-node last-nodes))
			     (ideal-action mode "The node ~A cannot be ordered last"
					   actual-last-node)
			     (return (values nil nil)))
			    ((null ordered-node-list)
			     ; Hack to put the actual-last-node in the end.
			     (setq last-nodes
				   (nconc (delete actual-last-node last-nodes)
					  (list actual-last-node))
				   actual-last-node nil))
			    (t (error "Erroneous state: This error implies that there ~
			    is an algorithm mistake in the ORDER function")))))

       (setq ordered-node-list (nconc last-nodes ordered-node-list))
       (setq n-list (nset-difference n-list last-nodes)))))

; This function could be programmed directly such that the consing that
; takes place in order is avioded. Is not worth it coz this function is
; used only for consistency checking etc and is not used in actual id or
; belief net algorithms.

(defun acyclic-p (diagram)
  (multiple-value-bind (ignore no-cycle?)
      (order diagram :mode :warn)
    (declare (ignore ignore))
    (values no-cycle?)))


(defun ordered-p (diagram &key (reverse nil))
  (let ((diag (if reverse (reverse diagram) diagram))
	(previous-nodes nil) present-node bad-node)
    (dolist (n diag)
      (setq bad-node
	    (find-if-not #'(lambda (p)(member p previous-nodes))(node-predecessors n)))
      (when bad-node
	(error "Ordering is wrong. Node ~A is not ordered before ~A" bad-node present-node))
      (push n previous-nodes))
    (values t)))
