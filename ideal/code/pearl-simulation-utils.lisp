;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: Ideal;  Base: 10 -*-

(in-package :ideal)



;;;;********************************************************
;;;;  Copyright (c) 1989, 1992 Rockwell International -- All rights reserved.
;;;;  Rockwell International Science Center Palo Alto Lab
;;;;********************************************************


;;;;;;;;;;;;;;;;;;;;;;;; Sampath ;;;;;;;;;;;;;;;;;;;;



(export '(MARKOV-BLANKET))

;--------------------------------------------------------

(defun markov-blanket (node)
  (discrete-dist-markov-blanket (node-distribution node)))

(defsetf markov-blanket (node)(value)
  `(setf (discrete-dist-markov-blanket
	   (node-distribution ,node)) ,value))

(defun transition-array (node)
  (discrete-dist-transition-array (node-distribution node)))

(defsetf transition-array (node)(value)
  `(setf (discrete-dist-transition-array (node-distribution ,node)) ,value))

(defun random-state-of (node)
  (node-old-state node))

(defsetf random-state-of (node)(value)
  `(setf (node-old-state ,node) ,value))

(defun make-transition-array (node)
  (make-probability-array (cons node (markov-blanket node))))

(defun transition-prob-of (node-case markov-blanket-case)
  (read-probability-array (transition-array (node-in node-case))
			  markov-blanket-case
			  (markov-blanket (node-in node-case))
			  :main-node-case node-case))

(defsetf transition-prob-of (node-case markov-blanket-case)(value)
  (let ((node-case-var (gentemp "node-case")))
    `(let ((,node-case-var ,node-case))
       (write-probability-array (transition-array (node-in ,node-case-var))
				,markov-blanket-case
				(markov-blanket (node-in ,node-case-var))
				,value
				:main-node-case ,node-case-var))))

(defun find-markov-blanket (node)
  (delete node
	  (delete-duplicates
	    (nconc (copy-list (node-predecessors node))
		   (copy-list (node-successors node))
		   (mapappend #'node-predecessors (node-successors node))))))

(defun simulation-normalize-beliefs (bel-net)
  (mapc #'simulation-normalize-node-beliefs bel-net))

(defun simulation-normalize-node-beliefs (node)
  (cond
    ((simulation-evidence-node-p node)
     (for-all-cond-cases (nc (list node))
       (setf (belief-of nc)
	     (if (eq (simulation-evidence-state-of node)
		     (state-in nc)) 1 0))))
    (t (let ((total 0))
	 (for-all-cond-cases (nc (list node))
	   (incf total (belief-of nc)))
	 (for-all-cond-cases (nc (list node))
	   (setf (belief-of nc)(/ (belief-of nc) total))))))
  (values node))


(defun simulation-evidence-node-p (node)
  (member (node-state node)(state-labels node)))

(defun simulation-evidence-state-of (node)(node-state node))

(defun generate-random-probability ()(random 1.0))

;---------------------------------------------------------------

;When triggering a node (in the fn trigger) a markov blanket case has to
;be generated.   Instead of consing the whole thing up each time it is
;better to keep a template of the cond-case and destructively change the
;states associated with each node every time that the blanket case needs
;to be generated. The following implements this (10 Apr)

(defun markov-blanket-case-template (node)
  (node-pi-msg node))

(defsetf markov-blanket-case-template (node)(value)
  `(setf (node-pi-msg ,node) ,value))

; The actual state in the markov-blanket-case template do not matter
; since they will be destructively changed anyway (see
; get-markov-blanket-case). Therefore arbitrary cond-case. The cond-case
; is just needed to "size" the data structure.

(defun make-markov-blanket-case-template (node)
  (make-arbitrary-cond-case (markov-blanket node)))

(defun make-arbitrary-cond-case (node-list)
  (make-conditioning-case
    (mapcar #'(lambda (node)(cons node (arbitrary-state node))) node-list)))

(defun arbitrary-state (node)
  (first (state-labels node)))

(defun generate-markov-blanket-case (node)
  (mapc #'(lambda (node.state)
	    (rplacd node.state (random-state-of (car node.state))))
	(markov-blanket-case-template node)))
;------------------------------------------------------------------
