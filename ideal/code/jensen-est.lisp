;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: Ideal;  Base: 10 -*-

(in-package :ideal)


;;;;********************************************************
;;;;  Copyright (c) 1989, 1992 Rockwell International -- All rights reserved.
;;;;  Rockwell International Science Center Palo Alto Lab
;;;;********************************************************

;;;;;;;;;;;;;;;;;;;;;;;; Sampath ;;;;;;;;;;;;;;;;;;;;



(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(JENSEN-INFER-EST)))

;--------------------------------------------------------

; Estimates number of combinatoric operations.

;The mode argument affects how the estimation for collect-evidence is
;done. If mode is :COMPLETE then the estimate used is for a complete
;traversal of the join tree. If it is :INCREMENTAL the estimate is for
;only traversing the subtree containing the evidence univs.   The actual
;fn jensen-infer is always in :INCREMENTAL mode. The mode arg has been
;put in in the estimator just to see whether there IS a big win by
;switching from the dumb and simple :COMPLETE approach to the fancy
;:INCREMENTAL approach.

(defun jensen-infer-est (join-tree belief-net &key (mode :INCREMENTAL))
  (reset-activations join-tree)
  (enter-evidence belief-net join-tree :mode :SIMULATE)
  (let ((estimate (+ (enter-evidence-est belief-net join-tree)
		     (jensen-propagate-est join-tree mode)
		     (set-beliefs-est belief-net join-tree))))
    (ideal-debug-msg "~%Total operations:     ~6D~%"  estimate)
    (values estimate)))

(defun jensen-propagate-est (join-tree mode)
  (+ (collect-evidence-est join-tree mode)(distribute-evidence-est join-tree)))

; During Collect-evidence:
; The state space of each univ is traversed twice (except for root and leaf univs).
; "Each univ" refers to each univ in the join tree if the mode is :COMPLETE and
; refers to each univ in the evidence sub tree if the mode is :INCREMENTAL.

(defun collect-evidence-est (join-tree mode)
  (let ((estimate 0))
    (ecase mode
      (:COMPLETE
	(dolist (u (jensen-join-tree-univs join-tree))
	  (incf estimate
		(* (+ 1 (if (or (leaf-univ-p u)(root-univ-p u join-tree)) 0 1))
		   (univ-cardinality u)))))
      (:INCREMENTAL
	(dolist (u (jensen-join-tree-univs join-tree))
	  (when (univ-ev-subtree-member-p u)
	    (incf estimate
		  (* (+ 1 (if (or (univ-ev-subtree-leaf-p u)
				  (root-univ-p u join-tree)) 0 1))(univ-cardinality u)))))))
    (ideal-debug-msg "~% Collect Ev:           ~6D" estimate)
    (values estimate)))

(defun leaf-univ-p (u)
  ; The following is equivalent to "u has less than two neighbours".
  (null (cdr (univ-sepset-neighbours u))))

(defun root-univ-p (u join-tree)
  (eq u (jensen-join-tree-root-univ join-tree)))

; During Distribute-evidence:
; The state space of each univ is traversed n times where n is the number of
; neighbours of the univ.

(defun distribute-evidence-est (join-tree)
  (let ((estimate
	  (sum-over (u (jensen-join-tree-univs join-tree))
	    (* (number-of-neighbours u)(univ-cardinality u)))))
    (ideal-debug-msg "~% Distribute Ev:        ~6D" estimate)
    (values estimate)))

(defun number-of-neighbours (u)
  (length (univ-sepset-neighbours u)))


(defun enter-evidence-est (belief-net join-tree)
  (let ((evidence-nodes (get-evidence-nodes belief-net))(total 0))
    (dolist (n evidence-nodes)
      (incf total (univ-cardinality (find-containing-univ n join-tree))))
    (ideal-debug-msg "~% Entering Evidence:    ~6D" total)
    (values total)))

(defun set-beliefs-est (belief-net join-tree)
  (let ((est 0))
    (dolist (n belief-net)
      (incf est
	    (+ (univ-cardinality
		 (find n (jensen-join-tree-univs join-tree)
		       :key #'univ-component-nodes :test #'member))
	       (number-of-states n))))
    (ideal-debug-msg "~% Calculating beliefs:  ~6D" est)
    (values est)))

   
