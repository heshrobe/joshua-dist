;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: Ideal;  Base: 10 -*-

(in-package :ideal)


;;;;********************************************************
;;;;  Copyright (c) 1989, 1992 Rockwell International -- All rights reserved.
;;;;  Rockwell International Science Center Palo Alto Lab
;;;;********************************************************

;;;;;;;;;;;;;;;;;;;;;;;; Sampath ;;;;;;;;;;;;;;;;;;;;



(export '(BELIEF-OF LAMBDA-OF PI-OF PI-MSG-OF DISPLAY-BELIEFS
		    DIAG-DISPLAY-BELIEFS PRINT-DIAG-BELIEFS))


;--------------------------------------------------------

; Creating a msg array

(defun make-vanilla-msg-array (sizer-node &key (initial-element 0))
  (make-array (number-of-states sizer-node) :initial-element initial-element
	      :element-type 'NUMBER))

; **** Access functions for the belnets stuff *********

; Thw following 2 fns are used in line by location-of, which is the
; primitive access fn for polytree datastructures.


(defun get-msg-array (main-node-case neighbour-node msg-type)
  (let ((array (ecase msg-type
		 (:BELIEF (node-bel (node-in main-node-case)))
		 (:ACTUAL-BELIEF (node-actual-bel (node-in main-node-case)))
		 (:LAMBDA (cdr (assoc (node-in main-node-case)
				      (node-lambda-msg neighbour-node))))
		 (:PI (cdr (assoc neighbour-node (node-pi-msg (node-in main-node-case))))))))
    (when (null array)
      (error "Could not find ~A in ~As ~A message field"
	     (node-in main-node-case) neighbour-node msg-type))
    (values array)))

(defun check-node-case (node-case)
  (cond
    ((not (and (listp node-case)(null (rest node-case))))
     (error "~A is not a list of length 1. Cannot be a node-case"))
    ((not (eq  (label-node (state-in node-case)) (node-in node-case)))
     (error " The state ~A in the node-case ~A is not a valid state of node ~A"
	    (state-in node-case) node-case (node-in node-case)))
    (t t )))

(proclaim '(inline get-msg-array check-node-case))

;---------
(defun location-of (node-case msg-type neighbour-node)
  (when (check-node-case node-case)
    (aref (get-msg-array node-case neighbour-node msg-type)
	  (label-id-number (state-in node-case)))))

(defun write-to-location-of (node-case msg-type neighbour-node value)
  (when (check-node-case node-case)
    (setf (aref (get-msg-array node-case neighbour-node msg-type)
		(label-id-number (state-in node-case))) value)))

(proclaim '(inline location-of write-to-location-of))

(defsetf location-of write-to-location-of)

; Notes about data structures:
; For a node A with a Parent B and a child C (say):

; 1. The beliefs of A are stored in an array in the node-bel field of A.
; 2. The node-pi-msg field of A contains an assoc list. The cars in the assoc
;    list are children of A (e.g, C) and the cdrs are the pi msg arrays.
;    These arrays are of size = number of states of A. Each array represents
;    the msg from A to the child which is the car in the assoc list.
; 3. The node-lambda-msg field contains an assoc list. The cars in the assoc
;    are parents of A (e.g B) and the cdrs are lambda msg arrays. These
;    arrays are each of size = number of states of the corresponding parent
;    array in the car. Each array represents the message from the node A
;    to the corresponding parent node.
; 4. Overall Pi and Overall Lambda for node A are stored in the assoc lists found
;   in the node-pi-msg and node-lambda-msg field respectively. The key (the car)
;   is node A itself in both cases and the size of each array = number of states
;    of A.

;-------

; The third argument in the following 2 definitions is irrelevant and
; could be nil but I have put in (node-in node-case) to aviod a compiler
; warning that comes up in Genera due to the in-line compilation.

(defun belief-of (node-case)
  (location-of node-case :BELIEF (node-in node-case)))

(defsetf belief-of (node-case)(value)
  (let ((node-case-var (gentemp "node-case")))
    `(let ((,node-case-var ,node-case))
       (write-to-location-of ,node-case-var :BELIEF (node-in ,node-case-var) ,value))))
;--------
(defun lambda-of (node-case)
  (location-of node-case :LAMBDA (node-in node-case)))

(defsetf lambda-of (node-case)(value)
  (let ((node-case-var (gentemp "node-case")))
    `(let ((,node-case-var ,node-case))
       (write-to-location-of ,node-case-var :LAMBDA (node-in ,node-case-var) ,value))))
;---------
(defun lambda-msg-of (node-case succ)
  (location-of node-case :LAMBDA succ))

(defsetf lambda-msg-of (node-case succ)(value)
  `(write-to-location-of ,node-case :LAMBDA ,succ ,value))
;----------
(defun pi-of (node-case)
  (location-of node-case :PI (node-in node-case)))

(defsetf pi-of (node-case)(value)
  (let ((node-case-var (gentemp "node-case")))
    `(let ((,node-case-var ,node-case))
       (write-to-location-of ,node-case-var :PI (node-in ,node-case-var) ,value))))
;-------
(defun pi-msg-of (node pred-case)
  (location-of pred-case :PI node))

(defsetf pi-msg-of (node pred-case)(value)
  `(write-to-location-of ,pred-case :PI ,node ,value))


; --- Lambda processing --------

; Update overall lambda of node

(defun update-overall-lambda (node)
  (ideal-debug-msg "~% UPDATE OVERALL LAMBDA: ~A" node)
  (for-all-cond-cases (node-case node)
    (setf (lambda-of node-case)(calculate-lambda-of node-case))))

(defun calculate-lambda-of (node-case)
  (product-over (succ (node-successors (node-in node-case)))
    (lambda-msg-of node-case succ)))

; Updates lambda msgs sent by node

(defun update-lambda-messages (node)
  (dolist (pred (node-predecessors node))
    (update-lambda-msg-to pred node)))

(defun update-lambda-msg-to (parent-node node)
  (ideal-debug-msg "~%LAMBDA: ~A -> ~A" node parent-node)
  (for-all-cond-cases (pred-case parent-node)
    (setf (lambda-msg-of pred-case node)(compute-lambda-msg pred-case node))))

; Computes lambda message

(defun compute-lambda-msg (pred-case node)
  (let ((outer-total 0))
    (for-all-cond-cases (node-case node)
      (let ((inner-total 0))
	(for-all-cond-cases (cond-case-without-pred
			      (remove-node (node-in pred-case)(node-predecessors node)))
	  (incf inner-total
		(* (prob-of node-case (combine-cond-cases cond-case-without-pred pred-case))
		   (pi-product node cond-case-without-pred))))
	(incf outer-total (* (lambda-of node-case) inner-total))))
    (values outer-total)))

; --- Pi processing ---------------

; Updates overall pi

(defun update-overall-pi (node)
  (ideal-debug-msg "~% OVERALL PI: ~A" node)
  (for-all-cond-cases (node-case node)
    (setf (pi-of node-case)(calculate-pi-of node-case))))

(defun calculate-pi-of (node-case)
  (let ((total 0))
    (for-all-cond-cases (cond-case (node-predecessors (node-in node-case)))
      (incf total
	    (* (prob-of node-case cond-case)(pi-product (node-in node-case) cond-case))))
    (values total)))

(defun pi-product (node cond-case)
  (product-over (pred.state cond-case)
    (pi-msg-of node (make-conditioning-case (list pred.state)))))

; Updates pi msgs sent by node

; Actually this normalization of the pi-msg is not required in theory
; but it is better to have it to aviod numerical errors caused by
; underflow (29 Sep).

(defun update-pi-messages (node)
  (dolist (succ (node-successors node))
    (update-pi-msg-to succ node)))

(defun update-pi-msg-to (succ node &key (normalize :YES))
  (when (not (dummy-node-p succ))
    (ideal-debug-msg "~%    PI: ~A -> ~A" node succ)
    (let ((total 0))
      (for-all-cond-cases (node-case node)
	(incf total (setf (pi-msg-of succ node-case)(compute-pi-msg succ node-case))))
      (ecase normalize
	(:YES
	  (for-all-cond-cases (node-case node)
	    (setf (pi-msg-of succ node-case) (/ (pi-msg-of succ node-case) total))))
	(:NO
	  nil)))))

; Computes pi message

(defun compute-pi-msg (succ node-case)
  (* (pi-of node-case)
     (product-over (s-node (remove-node succ (node-successors (node-in node-case))))
       (lambda-msg-of node-case s-node))))

;---- Calculating beliefs --------------------

(defun calculate-and-normalize-beliefs (poly-tree)
  (mapc #'calculate-and-normalize-node-beliefs poly-tree))

(defun calculate-and-normalize-node-beliefs (node)
  (let ((total 0))
    (for-all-cond-cases (node-case node)
      (incf total (setf (belief-of node-case)(* (lambda-of node-case)(pi-of node-case)))))
    (for-all-cond-cases (node-case node)
      (setf (belief-of node-case)(/ (belief-of node-case) total)))))

;; ***** Subsidiary utilities *****

(defun display-beliefs (node &optional (st t))
  (format st "~%***** Node: ~A *******" (node-name node))
  (for-all-cond-cases (node-case (list node))
    (format  st "~% State: ~A ==> Belief:  ~A"
	    (label-name (state-in node-case))(belief-of node-case)))
  (values))

(defun diag-display-beliefs (&optional (diagram *diagram*)(st t))
  (map nil #'(lambda (n)(display-beliefs n st))
       (sort (copy-list diagram) #'< :key #'node-id-number))
  (values))


(defun print-diag-beliefs (diagram file-name)
  (format t "~%; Printing beliefs in file ~A ...." file-name)
  (with-open-file (st file-name :direction :output)
    (diag-display-beliefs diagram st))
  (format t "Done")(values)) 

