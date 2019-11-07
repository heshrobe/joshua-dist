;;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: Ideal;  Base: 10 -*-


(in-package :ideal)


;;;;********************************************************
;;;;  Copyright (c) 1989, 1992 Rockwell International -- All rights reserved.
;;;;  Rockwell International Science Center Palo Alto Lab
;;;;********************************************************

;;;;;;;;;;;;;;;;;;;;;;;;; Sampath ;;;;;;;;;;;;;;;;;;;;



; The fields that are not exported are useful only to a person hacking
; IDEAL itself.

(export '(node node-name node-print-name node-state node-unused-slot
	       label label-print-name label-node))



;------------------------------- nodes ---------------------------------------------------

; Node structure

(defstruct (node (:print-function print-node))
  name
  print-name
  id-number
  type-*					; chance, decision or value.
  predecessors-*
  successors-*
  distribution
  state
  bel
  pi-msg
  lambda-msg
  activating-node
  old-state
  cutset-node-p  
  actual-bel
  changed-p
  initialization
  sim-node ; For Mark's stuff. Was using unused slot befor (17 Oct 90)
  unused-slot)

; Storing node info on the property list of node

(store-ideal-struct-info (node (:print-function print-node))
  name
  print-name
  id-number
  type-*					; chance, decision or value.
  predecessors-*
  successors-*
  distribution
  state
  bel
  pi-msg
  lambda-msg
  activating-node
  old-state
  cutset-node-p  
  actual-bel
  changed-p
  initialization
  sim-node ; For Mark's stuff. Was using unused slot before (17 Oct 90)
  unused-slot)

; Defining the special print function for node

(defidealprintfn node (print-node (struct st)
				  (format st "#<Node ~S>" (node-name struct))))

; -------------------------Distributions ---------------------------------------------

(defstruct (discrete-dist (:print-function print-discrete-dist))
  relation-type-*				; probabilistic or deterministic
  state-labels-*				; list of labels 
  number-of-states-*
  array
  markov-blanket
  transition-array
  noisy-or-p
  noisy-or-subtype
  noisy-or-info)
;
;  noisy-or-p
;  noisy-or-subtype ; :binary, :nary or :generic
;  noisy-or-inhibitor-probs
;  noisy-or-det-fn-array)


;Storing field info on plist of discrete-dist


(store-ideal-struct-info (discrete-dist (:print-function print-discrete-dist))
  relation-type-*				; probabilistic or deterministic
  state-labels-*				; list of labels 
  number-of-states-*
  array
  markov-blanket
  transition-array
  noisy-or-p
  noisy-or-subtype
  noisy-or-info)

; Defining special print function for discrete-dist

(defidealprintfn discrete-dist (print-discrete-dist (struct st)
					   (format st "#<discrete-dist ~A>"
						   (discrete-dist-relation-type-* struct))))


;-------------------------------------------  Labels -----------------------------------
(defstruct (label (:print-function print-label))
  name
  print-name
  id-number
  node)

; Storing field info on the plist of label

(store-ideal-struct-info  (label (:print-function print-label))
  name
  print-name
  id-number
  node)

; defining special print function for label

(defidealprintfn label (print-label (struct st)
				    (format st "#<state ~S ~S>" (label-name struct)
					    (if (node-p (label-node struct))
						(node-name (label-node struct))
						(label-node struct)))))
;------------------------- Extra info for noisy or nodes ---------------------------

(defstruct (noisy-or (:print-function print-noisy-or))
  inhibitor-probs
  det-fn-array
  owner-node)

(store-ideal-struct-info (noisy-or (:print-function print-noisy-or))
  inhibitor-probs
  det-fn-array
  owner-node)

(defidealprintfn noisy-or (print-noisy-or (struct st)
					  (format st "#<Noisy Or Info for Node ~A>"
						  (if (noisy-or-owner-node struct)
						      (node-name
							(noisy-or-owner-node struct))))))

;----------------------------------------------------------------------------------------

; This macro is required when setting up the definition in
; wrapped-access-functions.lisp.  It is used later to keep track of any
; operation which changes the basic info in a node.

(defmacro while-setting-change-flag-of ((node) &body body)
  `(unwind-protect (progn ,@body)(setf (node-changed-p ,node) t)))


