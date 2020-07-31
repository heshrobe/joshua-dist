;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: Ideal;  Base: 10 -*-

(in-package :ideal)

;;;;********************************************************
;;;;  Copyright (c) 1989, 1992 Rockwell International -- All rights reserved.
;;;;  Rockwell International Science Center Palo Alto Lab
;;;;********************************************************


;;;;;;;;;;;;;;;;;;;;;;;; Sampath ;;;;;;;;;;;;;;;;;;;;



(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(CREATE-EVIDENCE RESET-EVIDENCE REMOVE-EVIDENCE DUMMY-NODE-P CHECK-EVIDENCE)))

; ----------- Crude input function to create evidence lists

; I know this is messy but again, we are not providing fancy or
; maintainable i-o.  This stuff will be obsolete soon (hopefully) with
; the advent of a good graphical interface.

; This is the version to use on anything but Allegro CL.
#-:CORAL
(defun create-evidence (belief-net)
  (let* ((node-names (mapcar #'node-name belief-net))
	 (evidence-nodes
	   (mapcar #'(lambda (name)
		       (find-node name belief-net))
		   (input-subset-of node-names "Enter list of evidence node names"))))
    (dolist (evidence-node evidence-nodes)
      (let ((label-names (mapcar #'label-name (state-labels evidence-node))))
	(format t "~% ** Creating evidence for ~A" evidence-node)
	(setf (node-state evidence-node)
	      (cond
		((and (not (null (node-old-state evidence-node)))    ; old evidence exists
		      (y-or-n-p "Do u want to retract evidence for node ~A?" evidence-node))
		 nil)
		((y-or-n-p "Do u have specific evidence for node ~A ?" evidence-node)
		 (find
		   (query `(MEMBER ,@label-names)
			  "What is the observed state of the node ~A ?" evidence-node)
		   (state-labels evidence-node) :key #'label-name))
		( t (mapcar #'(lambda (label)
				(cons label
				      (query '(AND NUMBER (NOT (SATISFIES MINUSP)))
					     "What is the relative probability of ~@
                                                    [observation/ ~A = ~A ?"
					     evidence-node label)))
			    (state-labels evidence-node)))))))
    (values belief-net)))


; Goddam Allegro cant handle calls to y-or-n-p following a call to read.
; Therefore the yes-or-no-p. It can handle type specs like '(number 0
; *). Therefore the check loop.  Because of these changes a new version.

#+:CORAL
(defun create-evidence (belief-net)
  (let* ((node-names (mapcar #'node-name belief-net))
	 (evidence-nodes
	   (mapcar #'(lambda (name)
		       (find-node name belief-net))
		   (input-subset-of node-names "Enter list of evidence node names"))))
    (dolist (evidence-node evidence-nodes)
      (let ((label-names (mapcar #'label-name (state-labels evidence-node))))
	(format t "~% ** Creating evidence for ~A" evidence-node)
	(setf (node-state evidence-node)
	      (cond
		((and (not (null (node-old-state evidence-node)))	; old evidence exists
		      (yes-or-no-p "Do u want to retract evidence for node ~A?" evidence-node))
		 nil)
		((yes-or-no-p "Do u have specific evidence for node ~A ?" evidence-node)
		 (find
		   (query `(MEMBER ,@label-names)
			  "What is the observed state of the node ~A ?" evidence-node)
		   (state-labels evidence-node) :key #'label-name))
		( t (mapcar #'(lambda (label)
				(let (prob)
				  (loop
				    (setq prob (query '(AND NUMBER (NOT (SATISFIES MINUSP)))
						      "What is the relative probability of ~@
                                                    [observation/ ~A = ~A ?"
						      evidence-node label))
				    (if (not (< prob 0)) (return (cons label prob)))
				    (ideal-warning
				      "~%Your input should be a non-negative number"))))
			    (state-labels evidence-node)))))))
    (values belief-net)))

(defun input-subset-of (list &rest format-args)
  (let (names)
    (remove-duplicates
      (loop
	(setq names (apply #'query (cons 'LIST format-args)))
	(cond
	  ((subsetp names list) (return names))
	  ( t (ideal-warning "The input ~S is not a subset of list ~S :  Try again"
			names list)))))))


;--------- Evidence manipulation --------------------------------------------
(defun create-evidence-list (diagram)
	; Check the evidence for validity.
  (check-evidence diagram)
  (let* ((evidence-list (mapcan #'create-node-evidence-list diagram))
	 (evidence-nodes (mapcar #'car evidence-list)))
    (values evidence-list evidence-nodes)))

; Same as create-evidence-list except that there are no input checks
; AND there is no check whether evidence is unchanged. Used in conditioning.

(defun make-evidence-list (node-list)
  (mapcar #'(lambda (n)(cons n (node-state n))) node-list))

(defun create-node-evidence-list (node)
  (when (not (unchanged-evidence-p node))
    (cons-up-node-evidence-list node)))

(defun unchanged-evidence-p (node)
  (let ((new-evidence (node-state node))(old-evidence (node-old-state node)))
	; returns t if old-evidence is  same as new-evidence
    (cond
      ((null new-evidence)(null old-evidence))
      ((label-p new-evidence)(eq old-evidence new-evidence))
      ; Fixed to handle virtual evidence correctly (Feb 8 '91)
      ((listp new-evidence)(unchanged-virtual-evidence-p old-evidence new-evidence))
      (t (error "The content of node-state of node ~A is ~A. This is not valid evidence."
                node new-evidence)))))


; This function is called if new-evidence is virtual evidence.  It
; compares the new virtual evidence with the old evidence and returns t
; if the old evidence is 1) virtual evidence 2) The weight for every
; state normalized to 1 differs by less than the <tolerance> between old
; and new evidence.

(defun unchanged-virtual-evidence-p (old-elist new-elist &optional (tolerance 0.00001))
  (labels ((find-weight-total (ev-list)(sum-over (s.w ev-list)(cdr s.w)))
	   (same-virtual-evidence (new-el)
	     (let ((old-total (find-weight-total old-elist))
		   (new-total (find-weight-total new-el))
		   new-s.w)
	       (dolist (old-s.w old-elist)
		 (setq new-s.w (find (car old-s.w) new-el :key #'car))
		 (if (or (null new-s.w)
			 (> (abs (- (/ (cdr new-s.w) new-total)
				    (/ (cdr old-s.w) old-total))) tolerance))
		     (return nil))
		 (setq new-el (delete new-s.w new-el)))
	       (values (null new-el)))))
	; Old evidence *was* virtual and also was same as new evidence
    (and (listp old-elist)
	 (same-virtual-evidence (copy-list new-elist)))))


(defun cons-up-node-evidence-list (node)(list (cons node (node-state node))))

(defun set-evidence (evidence-list)
  (check-evidence-list evidence-list)
  (mapc #'(lambda (n.e)
	    (setf (node-state (car n.e)) (cdr n.e))) evidence-list))

(defun reset-evidence (node-list)
  (mapc #'reset-node-evidence node-list))

(defun reset-node-evidence (node)(setf (node-old-state node) nil))

(defun remove-evidence (node-list)
  (mapc #'remove-node-evidence node-list))

(defun remove-node-evidence (node)
  (setf (node-state node) nil ))

; Gets only specific evidence nodes

(defun get-evidence-nodes (belnet)
  (copy-list (remove-if-not #'specific-evidence-node-p belnet)))

(defun specific-evidence-node-p (n)(member (node-state n)(state-labels n)))

;--- Checking validity of evidence -------------

(defun check-evidence (diagram)
  (every #'(lambda (node)(check-evidence-item node (node-state node))) diagram))

(defun check-evidence-list (evidence-list)
  (every #'(lambda (n.e)(check-evidence-item (car n.e)(cdr n.e))) evidence-list))

(defun check-evidence-item (node evidence)
  (or (null evidence)
      (member evidence (state-labels node))
      (check-virtual-evidence evidence node)
      (error "The evidence  ~A for node ~A is not consistent evidence" evidence node)))

(defun check-virtual-evidence (state-contents node)
  (when (listp state-contents)
    (when  (every #'(lambda (state.numb)
		      (cond
			((not (consp state.numb))
			 (error "The entry ~A in the state field of node ~A is not a
                                    ~@ cons. This is not a valid evidence item"
				state.numb node))
			((not (label-p (car state.numb)))
			 (error "The entry ~A in the evidence ~A of node ~A is not a~
                                      state label" (car state.numb) state-contents node))
			((not (and (numberp (cdr state.numb))(>=  (cdr state.numb) 0)))
			 (error "The entry ~A in the evidence ~A of node ~A is not a ~
                                      number > 0" (cdr state.numb) state-contents node))
			( t t)))
		  state-contents)
      (check-all-states-present state-contents node))))

(defun check-all-states-present (state-contents node)
  (let ((state-labels (state-labels node))
	(evidence-state-labels (mapcar #'car state-contents)))
    (cond
      ((not (subsetp state-labels evidence-state-labels))
       (error "The evidence ~A does not contain all states of node ~A" state-contents node))
      ((not (equal (length state-labels)(length evidence-state-labels)))
       (error "The evidence ~A either contains repeats of states or elements ~@
               that are not states of node ~A" state-contents node))
      ( t t ))))



;-------------------------- Dummy Nodes --------------------------

; DUMMY NODES: Evidence is set up as a dummy node that is a child of the
; affected node. Including dummy nodes in the implementation has the
; following effects:

; 1) When initializing the diagram the dummy nodes have to be deleted.
; Dummy nodes may exist in a diagram that is being initialized if it is
; a digram that is being reused.

; 2) In checking singly-connected-p the check has to be made on a an
; "extended" diagram consisting both of the original nodes and the dummy
; nodes. This check is made just as u start poly-tree-infer. Dummy nodes
; may exist in a diagram at this stage if it is being used incrementally
; (i.e poly-tree-infer is run repeatedly on the diagram. with new
; evidence each time).

;3) When creating a dummy node for a piece of evidence relating to a
;node any dummy nodes  that are already attached to the nodes (i.e old
;evidence pertaining to the node) has  to be deleted.
; The way that dummy evidence nodes have been implemented now,
;create-evidence-list creates evidence-items for those nodes whose
;evidence has changed since the last pass of poly-tree-infer over the
;diagram. So effectively, poly-tree-infer is smart and propogates only
;evidence that has changed and does not repropogate evidence that is
;same but has been redeclared. To force evidence that is same to
;repropogate you have to ensure that node-old-state of the evidence node
;is different from node-state of the evidence node. One way to do this
;is to simply set node-old-state to nil after the evidence has been
;propogated.

;4)  When used by the clique algorithm the dummy node is created just to be
; a record of the evidence pertaining to the node that has been
; propogated so far. Why such a roundabout thing ? Well, then the
; functions create-evidence, create-evidence-list, check-evidence etc
; created for poly-trees can be used exactly as is for handling the
; evidence for the clique algorithms too. Also, the evidence details for
; both poly-tree and clique-algorithms remains exactly the same and this
; is good.


(defun set-up-dummy-inf-node-for-evidence (evidence)
  (let* ((affected-node (node-activated-by-evidence evidence))
	 (dummy-inf-node
	   (or (find-if #'dummy-node-p (node-successors affected-node))
	       (create-new-dummy-node-for-node affected-node))))
    (cond
      ((retracted-evidence-p evidence)	; retracted evidence
       (for-all-cond-cases (node-case (list affected-node))
	 (setf (lambda-msg-of node-case dummy-inf-node) 1)))
      ((label-p (cdr evidence))	; specific evidence
       (for-all-cond-cases (node-case (list affected-node))
	 (setf (lambda-msg-of node-case dummy-inf-node)
	       (if (eq (state-in node-case) (cdr evidence)) 1 0))))
      (t	; virtual evidence
       (for-all-cond-cases (node-case (list affected-node))
	 (setf (lambda-msg-of node-case dummy-inf-node)
	       (cdr (assoc (state-in node-case)(cdr evidence)))))))
    (values dummy-inf-node)))


(defun create-new-dummy-node-for-node (affected-node)
  (let ((dummy-inf-node (create-empty-node
			  :distribution-type (distribution-type affected-node))))
    (setf (node-name dummy-inf-node)
	  (gentemp (string-upcase (format nil "DUMMY-FOR-~A"
					  (symbol-name (node-name affected-node)))))
	  (node-type dummy-inf-node) :DUMMY-INF-NODE)
    (add-link dummy-inf-node affected-node)
    (setf (node-lambda-msg dummy-inf-node)
	  (list (cons affected-node (make-vanilla-msg-array affected-node))))
    (values dummy-inf-node)))


(defun node-activated-by-evidence (evidence)(car evidence))

(defun retracted-evidence-p (evidence)(null (cdr evidence)))

;--- Dummy node utils--------

(defun unlink-dummy-nodes-attached-to (node)
	; Note that dummy nodes can only be successors of an ordinary node
  (dolist (succ (node-successors node))
    (when (dummy-node-p succ)(delete-link succ node))))

(defun unlink-all-dummy-nodes (poly-tree)
  (mapc #'unlink-dummy-nodes-attached-to poly-tree)
  (values poly-tree))

; Predicate for checking dumminess of a node

(defun dummy-node-p (inf-node)
  (eq (node-type inf-node) :DUMMY-INF-NODE))

; -- Misc utils ---

(defun neighbours (node)
  (nconc (delete-if #'dummy-node-p (copy-list (node-successors node)))
	 (copy-list (node-predecessors node))))
