;;; -*- Mode: Lisp; Package: JOSHUA-INTERNALS; Syntax: Ansi-common-lisp -*-
;;;>
;;;> *****************************************************************************************
;;;> ** (c) Copyright 1989, 1988 Symbolics, Inc.  All rights reserved.
;;;> ** Portions of font library Copyright (c) 1984 Bitstream, Inc.  All Rights Reserved.
;;;>
;;;>    The software, data, and information contained herein are proprietary 
;;;> to, and comprise valuable trade secrets of, Symbolics, Inc., which intends 
;;;> to keep such software, data, and information confidential and to preserve 
;;;> them as trade secrets.  They are given in confidence by Symbolics pursuant 
;;;> to a written license agreement, and may be used, copied, transmitted, and 
;;;> stored only in accordance with the terms of such license.
;;;> 
;;;> Symbolics, Symbolics 3600, Symbolics 3670 (R), Symbolics 3675 (R), Symbolics 3630,
;;;> Symbolics 3640, Symbolics 3645 (R), Symbolics 3650 (R), Symbolics 3653, Symbolics
;;;> 3620 (R), Symbolics 3610 (R), Symbolics XL400, Symbolics Common Lisp (R),
;;;> Symbolics-Lisp (R), Zetalisp (R), Genera (R), Wheels (R), Dynamic Windows (R), Showcase,
;;;> SmartStore (R), Semanticue (R), Frame-Up (R), Firewall (R), MACSYMA (R), COMMON LISP
;;;> MACSYMA (R), CL-MACSYMA (R), LISP MACHINE MACSYMA (R), MACSYMA Newsletter (R), Document
;;;> Examiner (R), S-DYNAMICS (R), S-GEOMETRY (R), S-PAINT (R), S-RENDER (R), "Your Next
;;;> Step in Computing" (R), Ivory, MacIvory, Symbolics C, Symbolics Pascal, Symbolics Prolog,
;;;> Symbolics Fortran, CLOE, Joshua, Concordia, and Statice are trademarks of Symbolics, Inc.
;;;> 
;;;> RESTRICTED RIGHTS LEGEND
;;;>    Use, duplication, and disclosure by the Government are subject to restrictions 
;;;> as set forth in subdivision (c)(1)(ii) of the Rights in Trademark Data and Computer 
;;;> Software Clause at FAR 52.227-7013.
;;;> 
;;;>      Symbolics, Inc.
;;;>      8 New England Executive Park, East
;;;>      Burlington, Massachusetts  01803
;;;>      United States of America
;;;>      617-221-1000
;;;> *****************************************************************************************
;;;>
;;; Created 5/09/88 14:28:11 by HES running on MERLIN at SCRC.

;;;; Definitions used in compiling the rete network

(in-package :ji)
;; I have no idea why this is here
;; (eval-when (compile) (proclaim '(optimize (safety 0))))

;;; A Rete network is a left-linear tree.  The leaf nodes are match nodes
;;; and the internal nodes are merger nodes.  Nodes may be shared
;;; when variants of the same pattern are used in two different rules.
;;; There can also be procedure nodes that take a single input applying
;;; a Lisp procedure to the environment so produced.  If this environment
;;; passes the test it is passed on to the descendant of the procedure-node.
;;; The procedure node can also repeatedly succeed.
;;;
;;;  left-node  right-node right-node right-node
;;;       \     /          /          /		
;;;     left-node         /         /
;;;           \          /        /
;;;           procedure /       /
;;;             \      /      /
;;;           left-node     / 
;;;                   \   /   
;;;                   terminal-node


(defstruct (pattern-analysis)
  (variables-referenced nil)
  (name nil)
  (links nil)
  ;; an alist from variable to position
  ;; could be nil because no variables used in this
  ;; analysis get to the end
  (map nil)
  ;; mark to signify that the map has been generated
  ;; even if it's nil
  (map-generated nil)
  (rete-node nil)
  (non-local-parents nil)
  (variables-used nil)
  (pure-semi-unification? nil))

(defstruct (merge-pattern-analysis (:include pattern-analysis))
  (merge-id nil)
  (merge-code nil)
  (semi-merge-code nil))

(defstruct (match-pattern-analysis (:include pattern-analysis))
  (pattern nil)
  (truth-value nil)
  (full-unification-code nil)
  (semi-unification-code nil))

(defstruct (procedure-pattern-analysis (:include pattern-analysis))
  (expression nil)
  (function nil)
  (new-variables nil)
  (variables-referenced-previously nil)
  (variables-promised-to-be-bound nil)
  (original-form nil)
  )

(defstruct (and-group-pattern-analysis (:include pattern-analysis))
  (sub-patterns nil)
  (mergers nil))

(defstruct (or-group-pattern-analysis (:include pattern-analysis))
  (sub-patterns nil)
  (shufflers nil))

(defstruct (or-target-pattern-analysis (:include pattern-analysis))
  (inputs nil)
  (contributors nil))

(defstruct (object-match-pattern-analysis (:include pattern-analysis))
  (pattern nil)
  (truth-value nil)
  (semi-unification-code nil))

;;;; Phase 1 of compiling the rete network
;;; This phase figures out everything about the topology of the network
;;; This phase deals with the syntax of the rule
;;;   We deal with Logic-variables-makers and Predication-makers.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Insertions in below
;;;
;;;  Consider (and x y (or z procedure))
;;;   where x,y,z are matches
;;;   Logically this is (or (and x y procedure) (x y z))
;;;    It might be easier to actually demorganize such trigger patterns
;;; but what code below tries to do this incrementally.
;;; The key thing is that procedure should see the variables of x and y
;;; Z matches independently, so it really doesn't care.
;;; So when you hit the procedure, you make an and node for x & y
;;; insert it in whatever you're processing and then feed that into p.
;;; Giving topology something like.
;;; x ----- 
;;;       | ---- p ---or
;;; y -----           |
;;;                   |
;;; z -----------------
;;; I'm not sure I completely understand this right now (7/2/09) and I'm not
;;; sure that demorganizing to begin with wouldn't have been better

(defun build-rete-topology (trigger &optional variables-in-block environment-stack)
  #-sbcl(declare (values analysis bottom-node insertion))
;;  (let ((*print-level* 2) (*print-length* 5))
;;    (format t "~% ~{~a~^ ~}" (list (car trigger) (cadr trigger))))
  (rete-topologizer (car trigger) 
		    (cdr trigger)
		    variables-in-block
		    environment-stack))

(defmethod rete-topologizer ((type (eql :ignore)) data variables environment-stack)
  (declare (ignore data variables environment-stack))
  (values))

(defmethod rete-topologizer ((type (eql :and)) data variables environment-stack)
  (declare (ignore variables))
  (let ((analysis (make-and-group-pattern-analysis)))
    (loop with all-my-variables and entry and bottom-node and insertion and last-node
	and triggers = data
	for trigger in triggers
	do (if (pattern-analysis-p trigger)
	       (setq entry trigger bottom-node trigger insertion nil)
	     (multiple-value-setq (entry bottom-node insertion)
	       (build-rete-topology trigger all-my-variables
				    (if last-node
					(cons (cons last-node all-my-variables) environment-stack)
				      environment-stack))))
	when insertion
	collect insertion into entries
	and do ;; see above for when insertions happen
	    (setq all-my-variables (union all-my-variables (pattern-analysis-variables-referenced insertion)))
	    (when (procedure-pattern-analysis-p insertion)
	      (setq all-my-variables (union all-my-variables
					    (procedure-pattern-analysis-variables-referenced-previously insertion))))
	    (setq last-node (if last-node
				(tack-node-onto-group last-node last-node insertion all-my-variables)
			      insertion))
	when (not (null entry)) 
	collect entry into entries
	and do (setq all-my-variables (union all-my-variables (pattern-analysis-variables-referenced entry)))
	    ;; (format t "~%Variables: ~a ~a" all-my-variables (type-of entry))
	    (when (procedure-pattern-analysis-p entry)
	      (setq all-my-variables (union all-my-variables
					    (procedure-pattern-analysis-variables-referenced-previously entry))))
	    (setq last-node (tack-node-onto-group entry bottom-node last-node all-my-variables))
	finally (setf (pattern-analysis-variables-referenced analysis) all-my-variables)
		(setf (and-group-pattern-analysis-sub-patterns analysis) entries)
		(setf (pattern-analysis-non-local-parents analysis)
		  (pattern-analysis-non-local-parents last-node))
		(return (values analysis last-node)))
    ))

(defun tack-node-onto-group (entry-to-tack-on node-to-tack-on last-node variables-seen)
  ;; (format t "~%Tacking ~{~a~^ ~}" (list entry-to-tack-on last-node variables-seen))
  (cond ((and (eq entry-to-tack-on node-to-tack-on)
	      last-node
	      (member last-node (pattern-analysis-non-local-parents node-to-tack-on))
	      (procedure-pattern-analysis-p entry-to-tack-on))
	 ;; a single procedure node to tack onto a predecessor
	 ;; which is one of its non-local parents
	 (setf (pattern-analysis-non-local-parents node-to-tack-on)
	       (union (pattern-analysis-non-local-parents node-to-tack-on)
		      (pattern-analysis-non-local-parents last-node)))
	 (push `(procedure ,node-to-tack-on) (pattern-analysis-links last-node))
	 (setq last-node node-to-tack-on))
	((member last-node (pattern-analysis-non-local-parents node-to-tack-on))
	 (setf (pattern-analysis-non-local-parents node-to-tack-on)
	       (delete last-node (pattern-analysis-non-local-parents node-to-tack-on)))
	 (setq last-node node-to-tack-on))
	((procedure-pattern-analysis-p entry-to-tack-on)
	 (cond (last-node
		(setf (pattern-analysis-non-local-parents node-to-tack-on)
		      (union (pattern-analysis-non-local-parents node-to-tack-on)
			     (pattern-analysis-non-local-parents last-node)))
		(push `(procedure ,node-to-tack-on) (pattern-analysis-links last-node))
		(setq last-node node-to-tack-on))
	       (t (setq last-node node-to-tack-on))))
	(t (cond (last-node
		  ;; we need to build a merge
		  (let ((merge (make-merge-pattern-analysis :variables-referenced variables-seen)))
		    (push `(left ,merge ,node-to-tack-on) (pattern-analysis-links last-node))
		    (push `(right ,merge ,last-node) (pattern-analysis-links node-to-tack-on))
		    (setf (pattern-analysis-non-local-parents merge)
			  (union (pattern-analysis-non-local-parents node-to-tack-on)
				 (pattern-analysis-non-local-parents last-node)))
		    (setq last-node merge)))
		 (t (setq last-node node-to-tack-on)))))
  last-node)


(defmethod rete-topologizer ((type (eql :or)) data variables environment-stack)
  (declare (ignore variables))
  (let ((non-local-usage-alist nil)
	(all-entries nil))
    (loop with entry and bottom-node and insertion
	  for sub-trigger in data
	  for dummy = (progn (multiple-value-setq (entry bottom-node insertion)
			       (build-rete-topology sub-trigger nil environment-stack))
			     (when insertion
			       (push `(procedure ,bottom-node) 
				     (pattern-analysis-links insertion))))
	  for his-key = (pattern-analysis-non-local-parents entry)
	  for his-group = (assoc his-key non-local-usage-alist :test #'(lambda (a b) (null (set-exclusive-or a b))))
	  do (progn dummy)			;meaning ignore
	  when (null his-group)
	    do (setq his-group (list his-key nil nil))
	       (push his-group non-local-usage-alist)
	  do (push entry (second his-group))
	     (push entry all-entries)
	     (push bottom-node (third his-group)))
    ;; first separate into groups that share non-local-parents
    (let ((all-non-local-parents nil)
	  (targets nil))
      (loop for (key entries bottom-nodes) in non-local-usage-alist
	    do (setq all-non-local-parents (union all-non-local-parents key))
	       (if (cdr bottom-nodes)
		   (let ((target (make-or-target-pattern-analysis :non-local-parents key)))
		     (loop for bottom-node in bottom-nodes
			   doing (push (list 'or-shuffler target nil) (pattern-analysis-links bottom-node))
				 (push bottom-node (or-target-pattern-analysis-contributors target)))
		     (loop with all-my-variables = nil
			   for entry in entries
			   do (setq all-my-variables
				(union all-my-variables (pattern-analysis-variables-referenced entry)))
			 finally (setf (pattern-analysis-variables-referenced target) all-my-variables))
		     ;;(let ((*print-level* 2))
		     ;;  (format t "~%Target ~a Vars ~a" target (pattern-analysis-variables-referenced target)))
		     (push target targets))
		 (push (car bottom-nodes) targets)))
      ;; At this point we have groups which have been merged with different sets of guys outside the scope
      ;; Now we're going to get them all to the same merge level
      ;; and then build a big or out of them
      (let ((analysis (make-or-group-pattern-analysis :sub-patterns all-entries))
	    (real-targets (loop for target in targets
				for unaccounted-for-parent = (set-difference all-non-local-parents
									     (pattern-analysis-non-local-parents target))
				collect (if unaccounted-for-parent
					    (multiple-value-bind (junk bottom-node)
						(build-rete-topology `(:and ,@unaccounted-for-parent ,target) nil nil)
                                              (declare (ignore junk))
					      bottom-node)
					  target))))
	(cond
	 ((null (cdr real-targets))
	  ;; (break "AM I here?")
	  (setf (pattern-analysis-variables-referenced analysis)
	    (pattern-analysis-variables-referenced (car real-targets)))
	  (values analysis (car real-targets)))
	 (t
	    (let ((final-target (make-or-target-pattern-analysis :non-local-parents all-non-local-parents)))
	      (loop with all-my-variables = nil
		    for bottom-node in real-targets
		    doing (push (list 'or-shuffler final-target nil) (pattern-analysis-links bottom-node))
			  (push bottom-node (or-target-pattern-analysis-contributors final-target))
			  (setq all-my-variables
			      (union all-my-variables (pattern-analysis-variables-referenced bottom-node)))
		     finally (setf (pattern-analysis-variables-referenced final-target) all-my-variables))
	      (values analysis final-target))))))))

(defmethod rete-topologizer ((type (eql :match)) data variables environment-stack)
  (declare (ignore variables environment-stack))
  (destructuring-bind (pattern name truth-value) data
    (let ((analysis (make-match-pattern-analysis
		      :pattern pattern
		      :name name
		      :truth-value truth-value
		      :variables-referenced (cons name (logic-variable-makers-in-thing (first data))))))
      (values analysis analysis))))

(defmethod rete-topologizer ((type (eql :procedure)) data variables-in-block environment-stack)
  (let* ((code (car data))
	 (support-variable (second data))
	 (bound-variables (logic-variable-makers-in-thing (third data)))
	 (original-expression (fourth data))
	 (all-my-variables (logic-variable-makers-in-thing code))
	 (analysis (make-procedure-pattern-analysis :variables-referenced (if support-variable
									      (cons support-variable all-my-variables)
									    all-my-variables)
						    :name (second data)
						    :expression code
						    :variables-promised-to-be-bound bound-variables
						    :original-form original-expression)))
    ;; Maintain a record of what variables are accessible to this guy
    ;; from his context.  Start with those in his block
    (setf (procedure-pattern-analysis-variables-referenced-previously analysis) variables-in-block)
    (let ((variables-not-in-block (loop for var in all-my-variables
					unless (find var variables-in-block)
				      collect var)))
      (loop with used-parents = nil
	    and new-variables = nil
	    for var in variables-not-in-block
	    doing (loop for (his-analysis . his-variables) in environment-stack
			when (find var his-variables)
			  do (pushnew his-analysis used-parents)
			     (pushnew var (procedure-pattern-analysis-variables-referenced-previously analysis))
			     (return (values))
			finally (push var new-variables))
	    finally (when support-variable (push support-variable new-variables))
		    (setf (procedure-pattern-analysis-new-variables analysis) new-variables)
		    (setf (pattern-analysis-non-local-parents analysis) used-parents)
		    (let ((insertion nil))
		      (if (cdr used-parents)
			  (multiple-value-bind (his-analysis bottom his-insertion)
			      (build-rete-topology `(and ,@used-parents))
			    (declare (ignore his-analysis his-insertion))
			    (setq insertion bottom))
			(setq insertion (car used-parents)))
		      (return (values analysis analysis insertion)))))))

(defmethod rete-topologizer ((type (eql :object-match)) data variables environment-stack)
  (declare (ignore variables environment-stack))
  (destructuring-bind (pattern name truth-value) data
    (let ((analysis (make-object-match-pattern-analysis
		      :pattern pattern
		      :name name
		      :truth-value truth-value
		      :variables-referenced (cons name (logic-variable-makers-in-thing (first data))))))
      (values analysis analysis))))



(defun analyze-variable-consumption (pattern-analysis body-variables)
  (let ((visited-analyses nil))
    (labels ((do-one-analysis (analysis)
	       (unless (member analysis visited-analyses)
		  (push analysis visited-analyses)
		  (let ((all-variables-below nil))
		    (if (pattern-analysis-links analysis)
			(loop for link in (pattern-analysis-links analysis)
			  for child = (second link)
			  for his-vars = (do-one-analysis child)
			  do (setq all-variables-below
				   (union all-variables-below his-vars)))
			(setq all-variables-below body-variables))		    
		    (when (and-group-pattern-analysis-p analysis)
		      (loop for pattern in (and-group-pattern-analysis-sub-patterns analysis)
			    for his-vars = (do-one-analysis pattern)
			    do (setq all-variables-below
				     (union all-variables-below his-vars))))
		    (when (or-group-pattern-analysis-p analysis)
		      (loop for pattern in (or-group-pattern-analysis-sub-patterns analysis)
			    for his-vars = (do-one-analysis pattern)
			    do (setq all-variables-below
				     (union all-variables-below his-vars))))
		    (when (procedure-pattern-analysis-p analysis)
		      ;; check what variables I merge with a brother
		      ;; and treat them as used variables.
		      (loop with my-variables = (pattern-analysis-variables-referenced analysis)
			  with previous-variables = (procedure-pattern-analysis-variables-referenced-previously analysis)
			    for link in (pattern-analysis-links analysis)
			    when (member (car link) '(left right))
			      do (let ((brother (third link)))
				   (loop for variable in (pattern-analysis-variables-referenced brother)
				       when (or (member variable my-variables)
						;; if two procedures are contiguous the variables introduced
						;; by the first will get lost without this
						(member variable previous-variables))
					   do (pushnew variable all-variables-below)))))
		    (setf (pattern-analysis-variables-used analysis) all-variables-below)))
	       (let ((answer (if (or (merge-pattern-analysis-p analysis)
				     (or-target-pattern-analysis-p analysis))
				 (pattern-analysis-variables-used analysis)
				 (union (pattern-analysis-variables-used analysis)
					(pattern-analysis-variables-referenced analysis)))))
		 answer)))
      (do-one-analysis pattern-analysis))))



(defun analyze-semi-ability (pattern-analysis force)
  (let ((visited-analyses nil))
    (labels ((do-one-analysis (analysis)
	       (unless (member analysis visited-analyses)
		 (push analysis visited-analyses)
		 (cond ((match-pattern-analysis-p analysis)
			(when (or force
				  (data-is-guaranteed-variable-free
				    (match-pattern-analysis-pattern analysis)))
			  (setf (pattern-analysis-pure-semi-unification? analysis) t)
			  (chase-children analysis)))
		       ((object-match-pattern-analysis-p analysis)
			(when (or force
				  (data-is-guaranteed-variable-free
				   (object-match-pattern-analysis-pattern analysis)))
			  (setf (pattern-analysis-pure-semi-unification? analysis) t)
			  (chase-children analysis)))
		       ((procedure-pattern-analysis-p analysis)
			;; If nobody referenced any of my new variables
			;; or if the only referenced ones are promised to be bound
			;; Then I'm maintaing the semi-unifiability property
			(when (or force
				  (null (intersection
					  (set-difference
					    (procedure-pattern-analysis-new-variables analysis)
					    (procedure-pattern-analysis-variables-promised-to-be-bound analysis))
					  (pattern-analysis-variables-used analysis))))
			  (setf (pattern-analysis-pure-semi-unification? analysis) t)
			  (chase-children analysis)))
		       ((and-group-pattern-analysis-p analysis)
			(loop for pattern in (and-group-pattern-analysis-sub-patterns analysis)
			      unless (procedure-pattern-analysis-p pattern)
				do (do-one-analysis pattern)))
		       ((or-target-pattern-analysis-p analysis)
			(setf (pattern-analysis-pure-semi-unification? analysis) t)
			(chase-children analysis))
		       ((or-group-pattern-analysis-p analysis)
			(loop for pattern in (or-group-pattern-analysis-sub-patterns analysis)
			      unless (procedure-pattern-analysis-p pattern)
				do (do-one-analysis pattern)))
		       ((merge-pattern-analysis-p analysis)
			(setf (pattern-analysis-pure-semi-unification? analysis) t)
			(chase-children analysis)))))
	     (chase-children (analysis)
	       (loop for link in (pattern-analysis-links analysis)
		     for type-of-link = (first link)
		     for child = (second link)
		     do (cond ((eql type-of-link 'procedure)
			       (do-one-analysis child))
			      ((eql type-of-link 'or-shuffler)
			       (let ((contributors (or-target-pattern-analysis-contributors child)))
				 (when (loop for contributor in contributors
					     always (pattern-analysis-pure-semi-unification? contributor))
				   ;; All contributors are semi-unification case
				   (flet ((used-variables-of-analysis (analysis)
					    (intersection
					      (pattern-analysis-variables-referenced analysis)
					      (pattern-analysis-variables-used analysis))))
				     (when
				       ;; They all have the same used variables
				       (loop with variable-set = (used-variables-of-analysis (first contributors))
					     for contributor in (rest contributors)
					     always (null (set-exclusive-or
							    variable-set
							    (used-variables-of-analysis contributor))))
				       (do-one-analysis child))))))
			      ((member type-of-link '(left right))
			       (let ((other-guy-in-merge (third link)))
				 (when (pattern-analysis-pure-semi-unification? other-guy-in-merge)
				   (do-one-analysis child))))))))
      (do-one-analysis pattern-analysis))))




;;;; Phase 2 of the rete network compiler

;;; This phase compiles the match, merge, and Lisp procedures that get
;;; stuck in the network

(defgeneric rete-code-writer (type rule-name trigger-analysis environment previous-map))

(defun write-rete-procedures (rule-name trigger-analysis environment &optional previous-map)
  (let ((type-of-trigger (type-of trigger-analysis)))
    (unless (eq type-of-trigger 'merge-pattern-analysis)
      (rete-code-writer (type-of trigger-analysis) rule-name trigger-analysis environment previous-map)))
  (loop with map = (pattern-analysis-map trigger-analysis)
      for link in (pattern-analysis-links trigger-analysis)
      do (cond ((and (eq (car link) 'right)
		     ;; child not yet generated
		     (not (pattern-analysis-map-generated (second link)))
		     ;; partner generated
		     (pattern-analysis-map-generated (third link)))
		(let* ((left (third link))
		       (left-map (pattern-analysis-map left))
		       (merge-structure (second link)))
		  (multiple-value-bind (full semi merge-map merge-id)
		      (write-environment-mergers rule-name left-map map
						 ;; The semi-unifiability of the destination merge node
						 ;; used to be of the trigger from which we're tracing
						 ;; from which is wrong.
						 (pattern-analysis-pure-semi-unification? merge-structure))
		    (setf (merge-pattern-analysis-merge-id merge-structure) merge-id
			  (merge-pattern-analysis-merge-code merge-structure) full
			  (merge-pattern-analysis-semi-merge-code merge-structure) semi
			  (merge-pattern-analysis-map merge-structure) merge-map
			  (merge-pattern-analysis-map-generated merge-structure) t
			  )
		    (write-rete-procedures rule-name merge-structure environment))))
	       ((and (eq (car link) 'left)
		     ;; child not yet generated
		     (not (pattern-analysis-map-generated (second link)))
		     ;; partner generated
		     (pattern-analysis-map-generated (third link)))
		(let* ((right (third link))
		       (right-map (pattern-analysis-map right))
		       (merge-structure (second link)))
		  (multiple-value-bind (full semi merge-map merge-id)
		      (write-environment-mergers rule-name map right-map
						 ;; The semi-unifiability of the destination merge node
						 ;; used to be of the trigger from which we're tracing
						 ;; from, which is wrong.
						 (pattern-analysis-pure-semi-unification? merge-structure))
		    (setf (merge-pattern-analysis-merge-id merge-structure) merge-id
			  (merge-pattern-analysis-merge-code merge-structure) full
			  (merge-pattern-analysis-semi-merge-code merge-structure) semi
			  (merge-pattern-analysis-map merge-structure) merge-map
			  (merge-pattern-analysis-map-generated merge-structure) t)
		    (write-rete-procedures rule-name merge-structure environment))))
	       ((eq (car link) 'procedure)
		(write-rete-procedures rule-name (second link) environment map))
	       ((eq (car link) 'or-shuffler)
		(when (null (third link))
		  (let ((or-target (second link)))
		    ;; make sure his map is set
		    (write-rete-procedures rule-name or-target environment)
		    (let ((function-name (gentemp (format nil "~s-RULE-OR-TRIGGER-SHUFFLER-" rule-name))))
		      (write-forward-or-trigger-shuffler function-name map or-target)
		      (setf (third link) function-name)))))))
  trigger-analysis)

;;; These routines are dispatched to by write-rete-procedures
;;; to handle the different kinds of triggers.

;;; Write the matcher and semi matcher code for a match node

(defmethod rete-code-writer ((type (eql 'match-pattern-analysis)) rule-name analysis environment previous-map)
  (declare (ignore previous-map environment))
  (multiple-value-bind (full semi map)
      (write-forward-rule-matchers rule-name (match-pattern-analysis-pattern analysis)
				   nil
				   (pattern-analysis-name analysis))
    (setf (match-pattern-analysis-full-unification-code analysis) full
	  (match-pattern-analysis-semi-unification-code analysis) semi
	  (match-pattern-analysis-map-generated analysis) t
	  (match-pattern-analysis-map analysis) map)))

(defmethod rete-code-writer ((type (eql 'and-group-pattern-analysis)) rule-name analysis environment previous-map)
  (declare (ignore previous-map))
  (loop for trigger-analysis in (and-group-pattern-analysis-sub-patterns analysis)
	unless (pattern-analysis-map-generated trigger-analysis)
	  do (write-rete-procedures rule-name trigger-analysis environment)))

(defmethod rete-code-writer ((type (eql 'or-group-pattern-analysis)) rule-name analysis environment previous-map)
  (declare (ignore previous-map))
  (loop for sub-trigger in (or-group-pattern-analysis-sub-patterns analysis)
	unless (pattern-analysis-map-generated sub-trigger)
	  do (write-rete-procedures rule-name sub-trigger environment)))

(defmethod rete-code-writer ((type (eql 'or-target-pattern-analysis)) rule-name analysis environment previous-map)
  (declare (ignore rule-name previous-map environment))
  (when (null (pattern-analysis-map-generated analysis))
    (setf (pattern-analysis-map analysis)
	  (build-map (pattern-analysis-variables-referenced analysis)
		     (pattern-analysis-name analysis)
		     )
	  (pattern-analysis-map-generated analysis) t)))

;;; Write the trigger code for a procedure node
(defmethod rete-code-writer ((type (eql 'procedure-pattern-analysis)) rule-name analysis environment previous-map)
  (let ((procedure-function-name (gentemp (format nil "~s-RULE-PROCEDURAL-TRIGGER-" rule-name))))
    (let ((post-procedure-map (write-forward-procedure-code procedure-function-name previous-map environment analysis)))
      (setf (pattern-analysis-map analysis) post-procedure-map
	    (pattern-analysis-map-generated analysis) t)
      (setf (procedure-pattern-analysis-function analysis) procedure-function-name))))

(defmethod rete-code-writer ((type (eql 'object-match-pattern-analysis)) rule-name analysis environment previous-map)
  (declare (ignore previous-map environment))
  (multiple-value-bind (stuff semi map)
      (write-forward-rule-matchers rule-name (object-match-pattern-analysis-pattern analysis)
				   nil
				   (pattern-analysis-name analysis))
    (declare (ignore stuff))
    (setf (object-match-pattern-analysis-semi-unification-code analysis) semi
	  (pattern-analysis-map analysis) map 
	  (pattern-analysis-map-generated analysis) t)))



;;;; Phase 3 of rete compiler

;;; This phase walks the structures built in Phases 1 and 2
;;; producing the final trimmed down data structures which are the output of the compiler


;;; These are the data structures used in the rete network
;;; They come here because the code below builds the final rete network
;;; which uses these structures.

(defstruct (basic-rete-node)
  (children nil)
  (environments nil)
  (merges-underneath nil)
  (suspended-states nil))

;;; an abstract node type, only included in others (i.e. matchers and mergers)
(defstruct (Rete-node (:include basic-rete-node))
  ;; this one is sort of an "abstract defstruct", i.e. you only make
  ;; Rete-match-nodes or Rete-merge-nodes, never just Rete-node
  (code nil)
  (semi-unification-code nil))

(defun rete-node-printfn (self stream depth &optional stuff-to-insert)
  (declare (ignore depth))
  (format stream "#<~s~@[ ~s~]~@[ ~o~]>"
	  (type-of self)
	  stuff-to-insert
	  ;; would be pointer if I knew how
	  (or nil)))

;;; A node that's the result of a Lisp operation
(defstruct (Rete-procedure-node (:include basic-Rete-node)
				(:print-function rete-node-printfn))
  (code nil)
  (rule-name nil))

(defstruct (Rete-or-node (:include basic-Rete-node)
			     (:print-function rete-node-printfn))
  )



;;; A node that's the result of a pattern matching operation
(defstruct (Rete-match-node (:include Rete-node)
			    (:print-function
			      (lambda (o s d)
				(rete-node-printfn
				  o s d
				  (let ((match-ids (Rete-match-node-match-ids o)))
				    (if match-ids
					(match-id-pattern
					  (first match-ids))
					"No Pattern"))))))
  (match-ids nil)
  (truth-value nil)
  )

(defstruct (Rete-object-match-node (:include Rete-match-node)))

;;; A node that's the result of merging the stored environments at two other nodes
(defstruct (Rete-merge-node (:include Rete-node)
			    (:print-function rete-node-printfn))
  (merge-id nil))


;;; Two pseudo nodes
;;; these are the beginning of an AND group or an OR group
;;; there only function is to point to all the leaf nodes of this group
;;; doing this with a List screws the compiler as it tries to copy constants.

(defstruct (rete-and-group)
  (parts nil))

(defstruct (rete-or-group)
  (parts nil))

(defstruct (match-id)
  ;; a structure used in Rete-leaf nodes for traceability and to tell
  ;; whether two nodes are mergeable
  (rule-name nil)
  (pattern nil))



;;; The links of the rete network

(defstruct (basic-Rete-child-entry)
  ;; An abstract link type, only included in others
  (child nil))

(defstruct (Rete-child-entry (:include basic-Rete-child-entry))
  ;; A link that points to a merge node
  ;; says what other node to merge with and from which side
  ;; since the merge code is asymmetric in the two environments
  (brother nil)
  (side nil))

(defstruct (Rete-procedure-entry (:include basic-Rete-child-entry))
  ;; A link that points to a procedure node
  ;; The node it points to has the code the perform the operation
  (rule-name nil))

(defstruct (rete-or-entry (:include basic-rete-child-entry))
  (rule-name nil)
  (shuffling-code nil))

(defstruct (rete-terminal-entry)
  ;; A link that points to a rule-body
  (function nil)
  (rule-name nil)
  (truth-values nil)
  (importance nil))




;;;; The code that builds the network

(defun make-rete-network (rule-name top-level-node *pred-mapping*)
  (declare (special *pred-mapping*))
  (let ((*node-making-code* nil) 
	(*body-code* nil))
    (declare (special *node-making-code* *body-code*))
    (make-rete-network-dispatch rule-name top-level-node)
    (values (nreverse *node-making-code*) (nreverse *body-code*))
    ))
 
(defun make-rete-network-dispatch (rule-name trigger-structure)
  (or (pattern-analysis-rete-node trigger-structure)
      (let ((method-exists (find-method #'rete-net-constructor nil
					(list 
					 #-(or allegro sbcl) `(eql ,(type-of trigger-structure))
					 #+(or allegro sbcl) (intern-eql-specializer (type-of trigger-structure))
					 (find-class t)
					 (find-class t))
					nil)))
	(unless (or method-exists (merge-pattern-analysis-p trigger-structure))
	  (error "Rete network specification error, unknown element type ~s ~s"
		 (type-of trigger-structure) trigger-structure))
	(when method-exists (rete-net-constructor (type-of trigger-structure) rule-name trigger-structure))
	(let ((my-node (pattern-analysis-rete-node trigger-structure)))
	  (follow-links rule-name trigger-structure)	  
	  my-node))))

(defun follow-links (rule-name trigger-structure)
  (declare (special *body-code*))
  (loop with my-node = (pattern-analysis-rete-node trigger-structure)
	for link in (pattern-analysis-links trigger-structure)
	do (cond ((eq (car link) 'left))
		 ((eq (car link) 'right)
		  (let ((left (make-rete-network-dispatch rule-name (third link)))
			(child-analysis (second link)))
		    (add-rete-merge-node left my-node child-analysis)
		    (follow-links rule-name child-analysis)))
		 ((eq (car link) 'procedure)
		  (let ((target (make-rete-network-dispatch rule-name (second link))))
		    (push `(let ((procedure-entry (make-rete-procedure-entry :child ,target
									     :rule-name ',rule-name)))
			     (push procedure-entry (basic-rete-node-children ,my-node)))
			  *body-code*)))
		 ((eq (car link) 'or-shuffler)
		  (let ((target (make-rete-network-dispatch rule-name (second link))))
		    (push `(let ((or-entry (make-rete-or-entry
					     :child ,target
					     :rule-name ',rule-name
					     :shuffling-code ',(third link))))
			     (push or-entry (basic-rete-node-children ,my-node)))
			  *body-code*))))))

;;;; The routines dispatched to by MAKE-RETE-NETWORK-DISPATCH

;;; make a match node
(defmethod rete-net-constructor ((type (eql 'match-pattern-analysis)) rule-name trigger)
  (declare (special *node-making-code* *pred-mapping*))
  (let* ((name (gentemp "MATCH-NODE-"))
	 (pred-name (cdr (assoc (match-pattern-analysis-pattern trigger) *pred-mapping*)))
	 (match-node-code `(make-Rete-match-node
			     :code ',(match-pattern-analysis-full-unification-code trigger)
			     :semi-unification-code ',(match-pattern-analysis-semi-unification-code trigger)
			     :truth-value ',(match-pattern-analysis-truth-value trigger)
			     :match-ids (list (make-match-id
						:rule-name ',rule-name
						:pattern ,pred-name)))))
    (setf (pattern-analysis-rete-node trigger) name)
    (push (list name match-node-code) *node-making-code*)
    name))

(defmethod rete-net-constructor ((type (eql 'object-match-pattern-analysis)) rule-name trigger)
  (declare (special *node-making-code* *pred-mapping*))
  (let* ((name (gentemp "OBJECT-MATCH-NODE-"))
	 (pred-name (cdr (assoc (object-match-pattern-analysis-pattern trigger) *pred-mapping*)))
	 (match-node-code `(make-Rete-object-match-node
			    :semi-unification-code ',(object-match-pattern-analysis-semi-unification-code trigger)
			    :truth-value ',(object-match-pattern-analysis-truth-value trigger)
			    :match-ids (list (make-match-id
					      :rule-name ',rule-name
					      :pattern ,pred-name)))))
    (setf (pattern-analysis-rete-node trigger) name)
    (push (list name match-node-code) *node-making-code*)
    name))

(defmethod rete-net-constructor ((type (eql 'and-group-pattern-analysis)) rule-name trigger)
  (declare (special *node-making-code*))
  (loop for sub-trigger in (and-group-pattern-analysis-sub-patterns trigger)
	collect (make-rete-network-dispatch rule-name sub-trigger) into stuff
	finally (let ((name (gentemp "AND-GROUP-"))
		      (and-group-code `(make-rete-and-group :parts (list ,@stuff))))
		  (push (list name and-group-code) *node-making-code*)
		  (setf (pattern-analysis-rete-node trigger) name)
		  (return name))))

(defmethod rete-net-constructor ((type (eql 'or-group-pattern-analysis)) rule-name trigger)
  (declare (special *node-making-code*))
  (loop for sub-trigger in (or-group-pattern-analysis-sub-patterns trigger)
	collect (make-rete-network-dispatch rule-name sub-trigger) into stuff
	finally (let ((name (gentemp "OR-GROUP-"))
		      (or-group-code `(make-rete-or-group :parts (list ,@stuff))))
		  (push (list name or-group-code) *node-making-code*)
		  (setf (pattern-analysis-rete-node trigger) name)
		  (return name))))

(defmethod rete-net-constructor ((type (eql 'or-target-pattern-analysis)) rule-name trigger)
  (declare (special *node-making-code*))
  (declare (ignore rule-name))
  (let ((or-bottom-node-code `(make-rete-or-node))
	(name (gentemp "OR-JOIN-")))
    (setf (pattern-analysis-rete-node trigger) name)
    (push (list name or-bottom-node-code) *node-making-code*)
    name))

;;; make a procedure node
(defmethod rete-net-constructor ((type (eql 'procedure-pattern-analysis)) rule-name trigger)
  (declare (special *node-making-code*))
  (let* ((procedure-node-code `(make-rete-procedure-node
				:code ',(procedure-pattern-analysis-function trigger)
				:rule-name ',rule-name))
	 (name (gentemp "PROCEDURE-NODE-")))
    (setf (pattern-analysis-rete-node trigger) name)
    (push (list name procedure-node-code) *node-making-code*)
    name))

(defun add-rete-merge-node (left-name right-name merge-pattern-analysis)
  (declare (special *node-making-code* *body-code*))
  (let* ((child-node-code `(make-Rete-merge-node
			     :code ',(merge-pattern-analysis-merge-code merge-pattern-analysis)
			     :semi-unification-code ',(merge-pattern-analysis-semi-merge-code merge-pattern-analysis)
			     :merge-id ',(merge-pattern-analysis-merge-id merge-pattern-analysis)))
	 (child-name (gentemp "MERGE-NODE-")))
    (push (list child-name child-node-code) *node-making-code*)
    (setf (pattern-analysis-rete-node merge-pattern-analysis) child-name)
    (push `(setf (basic-rete-node-merges-underneath ,left-name) t) *body-code*)
    (push `(setf (basic-rete-node-merges-underneath ,right-name) t) *body-code*)
    (push `(push (make-Rete-child-entry
		   :child ,child-name
		   :brother ,right-name
		   :side 'left)
		 (basic-Rete-node-children ,left-name))
	  *body-code*)
    (push `(push (make-Rete-child-entry
		   :child ,child-name
		   :brother ,left-name
		   :side 'right)
		 (basic-Rete-node-children ,right-name))
	  *body-code*)
    child-name))

(defun make-rete-terminal (rule-name &optional importance)
  ;; Make a terminal entry.  Function field just has RULE-NAME, since there are no
  ;; forwarding pointers.
  (make-rete-terminal-entry
    :function rule-name
    :rule-name rule-name
    :importance importance))


;;;; Facilities for rule compiler that are needed here

;;; These variables and macros are referenced in this file but used by the rule system
;;; so we're putting them here.


(defvar *forward-rule-subsidiary-functions* nil
  "This variable collects functions that a forward rule needs compiled for it")

(defmacro collect-forward-rule-subsidiary-function (function)
  `(push ,function *forward-rule-subsidiary-functions*))




;;;; The rete network execution model

(eval-when(:compile-toplevel :execute :load-toplevel)
  (proclaim '(inline rete-entry-type)))
(defun rete-entry-type (entry)
  (type-of entry))

;;; This is a data structure to store the state that has
;;; advanced as far as a particular rete node.

(defstruct (rete-internal-state)
  (environment nil)				;the env propagated this far
  (predications nil)				;the preds that matched to get here
  (inactive 0 :type bit)			;Some pred that I matched has wrong truth value
  (dead     0 :type bit)			;I've been deleted
  (has-logic-variables 0 :type bit)		;The Environment has free logic variables
  (children nil)				;States derived from me
  (stimulate-list nil)				;Predications waiting for me to be active
  (my-node nil)					;The rete-network-node I'm stored in
  (parent-states nil)				;Who merge to get me
  )


;;;; The rete-network interning facility
;;; This is run-time code (i.e. not compile time).
;;;
;;; This takes the rete network belonging to a specific
;;; rule and "interns" it in the global rete network.
;;; If the rule already has a definition, the rete-network is
;;; updated so that all vestiges of the old rule are removed.
;;; 
;;; Thus there are 3 rete networks being played with:   The system's
;;; real rete network A rete network for the new version of the rule A
;;; part of the system's rete network which contains nodes belonging to
;;; the old version of this rule.
;;;
;;; We first find everything having to do with the old version of the 
;;; rule which will need to be removed.
;;;
;;; Then we install the match-nodes for the new version of the rule,
;;; keeping track of both the old and new node so that we can tell
;;; whether this is a brand new match node or not.
;;;
;;; Then we remove the stuff belonging to the old version of the rule.
;;; (this ordering lets the new version share as much as possible with
;;; the old version).
;;;
;;; Then we install all the stuff underneath the new match nodes,
;;; keeping track of new nodes.  Finally, we stimulate every new part of
;;; the network so that the rule will fire on everything that matches
;;; it.


(defun install-forward-rule-triggers (rule-name top-level-rete-node terminal-node importance if-part
				      &optional certainty-factor)
  ;; This is a hack
  ;; For slot valued object triggers, you meed to have a rule-debug-info in place
  ;; while you're installing the triggers, but it isn't put there until afterwards
  ;; I have no idea why this used to work and doesn't now.s
  (unless (rule-debug-info rule-name)
    (setf (rule-debug-info rule-name)
	    (make-rule-debug-info :name rule-name
                                  :certainty certainty-factor
				  :control :forward
				  :triggers nil
				  :context if-part
				  :network nil)))
  ;; The guy who does the actual work of making and installing rule triggers
  (push (make-Rete-terminal rule-name importance) (basic-rete-node-children terminal-node))
  (let ((match-nodes nil)
	(original-nodes nil)
	(pairs-to-remove-later nil)
	(terminal-nodes nil)
	(non-leading-procedural-nodes nil)
	(triggers nil)
	(interning-alist nil))
    (multiple-value-setq (original-nodes pairs-to-remove-later)
      (find-stuff-to-get-rid-of rule-name))
    ;; Now I can intern the new match nodes.
    ;; After this interning-alist will be an alist
    ;; of original terminal nodes of the network
    ;; to their interned version in the system rete network.
    (let ((visited-nodes nil))
      (flet ((intern-it (node top-level)
	       (unless (member node visited-nodes)
		 (push node visited-nodes)
		 (cond ((rete-match-node-p node)
			(let* ((match-ids (rete-match-node-match-ids node))
			       (trigger (match-id-pattern (car match-ids)))
			       (truth-value (rete-match-node-truth-value node)))
			  (let ((canonical-node (add-forward-rule-trigger trigger truth-value node if-part rule-name)))
			    (when (not (eql node canonical-node))
			      (loop for match-id in (rete-match-node-match-ids canonical-node)
				    when (and (eql (match-id-rule-name match-id) rule-name)
					      (variant (match-id-pattern match-id) trigger))
				      do (setq trigger (match-id-pattern match-id))
					 (return (values))))
			    (pushnew (list trigger truth-value) triggers)				
			    (push (cons node canonical-node) interning-alist)
			    (pushnew canonical-node match-nodes)
			    (pushnew canonical-node terminal-nodes))))
		       ((rete-procedure-node-p node)			
			(cond (top-level
			       (pushnew node terminal-nodes)
			       (push (cons node node) interning-alist))
			      (t (pushnew node non-leading-procedural-nodes))))))))
	(visit-rete-nodes top-level-rete-node #'intern-it)))
    (multiple-value-bind (new-stuff updated-alist) (intern-rete-network interning-alist)
      ;; Now we can get rid of old rules' rete-network entries.
      (loop for (node . entry) in pairs-to-remove-later
	    do (setf (basic-rete-node-children node) (delete entry (basic-rete-node-children node))))
      (loop with old-debug-info = (rule-debug-info rule-name)
	    for node in original-nodes
	    when (and (rete-match-node-p node)
		      (not (member node match-nodes)))
	      do (loop for match-id in (rete-match-node-match-ids node)
		       for his-rule = (match-id-rule-name match-id)
		       when (eql his-rule rule-name)
			 do (delete-forward-rule-trigger (match-id-pattern match-id)
							 (rete-match-node-truth-value node)
							 rule-name
							 (rule-debug-info-context old-debug-info))
			 and return (values)))
      ;; Now that the old stuff is gone and the new stuff is installed
      ;; update the rule-debug-info for the new version of the rule.
      (setq terminal-nodes (set-difference terminal-nodes non-leading-procedural-nodes))
      (setq triggers (nreverse triggers)
	    match-nodes (nreverse match-nodes)
	    terminal-nodes (nreverse terminal-nodes))
      (setf (rule-debug-info rule-name)
	    (make-rule-debug-info :name rule-name
                                  :certainty certainty-factor
				  :control :forward
				  :triggers triggers
				  :context if-part
				  :network terminal-nodes))
      (loop for (node . canonial-node) in updated-alist
	    when (eql node canonial-node)
	      ;; this is a new rule trigger
	      ;; go get matches for it.
	      do (typecase node
		   (rete-match-node
		     (let ((pattern (match-id-pattern (car (rete-match-node-match-ids node)))))
		       (prefetch-forward-rule-matches pattern if-part
						      (if (rete-object-match-node-p node)
							  #'(lambda (object)
							      (rete-network-match-object node object))
							  #'(lambda (predication)
							      (rete-network-match-predication node predication))))))
		   (rete-procedure-node
		     (when (member node terminal-nodes)
		       (Rete-network-apply-procedure-to-environment node
								    (make-rete-internal-state
								      :environment nil
								      :predications nil
								      :my-node node))))))
      (loop for (child parent child-entry) in new-stuff
	    do (typecase child-entry
		 (rete-child-entry
		   (let ((side (rete-child-entry-side child-entry))
			 (brother (rete-child-entry-brother child-entry)))
		     (loop for state-1 in (basic-rete-node-environments parent)
			   do (loop for state-2 in (basic-rete-node-environments brother)
				    do (rete-network-merge-environments child side state-1 state-2)))))
		 (rete-terminal-entry
		   (loop for state-entry in (basic-rete-node-environments parent)
			 do (run-forward-rule state-entry child-entry)))
		 (rete-or-entry
		   (loop for state-entry in (basic-rete-node-environments parent)
			 doing (funcall (rete-or-entry-shuffling-code child-entry) child state-entry)))
		 (rete-procedure-entry
		   ;; Lisp code will process this entry
		   (loop for state-entry in (basic-rete-node-environments parent)
			 doing (Rete-network-apply-procedure-to-environment child state-entry))
		   )))
      )))



(defun visit-rete-nodes (top-level-rete-node thing-to-do)
  (let ((visited-nodes nil))
    (labels ((traverse-rete-net (node top-level?)
	       (unless (member node visited-nodes)
		 (push node visited-nodes)
		 (cond ((basic-rete-node-p node)
			(when thing-to-do
			  (funcall thing-to-do node top-level?))
			(follow-links node))
		       ((rete-and-group-p node)
			(loop for thing in (rete-and-group-parts node)
			   doing (traverse-rete-net thing top-level?)))
		       ((rete-or-group-p node)
			(loop for thing in (rete-or-group-parts node)
			   doing (traverse-rete-net thing top-level?))))))
	     (follow-links (node)
	       (loop for child-entry in (basic-rete-node-children node)
		  when (basic-rete-child-entry-p child-entry)
		  do (traverse-rete-net (basic-rete-child-entry-child child-entry) nil))))
      (traverse-rete-net top-level-rete-node t))
    visited-nodes))

(defun find-stuff-to-get-rid-of (rule-name)
  (let ((old-debug-info (rule-debug-info rule-name)))
    (when old-debug-info
      (let ((original-nodes nil) (pairs nil))
	(when old-debug-info
	  (flet ((hack-subnodes (rete-node extra)
		   (declare (ignore extra))
		   (unless (member rete-node original-nodes)
		     (push rete-node original-nodes)
		     (loop for entry in (basic-rete-node-children rete-node)
			do (when (and (member (type-of entry) '(rete-procedure-entry rete-or-entry rete-terminal-entry))
				      (typecase entry
					(rete-procedure-entry (eq (rete-procedure-entry-rule-name entry) rule-name))
					(rete-or-entry (eq (rete-or-entry-rule-name entry) rule-name))
					(rete-terminal-entry (eql (rete-terminal-entry-rule-name entry) rule-name))))
				(push (cons rete-node entry) pairs))))))
	    (loop for node in (rule-debug-info-network old-debug-info)
		  when (or (rete-match-node-p node) (rete-procedure-node-p node))
		    do (visit-rete-nodes node #'hack-subnodes))))
	(values original-nodes pairs)))))

(defun install-and-intern-forward-rule-trigger (new-Rete-node triggers)
  ;; add a trigger to the index of trigger patterns of the appropriate
  ;; type.  We store a rule-trigger structure into the trigger-location.
  (loop with new-rete-node-truth-value = (rete-match-node-truth-value new-rete-node)
	and match-id = (car (Rete-match-node-match-ids new-Rete-node))
	with new-pattern = (match-id-pattern match-id)
	for Rete-node in triggers
	for stored-match-id = (car (Rete-match-node-match-ids Rete-node))
	for stored-pattern = (match-id-pattern stored-match-id)
	for stored-truth-value = (rete-match-node-truth-value rete-node)
	when (and (eql new-rete-node-truth-value stored-truth-value)
		  (variant new-pattern stored-pattern))
	  ;; nothing in the index changed
	  return (values triggers nil rete-node)
	finally (return (values (cons new-Rete-node triggers) t new-rete-node))))




(defun intern-rete-network (alist)
  (let ((new-nodes nil))
    (labels
	((intern-Rete-match-nodes (donator receptor)
	   (unless (eql donator receptor)
	     ;; intern match nodes so that triggers which are variants of each other
	     ;; share a node and don't do duplicate unifications.
	     (intern-Rete-nodes donator receptor)
	     (flet ((equal-matchids (match-id1 match-id2)
		      (and (eql (match-id-rule-name match-id1)
				(match-id-rule-name match-id2))
			   (variant (match-id-pattern match-id1)
				    (match-id-pattern match-id2)))))
	       (loop for match-id in (Rete-match-node-match-ids donator)
		  doing (pushnew match-id (Rete-match-node-match-ids receptor)
				 :test #'equal-matchids)))))
	 (intern-Rete-nodes (donator receptor)
	   ;; make sure you use the new code
	   (unless (eql donator receptor)
	     (pushnew (cons donator receptor) alist :key #'car)
	     (setq new-nodes (delete donator new-nodes :key #'car))
	     (setf (Rete-node-code receptor) (Rete-node-code donator))
	     (setf (Rete-node-semi-unification-code receptor) (Rete-node-semi-unification-code donator))
	     ;; make all references in the child links point to canonical nodes
	     (loop for child-entry in (basic-Rete-node-children donator)
		when (typep child-entry 'Rete-child-entry)
		do
		  (let ((brother (Rete-child-entry-brother child-entry)))
		    (loop for brother-child-entry in (basic-Rete-node-children brother)
		       when (and (typep brother-child-entry 'Rete-child-entry)
				 (eq (Rete-child-entry-brother brother-child-entry) donator))
		       do (setf (Rete-child-entry-brother brother-child-entry) receptor))))
					;(setf (basic-rete-node-merges-underneath receptor) t)
	     (attempt-to-merge-children donator receptor)))
	 (attempt-to-merge-children (donator receptor)
	   (loop for donator-child-entry in (basic-rete-node-children donator)
	      if (typep donator-child-entry 'Rete-child-entry)
	      do (let* ((donator-brother (Rete-child-entry-brother donator-child-entry))
			(donator-side (rete-child-entry-side donator-child-entry))
			(donator-child (Rete-child-entry-child donator-child-entry))
			(donator-merge-id (Rete-merge-node-merge-id donator-child)))
		   (loop for receptor-child-entry in (basic-rete-node-children receptor)
		      when (typep receptor-child-entry 'Rete-child-entry)
		      do (let* ((receptor-brother (Rete-child-entry-brother receptor-child-entry))
				(receptor-side (rete-child-entry-side receptor-child-entry))
				(receptor-child (Rete-child-entry-child receptor-child-entry))
				(receptor-merge-id (Rete-merge-node-merge-id receptor-child)))
			   (when (and (eql donator-brother receptor-brother)
				      (eql donator-side receptor-side)
				      (equal donator-merge-id receptor-merge-id))
			     (loop for receptor-brother-child-entry in (basic-rete-node-children receptor-brother)
				when (typep receptor-brother-child-entry 'Rete-child-entry)
				do (let ((receptor-brother-child (rete-child-entry-child receptor-brother-child-entry))
					 (receptor-brother-side (rete-child-entry-side receptor-brother-child-entry))
					 (receptor-brother-brother (rete-child-entry-brother
								    receptor-brother-child-entry)))
				     (when (and (eql receptor-brother-child donator-child)
						(eql receptor-brother-brother receptor)
						(not (eql receptor-brother-side receptor-side)))
				       (setf (basic-rete-node-children receptor-brother)
					     (delete receptor-brother-child-entry (basic-rete-node-children receptor-brother)))
				       (return (values)))))
			     (intern-Rete-nodes donator-child receptor-child)
			     (return (values))))
		      finally (push donator-child-entry (basic-rete-node-children receptor))
			(add-to-new-nodes donator-child receptor donator-child-entry)))
	      else do (push donator-child-entry (basic-rete-node-children receptor))
		(when (typep donator-child-entry 'basic-rete-child-entry)
		  (add-to-new-nodes (basic-rete-child-entry-child donator-child-entry) receptor donator-child-entry))))
	 (add-to-new-nodes (donator-child receptor donator-child-entry)
	   (unless (assoc donator-child new-nodes)
	     (push (list donator-child receptor donator-child-entry) new-nodes)
	     (when (basic-rete-node-p donator-child)
	       (loop for child-entry in (basic-rete-node-children donator-child)
		  when (typep child-entry 'basic-rete-child-entry)
		  do (remove-children (basic-rete-child-entry-child child-entry))))))
	 (remove-children (child)
	   (setq new-nodes (delete child new-nodes :key #'car))
	   (when (basic-rete-node-p child)
	     (loop for child-entry in (basic-rete-node-children child)
		when (typep child-entry 'basic-rete-child-entry)  
		do (remove-children (basic-rete-child-entry-child child-entry))))))
      (loop for (donator . receptor) in alist
	 doing (typecase receptor
		 (rete-match-node
		  (intern-rete-match-nodes donator receptor))
		 (otherwise (intern-rete-nodes donator receptor))))
      (values new-nodes alist))))
 



;;; Clearing and Killing rete networks

(defun clear-Rete-network (Rete-nodes)
  ;; flush the state of an entire Rete network (subroutine shared by undefrule and clear)
  (labels ((clear-Rete-node (node)
	     ;; flush the state of one Rete node
	     (let ((something-here (or (basic-Rete-node-environments node) (basic-rete-node-suspended-states node))))
	       (setf (basic-rete-node-environments node) nil)
	       (setf (basic-rete-node-suspended-states node) nil)
	       (when something-here
		 ;; now iterate over the kidz
		 (loop for child-entry in (basic-Rete-node-children node)
		       when (typep child-entry 'basic-Rete-child-entry)
			 ;; this kid is not terminal, so clear him too.
		    do (clear-Rete-node (basic-Rete-child-entry-child child-entry)))))))
    ;; clear each Rete node given
    (loop for node in Rete-nodes do (clear-Rete-node node))))

(defun disarm-rule-triggers (rule-name)
  ;; Pull a rule's teeth so it won't fire anymore. Useful for incremental recompilation
  ;; note that this won't disarm questions.
  (let ((debug-info (rule-debug-info rule-name)))
    (when debug-info
      (let ((direction (rule-debug-info-control debug-info)))
	(ecase direction
	  (:forward
	    ;; flush unreachable Rete terminal nodes
	    (loop for node in (rule-debug-info-network debug-info)
		  do (get-rid-of-terminal-entries node rule-name)))
	  (:backward))
	(setf (rule-debug-info-network debug-info) nil)
	(loop with context = (rule-debug-info-context debug-info)
	      for trigger in (rule-debug-info-triggers debug-info)
	      ;; Some triggers are procedure, only do this for predications
	      when (and (consp trigger)(predicationp (car trigger)))
		;; remove all the triggers for this rule from the trigger database
		do (destructuring-bind (pattern truth-value) trigger
		     (handler-case
			  (ecase direction
			    (:forward
			      (delete-forward-rule-trigger pattern truth-value rule-name context))
			    (:backward
			      (delete-backward-rule-trigger pattern truth-value rule-name context)))
			(error (condition) nil (values nil condition)))))))))

(defun rule-armed-p (rule-name)
  ;; how to tell if a rule is armed (i.e., might trigger) or not
  (let ((debug-info (rule-debug-info rule-name)))
    (when debug-info
      (not (null (rule-debug-info-network debug-info))))))


(defun remove-and-deinstall-if-necessary-forward-rule-triggers (pattern rule-name triggers)
  ;; delete a trigger from the trigger index.  Used by undefrule.
  (loop with something-changed = nil
	for Rete-node in triggers
	do (loop for match-id in (Rete-match-node-match-ids Rete-node)
		 for entry-rule-name = (match-id-rule-name match-id)
		 for entry-pattern = (match-id-pattern match-id)
		 when (and (eq rule-name entry-rule-name)
			   (eq entry-pattern pattern))
		   do (setf (Rete-match-node-match-ids Rete-node)
			    (delete match-id (Rete-match-node-match-ids Rete-node))))
	if (null (Rete-match-node-match-ids Rete-node))
	  ;; no longer matches for anyone, so flush it
	  do (setq something-changed t)
	     (remove-useless-Rete-node Rete-node)
	else collect Rete-node into remaining-good-triggers
	finally (return (values remaining-good-triggers something-changed))))

(defun remove-useless-rete-node (rete-node)
  (loop for entry in (basic-rete-node-children rete-node)
	when (rete-child-entry-p entry)
	  do (loop with brother = (rete-child-entry-brother entry)
		   for his-entry in (basic-rete-node-children brother)
		   unless (and (rete-child-entry-p his-entry)
			       (eq (rete-child-entry-brother his-entry) rete-node))
		     collect his-entry into good-children
		   finally (setf (basic-rete-node-children brother) good-children))))

(defun get-rid-of-terminal-entries (rete-node rule-name)
  #-sbcl(declare (values im-still-useful))
  ;; Disarming the triggers has to go through the network under this rule's
  ;; match nodes and get rid of any terminal entry for this rule
  ;; so its body function will never run.
  (loop with entries = (basic-rete-node-children rete-node)
	for entry in entries
	do (typecase entry
	     (rete-child-entry
	       (unless
		 (get-rid-of-terminal-entries (rete-child-entry-child entry) rule-name)
		 (setq entries (delete entry entries))))
	     (rete-procedure-entry
	       (when (eq (rete-procedure-entry-rule-name entry) rule-name)
		 (get-rid-of-terminal-entries (rete-procedure-entry-child entry) rule-name)
		 (setq entries (delete entry entries))))
	     (rete-or-entry
	       (when (eq (rete-or-entry-rule-name entry) rule-name)
		 (get-rid-of-terminal-entries (rete-or-entry-child entry) rule-name)
		 (setq entries (delete entry entries))))
	     (rete-terminal-entry
	       (when (eq (rete-terminal-entry-rule-name entry) rule-name)
		 (setq entries (delete entry entries)))))
	finally (setf (basic-rete-node-children rete-node) entries)
		(return (not (null entries)))))

(defun get-terminal-entry (rule-name)
  (labels ((do-one-level (this-node)
	     (loop with entries = (basic-rete-node-children this-node)
		   for entry in entries
		   do (typecase entry
			(rete-child-entry
			  (do-one-level (rete-child-entry-child entry)))
			(rete-procedure-entry
			  (when (eq (rete-procedure-entry-rule-name entry) rule-name)
			    (do-one-level (rete-procedure-entry-child entry))))
			(rete-or-entry
			  (when (eq (rete-or-entry-rule-name entry) rule-name)
			    (do-one-level (rete-or-entry-child entry))))
			(rete-terminal-entry
			  (when (eq (rete-terminal-entry-rule-name entry) rule-name)
			    (return-from get-terminal-entry (values entry))))))))
    (loop for rete-node in (rule-debug-info-network (rule-debug-info rule-name))
	  doing (do-one-level rete-node))))




;;;
;;; Invoking forward chaining rules.
;;;

(defun  execute-forward-rule (state-entry child-entry)
  ;; fire when ready
  (cond
   ((zerop (rete-internal-state-inactive state-entry))
    ;; state is alive, do it now.
    (incf *forward-fire-count*)
    (loop with rule-name = (rete-terminal-entry-rule-name child-entry)
          with body-function = (symbol-function rule-name)
          with debug-info = (rule-debug-info rule-name)
          with certainty-factor = (rule-debug-info-certainty debug-info)
          for supporter in (rete-internal-state-predications state-entry)
          for truth-value = (predication-truth-value supporter)
          if (= truth-value +true+) collect supporter into true-support
          else if (= truth-value +false+) collect supporter into false-support
          else if (= truth-value +unknown+) collect supporter into unknown-support
          else do (error "Contradictory truth-value of ~S in *support*: ~S" supporter *support*)
          finally 
          (when certainty-factor
            (setq rule-name (list rule-name certainty-factor)))
          (with-stack-list (justification rule-name true-support false-support unknown-support)
            (funcall body-function
                     (rete-terminal-entry-function child-entry)
                     (rete-internal-state-environment state-entry)
                     justification))))
  (t (push child-entry (rete-internal-state-stimulate-list state-entry)))))

(defmacro with-atomic-action (&body body)
  `(let ((somebody-else-is-managing-delayed-triggers *delay-rule-triggering*)
	 (somebody-emptying-queue *forward-queue-emptying-p*))
     (unwind-protect
	 (let ((*delay-rule-triggering* t))
	   ,@body)
       (unless somebody-else-is-managing-delayed-triggers
	 (atomic-action-cleanup somebody-emptying-queue)))))

(defun atomic-action-cleanup (somebody-emptying-queue)
  (unwind-protect
      (let ((delayed-stuff *delay-rule-trigger-list*)
	    (*forward-queue-emptying-p* t))
	(setq *delay-rule-trigger-list* nil)
	(mapc #'(lambda (thing)
		  (continue-suspended-forward-rule-triggering thing))
	      (nreverse delayed-stuff))
	(when (and *something-in-fwrd-q* (not somebody-emptying-queue))
	    (run-forward-queue)))
    (setq *delay-rule-trigger-list* nil)
    (unless somebody-emptying-queue
      (clear-forward-queue))))

(defun run-forward-rule (state-entry child-entry)
  ;; the guy who knows how to run a rule.  It's used by the Rete network.
  (let ((importance (Rete-terminal-entry-importance child-entry)))
    (cond (importance
	   ;; this rule has a importance, so queue it
	   (cond
	     ((zerop (rete-internal-state-inactive state-entry))
	      ;; state is alive, so queue it now
	      ;; mark that there might be something in the forward queue.
	      (setq *something-in-fwrd-q* t)
	      (enqueue-forward-rule state-entry child-entry importance))
	     (t (push child-entry (rete-internal-state-stimulate-list state-entry)))))
	  (t (execute-forward-rule state-entry child-entry)))))


;;;; Subst's used in the rete network execution model

(eval-when (:compile-toplevel :execute :load-toplevel)
  (proclaim '(inline add-to-suspended-brother-states)))
(defun add-to-suspended-brother-states (my-state-entry node)
  (loop for child-entry in (basic-rete-node-children node)
	when (eql (rete-entry-type child-entry) 'rete-child-entry)
	  do (let* ((sibling (Rete-child-entry-brother child-entry))
		    (his-states (basic-rete-node-suspended-states sibling)))
	       (when his-states
		 (let ((successor (Rete-child-entry-child child-entry))
		       (side (Rete-child-entry-side child-entry)))
		   ;; The suspended states are those that are waiting to
		   ;; get unretracted.  We add ourselves to their list
		   ;; of things to do if they come back in.
		   (loop with pair = (list successor (if (eql side 'left) 'right 'left) my-state-entry)
			 for his-state in his-states
			 doing (push pair (rete-internal-state-stimulate-list his-state))))))))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (proclaim '(inline rete-merge-with-brothers)))
(defun rete-merge-with-brothers (my-state-entry child-entry)
  (block rete-merge-with-brothers
  ;; Merge of two states
    (let ((sibling (Rete-child-entry-brother child-entry))
	  (successor (Rete-child-entry-child child-entry))
	  (side (Rete-child-entry-side child-entry)))
      ;; map over the other guy's states merging with ours
      ;; however if the other state is marked inactive ignore it.
      (loop for his-states on (basic-rete-node-environments sibling)
	    for his-state-entry = (car his-states)
	    when (not (zerop (rete-internal-state-inactive my-state-entry)))
	      ;; I've gone inactive after processing some
	      ;; of the merges with this sibling.  Save the
	      ;; rest of the merges with this sib and the
	      ;; rest of the processing left in the
	      ;; stimulate list.
	      do (when (zerop (rete-internal-state-dead my-state-entry))
		   ;; I'm not dead (i.e. deleted) so I need to all the
		   ;; unprocessed states on my stimulate list.
		   (loop for his-state in his-states
			 do (push (list successor side his-state) (rete-internal-state-stimulate-list my-state-entry))))
		 (return-from rete-merge-with-brothers)
	    when (zerop (rete-internal-state-inactive his-state-entry))
	      unless (and (eql my-state-entry his-state-entry)
			  (eql side 'right))
		;; he isn't flushed so process him
		do (Rete-network-merge-environments successor side my-state-entry his-state-entry)))))


;;;; The basic steps of rete network processing

(defun rete-proceed-to-next-node (state-entry node)
  #+genera (declare (dbg:invisible-frame joshua-internals))
  (add-to-suspended-brother-states state-entry node)
  (push state-entry (basic-rete-node-environments node))
  (loop for entries on (basic-Rete-node-children node)
	for child-entry = (car entries)
	;; if I've gone inactive for any reason I should stop
	;; and save all the unprocessed entries on my stimulate list.
	when (not (zerop (rete-internal-state-inactive state-entry)))
	  do (when (zerop (rete-internal-state-dead state-entry))
	       (setf (rete-internal-state-stimulate-list state-entry)
		     (append entries (rete-internal-state-stimulate-list state-entry))))
	     (return-from rete-proceed-to-next-node)
	do (case (rete-entry-type child-entry)
	     (rete-or-entry
	       (funcall (rete-or-entry-shuffling-code child-entry) (Rete-or-entry-child child-entry) state-entry))
	     (rete-procedure-entry
	       ;; Lisp code will process this entry
	       (Rete-network-apply-procedure-to-environment (Rete-procedure-entry-child child-entry) state-entry))
	     (Rete-child-entry
	       (rete-merge-with-brothers state-entry child-entry))
	     (Rete-terminal-entry (run-forward-rule state-entry child-entry)))))

(defun Rete-network-apply-procedure-to-environment (node state-entry)
  #+genera (declare (dbg:invisible-frame joshua-internals))
  (let ((old-state-has-lvs (rete-internal-state-has-logic-variables state-entry))
	(old-state-preds (rete-internal-state-predications state-entry))
	(old-state-env (rete-internal-state-environment state-entry)))
    (flet ((continuation (env support has-lvs)
             #+ (or genera cloe) (declare (sys:downward-function))
	     (let ((next-state (make-rete-internal-state
				 :environment env
				 :predications (cond ((null support) old-state-preds)
						     ((predicationp support)
						      (push state-entry (predication-rete-states support))
						      (cons support old-state-preds))
						     (t (loop for thing in support
							      doing (push state-entry (predication-rete-states thing)))
							(append support old-state-preds)))
				 :my-node node
				 :has-logic-variables (if has-lvs 1 0))))
	       (push next-state (rete-internal-state-children state-entry))
	       (rete-proceed-to-next-node next-state node))))
	  (declare (dynamic-extent #'continuation))
      (funcall (rete-procedure-node-code node) old-state-env (not (zerop old-state-has-lvs)) #'continuation))))

;;;; Rete network match entry point

(defmethod better-versions ((predication default-protocol-implementation-model)) nil)

(defun Rete-network-match-predication (node predication)
  #+genera (declare (dbg:invisible-frame joshua-internals))
  ;; what a match node does when you tell it to notice a new predication
  (cond ((and (eql (rete-match-node-truth-value node) (predication-truth-value predication))
	      (null (better-versions predication)))
	 ;; He has the right truth value so let's proceed
	 (incf *match-count*)
	 (multiple-value-bind (match-env has-logic-variables)
	     (cond ((predication-logic-variable-free-p predication)
		    (incf *semi-match-count*)
		    (funcall (Rete-node-semi-unification-code Node) predication))
		   (t
		     (funcall (Rete-node-code Node) predication)))
	   (when match-env
	     (incf *successful-match-count*)
	     (let ((state-entry (make-rete-internal-state
				  :environment match-env
				  :predications (list predication)
				  :has-logic-variables (if has-logic-variables 1 0)
				  :my-node node)))
	       (push state-entry (predication-rete-states predication))
	       (rete-proceed-to-next-node state-entry node)
	       t))))
	(t (push node (predication-stimulate-list predication)))))



(defgeneric add-rete-state (object rete-state)
  ;  "Anntate a matched object with the resultant state"
)

(defun Rete-network-match-object (node object)
  #+genera  (declare (dbg:invisible-frame joshua-internals))
  ;; what a match node does when you tell it to notice a new non-predication object
  (incf *match-count*)
  (multiple-value-bind (match-env has-logic-variables)
      (funcall (Rete-node-semi-unification-code Node) object)
    (when match-env
      (incf *successful-match-count*)
      (let ((state-entry (make-rete-internal-state
			   :environment match-env
			   :predications (if (typep object 'tms-object-mixin)
					     (list (basic-object-type-predication object)
						   (basic-object-part-predication object))
					   (list (basic-object-type-predication object)))					   
			   :has-logic-variables (if has-logic-variables 1 0)
			   :my-node node)))
	(add-rete-state object state-entry)
	(rete-proceed-to-next-node state-entry node)
	t))))



;;;; Rete network delete and retraction entry points

;;; To delete a predication chase down the pointer chains removing
;;; all descendant states.

(defun rete-network-delete-predication (predication)
  (labels ((get-rid-of-state-entry (state)
	     #+(or cloe genera) (declare (sys:downward-function))
	     (when (zerop (rete-internal-state-dead state))
	       (setf (rete-internal-state-inactive state) 1)
	       (setf (rete-internal-state-dead state) 1)
	       (let ((node (rete-internal-state-my-node state)))
		 (setf (basic-rete-node-suspended-states node)
		       (delete state (basic-rete-node-suspended-states node)))
		 (setf (basic-rete-node-environments node)
		       (delete state (basic-rete-node-environments node))))
	       (loop for sub-state in (rete-internal-state-children state)
		     doing (get-rid-of-state-entry sub-state)))))
    (declare (dynamic-extent #'get-rid-of-state-entry))	  
    (loop for state-entry in (predication-rete-states predication)
	  doing (get-rid-of-state-entry state-entry))
    (setf (predication-bits-ive-been-untold (predication-bits predication)) 1)
    (setf (predication-rete-states predication) nil)))

(defun rete-network-delete-object (object)
  (labels ((get-rid-of-state-entry (state)
	     #+(or cloe genera) (declare (sys:downward-function))
	     (when (zerop (rete-internal-state-dead state))
	       (setf (rete-internal-state-inactive state) 1)
	       (setf (rete-internal-state-dead state) 1)
	       (let ((node (rete-internal-state-my-node state)))
		 (setf (basic-rete-node-suspended-states node)
		       (delete state (basic-rete-node-suspended-states node)))
		 (setf (basic-rete-node-environments node)
		       (delete state (basic-rete-node-environments node))))
	       (loop for sub-state in (rete-internal-state-children state)
		     doing (get-rid-of-state-entry sub-state)))))
    (declare (dynamic-extent #'get-rid-of-state-entry))
    (loop for state-entry in (rete-states object)
	  doing (get-rid-of-state-entry state-entry))))

(defun rete-network-retract-predication (predication)
  (labels ((retract-state-entry (state)
	     #+(or cloe genera) (declare (sys:downward-function))
	     (when (zerop (rete-internal-state-inactive state))
	       (setf (rete-internal-state-inactive state) 1)
	       (let ((node (rete-internal-state-my-node state)))
		 (setf (basic-rete-node-environments node)
		       (delete state (basic-rete-node-environments node)))
		 (push state (basic-rete-node-suspended-states node)))
	       (loop for sub-state in (rete-internal-state-children state)
		     doing (retract-state-entry sub-state)))))
    (declare (dynamic-extent #'retract-state-entry))
    (loop for state-entry in (predication-rete-states predication)
	  doing (retract-state-entry state-entry))))

(defun stimulate-rete-state (rete-state truth-value)
  ;; check that my immediate parent states are all active
  ;; If so, I'm active.
  (labels
    ((stimulate-state-internal (rete-state node)
       (when (and (loop for parent-state in (rete-internal-state-parent-states rete-state)
			always (zerop (rete-internal-state-inactive parent-state)))
		  (not (eql (rete-internal-state-dead rete-state) 1)))
	 (setf (rete-internal-state-inactive rete-state) 0)
	 (setf (basic-rete-node-suspended-states node)
	       (delete rete-state (basic-rete-node-suspended-states node)))
	 (push rete-state (basic-rete-node-environments node))
	 (loop for child-state in (rete-internal-state-children rete-state)
	       for node = (rete-internal-state-my-node child-state)
	       do (stimulate-state-internal child-state node))
	 (loop for entries on (rete-internal-state-stimulate-list rete-state)
	       for entry = (car entries)
	       when (not (zerop (rete-internal-state-inactive rete-state)))
		 ;; oops i've been flushed again
		 do (when (zerop (rete-internal-state-dead rete-state))
		      (setf (rete-internal-state-stimulate-list rete-state) entries))
		    (return-from stimulate-state-internal)
	       doing (cond ((rete-child-entry-p entry)
			    (rete-merge-with-brothers rete-state entry))
			   ((rete-terminal-entry-p entry)
			    (run-forward-rule rete-state entry))
			   ((rete-procedure-entry-p entry)
			    (Rete-network-apply-procedure-to-environment (Rete-procedure-entry-child entry) rete-state))
			   ((rete-or-entry-p entry)
			    (funcall (rete-or-entry-shuffling-code entry) (Rete-or-entry-child entry) rete-state))
			   (t (destructuring-bind (successor side other-state) entry
				(rete-network-merge-environments successor side rete-state other-state)))))
	 (setf (rete-internal-state-stimulate-list rete-state) nil))))
    (when (eql 1 (rete-internal-state-inactive rete-state))
      (let ((my-node (rete-internal-state-my-node rete-state)))
	(when (and (rete-match-node-p my-node)
		   (eql truth-value (rete-match-node-truth-value my-node)))
	  (stimulate-state-internal rete-state my-node))))))





;;;; Rete net merge entry point

;;; This used to be called rete-network-notice-environment but this name seems more
;;; descriptive.

(defun Rete-network-merge-environments (node side first-state second-state)
  ;; what a merge node does when you tell it to notice an environment.
  #+genera (declare (dbg:invisible-frame joshua-internals))
  ;; We only call this if the first state is known to be active.  But
  ;; it's possible that the other state could have been retracted or
  ;; killed.  That would have set its inactive mark on.  If so, don't
  ;; process it, but add an entry to his stimulate list.
  (cond
    ((not (zerop (rete-internal-state-inactive second-state)))
     ;; the other state is inactive, we shouldn't do the merge now
     (when (zerop (rete-internal-state-dead second-state))
       ;; but he hasn't been killed.  Flip the value of side for his
       ;; entry.
       (push (list node (if (eql side 'left) 'right 'left) first-state)
	     (rete-internal-state-stimulate-list second-state))))
    (t
     (let ((left-state first-state) (right-state second-state))
       (when (eql side 'right) (rotatef left-state right-state))
     (let ((left-predications (rete-internal-state-predications left-state))
	   (right-predication (car (rete-internal-state-predications right-state)))
	   (left-env (rete-internal-state-environment left-state))
	   (right-env (rete-internal-state-environment right-state)))
       (incf *merge-count*)
       (multiple-value-bind (merge-env has-logic-variables)
	   (cond ((and (zerop (rete-internal-state-has-logic-variables left-state))
		       (zerop (rete-internal-state-has-logic-variables right-state)))
		  ;; both environments are variable-free, so semi-unification will work
		  (incf *semi-merge-count*)
		  (funcall (Rete-node-semi-unification-code Node) left-env right-env))
		 (t
		   ;; full unification required (but what if just 1 has variables?)
		   (funcall (Rete-node-code node) left-env right-env)))
	 (when merge-env
	   ;; merge succeeded
	   (incf *successful-merge-count*)
	   (let ((new-state-entry
		   (make-rete-internal-state
		     :environment merge-env
		     :predications (if right-predication
				       (cons right-predication left-predications)
				       left-predications)
		     :has-logic-variables (if has-logic-variables 1 0)
		     :parent-states (list left-state right-state)
		     :my-node node)))
	     (push new-state-entry (rete-internal-state-children left-state))
	     (push new-state-entry (rete-internal-state-children right-state))
	     (rete-proceed-to-next-node new-state-entry node))
	   t)))))))


;;; The graph rete net code is now in ptypes-and-commands
(eval-when (:compile-toplevel :execute :load-toplevel)
  (proclaim '(special *forward-rules*)))

