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
;;; Created 5/20/87 16:35:41 by sgr running on GROUSE at SCRC.

(in-package :ji)


;;;
;;; Putting it all together:
;;; 
;;; Here are the main methods that call out to the protocol g-f's
;;; and implement what Joshua's really doing under all protocol.
;;;

(define-predicate-model default-rule-compilation-model
			()
			()
  )

(define-predicate-model default-ask-model
			()
			()
  )

;;;  This used to be a :DEFAULT method, but CLOS has no such beast.
(define-predicate-method (fetch default-ask-model) (continuation)
  ;; cause an error if you try to ASK one of these
  (declare (ignore continuation))
  (error "You have failed to provide a fetch method for Predication ~S." (type-of self)))

(define-predicate-model default-tell-model
			()
			()
  )


(define-predicate-method (data-is-guaranteed-variable-free default-tell-model) () nil) 

;;;  This used to be a :DEFAULT method, but CLOS has no such beast.
(define-predicate-method (insert default-tell-model) ()
  ;; cause an error if you try to ASK one of these
  (error "You have failed to provide an insert method for Predication ~S." (type-of self)))

;;; this is the model that holds the default high-level implementation of the
;;; Joshua protocol
(define-predicate-model default-protocol-implementation-model
	;; STIMULATE-LIST is a list of actions to take the next time
	;; this predication comes in, i.e., gets a truth-value of
	;; something other than +UNKNOWN+
	((rete-states :initform nil :accessor predication-rete-states :Initarg :rete-states)
	 (stimulate-list :initform nil :accessor predication-stimulate-list :initarg :stimulate-list))
	(default-rule-compilation-model default-ask-model default-tell-model)
	)

(defmethod init-plist append ((p default-protocol-implementation-model))
  (list :rete-states (predication-rete-states p)
	:stimulate-list (predication-stimulate-list p)))

(defparameter *forward-priority-queue* (make-heap :predicate #'>)
  "The global heap for prioritizing forward rule firings.")

(defvar *forward-queue-emptying-p* nil "Flag to prevent multiple attempts to empty queue.")

(defvar *something-in-fwrd-q* nil "Flag to quickly tell us if there is something in the Forward Queue.")

(defvar *delay-rule-trigger-list* nil)
(defvar *delay-rule-triggering* nil)

(defun run-forward-queue ()
  (loop for entry = (heap-remove *forward-priority-queue*)
	;; note that the heap-elements iteration path is the wrong thing for this.
	when (heap-empty-p *forward-priority-queue*)
	  do (setq *something-in-fwrd-q* nil)
	while entry
	;; have to go through run-rule, 'cause the truth-values might have changed.
	doing (execute-forward-rule (car entry) (cadr entry))))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (proclaim '(inline clear-forward-queue)))
(defun clear-forward-queue ()
  ;; throw away a bunch of queued rules.
  (heap-clear *forward-priority-queue*)
  (setq *something-in-fwrd-q* nil))

(defresource backward-importance-queue-resource ()
  ;; resource of queues used by backward chaining (we only allocate one if we're actually gonna use it)
  :constructor (make-heap :size 10 :predicate #'>)
  ;; deinitializer instead of initializer to give the gc an even break
  :deinitializer heap-clear)

(defun run-backward-queue (queue function)
  ;; empty a backward queue, doing something to each entry
  #-sbcl (declare (dynamic-extent function))
  (loop for entry = (heap-remove queue)
	while entry
	doing (funcall function entry)))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (proclaim '(inline number-from-importance)))

(defun number-from-importance (importance)
  ;; convert the thing in the importance slot to a number
  ;; note that this gets done once, when the thing gets enqueued.  Subsequent frobbage
  ;; of state to alter its importance won't matter.
  (etypecase importance
    (number importance)
    (symbol (symbol-value importance))
    (function (funcall importance))))

;;; Removing the subst, turn it it to something I can encapsulate
(defun enqueue-backward (queue entry importance)
  ;; stick this in a backward queue
  (heap-insert queue entry (number-from-importance importance)))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (proclaim '(inline enqueue-forward-rule)))
(Defun enqueue-forward-rule (rete-state child-entry importance)
  ;; stick an entry into the priority queue with a given priority
  (heap-insert  *forward-priority-queue*
		(list rete-state child-entry)
		(number-from-importance importance)))

;;; What's on the stimulate list of a predication is a list of
;;; rete-match-nodes to process.
 
(defmethod stimulate ((self default-protocol-implementation-model) truth-value)
  ;; First look at all the match nodes that we haven't yet done the
  ;; match with.
  (with-slots (bits rete-states stimulate-list) self
   (loop until (/= (predication-bits-truth-value bits) truth-value)
	 for rete-state in rete-states
	 doing (stimulate-rete-state rete-state truth-value))
   (when stimulate-list
     (loop with head = (cons nil stimulate-list)
	   with pointer = head
	   for match-node = (cadr pointer)
	   ;; stop when my bits have been changed, or when there are no
	   ;; more entries in the stimulate-list
	   until (or (/= (predication-bits-truth-value bits) truth-value) (null match-node))
	   for required-truth-value = (rete-match-node-truth-value match-node)
	   if (/= required-truth-value truth-value)
	     ;; If this guy doesn't have the right truth-value
	     ;; just bypass him; he'll stay sitting on the stimulate-list
	     do (setf pointer (cdr pointer))
		;; otherwise process the entry and splice him out
	   else do (rete-network-match-predication match-node self)
		   (setf (cdr pointer) (cddr pointer))
	   finally (setq stimulate-list (cdr head))))))

;;;
;;; The actual default implementations of the protocol fns.
;;; Note that predication supports only SAY in any interesting manner.
;;; Note that default-protocol-implementation-model does not
;;; implement CLEAR, INSERT, or FETCH.  The default for those is on DN-MODEL, although it 
;;; should probably be here.
;;;

(define-predicate-method (say predication) (&optional (stream *standard-output*))
  ;; default implementation of the SAY protocol.  If used, this is usually overridden
  ;; at the predicate level, not the model level 
  (format stream "~S" self))

;;; the defaults for predication are to complain

(define-predicate-method (tell predication) (truth-value justification)
  (declare (ignore justification truth-value))
  (error "The TELL operation for the predicate ~S is not defined"
	 (predication-predicate self)))

(define-predicate-method (untell predication) ()
  (error "The UNTELL operation for the predicate ~S is not defined"
	 (predication-predicate self)))

(define-predicate-method (untell predication :around) ()
  (if (has-been-in-database-p self)
      (call-next-method)
      (ask self #'(lambda (backward-support)
		    (untell (ask-database-predication backward-support)))
	   :do-backward-rules nil
	   :do-questions nil)))

(define-predicate-method (ask predication) (truth-value continuation do-backward-rules do-questions)
  (declare (ignore truth-value continuation do-backward-rules do-questions))
  (error "The ASK operation for the predicate ~S is not defined"
	 (predication-predicate self)))

(defun parse-justification (justification)
  ;; parse up the justification into pieces to hand to justify
  #-sbcl (declare (values mnemonic true-supporters false-supporters unknown-supporters))
  (etypecase justification
    (null
      ;; not supplied, so default it from *support*
      (loop for supporter in *support* 
	    for truth-value = (predication-truth-value supporter)
	    if (= truth-value +true+) collect supporter into true-support
	    else if (= truth-value +false+) collect supporter into false-support
	    else if (= truth-value +unknown+) collect supporter into unknown-support
	    else do (error "Contradictory truth-value of ~S in *support*: ~S" supporter *support*)
	    finally (return (values *running-rule* true-support false-support unknown-support))))
    (symbol
      ;; no particular supporters
      (values justification nil nil nil))
    (cons
      ;; standard thing to hand off to justify
      (destructuring-bind (mnemonic &optional true-support false-support unknown-support)
	  justification
	(values mnemonic true-support false-support unknown-support)))))

(define-predicate-method (tell default-tell-model) (truth-value justification)
  ;; this works in a couple of stages, using other elements of the protocol:
  ;;   1. insert this into the virtual database.
  ;;   2. justify it.  At that point, the truth value gets set.  That will call
  ;;       notice-truth-value-change, which (among other things), will fire forward rules.
  (multiple-value-bind (database-predication new-p)
      (insert self)
    (when new-p
      (setf (predication-bits database-predication)
	    (logior (logandc2 (predication-bits database-predication)
			      ;; clear these fields
			      (make-predication-bits :truth-value -1
						     :has-been-in-database -1
						     :ive-been-in-before -1
						     :tms-bits -1
						     :ive-been-untold -1))
		    ;; and set them to
		    (make-predication-bits :truth-value +unknown+
					   :has-been-in-database 1
					   :ive-been-in-before 0
					   :tms-bits 0
					   :ive-been-untold 0))))
    (unless (eq justification :none)
      (let ((final-justification (or justification *support* :premise)))
	(etypecase final-justification
	  (symbol (justify database-predication truth-value final-justification))
	  (cons (apply #'justify database-predication truth-value final-justification)))))
    (values database-predication new-p)))

(define-predicate-method (untell default-tell-model) ()
  (unjustify self)
  (uninsert self)
  nil)

(define-predicate-method (untell default-protocol-implementation-model :before) ()
  (Rete-network-delete-predication self)
  nil)

;;; Default implementations of the 5 TMS generics.

(define-predicate-method (certainty-factor default-protocol-implementation-model) ()
  nil)

(define-predicate-method (justify default-tell-model) (truth-value &optional mnemonic true-support false-support unknown-support)
  (declare (ignore mnemonic true-support false-support unknown-support))
  ;; make some args be optional?
  ;; justify a predication.  this is really a stub until the rest of the tms.
  ;;   first, set the truth-value of the predication to what we're given.
  ;;   second, mess with tracing.
  ;;   third, set the ive-been-in-before bit to true (note this doesn't do that)
  ;;   fourth, if this is not the same truth value as before, notice that and maybe run postponed rules.
  (with-slots (bits) self
    (let* ((old-truth-value (predication-bits-truth-value bits))
	   (old-state bits)
	   (truth-changed-p (not (eql old-truth-value truth-value))))
      (setf (predication-bits-truth-value bits) truth-value)
      (when truth-changed-p
	(notice-truth-value-change self old-truth-value)
	;; truth-value has changed, allow noticers to run
	(act-on-truth-value-change self old-truth-value old-state)))))

(define-predicate-method (notice-truth-value-change default-tell-model) (old-truth-value)
  ;; default noticer, does nothing.
  (declare (ignore old-truth-value))
  nil)

(define-predicate-method (act-on-truth-value-change default-tell-model) (old-truth-value &optional old-state)
  ;; default acter, does nothing
  (declare (ignore old-truth-value old-state))
  nil)

;(define-predicate-method (justify default-protocol-implementation-model :after) (&rest ignore)
;  ;; calling justify from top-level might cause stimulate to get called.
;  ;; stimulate might try to run a rule, which might cause queuing.
;  ;; that queuing necessitates this.
;  (when (and *something-in-fwrd-q* (not *forward-queue-emptying-p*))
;    (run-forward-queue)))

(define-predicate-method (notice-truth-value-change default-protocol-implementation-model :before) (old-truth-value)
  ;; If it's the updating pass and there's a truth value change, then
  ;; update the rete network.
  (with-slots (bits) self
    (when (not (eql old-truth-value (predication-bits-truth-value bits)))
      (rete-network-retract-predication self))))

;;; Before any other truth value change noticing, first see
;;; if this is first time this guy is coming in and if so
;;; map over the forward rule triggers now.

(define-predicate-method (act-on-truth-value-change default-protocol-implementation-model :before) (old-truth-value &optional old-state)
  (declare (ignore old-state))
  (with-slots (bits) self
    (when (and (eql old-truth-value +unknown+)
	       ;; we only want to do this mapping the first time the guy
	       ;; has a real truth value.
	       (zerop (predication-bits-ive-been-in-before bits)))
      (setf (predication-bits-ive-been-in-before bits) 1)
      (let ((somebody-emptying-queue *forward-queue-emptying-p*)
	    (*forward-queue-emptying-p* t))
	(unwind-protect
	    (progn
	      (if *delay-rule-triggering*
		  (push self *delay-rule-trigger-list*)
		  (flet ((continuation (rete-node)
			   #+(or genera cloe) (declare (sys:downward-function))
			   ;; stop doing this if someone flushed this guy.
			   ;; this is because a rule could be triggered before this finishes
			   ;; mapping over all forward triggers.
			   (when (zerop (predication-bits-ive-been-untold bits))
			     (Rete-network-match-predication Rete-node self))))
		    (declare (dynamic-extent #'continuation))
		    (map-over-forward-rule-triggers self #'continuation)))
	      (unless *delay-rule-triggering*
		(when (and *something-in-fwrd-q* (not somebody-emptying-queue))
		  (run-forward-queue))))
	  (unless (or somebody-emptying-queue *delay-rule-triggering*)
	    (clear-forward-queue)))))))

(defmethod continue-suspended-forward-rule-triggering ((self default-protocol-implementation-model))
  (with-slots (bits) self
    (when (zerop (predication-bits-ive-been-untold bits))
      (flet ((continuation (Rete-node)
	       (declare #+(or genera cloe) (sys:downward-function))
	       ;; stop doing this if someone flushed this guy.
	       ;; this is because a rule could be triggered before this finishes
	       ;; mapping over all forward triggers.
	       (when (zerop (predication-bits-ive-been-untold bits))
		 (Rete-network-match-predication Rete-node self))))
	(declare (dynamic-extent #'continuation))
	(map-over-forward-rule-triggers self #'continuation)))))

(define-predicate-method (act-on-truth-value-change default-protocol-implementation-model :after) (old-truth-value &optional old-state)
  (declare (ignore old-state))
  (declare (ignore old-truth-value))
  (with-slots (bits) self
    (let ((truth-value (predication-bits-truth-value bits)))
      (when (not (= truth-value +unknown+))
	;; if value is now not +unknown+, see if postponed rules want to fire.
	(stimulate self truth-value)))))

(define-predicate-method (unjustify predication :around) (&optional justification)
  (cond ((has-been-in-database-p self)
	 (call-next-method))
	((not (null justification))
	 (error "You are trying to remove a specific justification ~s from a predication which isn't in the database ~s"
		justification self))
	(t (fetch self #'unjustify))))

(define-predicate-method (unjustify default-tell-model) (&optional justification)
  ;; default unjustifyor
  (declare (ignore justification))
  (with-slots (bits) self
    (let* ((old-truth-value (predication-bits-truth-value bits))
	   (truth-changed-p (not (eql old-truth-value +unknown+))))
      (setf (predication-bits-truth-value bits) +unknown+)
      (when truth-changed-p
	(notice-truth-value-change self old-truth-value)
	;; truth-value has changed, allow noticers to run
	(act-on-truth-value-change self old-truth-value bits))))
  nil)

(define-predicate-model trivial-tms-mixin () ())

(define-predicate-method (support trivial-tms-mixin) (&optional filter)
  ;; default support
  (declare (ignore filter))
  (list self))

(define-predicate-method (nontrivial-tms-p trivial-tms-mixin) ()
  ;; default is no TMS
  nil)

(define-predicate-method (current-justification trivial-tms-mixin) ()
  self)

(define-predicate-method (all-justifications trivial-tms-mixin) ()
  (list self))

(defmethod destructure-justification ((self trivial-tms-mixin))
  (values "Predication justified without a TMS" nil nil nil))

(define-predicate-method (ask default-ask-model) (truth-value continuation do-backward-rules do-questions)
  ;;
  ;; notice that by the time this guy runs he has a definite truth value (+true+ or +false+)
  ;; (no queries for +unknown+ please).  Therefore, when he goes through the rete network
  ;; code later on, he won't get postponed until coming in.
  ;;
  ;; first get stuff from the database and call the continuation on that.
  (ask-data self truth-value continuation)
  ;; Now go get stuff from rules.
  (when do-backward-rules
    (ask-rules self truth-value continuation do-questions))
  ;; now go hack questions
  (when do-questions
    (ask-questions self truth-value continuation))
  ;; make it clear that there is no interesting return value
  (values))

(define-predicate-method (ask-data default-ask-model) (truth-value continuation)
  (fetch self
	 #'(lambda (database-predication)
	     (let ((database-bits (predication-bits database-predication)))
	       (when (or (null truth-value)
			  (= (predication-bits-truth-value database-bits)
			     truth-value))
		 ;; the truth value we're looking for matches the database predication
		 (with-unification
		   ;; if the database predication has variables, copy it
		   ;; so the database isn't side-effected
		   (with-slots (statement) self
		     (if (zerop (predication-bits-has-logic-variables database-bits))
			 (unify statement (predication-statement database-predication))
			 (unify statement
				(copy-object-if-necessary
				  (predication-statement database-predication)))))
		   ;; the unification succeeded, so call the continuation
                   (with-stack-list (backward-support self
                                                      truth-value
                                                      database-predication)
		     (funcall continuation backward-support))))))))


;;; the entries in the backward trigger discrimination net
;;; Needs to be here to come before its first use.
(defstruct backward-trigger
  rule
  importance)

(define-predicate-method (ask-rules default-ask-model) (truth-value continuation do-questions)
  (let ((backward-importance-queue nil))	;only make it if you need it (speed bum)
    (flet ((queue-trigger (trigger importance)
	     (unless backward-importance-queue
	       ;; create it if not there already
	       (setq backward-importance-queue
		     (allocate-resource 'backward-importance-queue-resource)))
	     (enqueue-backward backward-importance-queue trigger importance))
	   (run-trigger (trigger)
	     (trigger-backward-rule (backward-trigger-rule trigger)
				    self truth-value
				    continuation do-questions)))
      (map-over-backward-rule-triggers
	self
	;; call this on all backward triggers that might unify with me.
	#'(lambda (trigger)
	    (let ((importance (backward-trigger-importance trigger)))
	      (cond (importance
		     ;; this rule has an importance
		     (queue-trigger trigger importance))
		    (t
		     (run-trigger trigger))))))
      (when backward-importance-queue
	;; queue has been created, so process it and deallocate it.
	;; above so that continuations won't get consed.)
	(run-backward-queue backward-importance-queue #'run-trigger)
	(deallocate-resource 'backward-importance-queue-resource backward-importance-queue)))))

(defun trigger-backward-rule (rule predication truth-value continuation do-questions)
  (declare #-sbcl(dynamic-extent continuation)
	   #+genera (dbg:invisible-frame joshua-internals))
  (incf *backward-fire-count*)
  ;; we're now 1 deeper in rules.
  (funcall rule predication truth-value continuation do-questions))

(define-predicate-method (ask-questions default-ask-model) (truth-value continuation)
  (let ((backward-question-importance-queue nil)) ;only make it if you need it (speed bum)
    (map-over-backward-question-triggers
      self
      #'(lambda (question)
	  (let ((importance (question-info-importance (question-info question))))
	    (cond (importance
		    ;; this question has an importance attached to it
		    (unless backward-question-importance-queue
		      (setq backward-question-importance-queue
			    (allocate-resource 'backward-importance-queue-resource)))
		    (enqueue-backward backward-question-importance-queue question importance))
		  (t
		    ;; no importance, so do it now
		    (ask-question question self truth-value continuation))))))
    (when backward-question-importance-queue
      ;; someone queued some backward questions, so run them
      (run-backward-queue backward-question-importance-queue
			  #'(lambda (question)
			      (ask-question question self truth-value continuation)))
      (deallocate-resource 'backward-importance-queue-resource
			   backward-question-importance-queue))))

(defparameter *forward-trigger-discrimination-net*
	      (make-discrimination-net-node :token '*begin-forward-trigger*)
  "Default general purpose indexing scheme for trigger patterns of forward rules.")

(defparameter *backward-trigger-discrimination-net*
	      (make-discrimination-net-node :token '*begin-backward-trigger*)
  "Default general purpose indexing scheme for trigger patterns of backward rules.")


(defparameter *question-discrimination-net*
	      (make-discrimination-net-node :token '*begin-backward-question*)
  "Default general purpose indexing scheme for backward questions.")

(define-predicate-method (prefetch-forward-rule-matches default-protocol-implementation-model) (context continuation)
  ;; ask for real data with no truth value check
  ;; Notice that if you don't have an ask-data method, you need one of these
  ;; to do the right thing.
  (declare (ignore context))
  (ask-data self nil
	    #'(lambda (derivation)
		(funcall continuation (ask-database-predication derivation)))))

(define-predicate-method (locate-forward-rule-trigger default-protocol-implementation-model)
			 (truth-value continuation context rule-name)
  (declare (ignore context truth-value rule-name))
  (let ((dn-node (dn-tell-predication self *forward-trigger-discrimination-net*)))
    (multiple-value-bind (new-triggers something-changed the-canonical-node) 
	(funcall continuation (discrimination-net-node-info-or-table dn-node))
      (when something-changed
	(setf (discrimination-net-node-info-or-table dn-node) new-triggers))
      the-canonical-node)))

(define-predicate-method (locate-backward-rule-trigger default-protocol-implementation-model)
			 (truth-value continuation context rule-name)
  (declare (ignore context truth-value rule-name))
  (let ((dn-node (dn-tell-predication self *backward-trigger-discrimination-net*)))
    (multiple-value-bind (new-triggers something-changed)
	(funcall continuation (discrimination-net-node-info-or-table dn-node))
      (when something-changed
	(setf (discrimination-net-node-info-or-table dn-node) new-triggers)))))

(define-predicate-method (locate-backward-question-trigger default-protocol-implementation-model)
			 (truth-value continuation context question-name)
  (declare (ignore context truth-value question-name))
  (let ((dn-node (dn-tell-predication self *question-discrimination-net*)))
    (multiple-value-bind (new-triggers something-changed)
	(funcall continuation (discrimination-net-node-info-or-table dn-node))
      (when something-changed
	(setf (discrimination-net-node-info-or-table dn-node) new-triggers)))))

(define-predicate-method (map-over-forward-rule-triggers default-protocol-implementation-model) (continuation)
  ;; how to collect all forward triggers that might be interested in me
  (discrimination-net-fetch *forward-trigger-discrimination-net* self continuation))

(define-predicate-method (map-over-backward-rule-triggers default-protocol-implementation-model) (continuation)
  ;; how to collect all backward triggers that might be interested in me
  (discrimination-net-fetch *backward-trigger-discrimination-net* self continuation)) 

(define-predicate-method (map-over-backward-question-triggers default-protocol-implementation-model) (continuation)
  ;; how to collect all backward questions that might be interested in me
  (discrimination-net-fetch *question-discrimination-net* self continuation)) 

(define-predicate-method (add-forward-rule-trigger default-protocol-implementation-model)
			 (truth-value forward-trigger context rule-name)
  ;; add a trigger to the index of forward rules
  (locate-forward-rule-trigger self
			       truth-value
			       #'(lambda (triggers)
				   (install-and-intern-forward-rule-trigger forward-trigger triggers))
			       context
			       rule-name))

(define-predicate-method (add-backward-rule-trigger default-protocol-implementation-model)
			 (truth-value backward-trigger context rule-name)
  ;; add a trigger to the index of backward rules
  (locate-backward-rule-trigger self truth-value
			   #'(lambda (triggers)
			       (if (member backward-trigger triggers
					   :key #'backward-trigger-rule)
				   (values triggers nil)
				   (values (cons backward-trigger triggers) t)))
			   context
			   rule-name))

(define-predicate-method (add-backward-question-trigger default-protocol-implementation-model)
			 (truth-value question-trigger context question-name)
  ;; how to store a backward question.
  (locate-backward-question-trigger self truth-value
				    #'(lambda (triggers)
					(if (member question-trigger triggers)
					    (values triggers nil)
					    (values (cons question-trigger triggers) t)))
				    context
				    question-name))

(define-predicate-method (delete-forward-rule-trigger default-protocol-implementation-model)
			 (truth-value rule-name context)
  ;; used by undefrule
  (locate-forward-rule-trigger self
			       truth-value
			       #'(lambda (triggers)
				   (remove-and-deinstall-if-necessary-forward-rule-triggers
				     self rule-name triggers))
			       context
			       rule-name))

(define-predicate-method (delete-backward-rule-trigger default-protocol-implementation-model)
			 (truth-value rule-name context)
  ;; used by undefrule
  (locate-backward-rule-trigger self truth-value
			   #'(lambda (triggers)
			       (values (delete-if #'(lambda (trigger)
						      (eql rule-name (backward-trigger-rule trigger)))
						  triggers)
				       t))
			   context
			   rule-name))

(define-predicate-method (delete-backward-question-trigger default-protocol-implementation-model)
			 (truth-value question-name context)
  ;; how to unstore a backward question.
  (locate-backward-question-trigger self truth-value
				    #'(lambda (triggers)
					(if (member question-name triggers)
					    (values (delete question-name triggers) t)
					    (values triggers nil)))
				    context
				    question-name))


(defun symbol-tails (predication-maker)
  ;; return list of tails of statement that are headed by symbols
  (loop for token = (predication-maker-statement predication-maker) then (cdr token)
	while (consp token)
	;; needn't deal with tail variable, since variables can't ever be skipped anyway
	when (symbolp (car token)) collect token))

(define-predicate-method (positions-forward-rule-matcher-can-skip default-rule-compilation-model) ()
  ;; returns a list of tails of the statement that can be ignored.
  (symbol-tails self))

(define-predicate-method (compile-forward-rule-action default-rule-compilation-model) (then-part rule-name environment)
  ;; default method for compiling forward actions
  (declare (ignore then-part rule-name environment))
  ;; maintain *support* as justification
  `(tell-internal ,self  +true+ nil))

(define-predicate-method (expand-forward-rule-trigger default-rule-compilation-model) (name truth-value context bound-variables)
  (declare (ignore context bound-variables))
  `(:match ,self ,name ,truth-value))

(define-predicate-method (expand-backward-rule-action default-rule-compilation-model) (name truth-value other-ask-args context)
  (declare (ignore context))
  `(:match ,self ,truth-value ,name ,other-ask-args))

;; The default does nothing
(define-predicate-method (expand-backward-rule-trigger default-rule-compilation-model) (truth-value if-part)
    (declare (ignore truth-value))
    (values self if-part))

;;; In a recent (6/09) change, I changed what this is to return for :procedure nodes
;;; Now it should return (1) The code (2) The support-variable-name (3) The bound variables and 
;;; (4) The original pseudo-predication.
;;; I modified all such uses in Joshua, but this method catches any that don't do this and adds in 
;;; the last field which can be defaulted to the original thing with no harm.
(define-predicate-method (expand-forward-rule-trigger predication :around) (name truth-value context bound-variables)
  (declare (ignore name truth-value context bound-variables))
  (let ((answer (call-next-method)))
    (if (and (eql (first answer) :procedure)
	     (null (cdddr (rest answer))))
	(append answer (list self))
      answer))
  )


