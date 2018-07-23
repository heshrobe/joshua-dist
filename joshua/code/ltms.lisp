;;; -*- Mode: Lisp; Package: LTMS; Syntax: Ansi-common-lisp -*-
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
;;; Created 7/25/87 17:37:30 by sgr running on GROUSE at SCRC.

(in-package :ltms)


;;;
;;; Conditions signalled by the LTMS.
;;;


(define-condition ltms-contradiction (tms-contradiction) ()
 ;; The following seems to be required due to defstruct-inclusion lossage.
 ;; It oughtn't to be necessary; the :REPORT for TMS-CONTRADICTION should suffice.
 #+++ignore
 (:report (lambda (c s)
	    (ji::report-tms-contradiction
	     s
	     (ltms-contradiction-premises c)
	     (ltms-contradiction-non-premises c)
	     (ltms-contradiction-justification c)
	     (ltms-contradiction-contradictory-predication c)))))

(setf (get 'ltms-contradiction 'tms-hard-contradiction-flavor) 'ltms-hard-contradiction)

(define-condition ltms-hard-contradiction (tms-hard-contradiction) ()
  #+++ignore
  (:report (lambda (c s)
	    (ji::report-tms-contradiction
	     s
	     (ltms-contradiction-premises c)
	     (ltms-contradiction-non-premises c)
	     (ltms-contradiction-justification c)
	     (ltms-contradiction-contradictory-predication c)))))


;;; Model for an LTMS (McAllester style).  This is, in effect, an implementation of
;;;   the 4 generic functions: justify, notice-truth-value-change (well, that's really 
;;;   the user's responsibility), support, and unjustify.
;;;

(define-predicate-model ltms-mixin
			((clauses-i-enter-into-positively :initform nil
			  :accessor predication-clauses-i-enter-into-positively
			  :initarg :clauses-i-enter-into-positively)
			 (clauses-i-enter-into-negatively :initform nil
			  :accessor predication-clauses-i-enter-into-negatively
			  :initarg :clauses-i-enter-into-negatively)
			 (supporting-clause :initform nil
			  :accessor predication-supporting-clause
			  :initarg :supporting-clause)
			 ;; used to hold the predication's bits from before
			 ;; when it was queued so that these can be passed onto 
			 ;; act-on-truth-value-change
			 (old-state :initarg :old-state :accessor old-state))
			(basic-tms-mixin)
			)

(defmethod init-plist append ((p ltms-mixin))
  (list :clauses-i-enter-into-positively (predication-clauses-i-enter-into-positively p)
	:clauses-i-enter-into-positively (predication-clauses-i-enter-into-positively p)
	:supporting-clause (predication-supporting-clause p)))


(define-predicate-model ltms-predicate-model
  (ji:statement)			;really in PREDICATION
  (ltms-mixin discrimination-net-data-mixin default-protocol-implementation-model))


(ji::defbitfields ltms-bits
 (old-truth		*unknown* :byte (byte 2 0))
 (in-noticer-queue	   0	  :byte (byte 1 2))
 (in-reprocess-queue	   0	  :byte (byte 1 3))
 (in-action-queue          0      :byte (byte 1 4)))
 
(defmethod old-truth ((self ltms-mixin))
  (with-slots () self
  ;; returns the truth-value of a predication
  (ltms-bits-old-truth (tms-bits self))))

(defmethod (setf old-truth) (new-truth-value (self ltms-mixin))
  (with-slots () self
  ;; sets the old-truth-value
  (let ((tms-bits (tms-bits self)))
    (setf (ltms-bits-old-truth tms-bits) new-truth-value)
    (setf (tms-bits self) tms-bits))))

(defmethod in-noticer-queue ((self ltms-mixin))
  (with-slots () self
  ;; returns the truth-value of a predication
  (ltms-bits-in-noticer-queue (tms-bits self))))

(defmethod (setf in-noticer-queue) (new-value (self ltms-mixin))
  (with-slots () self
  ;; sets the old-truth-value
  (let ((tms-bits (tms-bits self)))
    (setf (ltms-bits-in-noticer-queue tms-bits) new-value)
    (setf (tms-bits self) tms-bits))))

(defmethod in-action-queue ((self ltms-mixin))
  (ltms-bits-in-action-queue (tms-bits self)))

(defmethod (setf in-action-queue) (new-value (self ltms-mixin))
  (let ((tms-bits (tms-bits self)))
    (setf (ltms-bits-in-action-queue tms-bits) new-value)
    (setf (tms-bits self) tms-bits)))

(defmethod in-reprocess-queue ((self ltms-mixin))
  (with-slots () self
  ;; returns the truth-value of a predication
  (ltms-bits-in-reprocess-queue (tms-bits self))))

(defmethod (setf in-reprocess-queue) (new-value (self ltms-mixin))
  (with-slots () self
  ;; sets the in reprocess queue bits on
  (let ((tms-bits (tms-bits self)))
    (setf (ltms-bits-in-reprocess-queue tms-bits) new-value)
    (setf (tms-bits self) tms-bits))))


(defclass clause
	;; a structure internal to the LTMS, usually invisible to users.
	(justification)
	((name :initform 'anonymous :initarg :name :accessor clause-name)
	 (number-of-satisfiable-literals  :initform 0 :initarg :number-of-satisfiable-literals :accessor clause-number-of-satisfiable-literals)
	 (positive-literals :initform nil :initarg :positive-literals :accessor clause-positive-literals)
	 (negative-literals :initform nil :initarg :negative-literals :accessor clause-negative-literals)
	 (consequent :initform nil :initarg :consequent :accessor clause-consequent))
	)

(eval-when (:compile-toplevel :execute :load-toplevel)
 (proclaim '(inline make-clause))
 (defun make-clause (name positive-literals negative-literals)
  (make-instance 'clause :name name :positive-literals positive-literals :negative-literals negative-literals)))


(defmethod is-unit-clause ((self clause)) nil)


(defclass one-of-clause
	(clause)
	((creating-predication :initarg :creating-predication)))

(eval-when (:compile-toplevel :execute :load-toplevel)
 (proclaim '(inline make-one-of-clause))
 (defun make-one-of-clause (name positive-literals negative-literals creating-predication)
  (make-instance 'one-of-clause
   :name name
   :positive-literals positive-literals
   :negative-literals negative-literals
   :creating-predication creating-predication)))


(defclass assumption-clause
	  (clause)
  ((dont-use-me :initform nil :accessor assumption-clause-dont-use-me)
   (number-of-literals :initform 0 :accessor assumption-clause-number-of-literals :initarg :number-of-literals))
  )

(eval-when (:compile-toplevel :execute :load-toplevel)
 (proclaim '(inline make-assumption-clause))
 (defun make-assumption-clause (name positive-literals negative-literals number-of-literals)
  (make-instance 'assumption-clause
   :name name
   :positive-literals positive-literals
   :negative-literals negative-literals
   :number-of-literals number-of-literals)))

(defmethod is-unit-clause ((self assumption-clause)) t)

(defmethod print-object ((self clause) stream)
  (with-slots (NAME NUMBER-OF-SATISFIABLE-LITERALS POSITIVE-LITERALS NEGATIVE-LITERALS CONSEQUENT) self
  "Return a string that looks like FACT or  FACT or not FACT, for positive and negative nodes.
   By convention, Horn clauses are printed FACT and FACT implies FACT."
  (let ((reason name)
	(positives positive-literals)
	(negatives negative-literals))
    (let ((PositivesPP (mapcar #'(lambda (pred) (ji::print-without-truth-value pred nil)) Positives))
	  (NegativesPP (mapcar #'(lambda (pred) (ji::print-without-truth-value pred nil)) Negatives)))
      (Case (Length Positives)
	(0 (case (Length Negatives)
	     (0 (format stream "#<~S Illegal Clause>" reason))
	     (otherwise (format stream "#<~S not ~A~{ or not ~A~}>" reason (first NegativesPP) (rest NegativesPP)))))
	(1 (case (Length Negatives)
	     (0 (format stream "#<~S ~A>" reason (first PositivesPP)))
	     (otherwise (format stream "#<~S ~A ~{ and ~A~} implies ~A>" reason (first NegativesPP)
				(rest NegativesPP) (first PositivesPP)))))
	(otherwise
	  (case (Length Negatives)
	    (0 (format stream "<~S ~A~{ or ~A~}>" reason (first PositivesPP) (rest PositivesPP)))
	    (otherwise (format stream "#<~S ~A~{ or ~A~} or not ~A~{ or not ~A~}>" reason
			       (first PositivesPP) (rest PositivesPP)
			       (first NegativesPP) (rest NegativesPP))))))))))

(defmethod destructure-justification ((self clause))
  (with-slots (NAME NUMBER-OF-SATISFIABLE-LITERALS POSITIVE-LITERALS NEGATIVE-LITERALS CONSEQUENT) self
  (let ((consequent (clause-consequent self)))
    (flet ((destructure-for-consequent (the-consequent)
	     (let ((old-truth-value (predication-truth-value the-consequent)))
	       (cond
		 ((eql old-truth-value *true*)
		  (values (clause-name self)
			  the-consequent
			  (clause-negative-literals self)
			  (remove the-consequent (clause-positive-literals self))
			  nil))
		 ((eql old-truth-value *false*)
		  (values (clause-name self)
			  the-consequent
			  (remove the-consequent (clause-negative-literals self))
			  (clause-positive-literals self)
			  nil))
                 (t (values (clause-name self) the-consequent))))))
       (cond ((not (null consequent))
	      (destructure-for-consequent consequent))
	     ((= number-of-satisfiable-literals 1)
	      (destructure-for-consequent
	        (or
		  (loop for pos in (clause-positive-literals self)
			when (= (predication-truth-value pos) *true*)
			  do (return pos))
		  (loop for neg in (clause-negative-literals self)
			when (= (predication-truth-value neg) *false*)
			  do (return neg)))))
	     (t (values (clause-name self))))))))

(define-predicate-method (current-justification ltms-mixin) () supporting-clause)

(define-predicate-method (all-justifications ltms-mixin) ()
  (append clauses-i-enter-into-positively
	  clauses-i-enter-into-negatively))


;;;; Managing the TMS queues

;;; The following two parameters are required to prevent "blinking of noticers", i.e. firing a noticer
;;; for a transition to true then to unknown and then to true again during unjustification processing.
;;; Consider   a implies b, a implies c, b implies d, c implies d and d implies e.
;;; If you a goes out, then b goes out, causing d to go out, causing e to go out
;;; d then checks for other support and finds it in c, so d comes back in causing e to come back in
;;; then a finishes its work and causes b to go out, causing d to go out, causing e to go out.
;;; We only want to call one noticer for e!!!  
;;; To do this we queue up noticers during unjustification and then process them at the end.
;;; The queue consists of pairs of a predication and its original truth value.
;;; If a pred is already in the queue you don't make another entry.
;;; When emptying the queue you check whether its truth-value is different from the one
;;; in the queue.  If so the noticer executes.
;;; This is only done if we're under retraction, hence the switch.

(defparameter *ltms-noticer-queue* nil
  "When in retraction you need to queue noticer firings in this variable")

(defparameter *ltms-noticer-queue-back* nil
  "pointer to the last cell in the noticer queue, so that we can enqueue at the back efficiently")

(defparameter *ltms-action-queue* nil
  "When in retraction you need to queue action firings in this variable")

(defparameter *ltms-contradiction-queue* nil
  "Queue of contradictory clauses in this variable")

(defparameter *ltms-enqueuing* nil
  "This flag indicates that you need to queue noticer firings in *ltms-noticer-queue*")

(defvar *ltms-reestablish-queue* nil)

(defmacro with-ltms-queuing (&body body)
  `(let ((I-should-control-noticers (not *ltms-enqueuing*)))
    (let ((*ltms-enqueuing* t))
      ,@body
      (when I-should-control-noticers (empty-ltms-queues)))))

(define-predicate-method (clear ltms-mixin) (&optional clear-database clear-rules)
  (declare (ignore  clear-rules))
  ;; The LTMS keeps around a bunch of queues that become irrelevant when you flush all the predications.
  (when clear-database
    (setq *ltms-noticer-queue* nil
	  *ltms-noticer-queue-back* nil
	  *ltms-action-queue* nil
	  *ltms-contradiction-queue* nil
	  *ltms-enqueuing* nil
	  *ltms-reestablish-queue* nil)))

(defun enqueue-noticer (predication old-truth-value old-state)
  ;; Enqueue it if that's what we're doing, other notice the status change right now.
  (unless (= 1 (in-noticer-queue predication))
    (setf (old-truth predication) old-truth-value
	  (old-state predication) old-state)
    (setf (in-noticer-queue predication) 1)
    (cond ((null *ltms-noticer-queue*)
	   (push predication *ltms-noticer-queue*)
	   (setq *ltms-noticer-queue-back* *ltms-noticer-queue*))
	  (t (push predication (cdr *ltms-noticer-queue-back*))
	     (setq *ltms-noticer-queue-back* (cdr *ltms-noticer-queue-back*))))))

(defun pop-noticer-queue ()
  (let ((current-entry (pop *ltms-noticer-queue*)))
    (when current-entry
      (setf (in-noticer-queue current-entry) 0))
    (when (null *ltms-noticer-queue*)
      (setq *ltms-noticer-queue-back* nil))
    current-entry))

(defun enqueue-action (predication)
  ;; Enqueue it if that's what we're doing, other notice the status change right now.
  (unless (= 1 (in-action-queue predication)) ;not plusp?!  -W
    (setf (in-action-queue predication) 1)
    (push predication *ltms-action-queue*)))

(defun pop-action-queue ()
  (let ((current-entry (pop *ltms-action-queue*)))
    (when current-entry
	  (setf (in-action-queue current-entry) 0))
    current-entry))

(defun enqueue-reestablish (predication old-truth-value)
  (unless (= 1 (in-reprocess-queue predication))
    (setf (old-truth predication) old-truth-value)
    (setf (in-reprocess-queue predication) 1)
    (push predication *ltms-reestablish-queue*)))

(defun enqueue-contradiction (clause predication)
  (unless (assoc clause *ltms-contradiction-queue*)
    (push (list clause predication) *ltms-contradiction-queue*)))

(defun empty-ltms-queues ()
  ;; Notice all the delayed status changes
  (labels ((process-one-contradiction ()
             #+cloe (declare (sys:downward-function))
	     (let ((entry (pop *ltms-contradiction-queue*)))
	       (when entry
		 (destructuring-bind (clause cause) entry
		   (when (= 0 (clause-number-of-satisfiable-literals clause))
		     (backtrack-from-clause clause cause))))
	       entry))
	   (process-one-reestablish ()
	     #+cloe (declare (sys:downward-function))
	     (let ((predication (pop *ltms-reestablish-queue*)))
	       (when predication
		 (setf (in-reprocess-queue predication) 0)
		 (try-to-reestablish predication))
	       predication)))
    (declare (dynamic-extent #'process-one-contradiction #'process-one-reestablish))
    (loop until (and (null *ltms-contradiction-queue*)
		     (null *ltms-reestablish-queue*))
	  do (or (process-one-contradiction)
		 (process-one-reestablish)))
    ;; now before any propagations happen
    ;; do the first pass of noticing truth value changes
    (loop for predication = (pop-noticer-queue)
	  while predication
	  for old-truth = (old-truth predication)
	  when (not (eql old-truth (predication-truth-value predication)))
	    ;; we've gone through a real state transition
	    ;; call out to the noticer
	    do ;; (notice-truth-value-change predication old-truth)
	       ;; and enqueue it for action after all noticers
	       (enqueue-action predication))
    ;; Now it's okay to let stuff run free
    (setq *ltms-enqueuing* nil)
    (loop for predication = (pop-action-queue)
	  while predication
	  do (let ((previous-truth-value (old-truth predication)))
	       (unless (eql previous-truth-value (predication-truth-value predication))
		 ;; run one noticer, then look for more contradictions
		 (process-one-of-clauses predication)
		 (act-on-truth-value-change predication previous-truth-value
					    (old-state predication)))))))


(defmethod one-of-propagation ((self one-of-clause))
  (with-slots (NAME NUMBER-OF-SATISFIABLE-LITERALS POSITIVE-LITERALS NEGATIVE-LITERALS CONSEQUENT CREATING-PREDICATION) self
  (flet ((force-satisfaction ()
	   (dolist (pos positive-literals)
	     (when (= (predication-truth-value pos) *unknown*)
	       (justify pos *true* '(:assumption :choice) (list creating-predication))
	       (return-from force-satisfaction (values))))
	   (dolist (neg negative-literals)
	     (when (= (predication-truth-value neg) *unknown*)
	       (justify neg *false* '(:assumption :choice) (list creating-predication))
	       (return-from force-satisfaction (values))))
	   ;; after this, normal clause propagation will save your cookies
	   )
	 (clause-is-satisfied ()
	   (or (loop for pos in positive-literals thereis (= (predication-truth-value pos) *true*))
	       (loop for neg in negative-literals thereis (= (predication-truth-value neg) *false*)))))
    (unless (or (= number-of-satisfiable-literals 0) (clause-is-satisfied))
      (force-satisfaction)))))

(defmethod process-one-of-clauses ((self ltms-mixin))
  (with-slots (bits clauses-i-enter-into-positively clauses-i-enter-into-negatively) self
  "Force :ONE-OF clauses to make choices."
  (dolist (clause clauses-i-enter-into-positively)
    (unless (eql (predication-bits-truth-value bits) *true*)
      (when (eql (clause-name clause) :ONE-OF)
	(one-of-propagation clause))))
  (dolist (clause clauses-i-enter-into-negatively)
    (unless (eql (predication-bits-truth-value bits) *false*)
      (when (eql (clause-name clause) :ONE-OF)
	(one-of-propagation clause))))))

;;;
;;; Justifying
;;;

(define-predicate-method (justify ltms-mixin)
			 (truth-value &optional (reason :premise)
				      positive-support negative-support unknown-support)
  ;; LTMS-MIXIN's implementation of the JUSTIFY part of the Joshua protocol.
  (when unknown-support
    (error "Unknown support ~s provided but illegal for the LTMS" unknown-support))
  (cond
    ((eql truth-value *true*)
     (pushnew self negative-support))
    ((eql truth-value *false*)
     (pushnew self positive-support)))
  (when reason
    (let* ((number-of-literals (+ (length positive-support) (length negative-support)))
	   (name-means-assumption (if (listp reason) (eql (car reason) :assumption) (eql reason :assumption)))
	   (is-assumption (or name-means-assumption (eql number-of-literals 1))))
      (values 
       self
      (install-clause
	(cond ((eql reason :one-of)
	       (make-one-of-clause reason negative-support positive-support self))
	      (is-assumption
	       (let ((real-name (if (and name-means-assumption (listp reason)) (cadr reason) reason)))
		 (make-assumption-clause real-name negative-support positive-support number-of-literals)))
	      (t (make-clause reason negative-support positive-support))))))))

(defmethod install-clause ((self clause) &optional cause)
  (with-slots (NAME NUMBER-OF-SATISFIABLE-LITERALS POSITIVE-LITERALS NEGATIVE-LITERALS CONSEQUENT) self
    (when (is-unit-clause self)
      (let* ((consequent (first (or positive-literals negative-literals)))
	     (his-support (or (predication-clauses-i-enter-into-positively 
			       consequent)
			      (predication-clauses-i-enter-into-negatively
			       consequent))))
	(loop for clause in his-support
	    when (and (is-unit-clause clause)
		      (eql (clause-name clause) name))
		 ;; then this is a duplicate clause and we'll flush it
	    do (return-from install-clause (values)))))			   
  ;; how to install a clause as a justification.
  (loop for positive-supporter in positive-literals
	for actual-truth-value = (predication-truth-value positive-supporter)
	do (push self (predication-clauses-i-enter-into-positively positive-supporter))
	when (/= actual-truth-value *false*)
	  do (incf number-of-satisfiable-literals))
  (loop for negative-supporter in negative-literals
	for actual-truth-value = (predication-truth-value negative-supporter)
	do (push self (predication-clauses-i-enter-into-negatively negative-supporter))
	when (/= actual-truth-value *true*)
	  do (incf number-of-satisfiable-literals))
  (propagate self cause)
  (when (eql name :one-of)
    (one-of-propagation self))
  ;; An optimization: If you're installing a premise support for
  ;; an assertion that's still true or false, make him the supporting
  ;; clause, since he'll never go away.
  (when (and (is-unit-clause self) (eql (clause-name self) :premise))
    (let ((victim (or (first positive-literals) (first negative-literals))))
      (when (not (eql (predication-truth-value victim) *unknown*))
	(setf (predication-supporting-clause victim) self
	      (clause-consequent self) victim)))))
  self)

;;;
;;; Here's what TMS's are mostly good for, i.e., propagating truth values around.ste
;;;

(defmethod propagate ((self clause) &optional cause)
  (with-slots (NAME NUMBER-OF-SATISFIABLE-LITERALS POSITIVE-LITERALS NEGATIVE-LITERALS CONSEQUENT) self
  ;; How to propagate a justification.
  ;; it's possible to speed this up by keeping counts of satisfiable
  ;; literals, satisfiable positive literals, and satisfiable negative
  ;; literals, and then only searching the appropriate list.
  (with-ltms-queuing
    (cond ((= number-of-satisfiable-literals 1)
	   ;; check for positive guys
	   (loop for pred in positive-literals
		 when (= (predication-truth-value pred) *unknown*)
		   do (bring-in-with-truth-value pred self *true*)
		      ;; if he wins leave
		      (return (values))
		      ;; if no positive guy wins check negatives
		 finally (loop for pred in negative-literals
			       when (= (predication-truth-value pred) *unknown*)
				 do (bring-in-with-truth-value pred self *false*)
				    (return (values)))))
	  ((= number-of-satisfiable-literals 0)
	   ;; when it's a unit clause initialize consequent since that's unambiguous
	   (when (is-unit-clause self)
	     (setq consequent (first (or positive-literals negative-literals))))
	   (enqueue-contradiction self cause))))))

(defmethod bring-in-with-truth-value ((self ltms-mixin) new-clause truth-value)
  (with-slots (bits clauses-i-enter-into-positively clauses-i-enter-into-negatively supporting-clause) self
    ;; called by propagate to change the truth-value of a predication.
    (let ((old-truth-value (predication-bits-truth-value bits))
	  (old-state (predication-bits self)))
      (cond ((= old-truth-value *unknown*)
	     ;; the easy case, he's unknown and we want to set him
	     (setf (predication-bits-truth-value bits) truth-value)
	     ;; if i've become true (false) then i affect the satisfiability count
	     ;; of any clause that i enter into negatively (positively)
	     ;; because I can't satisfy that clause any longer
	     (enqueue-noticer self *unknown* old-state)
	     (setf (clause-consequent new-clause) self)
	     (setq supporting-clause new-clause)
	     (notice-truth-value-change self old-truth-value)
	     (cond
	       ((eql  truth-value *true*)
		(loop for clause in clauses-i-enter-into-negatively
		      while (= (predication-bits-truth-value bits) truth-value)
		      do (propagate-changed-truth-value-through-clause clause self)))
	       ((eql  truth-value *false*)
		(loop for clause in clauses-i-enter-into-positively
		      while (= (predication-bits-truth-value bits) truth-value)
		      do (propagate-changed-truth-value-through-clause clause self)))))
	    ((not (= old-truth-value truth-value))
	     ;; He's got a definite truth value now and it isn't the one
	     ;; we're trying to apply.  So set the appropriate supporting
	     ;; clause to the new justification and then backtrack.  Don't
	     ;; stimulate and don't propagate-changed-truth-value-through-clause, since we're only
	     ;; here temporarily.
	     (enqueue-contradiction new-clause self))))))

(defmethod propagate-changed-truth-value-through-clause ((self clause) originator)
  (with-slots (NAME NUMBER-OF-SATISFIABLE-LITERALS POSITIVE-LITERALS NEGATIVE-LITERALS CONSEQUENT) self
  ;; recursion case of propagation
  (decf number-of-satisfiable-literals)
  (propagate self originator)))

;;;
;;; Unjustifying
;;;

(define-predicate-method (unjustify ltms-mixin) (&optional (justification nil))
  ;; Remove this justification and the then out this guy if he doesn't have a justification
  (unless justification (setq justification supporting-clause))
  (when justification
    (uninstall-clause justification)))

(defmethod uninstall-clause ((self clause))
  (with-slots (NAME NUMBER-OF-SATISFIABLE-LITERALS POSITIVE-LITERALS NEGATIVE-LITERALS CONSEQUENT) self
  (let ((supportee nil)
	(his-truth-value nil))
    (loop for positive in positive-literals
	  for his-supporting-clause = (predication-supporting-clause positive)
	  when (eql his-supporting-clause self)
	    do (setq supportee positive his-truth-value (predication-truth-value positive))
	  doing (setf (predication-clauses-i-enter-into-positively positive)
		      (delete self (predication-clauses-i-enter-into-positively positive))))
    (loop for negative in negative-literals
	  for his-supporting-clause = (predication-supporting-clause negative)
	  when (eql his-supporting-clause self)
	    do (setq supportee negative his-truth-value (predication-truth-value negative))
	  do (setf (predication-clauses-i-enter-into-negatively negative)
		   (delete self (predication-clauses-i-enter-into-negatively negative))))
    (when supportee
      (update-predication-to-reflect-retraction supportee his-truth-value)))))

(defmethod update-predication-to-reflect-retraction ((self ltms-mixin) previous-truth-value)
  (with-slots (bits clauses-i-enter-into-positively clauses-i-enter-into-negatively supporting-clause) self
  ;; Initialize for noticer collection and begin the retraction phase
  (with-ltms-queuing
      (let ((old-truth-value (predication-bits-truth-value bits))
	    (old-state (predication-bits self)))
      (when (not (= 0 (logand previous-truth-value old-truth-value)))
	;; Currently has the definite truth value being removed, there's work to do.
	(setf (predication-bits-truth-value bits) *unknown*)
	;; indent as though this were a rule. The tracer still obeys this (JMH).
	(ji::with-another-rule
	  ;; clause your supporting clause field
	  (setq supporting-clause nil)
	  (cond
	    ((eql old-truth-value *true*)
	     ;; This guy used to be true and now he's retracted.
	     ;; Right now we'll recursively retract all his dependents
	     (loop for clause in clauses-i-enter-into-negatively
		   do (update-clause-to-reflect-retraction clause self)))
	    ((eql old-truth-value *false*)
	     (loop for clause in clauses-i-enter-into-positively
		   do (update-clause-to-reflect-retraction clause self)))))
	;; make queue entry to see later if there is other well founded support
	(enqueue-reestablish self old-truth-value)
	;; make queue entries to process Noticers after everything settles
	(enqueue-noticer self old-truth-value old-state)
	(notice-truth-value-change self old-truth-value))))))

(defmethod update-clause-to-reflect-retraction ((self clause) predication)
  (with-slots (NAME NUMBER-OF-SATISFIABLE-LITERALS POSITIVE-LITERALS NEGATIVE-LITERALS CONSEQUENT) self
  ;; When a predication gets retracted to unknown truth value we have to
  ;; check the clauses it enters into to see whether support for someone
  ;; else has evaporated.
  (incf number-of-satisfiable-literals)
  (when (> number-of-satisfiable-literals 1)
    (when (and consequent (not (eq consequent predication)))
      (update-predication-to-reflect-retraction consequent (predication-truth-value consequent))
      ;; clear your consequences out
      (setq consequent nil)))))

(defmethod try-to-reestablish ((self ltms-mixin))
  (with-slots (bits clauses-i-enter-into-positively clauses-i-enter-into-negatively) self
    ;; Don't try to reestablish if you already have a truth-value other than *unknown*
    ;; something has caused you to come in before we got here
    (when (= (predication-bits-truth-value bits) *unknown*)
      (let ((old-truth-value (old-truth self)))
	(cond
	 ((eql old-truth-value *true*)
	  (loop for clause in clauses-i-enter-into-positively
	      until (/= (logand *true* (predication-bits-truth-value bits)) 0)
	      do (propagate clause))
	  ;; Finally check if you just made some clause have exactly one
	  ;; satisfiable literal so that it should make someone else come in.
	  ;; this happens if the clause was contradictory just before your retraction.
	  (loop for clause in clauses-i-enter-into-negatively
	      do (propagate clause self)))
	 ((eql old-truth-value *false*)
	  ;; see explanation for the true case
	  (loop for clause in clauses-i-enter-into-negatively
	      until (/= (logand *false* (predication-bits-truth-value bits)) 0)
	      do (propagate clause))
	  (loop for clause in clauses-i-enter-into-positively
	      do (propagate clause self))))))))

(defmethod clausal-support-of-contradiction ((self ltms-mixin) generation &optional (assumption-already-seen nil))
  (with-slots (supporting-clause ji:generation-mark) self
  "Returns the unit clauses supporting a predication"
  (when (null ji:generation-mark) (setq ji:generation-mark -1))
  (let ((ive-seen-this-node-before (= ji:generation-mark generation)))
    (setq ji:generation-mark generation)
    (trace-contradiction-support-through-clause supporting-clause
						generation
						assumption-already-seen
						ive-seen-this-node-before
						))))

(defmethod trace-contradiction-support-through-clause ((self clause) generation &optional (assumption-already-seen nil) node-already-seen)
  (with-slots (NAME NUMBER-OF-SATISFIABLE-LITERALS POSITIVE-LITERALS NEGATIVE-LITERALS CONSEQUENT) self
  ;; First argument is an internal one used for node marking, to avoid cycles.
  ;; Second argument means that we've seen an assumption along this path already.
  ;;  So if I'm an assumption, I'm a second level one which means don't collect me.
  ;; The third argument means that this node has already been processed.
  (let* ((unit-clause? (is-unit-clause self))
	 (number-of-literals (when unit-clause? (assumption-clause-number-of-literals self))))
    (unless node-already-seen
      (when unit-clause? (setf (assumption-clause-dont-use-me self) nil)))
    (when (and assumption-already-seen unit-clause?)
      ;; If there was another assumption earlier on this path,
      ;; mark me as an ignorable assumption.
      (setf (assumption-clause-dont-use-me self) t))
    (cond (node-already-seen nil)
	  (unit-clause?
	   (if (and (= number-of-literals 1)
		    (= number-of-satisfiable-literals 0))
	       ;; If we're at a contradictory unit clause
	       ;; collect it plus the support that caused the contradicton
	       (cons self
		     (trace-contradiction-support-through-clause
		       (predication-supporting-clause consequent)
		       generation assumption-already-seen))
	       (progn
		  (unless (= number-of-literals 1)
		   ;; it has support, so we need to go further back just to mark
		   ;; anybody in my support that might also be getting collected.
		   (loop for supporter in positive-literals
		         unless (eq supporter consequent)
			   doing (clausal-support-of-contradiction supporter generation t))
		   (loop for supporter in negative-literals
		         unless (eq supporter consequent)
			   doing (clausal-support-of-contradiction supporter generation t)))
	         (list self))))
	  (t (nconc (loop for supporter in positive-literals
			  unless (eq supporter consequent)
			    nconc (clausal-support-of-contradiction supporter generation assumption-already-seen))
		    (loop for supporter in negative-literals
			  unless (eq supporter consequent)
			    nconc (clausal-support-of-contradiction supporter generation assumption-already-seen))))))))

#|

;;; these are now provided by a generic version in suppplied.
;;; that relies on current-justification, all-justifications and destructure-justification
;;; from this file.

(define-predicate-method (consequences ltms-mixin) ()
  (with-slots (bits clauses-i-enter-into-positively clauses-i-enter-into-negatively) self
    (let ((old-bits (predication-bits-truth-value bits)))
      (cond
	((eql old-bits *true*)
	 (loop for clause in clauses-I-enter-into-negatively
	       append (consequences clause)))
	((eql old-bits *false*)
	 (loop for clause in clauses-I-enter-into-positively
	       append (consequences clause)))))))

(defmethod consequences ((self clause))
  (with-slots (NAME NUMBER-OF-SATISFIABLE-LITERALS POSITIVE-LITERALS NEGATIVE-LITERALS CONSEQUENT) self
    (when consequent
      (cons consequent (consequences consequent)))))

|#


;;;
;;; backtracking
;;;

;;; This is the routine when we notice the problem at a predication.

(defmethod backtrack ((self ltms-mixin) &optional value-not-to-have condition-builder)
  (with-slots (bits supporting-clause) self
  (unless value-not-to-have (setq value-not-to-have (predication-bits-truth-value bits))) 
  (loop until (/= value-not-to-have (predication-bits-truth-value bits))
	do (let ((support (clausal-support-of-contradiction self (incf ji:*generation-counter*))))
	     (pick-and-unjustify-a-supporter support supporting-clause self condition-builder)))))

;;; This is the routine when we notice the problem at a clause
;;; the variable cause is the pred we were processing if we know that.
(defmethod backtrack-from-clause ((self clause) &optional cause)
  (with-slots (NAME NUMBER-OF-SATISFIABLE-LITERALS POSITIVE-LITERALS NEGATIVE-LITERALS CONSEQUENT) self
    ;; This case is odd.  You're trying to backtrack from an assumption and the reason is that
    ;; you already know that the consequent is in and with the other truth value.
    ;; So you just remove the assumption.
    (cond
     ((and (is-unit-clause self)
	   (eql name :assumption)
	   cause
	   (eql consequent cause))
      (uninstall-clause self))
     (t
      (loop until (or (/= 0 number-of-satisfiable-literals)		    
		      (and (is-unit-clause self)
			   (not (member self (possibly-refuting-clauses consequent)))))
	  for support = (trace-contradiction-support-through-clause self (incf ji:*generation-counter*))
	  do (pick-and-unjustify-a-supporter support self cause))))))

(defmethod possibly-refuting-clauses ((self ltms-mixin))
  (with-slots (bits clauses-i-enter-into-positively clauses-i-enter-into-negatively) self
    (let ((old-bits (predication-bits-truth-value bits)))
      (cond ((eql old-bits *false*) clauses-i-enter-into-positively)
	    ((eql old-bits *true*) clauses-i-enter-into-negatively)))))

;;; Given the primitive support, unjustify some of it, hopefully removing
;;; the contradiction.  Also build a nogood if more than one supporter
;;; is involved.

(defun pick-and-unjustify-a-supporter (support clause originator &optional condition-builder)
  ;; signal a condition with enough information to help anybody
  (loop with premises and non-premises
      for unit-clause in support
      if (member (clause-name unit-clause) '(:premise nogood))
      do (push unit-clause premises)
      else unless (assumption-clause-dont-use-me unit-clause)
      do (push unit-clause non-premises)
      finally
        (with-ltms-queuing
	    (build-nogood support)
          (restart-case
	      (if condition-builder
		  (error
		   (funcall condition-builder non-premises support originator clause))
		(error
		 'ltms-contradiction
		 :premises premises
		 :non-premises non-premises
		 :support support
		 :contradictory-predication originator
		 :justification clause))   
            (:unjustify-subset (predications)
		:report "Unjustify some support"
		:interactive
		  (lambda ()
		    (ji::contradiction-restart-interactively-unjustify-subset
		     premises non-premises))
	      (ji::contradiction-restart-unjustify-subset predications))))))

(defun build-nogood (unit-clauses)
  (loop for clause in unit-clauses
	for consequent = (clause-consequent clause)
	if (member consequent (clause-positive-literals clause))
	  collect consequent into positives
	else collect consequent into negatives
	finally (when (or positives negatives)
		  (let ((clause (make-clause 'nogood negatives positives)))
		    (install-clause clause)
		    (return clause)))))

;;;
;;; How we use the LTMS to control search.
;;;

;;; Someday we'll figure out how to do OR this way.

(define-predicate one-of (&rest disjuncts) (ltms-predicate-model)) 

(define-predicate-method (tell one-of :after) (truth-value justification)
  (declare (ignore justification))
  ;; bust this up into individuals, with :one-of justification
  (with-statement-destructured (&rest disjuncts) self
    (unless disjuncts
      (error "You can't mean ~S; it has to have some disjuncts." self))
    (unless (= truth-value *true*)
      (error "You can't TELL ~S with a non-*TRUE* truth-value." self))
    (loop for disjunct in disjuncts
	  for disjunct-truth-value = (case (predication-predicate disjunct)
				       ((not) *false*)
				       ((or and known provable)
					(error "Can't use ~S in ~S"
					       disjunct self))
				       (otherwise *true*))
	  for canonicalized-disjunct = (tell disjunct :justification :none)
	  if (= disjunct-truth-value *true*)
	    collect canonicalized-disjunct into true-guys
	  else collect canonicalized-disjunct into false-guys
	  finally
	    (justify self *false* :one-of false-guys true-guys nil))))

(define-predicate-method (say one-of) (&optional (stream *standard-output*))
  ;; how to talk about ONE-OF's
  (with-statement-destructured (&rest disjuncts) self
    (princ "One of " stream)
    (ji::format-textual-list disjuncts #'say :conjunction "or" :stream stream)
    (princ " must be *TRUE*." stream)))

(define-predicate contradiction () (ltms-predicate-model))

(define-predicate-method (act-on-truth-value-change contradiction) (old-truth-value &optional old-state)
  (declare (ignore old-state))
  ;; prevent conflicts from occurring -- this ensures TMS action to rectify the situation
  ;; if you ever assert [CONTRADICTION].
  (declare (ignore old-truth-value))
  (with-slots (bits) self
  (when (= (predication-bits-truth-value bits) *true*)
    ;; we only care when this becomes *true*, in which case we call the LTMS's backtracker
    (backtrack self)))) 

(define-predicate-method (say contradiction) (&optional (stream *standard-output*))
  ;; how to talk about CONTRADICTION's.
  (princ "There is a contradiction!" stream))

;;; This is a utility.  
;;; Assume with no argument just does tell with a justification of :assumption
;;; If there is a justification, then it builds a more complicated structure
;;; including an assumption that is added to the justification
(define-predicate assumption-for (predication) (ltms:ltms-predicate-model))

(defun assume (predication &key justification)
  (if (null justification)
      (tell predication :justification :assumption)
    (destructuring-bind (mnemonic &optional positive negative unknown) justification
       (let* ((interned-pred (tell predication :justification :none))
	      (assumption-pred (tell (predication-maker `(assumption-for ,interned-pred)) :justification :assumption)))
	 (values
	  (tell interned-pred :justification `(,mnemonic (,assumption-pred ,@positive) ,negative ,unknown))
	  assumption-pred)))))
