;;; -*- Mode: Commom-lisp; Package: ji; readtable: joshua; syntax: joshua -*-
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

(in-package :ji)

#-genera
(eval-when (:compile-toplevel :execute :load-toplevel) (enable-joshua))

;;; This condition is signalled by follow path and its kin.

(eval-when (:compile-toplevel :execute :load-toplevel)
  (define-condition bad-path (error)
    ((first-bad-token . #-lucid (:reader bad-path-first-bad-token :initarg :first-bad-token) #+lucid nil)
     (whole-path . #-lucid (:reader bad-path-whole-path :initarg :whole-path) #+lucid nil)
     (current-object . #-lucid (:reader bad-path-current-object :initarg :current-object) #+lucid nil)
     (remaining-path . #-lucid (:reader bad-path-remaining-path :initarg :remaining-path) #+lucid nil))
    (:report (lambda (self stream)
	       (format stream "The path ~s is incorrect.~%There is no part or slot of ~s named ~s.~%The rest of the path is ~s"
		       (bad-path-whole-path self)
		       (bad-path-current-object self)
		       (bad-path-first-bad-token self)
		       (bad-path-remaining-path self)))))

#+genera (scl:compile-flavor-methods bad-path))


;;;; The slot protocol.

;;; This predicate model is the mixin that links us to the slot protocol.
;;; It's defined here (but all the methods come much later) so that we can refer to 
;;; a writable-instance-variable from the slot methods.

(define-predicate-model slot-value-mixin
    ((my-slot :accessor predication-my-slot :initarg :my-slot))
  (no-variables-in-data-mixin)
					; (:conc-name "PREDICATION-")
  ; :writable-instance-variables
  )

(defmethod init-plist append ((p slot-value-mixin))
  (list :my-slot (predication-my-slot p)))

;;; These generic-functions constitute the slot protocol as understood
;;; so far.  These are methods on Slot mixins.

;;; Called to index the predication
(defgeneric insert-new-predication (slot predication value))

;;; Called to remove the predication from the index
(defgeneric uninsert-predication (slot predication))

;;; Called to update the value cache when a predication changes truth-value.
(defgeneric notice-new-value (slot value predication old-truth-value))

;;; Called after all updating is done to allow inferences to be made in a consistent
;;; view of the world.
(defgeneric act-on-new-value (slot predication old-truth-value)
  ;; can pcl can handle this?
  ;; Genra CLOS can
  (:method-combination progn)
  )

;;; Called to map over the values of a slot for ASK.
(defgeneric map-over-values (slot query continuation value-in-predication)
  (:documentation "The Slot Protocol's Implementation of Ask-Data")
  ;; I think that this isn't legal anywhere, but SBCL complains.
  #-sbcl(declare (dynamic-extent continuation))
  )

(defgeneric map-over-slot-backward-rule-triggers (slot continuation)
  (:documentation "The Slot Protocol's Implementation of Map-over-backward-rule-triggers")
  #-sbcl (declare (dynamic-extent continuation))
  )

(defgeneric map-over-slot-backward-question-triggers (slot continuation)
  (:documentation "The Slot Protocol's Implementation of Map-over-backward-Question-triggers")
  #-sbcl (declare (dynamic-extent continuation))
  )

(defgeneric slot-is-empty-p (slot)
  (:documentation "Does this slot have an explicit value?"))

(defgeneric slot-is-set-valued (slot)
  (:documentation "Does this slot store multiple values?"))

(defclass prototype-slot ()
    ((name :initarg :name)
     (my-object :initarg :my-object :accessor slot-my-object)
     (forward-triggers :initform nil :accessor slot-forward-triggers)
     (backward-triggers :initform nil :accessor slot-backward-triggers)
     (backward-question-triggers :initform nil :accessor slot-backward-question-triggers))
  )

(defmacro make-prototype-slot (name object)
  `(make-instance 'prototype-slot
		  :name ,name
		  :my-object ,object))

(defmethod print-object ((self prototype-slot) stream)
  (with-slots (name my-object) self
    (format stream "#<SLOT ~a of ~a>" name my-object)))



(defclass basic-slot
	()
	((name :initarg :name :accessor slot-name)
	 (current-value :accessor slot-current-value)
	 (current-predication :accessor slot-current-predication)
	 (all-predications :initform nil :accessor slot-all-predications)
	 (my-object :accessor slot-my-object :initarg :my-object)
	 (prototype-slot :accessor slot-prototype-slot))
  )

(defmethod reset-slot ((self basic-slot))
  (with-slots (current-value current-predication all-predications) self
    (slot-makunbound self 'current-value)
    (slot-makunbound self 'current-predication)
    (setq all-predications nil)))

;;; Prototype slots don't have anything to reset
(defmethod reset-slot ((self prototype-slot))
  nil)

;;; Two default methods that do nothing.
(defmethod notice-new-value ((self basic-slot) value predication old-truth-value)
  (declare (ignore value predication old-truth-value))
  nil)

;;; This should be :DEFAULT, if there were such a thing in CLOS. 
(defmethod act-on-new-value progn  ((self basic-slot) predication old-truth-value)
  (declare (ignore predication old-truth-value))
  nil)

;;; And one which does something.
(defmethod insert-new-predication ((self basic-slot) predication value)
  (with-slots (all-predications) self
    (let ((already-there (assoc value all-predications)))
      (if already-there
	  (values (cdr already-there) nil)
	(let ((canonicalized-predication `[,(class-name (class-of predication)) ,self ,value]))
	  (setf (predication-my-slot canonicalized-predication) self)
	  (push (cons value canonicalized-predication) all-predications)
	  (values canonicalized-predication t))))))

(defmethod map-over-slot-backward-rule-triggers ((self basic-slot) continuation)
  (loop for trigger in (slot-backward-triggers (slot-prototype-slot self))
	do (funcall continuation trigger)))

(defmethod map-over-slot-backward-question-triggers ((self basic-slot) continuation)
  (loop for question in (slot-backward-question-triggers (slot-prototype-slot self))
	do (funcall continuation question)))

(defmethod uninsert-predication ((self basic-slot) predication)
  (with-slots (all-predications) self
    (setq all-predications (delete predication all-predications :key #'cdr))))

(defmethod nth-superpart ((self basic-slot) n)
  (loop for i below n
	for object = (slot-my-object self) then (basic-object-superpart-object object)
	finally (return object)))

(defmethod role-name ((self basic-slot))
  (slot-value self 'name))

(defmethod path-name ((self basic-slot) &optional relative-to)
  (with-slots (my-object name) self
    (cond ((null my-object) (list name))
	  ((eql self relative-to) nil)
	  ((eql my-object relative-to) (list name))
	  (t (append (path-name my-object relative-to) (list name))))))

(defmethod print-object ((self basic-slot) stream)
  (format stream "#<SLOT ~a>" (path-name self)))

;;; This is used when querying with a truth-value of NIL at the ask-data level.
;;; It returns all possible values whether they're the current ones or not.
;;; This is here for a possible extention for "stateful predications" that can 
;;; be true in one state of the world and false in another
(defmethod map-over-all-values ((self basic-slot) query continuation value-in-query)
  (with-slots (all-predications) self
    (let ((query-predicate (predication-predicate query)))
      (loop for (his-value . predication) in all-predications
	  when (eql (predication-predicate predication) query-predicate)
	  doing (with-unification
		 (unify value-in-query his-value)
		 (stack-let ((backward-support `(,query ,+true+ ,predication)))
		   (funcall continuation backward-support)))))))


(defclass truth-maintained-slot-mixin
	()
	()
  ;;:abstract-flavor
  )

(defmethod truth-maintained? ((self truth-maintained-slot-mixin)) t)

(defclass non-truth-maintained-slot-mixin
	()
	()
  ;; :abstract-flavor
	)

(defmethod truth-maintained? ((self non-truth-maintained-slot-mixin)) nil)


;;; A mixin for slots whose value is a set.
;;; A set valued slot simply adds the value when it becomes
;;; true and deletes the value otherwise.
(defclass set-valued-slot-mixin
  ()
  ((current-value :initform nil)
   (current-predication :initform nil))
  )

(defmethod reset-slot ((self set-valued-slot-mixin))
  (with-slots (current-value current-predication all-predications) self
    (setq current-value nil
	  current-predication nil)
    (setq all-predications nil)))

(defmethod slot-is-empty-p ((self set-valued-slot-mixin))
  (with-slots (current-value) self
    (null current-value)))

(defmethod slot-is-set-valued ((self set-valued-slot-mixin)) t)

(defmethod notice-new-value ((self set-valued-slot-mixin) value predication old-truth-value)
  (declare (ignore old-truth-value))
  (with-slots (current-value current-predication) self
    (cond ((eql (predication-truth-value predication) +true+)
	   (pushnew value current-value)
	   (pushnew predication current-predication))
	  (t (setq current-value (delete value current-value))
	     (setq current-predication (delete predication current-predication))))))

(defmethod map-over-values ((self set-valued-slot-mixin) query continuation value-in-query)
  (with-slots (current-value current-predication) self
    (let ((query-predicate (predication-predicate query)))
      (loop for value in current-value
	    for predication in current-predication
	    when (eql (predication-predicate predication) query-predicate)
            doing (with-unification
                    (unify value-in-query value)
                    (stack-let ((backward-support `(,query ,+true+ ,predication)))
                      (funcall continuation backward-support)))))))

;;; The mixin for slots which are allowed to have only a unique value
;;; at a time.

(defclass unique-valued-slot-mixin
	  ()
  ((number-of-true-predications :initform 0 :initarg :number-of-true-predications))
  )

(defmethod slot-is-empty-p ((self unique-valued-slot-mixin)) (not (slot-boundp self 'current-value)))
(defmethod slot-is-set-valued ((self unique-valued-slot-mixin)) nil)

(defclass truth-maintained-unique-valued-slot-mixin
	  (unique-valued-slot-mixin)
  ()
  )

(defclass value-overriding-unique-valued-slot-mixin
	  (unique-valued-slot-mixin)
  ()
  )

(defmethod insert-new-predication :around ((self truth-maintained-unique-valued-slot-mixin) predication value)
  (declare (ignore predication value))
  (with-slots (all-predications) self
    (multiple-value-bind (canonical-pred new) (call-next-method)
      (when new
	(loop for (nil . other-predication) in all-predications
	      unless (eql canonical-pred other-predication)
		do (justify other-predication +false+ 'unique-valued-slot (list canonical-pred))))
      (values canonical-pred new))))

(defmethod notice-new-value :before ((self value-overriding-unique-valued-slot-mixin) value predication old-truth-value)
  (declare (ignore old-truth-value))
  (with-slots (current-value current-predication) self
    (when (and (eql (predication-truth-value predication) +true+)
	       (slot-boundp self 'current-value)
	       (not (eql value current-value)))
      ;; Should this be UNTELL?   [as opp. unjustify, maybe?  -Weav]
      (untell current-predication))))

(defmethod notice-new-value ((self unique-valued-slot-mixin) value predication old-truth-value)
  (with-slots (current-value current-predication number-of-true-predications) self
    (cond
      ((eql (predication-truth-value predication) +true+)
       ;; just blindly overwrite the value cell.  if this is the
       ;; unique true value, that's what we want.
       ;; If there turns out to be more than one true pred
       ;; we've got expensive contradiction handling to do anyhow.
       (setq current-value value)
       (setq current-predication predication)
       (incf number-of-true-predications))
      ;; Only if the old truth-value was true
      ;; then we need to determine if there are any true predications left
      ((eql old-truth-value +true+)
       (decf number-of-true-predications)
       (when (zerop number-of-true-predications)
	 ;; If there are now no true predications, then
	 ;; make the cache unbound.  Otherwise, somebody will
	 ;; write a true value into the slot sooner or later.
	 (slot-makunbound self 'current-value)
	 (slot-makunbound self 'current-predication))))))

(defmethod map-over-values ((self unique-valued-slot-mixin) query continuation value-in-query)
  (with-slots (current-predication current-value) self
    (when (slot-boundp self 'current-value)
      (when (eql (predication-predicate current-predication) (predication-predicate query))
	(with-unification
	  (unify value-in-query current-value)
	  (stack-let ((backward-support `(,query ,+true+ ,current-predication)))
	    (funcall continuation backward-support)))))))

(defun build-justification-from-backward-support (backward-support)
  (let (true false unknown)
    (labels ((build-justification-from-backward-support-internal (backward-support)
	       (when backward-support
		 (destructuring-bind (mnemonic truth-value type &rest stuff) backward-support
		   (declare (ignore mnemonic))
		   (let ((cached-result-in-query (when (listp type) (third type))))
		     (cond
		       ((typep type 'predication)
			(truth-value-case truth-value
			  (+true+ (push type true))
			  (+false+ (push type false))
			  (+unknown+ (push type unknown))))
		       (cached-result-in-query
			(truth-value-case truth-value
			  (+true+ (push cached-result-in-query true))
			  (+false+ (push cached-result-in-query false))
			  (+unknown+ (push cached-result-in-query unknown))))
		       (t (loop for thing in stuff
				doing (build-justification-from-backward-support-internal thing)))))))))
      (declare (dynamic-extent #'build-justification-from-backward-support-internal))
      (build-justification-from-backward-support-internal backward-support))
    (list (second (third backward-support)) true false unknown)))


(defun supporting-predications-from-backward-support (backward-support)
  (let (preds)
    (labels ((collect-preds-from-backward-support-internal (backward-support)
	       (when backward-support
		 (destructuring-bind (mnemonic truth-value type &rest stuff) backward-support
		   (declare (ignore mnemonic truth-value))
		   (let ((cached-result-in-query (when (listp type) (third type))))
		     (cond
		      ((typep type 'predication)
		       (push type preds))
		      (cached-result-in-query
		       (push cached-result-in-query preds))
		      (t (loop for thing in stuff
			     doing (collect-preds-from-backward-support-internal thing)))))))))
      (declare (dynamic-extent #'collect-preds-from-backward-support-internal))
      (collect-preds-from-backward-support-internal backward-support))
    preds))


;;; This mixin allows this slot to be connected to other slots.
;;; This allows "Wire Rules" to be run in the data representation.
;;; It's not clear that this makes sense with set-valued slots.
(defclass slot-with-equalities-mixin
	  ()
  ((equal-cells :initform nil :initarg :equal-cells :accessor slot-equal-cells)
   (block-propagation :initform nil :initarg :block-propagation :accessor slot-block-propagation))
  ) 

(eval-when (:compile-toplevel :execute :load-toplevel)
  (proclaim '(inline propagate-equality)))
(defun propagate-equality (value destination source-predication equality-predication)
  (tell `[,(predication-predicate source-predication) ,destination ,value]
	:justification `(equal-to (,equality-predication ,source-predication))))

(defmethod act-on-new-value progn ((self slot-with-equalities-mixin) predication old-truth-value)
  (declare (ignore old-truth-value))
  (when (eql (predication-truth-value predication) +true+)
    (with-slots (equal-cells current-value) self
      (loop for entry in equal-cells
	    for (cell justification) = entry
	    if (or (eql (predication-truth-value justification) +true+)
		   (truth-maintained? self))
	      do (propagate-equality current-value cell predication justification)
	    else do (push (cons current-value predication) (third entry))))))


(defmethod allow-equalities? ((self basic-slot)) nil)
(defmethod allow-equalities? ((self slot-with-equalities-mixin)) t)

(defmethod insert-equality ((self slot-with-equalities-mixin) other-cell equality-predication &optional (reflect t))
  (when (allow-equalities? other-cell)
    (with-slots (equal-cells current-value) self
      (let ((cell-already-there (assoc other-cell equal-cells)))
	(cond (cell-already-there (values (second cell-already-there) nil))
	      (t (push (list other-cell equality-predication nil) equal-cells)
		 (when reflect
		   (insert-equality other-cell self equality-predication nil)
		   (when (slot-boundp self 'current-value)
		     (if (listp (slot-current-value self))
			 (loop for value in (slot-current-value self)
			       for his-predication in (slot-current-predication self)
			       do (propagate-equality value other-cell his-predication equality-predication))
			 (propagate-equality (slot-current-value self) other-cell
					     (slot-current-predication self) equality-predication))))
		 (values equality-predication t)))))))

(defmethod uninsert-equality ((self slot-with-equalities-mixin) other-thing &optional (reflect t))
  (with-slots (equal-cells) self
    (setq equal-cells (delete other-thing equal-cells :key #'car))
    (when reflect (uninsert-equality other-thing self nil))))

;; This is tricky.
;;
;; We're doing a backward-chaining kind of inference  but we
;; take advantage of the fact that there's an underlying
;; forward-chaining mechanism  for equalities.
;;
;; If there is no value in this slot, we spread out an activation
;; ring to everyone it's connected to, ASK'ing each of those
;; slots to get the value.  We allow rules and questions to be
;; invoked on them.  
;;
;; Since we cache deduced values, if one of the slots chained to
;; deduces a value, he'll TELL that that's his
;; value, and then the value will propagate back to us by the
;; forward-chaining slot-with-equalities mechanisms.
;;
;; We pass Ignore in as the continuation of ASK because this is
;; just a before method that causes the other slots to do some work
;; The first thing the main method does is to check for values
;; explicitly in the slot.  As just argued, the values will be
;; there.  So he'll call the continuation on it and therefore we
;; don't need to notice the other slots succeeding.
;;
;; Finally notice that this is a symmetric backward chaining
;; problem which has the usual problem of looping back and forth.
;; We block that by setting the "block propagation" instance-variable in the
;; slots.

(defmethod map-over-values :before ((self slot-with-equalities-mixin) query continuation value-in-query)
  (declare (ignore continuation))
  (with-slots (equal-cells block-propagation) self
    (when (and (slot-is-empty-p self) (not block-propagation))
      (setq block-propagation t)
      (unwind-protect
	  (loop for equality-entry in equal-cells
		for cell = (first equality-entry)
		unless (slot-block-propagation cell)
		  do (ask `[,(class-name (class-of query)) ,cell ,value-in-query]
			  #'(lambda (stuff) stuff)	;meaning #'ignore
			  :do-backward-rules t :do-questions t))
	(setq block-propagation nil)))))


;;; This mixin allows specific actions to be attached to the slot which
;;; are run after the slot is updated.  This allows "meters" "dials"
;;; etc. to work easily.  It also allows integrity constraints to be
;;; checked.
(defclass slot-with-attached-actions-mixin
	  ()
  ((actions :initform nil :initarg :actions :accessor slot-actions))
  )

;;;  This should be PROGN combination, if VD PCL had it.
(defmethod act-on-new-value progn ((self slot-with-attached-actions-mixin) predication old-truth-value)
  (with-slots (current-value actions) self
    (let ((value (if (and (eql (predication-truth-value predication) +true+)
			  (slot-boundp self 'current-value))
		     current-value
		     (with-statement-destructured (ignore value) predication
		       (declare (ignore ignore))
		       value))))
      (loop for (nil . function) in actions
	    doing (funcall function self value predication old-truth-value)))))

;;; actions are indexed under a name
;;; This is preserved for reference; the unspecialized method below replaces it.
#|| #+++ignore
(defgeneric add-action (slot-or-path function &optional (name :action))
  (:function (if (typep slot-or-path 'slot-with-attached-actions-mixin)
		 (funcall (flavor:generic add-action) slot-or-path function name)
		 (let ((real-slot (follow-path-to-slot slot-or-path)))
		   (funcall (flavor:generic add-action) real-slot function name))))) 
||#

;;; This covers everything that's not a S.-W.-A.-A.-Mixin instance.
(defmethod add-action (path function &optional (name :action))
  (add-action (follow-path-to-slot path) function name))

(defmethod add-action ((self slot-with-attached-actions-mixin) function &optional (name :action))
  (with-slots (actions) self
    (let ((existing-action-pair (find name actions :key #'car)))
      (if existing-action-pair
	  (setf (cdr existing-action-pair) function)
	(push (cons name function) actions)))))

;;; This is preserved for reference; the unspecialized method below replaces it.
#|| #+++ignore
(defgeneric remove-action (slot-or-path &optional (name :action))
  (:function (if (typep slot-or-path 'slot-with-attached-actions-mixin)
		 (funcall (flavor:generic remove-action) slot-or-path name)
		 (let ((real-slot (follow-path-to-slot slot-or-path)))
		   (funcall (flavor:generic remove-action) real-slot name)))))
||#

(defmethod remove-action (path &optional (name :action))
  (remove-action (follow-path-to-slot path) name))

(defmethod remove-action ((self slot-with-attached-actions-mixin) &optional (name :action))
  (with-slots (actions) self
    (setq actions (delete name actions :key #'car)))) 


;;; This mixin notifies the parent object that the slot has been updated.
;;; This allows methods on the parent to do something.
(defclass object-notifying-slot-mixin
	()
	()
  ;:abstract-flavor
  )

(defgeneric notice-slot-change (object slot-name new-value justifying-predication))

(defmethod act-on-new-value progn ((self object-notifying-slot-mixin) predication old-truth-value)
  (declare (ignore old-truth-value))
  (when (slot-boundp self 'current-value)
    (notice-slot-change (slot-value self 'my-object)
			(slot-value self 'name)
			(slot-value self 'current-value)
			predication)))

(eval-when (:compile-toplevel :execute :load-toplevel)
(defvar *slot-options* '(:set-valued :truth-maintenance :equalities :attached-actions :object-notifying))

(defvar *default-slot-options*
	`(:set-valued nil :truth-maintenance nil :equalities t :attached-actions nil :object-notifying nil))

(defvar *slot-mixture* '((:set-valued
			   (t set-valued-slot-mixin)
			   (nil (:truth-maintenance
				  (t truth-maintained-unique-valued-slot-mixin)
				  (nil value-overriding-unique-valued-slot-mixin))))
			 (:truth-maintenance
			   (t truth-maintained-slot-mixin)
			   (nil non-truth-maintained-slot-mixin))
			 (:equalities (t slot-with-equalities-mixin))
			 (:attached-actions (t slot-with-attached-actions-mixin))
			 (:object-notifying (t object-notifying-slot-mixin))))


(defun decode-slot-options-to-flavors (options)
  (labels ((decode (entry)
	     (if (symbolp entry)
		 (if (null entry) nil (list entry))
		 (let* ((name (car entry))
			(present (getf options name))
			(subentry (second (assoc present (cdr entry)))))
		   (decode subentry)))))
    (declare (dynamic-extent #'decode))
    (loop for entry in *slot-mixture*
	  append (decode entry)))))

(defun decode-slot-options-to-slot-type-constructor (options)
  (loop with type-number = 0
	for bit-position below 5
	for option-name in *slot-options*
	when (let ((value (getf options option-name 'not-present)))
	       (if (eql value 'not-present)
		   (getf *default-slot-options* option-name)
		   value))
	  do (setq type-number (logior (ash 1 bit-position) type-number))
	finally (return (intern (string-upcase (format nil "slot-type-~d" type-number)) (find-package "JI")))))


(defmacro create-all-slot-types ()
  (let ((all-slot-names nil)
	(package (find-package "JI")))    
    (flet ((decode-integer-to-slot-options (i)
	     (loop for bit-position below 5
		   for option-name in *slot-options*
		   for bit-present = (not (zerop (logand (ash 1 bit-position) i)))
		   when bit-present
		     collect option-name
		   and collect t)))
      `(progn
	 ,@(loop for i below 32
		 for options = (decode-integer-to-slot-options i)
		 for name = (intern (string-upcase (format nil "slot-type-~d" i)) package)
		 do (push name all-slot-names)
		 collect `(defclass ,name
				  (,@(decode-slot-options-to-flavors options)
				   basic-slot)
				  ()))
	 ;;(compile-flavor-methods ,@all-slot-names)
	 ))))

(create-all-slot-types)



;;; associates name > object-type

(defvar *all-object-types* (make-hash-table :size 20))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (proclaim '(inline object-type-named)))
(defun object-type-named (type-name)
  (gethash type-name *all-object-types*))

(defclass object-type
	  ()
    ((typical-instance :initform nil :accessor object-type-typical-instance)
     (instances :accessor object-type-instances :initform nil)
     (slot-names :accessor object-type-slot-names :initarg :slot-names) ;just my local slots
     (part-names :accessor object-type-part-names :initarg :part-names)
     (all-slot-names :initform nil :accessor object-type-all-slot-names)	;includes names of all supertypes' slots
     (name :initarg :name :accessor object-type-name)
     (subtypes :initform nil :accessor object-type-subtypes)
     (supertypes :initform nil :accessor object-type-supertypes :initarg :supertypes)
     (all-rules :initform nil :accessor object-type-all-rules)
     (rebuild-list :initform nil :accessor object-type-rebuild-list)	;used when redefining an object type
     (rule-triggers :initform nil :accessor object-type-rule-triggers)
     ))

(defmethod print-object ((self object-type) stream)
  ;; how to print a slot with the ugly-printer
  (with-slots (name) self
    (format stream "#<TYPE ~s>" name)))

(defvar *building-prototype* nil)

(defmethod find-or-create-typical-instance ((self object-type))
  (with-slots (typical-instance) self
    (or typical-instance
	(let ((*building-prototype* t))
	  (make-instance (object-type-name self)
			 :typical-instance-of-type? t
			 :superpart-object nil
			 :role-name nil)))))

(defmethod map-over-subtypes ((self object-type) function)
  ;; I think that this isn't really what one wants
  ;; In genera this was called downward-funarg I think
  ;; SBCL doesn't like it
  #-sbcl (declare (dynamic-extent function))
  (let ((subtypes-visited (make-hash-table)))
    (labels ((do-one-subtype (type)
	       (unless (gethash type subtypes-visited)
		 (setf (gethash type subtypes-visited) t)
		 (funcall function type)
		 (loop for subtype in (object-type-subtypes type)
		       do (do-one-subtype subtype)))))
      (declare (dynamic-extent #'do-one-subtype))
      (do-one-subtype self))))

(defmethod map-over-supertypes ((self object-type) function)
  #-SBCL (declare (dynamic-extent function))
  (let ((supertypes-visited (make-hash-table)))
    (labels ((do-one-supertype (type)
	       (unless (gethash type supertypes-visited)
		 (setf (gethash type supertypes-visited) t)
		 (funcall function type)
		 (loop for supertype in (object-type-supertypes type)
		       do (do-one-supertype supertype)))))
      (declare (dynamic-extent #'do-one-supertype))
      (do-one-supertype self)))) 

(defmethod clean-up-for-redefinition ((self object-type))
  (with-slots (rebuild-list) self
  (let ((all-affected-types (make-hash-table)))
    (labels ((find-all-affected-stuff (type depth)
	       ;; Collect everything affected
	       ;; Subtypes and types whose typical object
	       ;; contains one of my objects as a subpart
	       ;; appropriately ordered.
	       (let ((his-current-depth (gethash type all-affected-types))
		     (next-depth (1+ depth)))
		 (when (or (null his-current-depth)
			   (> depth his-current-depth))
		   ;; If he's never been seen before or we're
		   ;; getting a deeper level, then keep going
		   ;; to the stuff he affects.  Otherwise, it's been
		   ;; seen witht the correct values already.
		   (setf (gethash type all-affected-types) depth)
		   (loop for subtype in (object-type-subtypes type)
			 do (find-all-affected-stuff subtype next-depth))
		   (loop for instance in (object-type-instances type)
			 for ultimate-superior = (ultimate-superpart instance)
			 when (basic-object-typical-instance-of-type? ultimate-superior)
			   do (let ((superior-type  (basic-object-type ultimate-superior)))
                                (when superior-type
                                  (find-all-affected-stuff superior-type next-depth))))))))
      (declare (dynamic-extent #'find-all-affected-stuff))
      (find-all-affected-stuff self 0))
    (let ((queue (make-heap)))
      ;; now heap sort them.
      (maphash #'(lambda (type depth)
		   (heap-insert queue type depth))
	       all-affected-types)
      ;; (loop for depth being the hash-values of all-affected-types with-key type
      ;;        do (heap-insert queue type depth))
      (setq rebuild-list (loop for type = (heap-remove queue)
			       until (null type)
			       collect type))))
  ;; now kill all the prototypes
  (loop for type in rebuild-list
	for his-prototype = (object-type-typical-instance type)
	when his-prototype
	  do (kill his-prototype))
  (values))) 


(defmethod initialize-instance :after ((self object-type) &rest stuff)
  (declare (ignore stuff))
  (with-slots (name subtypes supertypes all-rules instances rule-triggers) self
    (let* ((previous-type-object (object-type-named name))
	   (rebuild-list (when previous-type-object (object-type-rebuild-list previous-type-object))))
      (when previous-type-object
	(setq subtypes (object-type-subtypes previous-type-object)
	      all-rules (object-type-all-rules previous-type-object)
	      instances (object-type-instances previous-type-object)
	      rule-triggers (object-type-rule-triggers previous-type-object))
	(remove-object-type previous-type-object))
      (setf (gethash name *all-object-types*) self)
      (map-over-subtypes self #'(lambda (type) (setf (object-type-all-slot-names type) nil)))
      (loop for subtype in subtypes
	    do (add-supertype subtype self))
      (loop for supertype in supertypes
	    do (add-subtype supertype self))
      ;; This Cleans up the world when we make a new one of these
      ;; Links up the rule triggers to the new type
      ;; Rebuilds old instances
      (rebuild-dependent-types self (delete previous-type-object rebuild-list)))))

;;; Notice that this guy only pulls stuff from its supertypes
;;; and it does this just by building the prototypical instance.
;;; There's no issue of needing to push to subtypes since
;;; they're going to get rebuilt too and only after I'm rebuilt.

(defvar *im-handling-bad-rule-patterns* nil) 

(defvar *rebuilding-rules-for-type-redefinition* nil)

(defmethod rebuild-dependent-types ((self object-type) rebuild-list)
  (labels ((rebuild-one-type (type)
	     ;; Nil out his slot-names so that no stale data is kept.
	     ;; This will be recalculated on demand.
	     ;; The prototype will only get built when needed.
	     ;; Now relink up rules.
	     (rebuild-rules type)
	     (rebuild-instances type))
	   (rebuild-rules (type)
	     (let ((*rebuilding-rules-for-type-redefinition* t))
	       ;; Notice that if this is the first time this type is defined there
	       ;; won't be any so nothing will happen.
	       (let ((losing-match-nodes nil)
		     (losing-backward-rules nil)
		     (losing-questions nil)
		     (old-rules (object-type-all-rules type)))
		 (setf (object-type-all-rules type) nil)
		 (let ((*im-handling-bad-rule-patterns* t))
		   (loop for rule-name in old-rules
			 for debug-info = (or (rule-debug-info rule-name) (question-info rule-name))
			 for rule-type = (if (typep debug-info 'rule-debug-info)
					     (rule-debug-info-control debug-info)
					     :backward-question)
			 when debug-info
			   do (case rule-type
				(:forward
				  (loop for (pattern pattern-truth-value) in (rule-debug-info-triggers debug-info)
					do (block find-node
					     (loop for node in (rule-debug-info-network debug-info)
						   for truth-value = (rete-match-node-truth-value node)
						   when (eql pattern-truth-value truth-value)
						     do (loop for match-id in (rete-match-node-match-ids node)
							    when (and (eql rule-name (match-id-rule-name match-id))
								      ;; Note: This used to check eql, which is clearly wrong
								      ;; because when you actually build the network, if you find
								      ;; a pattern that is a variant of the one in the rule, then you use 
								      ;; it.  So this is correct here.  For backward rules
								      ;; I don't think we do that kind of interning.
									(variant pattern (match-id-pattern match-id)))
								do (handler-case
								     (add-forward-rule-trigger pattern
											       truth-value
											       node
											       (rule-debug-info-context debug-info)
											       rule-name)
								     (bad-path () (pushnew node losing-match-nodes)))
								   (return-from find-node (values)))
						 finally (error "No such pattern ~a ~a" 
								pattern pattern-truth-value)))))
				(:backward
				  (destructuring-bind (pattern truth-value) (first (rule-debug-info-triggers debug-info))
				    (handler-case 
				      (add-backward-rule-trigger pattern
								 truth-value
								 (rule-debug-info-network debug-info)
								 (rule-debug-info-context debug-info)
								 rule-name)
				      (bad-path () (push rule-name losing-backward-rules)))))
				(:backward-question
				  (destructuring-bind (pattern truth-value) (question-info-pattern debug-info)
				    (handler-case
				      (add-backward-question-trigger pattern truth-value
								     rule-name
								     (question-info-context debug-info)
								     rule-name)
				      (bad-path () (push rule-name losing-questions)))))))
		   (flet ((grumble-about-type-redefinition (base-type dependent-type kind-of-thing)
			    (if (eql base-type dependent-type)
				(format t "~&Because of the redefinition of type ~a the following ~a no longer make sense:"
					(object-type-name base-type) kind-of-thing)
				(format t
					"~&Because the redefinition of type ~a affects type ~a the following ~a no longer make sense:"
					(object-type-name base-type) (object-type-name dependent-type) kind-of-thing)))
			  (format-item-list (item-list)
			    (loop for item in item-list
				  do (princ item))))
		     (declare (dynamic-extent #'grumble-about-type-redefinition))
		     (when losing-match-nodes
		       (grumble-about-type-redefinition self type "forward rules")
		       (let ((rule-names nil))
			 (loop for match-node in losing-match-nodes
			       do (loop for match-id in (rete-match-node-match-ids match-node)
					for rule-name = (match-id-rule-name match-id)
					do (pushnew rule-name rule-names)
					   (format t "~&The ~a pattern of ~a is invalid"
						   (match-id-pattern match-id) rule-name)))
			 (format t "~&These rules are being removed, please redefine them")
			 (mapc #'undefrule rule-names)))
		     (when losing-backward-rules
		       (grumble-about-type-redefinition self type "backward rules")
		       (format-item-list losing-backward-rules)
		       (format t "~&These rules are being removed, please redefine them")
		       (mapc #'undefrule losing-backward-rules))
		     (when losing-questions
		       (grumble-about-type-redefinition self type "questions")
		       (format-item-list losing-questions)
		       (format t "~&These questions are being removed, please redefine them")
		       (mapc #'undefquestion losing-questions)))))))
	   (rebuild-instances (type)
	     (loop for existing-instance in (object-type-instances type)
		   unless (basic-object-typical-instance-of-type? (ultimate-superpart existing-instance))
		   do (rebuild-object existing-instance))))
    (declare (dynamic-extent #'rebuild-instances #'rebuild-rules #'rebuild-one-type))
    (rebuild-one-type self)
    (mapc #'rebuild-one-type rebuild-list)))

(defmethod all-slot-names ((self object-type))
  (with-slots (all-slot-names supertypes slot-names) self
    (unless all-slot-names
      (setq all-slot-names
	    (loop with all-names = slot-names
		  for type in supertypes
		  do (setq all-names (union all-names (all-slot-names type)))
		  finally (return all-names))))
    all-slot-names))

(defun undefine-object-type (type-name)
  (let ((type (object-type-named type-name)))
    (when type
      (kill type))))

;; Should this kill all its instances first?
(defmethod kill ((self object-type))
  ;; (flavor:remove-flavor name)
  (with-slots (name) self
    (setf (find-class name) nil)
    (remove-object-type self)))

(defvar *root*)

(defmethod all-dependent-instances ((self object-type))
  (let ((top-level-parts nil) (typical-instances nil))
    (labels ((one-more-time (type-object)
	       (loop for sub-type in (object-type-subtypes type-object)
		     doing (one-more-time sub-type))
	       (loop for instance in (object-type-instances type-object)
		     doing (do-an-instance instance)))
	     (do-an-instance (object)
	       (cond ((basic-object-typical-instance-of-type? object)
		      (pushnew object typical-instances))
		     ((or (eql (basic-object-superpart-object object) *root*)
			  (null (basic-object-superpart-object object)))
		      (pushnew object top-level-parts))
		     (t (do-an-instance (basic-object-superpart-object object))))))
      (declare (dynamic-extent #'one-more-time #'do-an-instance))
      (one-more-time self)
      (values top-level-parts typical-instances))))

(defmethod remove-object-type ((self object-type))
  (with-slots (name supertypes subtypes) self
    (remhash name *all-object-types*)
    (loop for supertype in supertypes
	  doing (remove-subtype supertype self))
    (loop for subtype in subtypes
	  doing (remove-supertype subtype self))))

(defmethod add-supertype ((self object-type) who)
  (with-slots (supertypes) self
    (push who supertypes)))

(defmethod add-subtype ((self object-type) who)
  (with-slots (subtypes) self
    (push who subtypes)))

(defmethod remove-supertype ((self object-type) who)
  (with-slots (supertypes) self
    (setq supertypes (delete who supertypes))))

(defmethod remove-subtype ((self object-type) who)
  (with-slots (subtypes) self
    (setq subtypes (delete who subtypes))))

(defmethod add-instance ((self object-type) object)
  (with-slots (instances typical-instance) self
    (pushnew object instances)
    ;; If this guy isn't part of a prototype structure
    ;; Then link up his slots to those of his prototype
    (unless *building-prototype*
      (labels ((link-up (prototype object)
		 (let ((slot-names (all-slot-names prototype)))
		   (loop for slot-name in slot-names 
		       for object-slot = (funcall slot-name object nil)
		       for prototype-slot = (funcall slot-name prototype nil)
		       do (setf (slot-prototype-slot object-slot) prototype-slot)
			  (unless (slot-is-empty-p object-slot)
			    ;;check if I already have values and trigger rules if so
			    ;; (trigger-rules-of-linked-up-slot object-slot prototype-slot)
			    ))
		   )))
	(declare (dynamic-extent #'link-up))
	(map-over-parallel-object-hierarchies (find-or-create-typical-instance self)
					      object #'link-up)))))

;; This isn't used right now, instead the telling of initializations
;; is delayed, see comment in rebuild-object
;; This gets called when a new object is created
;; and its slos already have init values that match rule triggers
;; Right now it's being called as each sub-object of the parent is built
;; in a bottom up fashion.  
;; It's possible that we might need to delay this until the full
;; object is built, then pass through the structure again.  Trick would be to bind
;; some special variable saying don't do this, I'll handle it later.  Motivation for that approach
;; is that a rule could fire and try to examine this object before all its parts are built and initialized.
;;
;; What if there's some form of delayed or queued triggering going on?

;; caller should have checked that the slot isn't empty
(defun trigger-rules-of-linked-up-slot (object-slot prototype-slot)
  (let ((forward-triggers (slot-forward-triggers prototype-slot))
	(slot-predication (slot-current-predication object-slot)))
    (if (slot-is-set-valued object-slot)
	(loop for predication in slot-predication
	    do (loop for trigger in forward-triggers do (rete-network-match-predication trigger predication)))
      (loop for trigger in forward-triggers do (rete-network-match-predication trigger slot-predication)))))

(defmethod set-typical-instance ((self object-type) object)
  (with-slots (typical-instance) self
    (setq typical-instance object)
    (copy-rule-triggers self)))

(defmethod remove-instance ((self object-type) object typical-instance-p)
  (with-slots (instances typical-instance) self
  (if typical-instance-p
      (setq typical-instance nil)
      (setq instances (delete object instances)))))

(defmethod copy-rule-triggers ((self object-type))
  (with-slots (typical-instance) self
    (labels ((copy-triggers (from-prototype to-prototype)
	       ;; Copy the triggers in those slots that from-prototype
	       ;; provides to to-prototype.
	       (loop for slot-name in (all-slot-names from-prototype)
		     for from-slot = (funcall slot-name from-prototype nil)
		     for to-slot = (funcall slot-name to-prototype nil)
		     do (setf (slot-forward-triggers to-slot)
			      (append (slot-forward-triggers from-slot)
				      (slot-forward-triggers to-slot)))
			(setf (slot-backward-triggers to-slot)
			      (append (slot-backward-triggers from-slot)
				      (slot-backward-triggers to-slot)))
			(setf (slot-backward-question-triggers to-slot)
			      (append (slot-backward-question-triggers from-slot)
				      (slot-backward-question-triggers to-slot)))))
	     (do-one-piece-of-substructure (prototype)
	       (flet ((do-one-level-of-type (from-type)
		      ;;; Copy triggers throughout the substructure of this guy
		      ;;; from the slots it inherits from from-type
			(let ((from-prototype (object-type-typical-instance from-type)))
			  (unless (or (null from-prototype)
				    (eql from-prototype prototype))
			    ;; Map over substructure and copy slots
			    (map-over-parallel-object-hierarchies from-prototype prototype #'copy-triggers)))))
		 (declare (dynamic-extent #'do-one-level-of-type))
		 ;; Map Over all supertypes doing this
		 ;; Copying rule triggers throughout its part structure
		 ;; that it gets from any of its supertypes.
		 (map-over-supertypes (basic-object-type prototype) #'do-one-level-of-type))))
      (declare (dynamic-extent #'copy-triggers #'do-one-piece-of-substructure))
      (map-over-object-hierarchy #'do-one-piece-of-substructure typical-instance))))



(defgeneric basic-object-body-builder (basic-object) (:method-combination progn))
(defgeneric basic-object-initializer (basic-object &key))
(defgeneric basic-object-prototype-builder (basic-object) (:method-combination progn))
(defgeneric basic-object-substructure-builder (basic-object) (:method-combination progn))

(defclass basic-object
	  ()
    ((superpart-object :initform nil :initarg :superpart-object :accessor basic-object-superpart-object)
     (role-name :initform nil :initarg :role-name :accessor basic-object-role-name)
     ;; This is the object-type-of predication all objects need this
     (type-predication :initform nil :initarg :type-predication :accessor basic-object-type-predication)
     (part-predication :initform nil :initarg :part-predication :accessor basic-object-part-predication)
     (subparts :initform (make-hash-table :size 10) :accessor basic-object-subparts)
     (type :reader basic-object-type)
     (typical-instance-of-type? :initform nil :initarg :typical-instance-of-type? :reader basic-object-typical-instance-of-type?)
     (rete-states :initform nil)))

(defclass tms-object-mixin
    ()
  ()
  )

(defmethod basic-object-body-builder progn ((self basic-object)) nil)
(defmethod basic-object-initializer ((self basic-object) &key &allow-other-keys) nil)
(defmethod basic-object-prototype-builder progn ((self basic-object)) nil)
(defmethod basic-object-substructure-builder progn ((self basic-object)) nil)

;;; This is the default for object-notifying slot interactions.
(defmethod notice-slot-change ((self basic-object) slot-name new-value justifying-predication)
  (declare (ignore slot-name new-value justifying-predication))
  (values))

(defmethod all-slot-names ((self basic-object))
  (with-slots (type) self
    (when type
      (all-slot-names type))))

(defmethod role-name ((self basic-object)) (slot-value self 'role-name))

(defmethod path-name ((self basic-object) &optional relative-to)
  (with-slots (superpart-object role-name typical-instance-of-type?) self
    (cond ((eql relative-to self) nil)
	  (superpart-object
	   ;;could be nconc?
	   (append (path-name superpart-object relative-to) (list role-name)))
	  (role-name (list role-name))
	  (typical-instance-of-type? (list self))
	  (t nil))))

(defmethod object-type-of ((self basic-object))
  (let ((type (slot-value self 'type)))
    (when type
      (object-type-name type))))

(defmethod superpart ((self basic-object)) (slot-value self 'superpart-object))

(defmethod ultimate-superpart ((self basic-object))
  (loop for thing = self then next
	for next = (superpart thing)
	until (null next)
	finally (return thing)))

(defmethod print-object ((self basic-object) stream)
  (declare (special *root*))
  (with-slots (typical-instance-of-type? type) self
    ;; how to print an object with the ugly-printer
    (let ((name (if (and (slot-boundp self 'type) type) (object-type-name type) (type-of self))))
      (if typical-instance-of-type?
          (format stream "#<TYPICAL-~a>" name)
        (let ((path (path-name self)))
          (when (and (null path) (eql self *root*))
            (setq path '(*root*)))
          (format stream "#<OBJECT ~a>" (or path '(unnamed))))))))

;;; apparently there is no need for error checking the case of running out of superparts

(defmethod nth-superpart ((self basic-object) n)
  (loop for i below n
	for object = self then (basic-object-superpart-object object)
	finally (return object)))

(defmethod subpart-named ((self basic-object) role-name)
  (with-slots (subparts) self
    (gethash role-name subparts)))

(defmethod add-part ((self basic-object) role-name part-object)
  (setf (gethash role-name (slot-value self 'subparts)) part-object))

(defmethod remove-part ((self basic-object) role-name part-object)
  (declare (ignore part-object))
  (remhash role-name (slot-value self 'subparts)))

;;; The &key &allow-other-keys is here to keep SBCL from complaining about a slot initialization that gets
;;; passed through to the make-instance method, which it interprets very strictly as
;;; not allowing a keyword arg that isn't either a slot initializer or a default-init-plist kind of thing.

(defmethod initialize-instance :after ((self basic-object) &rest plist &key &allow-other-keys)
  (apply #'rebuild-object self plist)
  (trigger-rules-when-created self))

(defmethod trigger-rules-when-created ((self basic-object))
  (with-slots (type) self
    (when (and type (not *building-prototype*))
      (if *delay-rule-triggering*
	  (push self *delay-rule-trigger-list*)
	(map-over-supertypes type
			     #'(lambda (his-type)
                                 #+cloe (declare (sys:downward-function))
				 (loop for trigger in (object-type-rule-triggers his-type)
				     do (rete-network-match-object trigger self))))))))

(defmethod continue-suspended-forward-rule-triggering ((self basic-object))
  (with-slots (type) self
    (when type
      (map-over-supertypes type
			   #'(lambda (his-type)
                               #+cloe (declare (sys:downward-function))
			       (loop for trigger in (object-type-rule-triggers his-type)
				     do (rete-network-match-object trigger self)))))))

(defmethod add-rete-state ((self basic-object) rete-state)
  (with-slots (rete-states) self
    (push rete-state rete-states)))

(defmethod rete-states ((self basic-object)) (slot-value self 'rete-states))

(defmethod rebuild-object ((self basic-object) &rest plist)
  (with-slots (superpart-object role-name type typical-instance-of-type?) self
    (let* ((type-object (object-type-named (class-name (class-of self))))
	   ;; Am I the top level thing being built or part of his sub-structure
	   ;;(parent (basic-object-superpart-object self))
	   ;;(Im-top-level (or (null parent) (eql parent *root*)))
	   )
      (setq type type-object)
      (when superpart-object (add-part superpart-object role-name self))
      (when type
	(with-atomic-action
	 (if *building-prototype*
	     ;; If we're building prototype structure run the guy who
	     ;; makes prototype slots
	     (basic-object-prototype-builder self)
	   ;; We're building a real object
	   ;; use the guy who builds instance slots.
	   (basic-object-body-builder self))
	 ;; Delay rule execution until after both the sub-structure is built
	 ;; and all of the initializatons have been run.
	 (basic-object-substructure-builder self)
	 ;; Now that it's completely built
	 ;; tell the type about it.
	 (cond (typical-instance-of-type?
		;; This will also collect all rule triggers from the supertypes.
		(set-typical-instance type-object self))
	       (t
		;; this links the object slots to those of its prototype.
		(add-instance type-object self)
		;; Now that it's rules are linked
		(unless *building-prototype*
		  ;; It might still be part of a prototype even if
		  ;; it itself isn't the prototype.  If it's not, I.e. if
		  ;; it's a real object, run the
		  ;; initializations.
		  ;; We don't run the initializations until all the sub-structure
		  ;; is built and linked up to prototypes (for rule triggering).
		  ;; This is because the initializations do Tell's which require the structure to be there
		  ;; already. See note below.
		  ;; So we delay calling this until the top-level guy is built
		  ;; He then maps down the part hierarchy doing the initializations
		  ;; for all the sub-structure.
		  ;; Examine Me: With the "(with-atomic-action wrapping, the explanation above may be wrong
		  (apply #'basic-object-initializer self plist)
		  ))))))))


;;; The motivating case: Imaging a rule that talks about the sub-structure of an object 
;;; (e.g. a plane with wings and stabilizers) and these sub-structures have initializations
;;; that fill in default values for some property of the wing that the rule talks about.
;;; Were we to do things the obvious way, then the Tell's corresponding to those initiazations
;;; would happen before the wing object was hooked up to the wing object in the plane prototype 
;;; (it would be hooked up to the prototype for wings in general) which is where the rule trigger 
;;; is.  This could be fixed by checking for predications that match the rule trigger as it's inserted
;;; and there's even code below that can do that in trigger-rules-of-linked-up-slot which could be called
;;; from add-object (which is where this happens).  However, this guarantees that rules won't get run
;;; until all the structure is built.


(defmethod kill ((self basic-object))
  (with-slots (subparts typical-instance-of-type? superpart-object role-name type) self
    (rete-network-delete-object self)
    (map-over-slots-of-object #'reset-slot self)
    (maphash #'(lambda (key object)
		 (declare (ignore key))
		 (kill object))
	     subparts)		 
    ;;(loop for object being the hash-values of subparts with-key key
    ;;      doing (progn key)
    ;;            (kill object))
    (when (and (slot-boundp self 'type) (slot-value self 'type))
      (remove-instance type self typical-instance-of-type?)
      (setq type nil))
    (when superpart-object
      (remove-part superpart-object role-name self))))

(defmethod kill :before ((object basic-object))
  ;; Kill the part-of relationship
  (let ((part-predication (basic-object-part-predication object))
	(type-predication (basic-object-type-predication object)))
    (when part-predication (unjustify type-predication)
    (when type-predication (unjustify part-predication)))))

;;; don't reveal *root* to the casual typer of c-sh-A

(setq *root* (make-instance 'basic-object :role-name nil))

(defun make-object (object-type &rest stuff &key name superpart-object &allow-other-keys)
  (when (null superpart-object) (setq superpart-object *root*)) 
  (when (subpart-named superpart-object name)
    (error "There is already a subpart of ~a named ~a"
	   superpart-object name))
  (let ((new-plist (copy-seq stuff)))
    (remf new-plist :name)
    (remf new-plist :superpart-object)
    (apply #'make-instance object-type :role-name name :superpart-object superpart-object new-plist)))

(defmethod type-of-predicate-for-object-type ((thing basic-object)) 'object-type-of)
(defmethod type-of-predicate-for-object-type ((thing tms-object-mixin)) 'ltms:object-type-of)
					      

(defmethod part-of-predicate-for-object-type ((thing basic-object)) 'named-part-of)
(defmethod part-of-predicate-for-object-type ((thing tms-object-mixin)) 'ltms:named-part-of)

(defmethod trigger-rules-when-created :before ((object basic-object))
  (let* ((object-type (object-type-of object))
	 (type-pred (when object-type `[,(type-of-predicate-for-object-type object) ,object ,object-type]))
	 (superior (basic-object-superpart-object object))
	 (role-name (role-name object))
	 (part-of-pred (when (and superior object-type) `[,(part-of-predicate-for-object-type object) ,superior ,role-name ,object])))
    (when type-pred
      (setf (basic-object-type-predication object) type-pred
	    (been-in-database-p type-pred) t))
    (when part-of-pred
      (setf (basic-object-part-predication object) part-of-pred
	    (been-in-database-p part-of-pred) t))
    (when part-of-pred (justify part-of-pred +true+ :premise))
    (when type-pred (justify type-pred +true+ :premise))
    ))

(defmacro part (role-name type)
  `(make-instance ',type
		  :role-name ',role-name
		  :superpart-object *root*))

;;; In the CLOS version you'll provide CLOS slot-descriptors for the 
;;; "other instance variables" field.

(defun build-init-tell (predicate slot-name init-form-present init-form key-variable key-p set-valued?)
  (cond 
   ((and key-p init-form-present set-valued?)
    `(if ,key-p
         (loop for value in ,key-variable
             do (tell `[,',predicate (,self ,',slot-name) ,value]))
       (loop for value in ,init-form
           do (tell `[,',predicate (,self ,',slot-name) ,value]))))
   ((and key-p init-form-present)
    `(if ,key-p
         (tell `[,',predicate (,self ,',slot-name) ,,key-variable])
       (tell `[,',predicate (,self ,',slot-name) ,,init-form])))
   ((and key-p set-valued?)
    `(when ,key-p
       (loop for value in ,key-variable
           do (tell `[,',predicate (,self ,',slot-name) ,value]))))
   (key-p
    `(when ,key-p
       (tell `[,',predicate (,self ,',slot-name) ,,key-variable])))
   ((and init-form-present set-valued?)
    `(loop for value in ,init-form
         do (tell `[,',predicate (,self ,',slot-name) ,value])))
   (init-form-present
    `(tell `[,',predicate (,self ,',slot-name) ,,init-form]))))

(def-defining-form define-object-type
    :definer
  ((name &key slots parts equalities initializations included-object-types
	 tms
         other-instance-variables other-flavors other-classes base-classes)
   (multiple-value-bind (slot-names slot-options)
       (loop for s-d in slots
           if (symbolp s-d)
           collect s-d into slot-names
           and collect nil into slot-options
           else collect (car s-d) into slot-names
		   and collect (cdr s-d) into slot-options
           finally (return (values slot-names slot-options)))
     (let ((initialization-tells nil)
           (slot-keys nil))
       (loop for init-form-present = nil 
           for slot-key-variable = nil
           for slot-key-p = nil
           for init-form = nil
           for slot-name in slot-names
           for slot-options in slot-options
           for tms? = (getf slot-options :truth-maintenance)
           for predication-name = (if tms? (if (eql tms? t) 'ltms:value-of tms?) 'value-of)
           for set-valued? = (getf slot-options :set-valued)
           do (loop for (indicator value) on slot-options by #'cddr
                  when (eql indicator :initarg)
                  do (setq slot-key-p (intern (string-upcase (concatenate 'string (string value) "-p")))
                           slot-key-variable (intern (string value)))
                     (push (list slot-key-variable nil slot-key-p) slot-keys)			    
                  when (eql indicator :initform)
                  do (setq init-form-present t init-form value))
           when (or init-form-present slot-key-p)
           do (push (build-init-tell predication-name slot-name init-form-present init-form
                                     slot-key-variable slot-key-p set-valued?) 
                    initialization-tells))
       `(progn
          (let ((old-type-object (object-type-named ',name)))
            (when old-type-object
              (clean-up-for-redefinition old-type-object)))
          (defclass ,name
              (,@other-flavors ,@other-classes ,@included-object-types ,@(when tms (list 'tms-object-mixin)) ,@base-classes basic-object)
            (,@slot-names ,@other-instance-variables))
          ,@(loop for slot-name in slot-names
                collect `(defmethod ,slot-name ((self ,name) &optional (value t))
                           (with-slots (,slot-name) self
                             (if value
                                 (slot-current-value ,slot-name)
                               ,slot-name))))
          ,@(when slot-names
              `((defmethod basic-object-prototype-builder progn ((self ,name))
                  (with-slots ,slot-names self
                    ,@(loop for slot-name in slot-names 
                          collect `(setq ,slot-name 
                                     (make-prototype-slot ',slot-name self)))))
                (defmethod basic-object-body-builder progn ((self ,name))
                  (with-slots ,slot-names self
                    ,@(loop for slot-name in slot-names
                          for his-slot-options in slot-options
                          for slot-type-name = (decode-slot-options-to-slot-type-constructor his-slot-options)
                          collect `(setq ,slot-name (make-instance ',slot-type-name :name ',slot-name :my-object self)))))))
          ,@(when parts
              `((defmethod basic-object-substructure-builder progn ((self ,name))
                  ,@(loop for (role-name type) in parts 
                        if (symbolp type)
                        collect `(make-instance ',type :role-name ',role-name :superpart-object self)
                        else collect `(make-instance ,type :role-name ',role-name :superpart-object self)))))
          ;; even if there are no init's we need to do this in order to override
          ;; an existing initializer from a previous definition of the type
          (defmethod basic-object-initializer :after ((self ,name) &key ,@slot-keys)
            ,@initialization-tells
            ,@initializations
            ,@(when equalities
                `((impose-equalities self ',equalities))))
          (make-instance 'object-type
			:supertypes (loop for name in ',included-object-types
                            collect (object-type-named name))
			:name ',name
			:part-names ',parts
			:slot-names ',slot-names)))))
  :killer undefine-object-type
  :type-name "Object Type"
  )

;;; I think that this is handling the condition case in Ansi Common Lisp way
(defmethod impose-equalities ((self basic-object) equality-list)
  (flet ((impose-one-equality (from to)
	   (let* ((from-cell (handler-case (follow-path-to-slot (cons self from))
				(bad-path (condition)
				  (format *error-output* "~&While building ~a, trying to impose equality between ~a and ~a"
					  self from to)
				  (terpri *error-output*)
				  ;; is this the equivalent of dbg:report
				  (princ condition *error-output*)
				  (return-from impose-one-equality (values)))))
		  (to-cell (handler-case (follow-path-to-slot (cons self to))
			      (bad-path (condition)
				(format *error-output* "~&While building ~a, trying to impose equality between ~a and ~a"
					  self from to)
				(terpri *error-output*)
				;; is this the equivalent of dbg:report
				(princ condition *error-output*)
				(return-from impose-one-equality (values)))))
		  (tms? (truth-maintained? from-cell)))
	     (tell `[,(if tms? 'ltms:equated 'equated) ,from-cell ,to-cell]))))
    (declare (dynamic-extent #'impose-one-equality))
    (loop for (from to) in equality-list
	  do (impose-one-equality from to))))

(defun object-named (name) (subpart-named *root* name))

;; Signal goes to KMP error?
(defun follow-path (path &optional (fetch-value t) (error-if-bad-path t))
  (if (null path)
      *root*
    (multiple-value-bind (initial-object list-of-keys)
	(if (symbolp (car path))
	    (values *root* path)
	  (values (car path) (cdr path)))
      (loop for keys on list-of-keys
	  for key = (first keys)
	  for current-object = initial-object then next-object
	  until (null (cdr keys))
	  for next-object = (or (subpart-named current-object key)
				(and (member key (all-slot-names current-object))
				     (funcall key current-object)))
	  when (and error-if-bad-path (null next-object))
	  do (error 'bad-path
		    :remaining-path keys
		    :whole-path path
		    :first-bad-token key
		    :current-object current-object)
	  finally (return
		    (cond ((subpart-named current-object key))
			  ((member key (all-slot-names current-object))
			   (funcall key current-object fetch-value))
			  ((null error-if-bad-path) nil)
			  (t (error 'bad-path
				    :remaining-path keys
				    :whole-path path
				    :first-bad-token key
				    :current-object current-object))))))))

;;; for internal use only - always returns a slot

;; ditto
(defun follow-path-to-slot (path &optional (error-if-bad-path t))
  (if (typep path 'basic-slot)
    path
    (multiple-value-bind (initial-object list-of-keys)
	                 (if (symbolp (car path))
	                   (values *root* path)
	                   (values (joshua-logic-variable-value (car path)) (cdr path)))
      (loop for keys on list-of-keys
            for key = (car keys)
            for current-object = initial-object then next-object
            until (null (cdr keys))
	  for next-object = (or (subpart-named current-object key)
				(and (member key (all-slot-names current-object))
				     (funcall key current-object)))
				
            when (null next-object)
            do (if error-if-bad-path
                 (error 'bad-path
                        :remaining-path keys
                        :whole-path path
                        :first-bad-token key
                        :current-object current-object)
                 (return nil))
            finally (if (not (member key (all-slot-names current-object)))
                      (if error-if-bad-path
                        (error 'bad-path
                               :remaining-path keys
                               :whole-path path
                               :first-bad-token key
                               :current-object current-object)
                        (return nil))
                      (return (funcall key current-object nil)))))))


;;; This is what should be used by the ask-data method for value-of predicates
(defun follow-path-to-slot* (path &optional continuation (error-if-bad-path t))
  (if (typep path 'basic-slot)
      path
      (labels 
	  ((do-one-more (current-object list-of-keys)
	     (let ((key (pop list-of-keys)))
	       (cond
		;; If this is the last key then this has to be a slot name or
		;; or a part of the object
		;; or we'll error if asked to		
		((null list-of-keys)
		 (cond 
		  ((member key (all-slot-names current-object))
		   (funcall continuation (funcall key current-object nil)))
		  ((subpart-named current-object key)
		   (funcall continuation (subpart-named current-object key)))
		  (error-if-bad-path
		   (error 'bad-path
			  :remaining-path list-of-keys
			  :whole-path path
			  :first-bad-token key
			  :current-object current-object))))
		(t (let ((next-object (or (subpart-named current-object key)
					  (and (member key (all-slot-names current-object))
					       ;; get the slot not the value
					       (apply key current-object (list nil))))))
		     (when (null next-object)
		       (if error-if-bad-path
			   (error 'bad-path
				  :remaining-path list-of-keys
				  :whole-path path
				  :first-bad-token key
				  :current-object current-object)
			 (return-from follow-path-to-slot* nil)))
		     (typecase next-object
		       (basic-slot
			(if (slot-is-set-valued next-object)
			    (loop for thing in (slot-current-value next-object) do (do-one-more thing list-of-keys))
			  (do-one-more (slot-current-value next-object) list-of-keys)))
		       (t (do-one-more next-object list-of-keys)))))))))
	(multiple-value-bind (initial-object list-of-keys)
	    (if (symbolp (car path))
		(values *root* path)
	      (values (joshua-logic-variable-value (car path)) (cdr path)))
	  (do-one-more initial-object list-of-keys)))))



(defun map-over-object-hierarchy (function-to-apply &optional initial-object)
  #-sbcl (declare (dynamic-extent function-to-apply))
  (labels ((handle-object (object)
                          #+cloe (declare (sys:downward-function))
	     (funcall function-to-apply object)
	     (maphash #'(lambda (key part)
			  (declare (ignore key))
			  (handle-object part))
		      (basic-object-subparts object))
	     ;;(loop for part being the hash-values of (basic-object-subparts object) with-key key
	     ;;      doing (progn key)
	     ;;            (handle-object part))
	     ))
    (declare (dynamic-extent #'handle-object))
    (if initial-object
	(handle-object initial-object)
	(maphash #'(Lambda (key object-type)
		     (declare (ignore key))
		     (loop for part in (object-type-instances object-type)
			   doing (handle-object part)))
		 *all-object-types*)
	;;(loop for object-type being the hash-values of *all-object-types* with-key key
	;;      do (progn key)
	;;         (loop for part in (object-type-instances object-type)
	;;               doing (handle-object part)))
	)))

(defun map-over-parallel-object-hierarchies (from-object to-object function)
  (labels ((do-one-level (from to)
                         #+cloe (declare (sys:downward-function))
	     (funcall function from to)
	     (maphash #'(lambda (role-name part-of-from)
			  (let ((part-of-to (gethash role-name (basic-object-subparts to))))
			    (do-one-level part-of-from part-of-to)))
		      (basic-object-subparts from))
	     ;; (loop for part-of-from being the hash-values of (basic-object-subparts from)
	     ;;       with-key role-name
	     ;;       for part-of-to = (gethash role-name (basic-object-subparts to))
             ;;       doing (do-one-level part-of-from part-of-to))
	     ))
    (declare (dynamic-extent #'do-one-level))
    (do-one-level from-object to-object)))

(defun map-over-slots-of-object (function-to-apply object)
  #-sbcl (declare (dynamic-extent function-to-apply))
  (loop for slot-name in (all-slot-names object)
	for slot = (funcall slot-name object nil)
	doing (funcall function-to-apply slot)))

(defun map-over-slots-in-object-hierarchy (function-to-apply &optional initial-object)
  #-sbcl (declare (dynamic-extent function-to-apply))
  (flet ((slot-mapper (object)
           #+cloe (declare (sys:downward-function))
	   (map-over-slots-of-object function-to-apply object)))
    (declare (dynamic-extent #'slot-mapper))
    (map-over-object-hierarchy #'slot-mapper initial-object)))

(defun clean-object-hierarchy ()
  (let ((protected-instances nil))
    ;; First protect the hierarchy under all the typical instances.
    (maphash #'(lambda (key object-type)
		 (declare (ignore key))
		 (when (object-type-typical-instance object-type)
		   (map-over-object-hierarchy
		     #'(lambda (thing)
			 (pushnew thing protected-instances))
		     (object-type-typical-instance object-type))))
	     *all-object-types*)
    ;;    (loop for object-type being the hash-values of *all-object-types* with-key key
    ;;	        do (progn key)
    ;;	        when (object-type-typical-instance object-type)
    ;;	        do (map-over-object-hierarchy
    ;;	              #'(lambda (thing)
    ;;		           (pushnew thing protected-instances))
    ;;	              (object-type-typical-instance object-type)))
    ;; Now kill any unprotected instances.
    (map-over-object-hierarchy
      #'(lambda (object)
	  (unless (member object protected-instances)
	    (kill object))))))


;;; The method for the slot-value-mixin Predicate-Model.

(define-predicate-method (clear slot-value-mixin) (&optional database-p undefrule)
  (declare (ignore database-p))
  (map-over-slots-in-object-hierarchy #'reset-slot)
  (when undefrule
    (maphash #'(lambda (key type)
		 (declare (ignore key))
		 (let ((typical-instance (object-type-typical-instance type)))
		   (when typical-instance
		     (loop for slot-name in (object-type-all-slot-names type)
			 for slot = (funcall slot-name typical-instance nil)
			 when slot
			 do (setf (slot-forward-triggers slot) nil))))					
		 (setf (object-type-rule-triggers type) nil)
		 (setf (object-type-all-rules type) nil))
	     *all-object-types*)
    ;;    (loop for type being the hash-values of *all-object-types* with-key key
    ;;	        do (progn key)
    ;;	           (setf (object-type-all-rules type) nil))
    ))

(define-predicate-method (insert slot-value-mixin) () 
  (with-statement-destructured (path value) self
    (let ((final-slot (if (listp path) (follow-path-to-slot path) path)))
      (insert-new-predication final-slot self value))))

(define-predicate-method (uninsert slot-value-mixin) ()
  (let ((slot (predication-my-slot self)))
    (when slot
      (setf (predication-my-slot self) nil)
      (uninsert-predication slot self))))

(define-predicate-method (notice-truth-value-change slot-value-mixin) (old-truth-value)
  (with-statement-destructured (slot value) ()
    (notice-new-value slot value self old-truth-value)))

(define-predicate-method (act-on-truth-value-change slot-value-mixin) (old-truth-value &optional old-state)
  (declare (ignore old-state))
  (unless (= (predication-bits-truth-value (slot-value self 'bits)) old-truth-value)
    ;; before acting on it make sure it's still different
    (let ((slot (predication-my-slot self)))
      ;; Need to check this because telling this fact could have triggered
      ;; a rule which untold this fact which would have nil'd out the slot.
      (when slot
	(act-on-new-value slot self old-truth-value)))))

;;; We define our own Ask for slot-value-mixins for two reasons.
;;; First we resolve what the path means at this level rather than
;;; doing it once for data, once for rules and once for questions.
;;; Second we build a new continuation which caches the results of
;;; successful rule or question firing (i.e. it TELL's the answer).

(define-predicate-method (ask slot-value-mixin) (truth-value continuation do-backward-rules do-questions)
  (unless (eql truth-value +true+)
    (error 'model-can-only-handle-positive-queries
	    :query self
	    :model (type-of self)))
  (with-statement-destructured (path value-in-query) self
    (flet ((handle-one-slot (slot)
             #+cloe (declare (sys:downward-function))
	     (setq my-slot slot)
	     (let ((he-was-empty (slot-is-empty-p my-slot)))
	       (setq do-backward-rules (and he-was-empty do-backward-rules)
		     do-questions (and he-was-empty do-questions)))
	     (ask-data self truth-value continuation)
	     ;; Now go get stuff from rules.  The continuation we
	     ;; pass on the rule stuff is hacked up to Tell the
	     ;; answer found and the call the real continuation.
	     ;; For efficiency we test here whether the answer
	     ;; will need truth-maintenance or not and build the
	     ;; correct continuation for that case.
	     (if (truth-maintained? slot)
		 (flet ((caching-continuation (backward-support)
			  #+cloe (declare (sys:downward-function))
			  (with-statement-destructured (ignore value-in-query) self
			    (declare (ignore ignore))
			    (let ((new-pred (tell `[,(type-of self) ,slot ,(joshua-logic-variable-value value-in-query)]
						  :justification (build-justification-from-backward-support
								   backward-support))))
			      (let ((rule-query (third backward-support)))
				(with-stack-list (new-rule-query (first rule-query) (second rule-query) new-pred)
				  (with-stack-list* (new-backward-support (first backward-support)
									  (second backward-support)
									  new-rule-query
									  (cdddr backward-support))
				    (funcall continuation new-backward-support))))))))
                   (declare (dynamic-extent #'caching-continuation))
		   (when do-backward-rules
		     (ask-rules self truth-value #'caching-continuation do-questions))
		   ;; now go hack questions
		   (when do-questions
		     (ask-questions self truth-value #'caching-continuation)))
		 (flet ((caching-continuation (backward-support)
			  #+cloe (declare (sys:downward-function))
			  (with-statement-destructured (ignore value-in-query) self
			    (declare (ignore ignore))
			    (tell `[,(type-of self) ,slot ,(joshua-logic-variable-value value-in-query)])
			    (funcall continuation backward-support))))
                   (declare (dynamic-extent #'caching-continuation))
		   (when do-backward-rules
		     (ask-rules self truth-value #'caching-continuation do-questions))
		   ;; now go hack questions
		   (when (and do-questions (slot-is-empty-p my-slot))
		     (ask-questions self truth-value #'caching-continuation))))))
      (declare (dynamic-extent #'handle-one-slot))
      (cond
	((typep path 'basic-slot) (handle-one-slot path))
	((unbound-logic-variable-p path)
	 ;; If the path is a variable it's a request to
	 ;; show everything.
	 (map-over-slots-in-object-hierarchy
	   #'(lambda (slot)
	       (with-unification
		 (unify path slot)
		 (handle-one-slot slot)))
	   *root*))
	(t ;; we have a real initial object.
	 ;; What about variables later in the path?
	 (flet ((slot-continuation (final-slot)
		  (typecase final-slot
		    (basic-slot (handle-one-slot final-slot))
		    ;; If resolving the path takes you to an actual object
		    ;; then you just call the continuation.  Notice, this is
		    ;; different than the case where the final thing is a slot
		    ;; whose value is an object.  In that case, the path specifies a slot
		    ;; whose value could change.  In this case, the last step takes
		    ;; you to a "part" of the previous object.  Parts are fixed parts of the hierarchy
		    ;; and can't be deduced by backward rules (I think).  Also in this case it's not set valued
		    ;; so we just call the continuation after unifying the value part of the query to the object
		    (basic-object
		     (with-unification
		      (unify final-slot value-in-query )
		      (stack-let ((backward-support `(,self ,+true+ ,final-slot)))
			(funcall continuation backward-support)))))))
	   (follow-path-to-slot* path #'slot-continuation nil)))))
  ;; make it clear that there is no interesting return value
  (values)))

;;; This is a utility for taking a path and finding
;;; all slots that the path maps to (there could be variables in the path)
;;; and then calling a continuation on each one.
;;; A complication is that the path might end in a "part" rather than a slot
;;; In that case you still call the continuation but with enough information
;;; so that it can make sense of things.

;;; This is duplicating code that's in the ask method
;;; This allows you to call ask-data directly without going through ask.
;;; This in turn means you get to pass in the truth-value directly and so 
;;; you can pass in a Null value, meaning give me everything true or not.
;;; This assumes that my-slot is bound, which would be true if called from ask
;;; But if called as a top-level, you'll have to follow paths to slots
;;; If resolving the path takes you to an actual object
;;; then you just call the continuation.  Notice, this is
;;; different than the case where the final thing is a slot
;;; whose value is an object.  In that case, the path specifies a slot
;;; whose value could change.  In this case, the last step takes
;;; you to a "part" of the previous object.  Parts are fixed parts of the hierarchy
;;; and can't be deduced by backward rules (I think).  Also in this case it's not set valued
;;; so we just call the continuation after unifying the value part of the query to the object



(define-predicate-method (ask-data slot-value-mixin) (truth-value continuation)
  ;; (declare (ignore truth-value))
  (with-statement-destructured (slot-in-query value-in-query) self
    (if (null truth-value)
	;; In this case the caller is asking for all possible answers regardess of truth-value
	;; that match the query.  So first of all get to the slot or if the path ends at a part
	;; then the object
	(follow-path-to-slot* slot-in-query
		  #'(lambda (thing)
		      (typecase thing
			(basic-slot
			 ;; if a slot then map over all possible values in the slot
			 ;; (not the current value)
			 (map-over-all-values thing self continuation value-in-query))
			;; This happens if the path resolves to a sub-part of the object
			;; rather than a slot of the object
			(basic-object
			 ;; There's nothing more to do at this point other than to
			 ;; invoke the caller's continuation
			 (stack-let ((backward-support `(,self ,+true+  ,(basic-object-part-predication thing)
							       (ask-data ,(part-of-predicate-for-object-type thing))
							       ,thing)))
			   (funcall continuation backward-support))
			 )))
		  nil)
      (map-over-values my-slot self continuation value-in-query))))

(define-predicate-method (map-over-backward-rule-triggers slot-value-mixin) (continuation)
  (map-over-slot-backward-rule-triggers my-slot continuation))

(define-predicate-method (map-over-backward-question-triggers slot-value-mixin) (continuation)
  (map-over-slot-backward-question-triggers my-slot continuation))

(define-predicate-method (prefetch-forward-rule-matches slot-value-mixin) (context continuation)
  (with-statement-destructured (path value) self
    (let ((type-name (find-object-type-in-trigger-pattern self (car path) context)))
      ;; map over sub-hierarchy to find all objects of this type
      (map-over-subtypes 
       (object-type-named type-name)
       #'(lambda (type)
	   (loop for object in (object-type-instances type)
	       unless (basic-object-typical-instance-of-type? (ultimate-superpart object))
	       do (let ((final-slot (let ((*root* object)) (follow-path-to-slot (cdr path)))))
		    (with-unification
			(unify (car path) object)
		      (map-over-values final-slot self 
				       #'(lambda (derivation)
					   (funcall continuation (ask-database-predication derivation)))
				       value)))))))))

(define-predicate-method (add-backward-rule-trigger slot-value-mixin :after) (truth-value trigger-object context rule-name)
  (declare (ignore trigger-object truth-value))
  (with-statement-destructured (full-path ignore) self
    (declare (ignore ignore))
    (destructuring-bind (object &rest ignore) full-path
      (declare (ignore ignore))
      (let ((type (object-type-named (find-object-type-in-trigger-pattern self object context))))
	(pushnew rule-name (object-type-all-rules type))))))

(define-predicate-method (add-backward-question-trigger slot-value-mixin :after) (truth-value trigger-object context rule-name)
  (declare (ignore trigger-object truth-value))
  (with-statement-destructured (full-path ignore) self
    (declare (ignore ignore))
    (destructuring-bind (object &rest ignore) full-path
      (declare (ignore ignore))
      (let ((type (object-type-named (find-object-type-in-trigger-pattern self object context))))
	(pushnew rule-name (object-type-all-rules type))))))

(define-predicate-method (add-forward-rule-trigger slot-value-mixin :after) (truth-value trigger-object context rule-name)
  (declare (ignore trigger-object truth-value))
  (with-statement-destructured (full-path ignore) self
    (declare (ignore ignore))
    (destructuring-bind (object &rest ignore) full-path
      (declare (ignore ignore))
      (let ((type (object-type-named (find-object-type-in-trigger-pattern self object context))))
	(pushnew rule-name (object-type-all-rules type))))))

(define-predicate-method (delete-backward-rule-trigger slot-value-mixin :after) (truth-value rule-name context)
  (declare (ignore truth-value))
  (with-statement-destructured (full-path ignore) self
    (declare (ignore ignore))
    (destructuring-bind (object &rest ignore) full-path
      (declare (ignore ignore))
      (let ((type (object-type-named (find-object-type-in-trigger-pattern self object context))))
	(setf (object-type-all-rules type) (delete rule-name (object-type-all-rules type)))))))

(define-predicate-method (delete-backward-question-trigger slot-value-mixin :after) (truth-value question-name context)
  (declare (ignore truth-value))
  (with-statement-destructured (full-path ignore) self
    (declare (ignore ignore))
    (destructuring-bind (object &rest ignore) full-path
      (declare (ignore ignore))
      (let ((type (object-type-named (find-object-type-in-trigger-pattern self object context))))
	(setf (object-type-all-rules type) (delete question-name (object-type-all-rules type)))))))

(define-predicate-method (delete-forward-rule-trigger slot-value-mixin :after) (truth-value rule-name context)
  (declare (ignore truth-value))
  (with-statement-destructured (full-path ignore) self
    (declare (ignore ignore))
    (destructuring-bind (object &rest ignore) full-path
      (declare (ignore ignore))
      (let ((type (object-type-named (find-object-type-in-trigger-pattern self object context))))
	(setf (object-type-all-rules type) (delete rule-name (object-type-all-rules type)))))))

;;; The context is always source code
(defmethod find-object-type-in-trigger-pattern ((self slot-value-mixin) object context)
  ;; object is a real object
  (let ((thing-to-match (if (unbound-logic-variable-p object)
			    (logic-variable-name object)
			    object)))
    (labels ((find-in-current-context (sub-context)
	       (cond ((predication-maker-p sub-context)
		      (let ((predicate (predication-maker-predicate sub-context)))
			(cond ((is-an-object-type-of-predicate predicate)
			       (with-predication-maker-destructured (thing type) sub-context
				 (when (same-as-object thing)
				   (return-from find-object-type-in-trigger-pattern type))))
			      (t
			       (loop for piece in (predication-maker-statement sub-context)
				     doing (find-in-current-context piece))))))
		     ((listp sub-context)
		      (loop for thing in sub-context doing (find-in-current-context thing)))))
	     (is-an-object-type-of-predicate (predicate-name)
	       ;; Lucid warns if you provide the environment argument.  This shuts him up.
	       (let ((his-flavor (find-class predicate-name nil #-(or mcl lucid allegro sbcl) 'compile)))
		 (and his-flavor
		      (member (find-class 'type-of-mixin) (class-precedence-list his-flavor)))))
	     (same-as-object (thing)
	       ;; Thing is from the context, meaning it's a designator
	       (cond ((logic-variable-maker-p thing)
		      (eql (logic-variable-maker-name thing) thing-to-match))
		     (t (eql thing thing-to-match)))))
      (declare (dynamic-extent #'find-in-current-context #'is-an-object-type-of-predicate #'same-as-object))
      (find-in-current-context context))))

(define-predicate-method (write-backward-rule-matcher slot-value-mixin) (variables environment name-of-pred-to-match)
  (declare (ignore variables environment))
  (with-predication-maker-destructured (path value) ()
    (let ((my-object (car path))
	  (my-path-length (length (cdr path))))
      (values
	`(progn
	   (let ((statement (cdr (predication-statement ,name-of-pred-to-match))))
	     (let ((his-path (pop statement)))
	       (unify ,(if (logic-variable-maker-p my-object)
			   (logic-variable-maker-name my-object)
			   `',my-object)
		      (nth-superpart (follow-path-to-slot his-path) ,my-path-length)))
	     (let ((his-value (pop statement)))
	       (unify ,(if (logic-variable-maker-p value)
			  (logic-variable-maker-name value)
			  `',value)
		      his-value))))
	nil
	nil))))

(define-predicate-method (locate-backward-rule-trigger slot-value-mixin) (truth-value continuation context rule-name)
  (unless (eql truth-value +true+)
    (error "this ~s pattern for rule ~s has a truth value which isn't +true+" self rule-name))
  (with-statement-destructured (full-path other-stuff) self
    (declare (ignore other-stuff))
    (destructuring-bind (object &rest path) full-path
      (let ((type (find-object-type-in-trigger-pattern self object context)))
	(unless type
	  (error "this rule uses slot-value pattern ~s but does not contain a type pattern" self))
	(setq type (object-type-named type))
	(let ((canonical-node-to-return nil))
	  (labels ((do-one-type (subtype)
		     ;; (format t "~&Doing type ~s for rule ~s pattern ~s" (object-type-name type) rule-name self)
		     (let ((typical-instance (cond (*rebuilding-rules-for-type-redefinition*
						     (find-or-create-typical-instance subtype))
						   (t (if (eql subtype type)
							  (find-or-create-typical-instance subtype)
							  (object-type-typical-instance subtype))))))
		       ;; Make sure there is one.
		       ;; Necessary because this can get called during the course
		       ;; of redefining a type.  In that case the type has subtypes
		       ;; but their typical instances have been killed.
		       ;; We don't need to push stuff forward to those guys anyhow
		       ;; since they're about to get built and they'll get this stuff
		       ;; by pulling it.
		       (when typical-instance
			 (do-one-object typical-instance)))
		     (loop for instance in (object-type-instances subtype)
			   when (basic-object-typical-instance-of-type? (ultimate-superpart instance))
			     do (do-one-object instance)))
		   (do-one-object (object)
		     (let ((real-path (cons object path)))
		       (let ((slot (handler-bind ((bad-path
						    #'(lambda (condition)
							(declare (ignore condition))
							(unless *im-handling-bad-rule-patterns*
							  (format *error-output*
								  "~%While trying to install the ~a pattern~%of the backward rule ~a~%"
								  self rule-name))
							nil)))
				     (follow-path-to-slot real-path))))
			   (multiple-value-bind (new-triggers something-changed the-canonical-node)
			       (funcall continuation (slot-backward-triggers slot))
			   ;; (format t "~&Something changed? ~s canonical ~s" something-changed the-canonical-node)
			     (when something-changed
			       (setf (slot-backward-triggers slot) new-triggers))
			     (setq canonical-node-to-return the-canonical-node))))))
	    (declare (dynamic-extent #'do-one-type #'do-one-object))
	    (if *rebuilding-rules-for-type-redefinition*
		(do-one-type type)
		(map-over-subtypes type #'do-one-type)))
	  canonical-node-to-return)))))

(define-predicate-method (locate-backward-question-trigger slot-value-mixin) (truth-value continuation context question-name)
  (unless (eql truth-value +true+)
    (error "The ~s question's pattern ~s has a truth value which isn't +true+" question-name self))
  (with-statement-destructured (full-path ignore) self
    (declare (ignore ignore))
    (destructuring-bind (object &rest path) full-path
      (let ((type (find-object-type-in-trigger-pattern self object context)))
	(unless type
	  (error "this rule uses slot-value pattern ~s but does not contain a type pattern" self))
	(setq type (object-type-named type))
	(let ((canonical-node-to-return nil))
	  (labels ((do-one-type (subtype)
		     ;; (format t "~&Doing type ~s for rule ~s pattern ~s" (object-type-name type) rule-name self)
		     (let ((typical-instance (cond (*rebuilding-rules-for-type-redefinition*
						     (find-or-create-typical-instance subtype))
						   (t (if (eql subtype type)
							  (find-or-create-typical-instance subtype)
							  (object-type-typical-instance subtype))))))
		       ;; Make sure there is one.
		       ;; Necessary because this can get called during the course
		       ;; of redefining a type.  In that case the type has subtypes
		       ;; but their typical instances have been killed.
		       ;; We don't need to push stuff forward to those guys anyhow
		       ;; since they're about to get built and they'll get this stuff
		       ;; by pulling it.
		       (when typical-instance
			 (do-one-object typical-instance)))
		     (loop for instance in (object-type-instances subtype)
			   when (basic-object-typical-instance-of-type? (ultimate-superpart instance))
			     do (do-one-object instance)))
		   (do-one-object (object)
		     (let ((real-path (cons object path)))
		       (let ((slot (handler-bind
				     ((bad-path
					#'(lambda (condition)
					  (declare (ignore condition))
					  (unless *im-handling-bad-rule-patterns*
					    (format *error-output*
						    "~%While trying to install the ~a pattern~%of the backward question ~a~%"
						    self question-name))
					  nil)))
				     (follow-path-to-slot real-path))))
			 (multiple-value-bind (new-triggers something-changed the-canonical-node)
			     (funcall continuation (slot-backward-question-triggers slot))
			   ;; (format t "~&Something changed? ~s canonical ~s" something-changed the-canonical-node)
			   (when something-changed
			     (setf (slot-backward-question-triggers slot) new-triggers))
			   (setq canonical-node-to-return the-canonical-node))))))
	    (declare (dynamic-extent #'do-one-type #'do-one-object))
	    (if *rebuilding-rules-for-type-redefinition*
		(do-one-type type)
		(map-over-subtypes type #'do-one-type)))
	  canonical-node-to-return)))))

(define-predicate-method (write-forward-rule-semi-matcher slot-value-mixin) (predication-to-match stuff)
  (declare (ignore stuff))
  (with-predication-maker-destructured (path value) ()
    (let ((my-object (car path))
	  (my-path-length (length (cdr path))))
      `(and
	 (let ((statement (cdr (predication-statement ,predication-to-match))))
	   (let ((his-cell (pop statement)))
	     ,(cond ((logic-variable-maker-p my-object)
		     `(progn (setq ,(logic-variable-maker-name my-object)
				   (nth-superpart his-cell ,my-path-length))
			     t))
		    (t `(eq ',my-object (nth-superpart his-cell ,my-path-length)))))
	   (let ((his-value (pop statement)))
	     ,(cond ((logic-variable-maker-p value)
		     `(progn (setq ,(logic-variable-maker-name value) his-value)
			     t))
		    (t `(eq ',value his-value)))))))))

(define-predicate-method (positions-forward-rule-matcher-can-skip slot-value-mixin) ()
  ;; return list of tails of statement that are headed by symbols
  (let ((statement (predication-maker-statement self)))
    (cons statement
	  (loop for token = (cadr statement) then (cdr token)
		while (consp token)
		;; needn't deal with tail variable, since variables can't ever be skipped anyway
		when (or (symbolp (car token)) (numberp (car token))) collect token))))

;;; This checks every instance to see if its ultimate parent 
;;; is a typical instance and if so shoves the thing there also.

;;; I discovered a long-standing bug 7/15/2014 in how this works.
;;; Consider two variant slot-value-patters (e.g. [value-of (?x host-os) ?y] [value-of (?a host-os) ?b]
;;; but the object type of ?x isn't the object-type of ?a
;;; In that case, the triggers ought not to be merged
;;; But the continuation that's passed to tell whether things are the same or not, doesn't
;;; check for that.  The fix is to make sure that not only do we agree on truth-value and variant of the pattern
;;; but that the object-type of the two are also the same.
;;; To see that this is right, consider that the [value-of (?foo )...] predication in effect has another argument, the 
;;; type of ?foo which is provided by the [object-type-of ?foo ] predication elsewhere in the pattern.
;;; So the variant check for merging needs to include a check for identical types.

(define-predicate-method (add-forward-rule-trigger slot-value-mixin)
			 (truth-value forward-trigger context rule-name)
  ;; add a trigger to the index of forward rules
  (unless (eql truth-value +true+)
    (error "this rule's pattern ~s has a truth value which isn't +true+" self))
  (with-statement-destructured (full-path ignore) self
    (declare (ignore ignore))
    (let ((type-name (find-object-type-in-trigger-pattern self (first full-path) context)))
      (locate-forward-rule-trigger self
				   truth-value
				   #'(lambda (triggers)
				       (slot-value-install-and-intern-forward-rule-trigger forward-trigger triggers self type-name))
				   context
				   rule-name))))

(defun slot-value-install-and-intern-forward-rule-trigger (new-Rete-node triggers trigger type-name)
  ;; add a trigger to the index of trigger patterns of the appropriate
  ;; type.  We store a rule-trigger structure into the trigger-location.
  (loop with new-rete-node-truth-value = (rete-match-node-truth-value new-rete-node)
      and match-id = (car (Rete-match-node-match-ids new-Rete-node))
      with new-pattern = (match-id-pattern match-id)
      for Rete-node in triggers
      for stored-match-id = (car (Rete-match-node-match-ids Rete-node))
      for stored-pattern = (match-id-pattern stored-match-id)
      for stored-object = (with-statement-destructured (full-path ignore) stored-pattern
			    (declare (ignore ignore))
			    (first full-path))
      for stored-rule-name = (match-id-rule-name stored-match-id)
      for stored-context = (rule-debug-info-triggers (rule-debug-info stored-rule-name))
      for stored-type = (find-object-type-in-trigger-pattern trigger stored-object stored-context)
      for stored-truth-value = (rete-match-node-truth-value rete-node)
      when (and (eql new-rete-node-truth-value stored-truth-value)
		(eql stored-type type-name)
		(variant new-pattern stored-pattern))
	   ;; nothing in the index changed
      return (values triggers nil rete-node)
      finally (return (values (cons new-Rete-node triggers) t new-rete-node))))

(define-predicate-method (locate-forward-rule-trigger slot-value-mixin) (truth-value continuation context rule-name)
  (unless (eql truth-value +true+)
    (error "this rule's pattern ~s has a truth value which isn't +true+" self))
  (with-statement-destructured (full-path ignore) self
    (declare (ignore ignore))
    (destructuring-bind (object &rest path) full-path
      (let ((type-name (find-object-type-in-trigger-pattern self object context))
	    (type nil))
 	(unless type-name 
	  (error "This rule uses slot-value pattern ~s but does not contain a type pattern" self))
	(setq type (object-type-named type-name))
	(unless type
	  (error "The rule pattern ~s believes that ~s has object-type ~s which isn't defined" self object type-name))
	(let ((canonical-node-to-return nil))
	  (labels ((do-one-type (subtype)
		     ;; (format t "~&Doing subtype ~a for type ~s for rule ~s pattern ~s" (object-type-name subtype) (object-type-name type) rule-name self)
		     (let ((typical-instance (cond (*rebuilding-rules-for-type-redefinition*
						     (find-or-create-typical-instance subtype))
						   (t (if (eql subtype type)
							  (find-or-create-typical-instance subtype)
							  (object-type-typical-instance subtype))))))
		       ;; Make sure there is one.
		       ;; Necessary because this can get called during the course
		       ;; of redefining a type.  In that case the type has subtypes
		       ;; but their typical instances have been killed.
		       ;; We don't need to push stuff forward to those guys anyhow
		       ;; since they're about to get built and they'll get this stuff
		       ;; by pulling it.
		       (when typical-instance
			 (do-one-object typical-instance)))
		     (loop for instance in (object-type-instances subtype)
			   when (basic-object-typical-instance-of-type? (ultimate-superpart instance))
			     do (do-one-object instance)))
		   (do-one-object (object)
		     ;; (format t "~%Doing object ~a" object)
		     (let ((real-path (cons object path)))
		       (let ((slot (handler-bind
				     ((bad-path
					#'(lambda (condition)
					  (declare (ignore condition))
					  (unless *im-handling-bad-rule-patterns*
					    (format *error-output*
						    "~%While trying to install the ~a pattern~%of the forward rule ~a~%"
						    self rule-name))
					  nil)))
				     (follow-path-to-slot real-path))))
			 (multiple-value-bind (new-triggers something-changed the-canonical-node)
			     (funcall continuation (slot-forward-triggers slot))
			   ;; (format t "~&Something changed? ~s canonical ~s" something-changed the-canonical-node)
			   (when something-changed
			     (setf (slot-forward-triggers slot) new-triggers))
			   (setq canonical-node-to-return the-canonical-node))))))
	    (declare (dynamic-extent #'do-one-type #'do-one-object))
	    (if *rebuilding-rules-for-type-redefinition*
		(do-one-type type)
		(map-over-subtypes type #'do-one-type)))
	  canonical-node-to-return)))))

(define-predicate-method (map-over-forward-rule-triggers slot-value-mixin) (continuation)
  (let ((final-slot (slot-prototype-slot (predication-my-slot self))))
    (loop for trigger in (slot-forward-triggers final-slot) 
	  do (funcall continuation trigger))))

(define-predicate value-of (path value) (slot-value-mixin trivial-tms-mixin default-protocol-implementation-model))

(define-predicate ltms:value-of (path value) (slot-value-mixin ltms:ltms-mixin default-protocol-implementation-model))


;;; Object-Type-of can only be asked, never told.

(define-predicate-model type-of-mixin () (tell-error-model ask-data-only-mixin no-variables-in-data-mixin))

(define-predicate-method (locate-forward-rule-trigger type-of-mixin) (truth-value continuation context rule-name)
  (declare (ignore context rule-name))
  (unless (eql truth-value +true+)
    (error "this rule's pattern ~s has a truth value which isn't +true+" self))
  (with-statement-destructured (object type-name) self
    (declare (ignore object))
    (let ((type (object-type-named type-name)))
      (unless type
	(error "The rule pattern ~s refers to the object-type ~s which isn't defined" self type-name))
      (multiple-value-bind (new-triggers something-changed the-canonical-node)
	  (funcall continuation (object-type-rule-triggers type))
	(when something-changed
	  (setf (object-type-rule-triggers type) new-triggers))
	the-canonical-node))))

(define-predicate-method (prefetch-forward-rule-matches type-of-mixin) (context continuation)
  (declare (ignore context))
  (with-statement-destructured (object type-name) self
    (declare (ignore object))
    (let ((type (object-type-named type-name)))
      (labels ((next-subtype (type)
		 ;; dont index parts of typical instances
		 (loop for instance in (object-type-instances type)
		     for his-ultimate-parent = (ultimate-superpart instance)
		     unless (basic-object-typical-instance-of-type? his-ultimate-parent)
		       do (funcall continuation instance))
		 (loop for subtype in (object-type-subtypes type)
		       do (next-subtype subtype))))
	(declare (dynamic-extent #'next-subtype))
	(next-subtype type)))))

(define-predicate-method (write-forward-rule-semi-matcher type-of-mixin) (object-to-match environment)
  (declare (ignore environment))
  (with-predication-maker-destructured (my-object type) ()
    (declare (ignore type))
    (cond ((logic-variable-maker-p my-object)
	   `(progn (setq ,(logic-variable-maker-name my-object) ,object-to-match)
		   t))
	  (t `(eq ',my-object ,object-to-match)))))

(define-predicate-method (positions-forward-rule-matcher-can-skip type-of-mixin) ()
  ;; return list of tails of statement that are headed by symbols
  (loop for token = (cdr (predication-maker-statement self)) then (cdr token)
	while (consp token)
	;; needn't deal with tail variable, since variables can't ever be skipped anyway
	when (or (symbolp (car token)) (numberp (car token))) collect token))

(define-predicate-method (ask-data type-of-mixin) (truth-value continuation)
  (unless (eql truth-value +true+)
    (error 'model-can-only-handle-positive-queries
	    :query self
	    :model (type-of self)))
  (with-statement-destructured (the-object type) self
    (when (and (unbound-logic-variable-p type)  (unbound-logic-variable-p the-object))
      (error 'model-cant-handle-query
	      :query self
	      :model (type-of self)))
    (cond ((unbound-logic-variable-p the-object)
	   (labels ((do-one-type (object-type)
		      (loop for instance in (object-type-instances object-type)
			    unless (basic-object-typical-instance-of-type? (ultimate-superpart instance))
			    do (with-unification
				 (unify instance the-object)
				 (stack-let ((backward-support (list self +true+ '(ask-data object-type-of)
								     (make-predication
								       `(,(predication-predicate self)
									 ,instance
									 ,(object-type-name object-type))
								       :stack)
								     )))
				   (funcall continuation backward-support))))
		      (loop for sub-type in (object-type-subtypes object-type)
			    do (do-one-type sub-type))))
	     (declare (dynamic-extent #'do-one-type))
	     (do-one-type (object-type-named type))))
          ((unbound-logic-variable-p type)
           (map-over-supertypes (basic-object-type the-object)
                                #'(lambda (supertype)
                                    (with-unification
                                      (unify type supertype)
                                      (stack-let ((backward-support (list self +true+ '(ask-data object-type-of))))
                                        (funcall continuation backward-support))))))                      
           
	  (t (when (typep the-object type)
	       (stack-let ((backward-support (list self +true+ '(ask-data object-type-of))))
		 (funcall continuation backward-support)))))))

(defun find-all-uses-of-object-type-in-trigger-pattern (object context)
  ;; Here we're working totally on source code
  (let ((object-name (logic-variable-maker-name object))
	(types nil)
	(uses-in-path nil)
	(uses-as-value nil))
    (labels ((find-in-current-context (sub-context enclosing-predication-maker kind-of-usage)
	       (cond ((predication-maker-p sub-context)
		      (let ((predicate (predication-maker-predicate sub-context)))
			(cond ((is-an-object-type-of-predicate predicate)
			       (with-predication-maker-destructured (thing type) sub-context
				 (when (same-as-object thing)
				   (push type types))))
			      ((is-a-value-of-predicate predicate)
			       (with-predication-maker-destructured (path value) sub-context
				 (find-in-current-context path sub-context 'in-path)
				 (find-in-current-context value sub-context 'as-value)))
			      (t
			       (loop for piece in (predication-maker-statement sub-context)
				     doing (find-in-current-context piece sub-context kind-of-usage))))))
		     ((and (logic-variable-maker-p sub-context)
			   (same-as-object sub-context))
		      (if (eql kind-of-usage 'in-path)
			  (push enclosing-predication-maker uses-in-path)
			  (push enclosing-predication-maker uses-as-value)))
		     ((listp sub-context)
		      (loop for thing in sub-context
			    doing (find-in-current-context thing enclosing-predication-maker kind-of-usage)))))
	     (is-an-object-type-of-predicate (predicate-name)
	       ;; Lucid warns if you provide the environment argument.  This shuts him up.
	       (let ((his-flavor (find-class predicate-name nil #-(or lucid allegro mcl sbcl) 'compile)))
		 #+(or allegro sbcl)
		 (when (and his-flavor (not (class-finalized-p his-flavor)))
		   (finalize-inheritance his-flavor))
		 (and his-flavor (member (find-class 'type-of-mixin) (class-precedence-list his-flavor)))))
	     (is-a-value-of-predicate (predicate-name)
	       ;; Lucid warns if you provide the environment argument.  This shuts him up.
	       (let ((his-flavor (find-class predicate-name nil #-(or mcl allegro sbcl lucid) 'compile)))
		 (and his-flavor
		      (member (find-class 'slot-value-mixin) (class-precedence-list his-flavor)))))
	     (same-as-object (thing)
	       (and (logic-variable-maker-p thing)
		    (equal (logic-variable-maker-name thing) object-name))))
      (declare (dynamic-extent #'find-in-current-context #'is-an-object-type-of-predicate
			       #'is-a-value-of-predicate #'same-as-object))
      (find-in-current-context context context nil)
      (values types uses-as-value uses-in-path))))


(define-predicate-method (expand-forward-rule-trigger type-of-mixin) (support-variable-name truth-value context bound-variables)
  (declare (ignore context bound-variables))
  (unless (eql truth-value +true+)
    (error "The rule pattern ~s does not have a truth-value of true" self))
  `(:object-match ,self ,support-variable-name ,truth-value))

(define-predicate-method (expand-backward-rule-action type-of-mixin) (support-variable-name truth-value other-ask-args context)
  (declare (ignore other-ask-args))
  (unless (eql truth-value +true+)
    (error "The rule pattern ~s does not have a truth-value of true" self))
  (with-predication-maker-destructured (thing type) self
    (multiple-value-bind (all-types uses-as-value uses-in-path)
	(find-all-uses-of-object-type-in-trigger-pattern thing context)
      (declare (ignore all-types))
      ;; If the variable is used as the value of another predicate
      ;; and this isn't the leading thing in the context
      ;; it should turn into a type-of procedure check.
      (if (or uses-as-value (and (null uses-as-value) uses-in-path))
	  `(:procedure
	     ,(if (or (logic-variable-maker-p type) (logic-variable-maker-p thing))
		 `(ask-data ,self +true+ #'(lambda (ignore) (declare (ignore ignore)) (succeed)))
		 `(typep ,thing ',type))
	     ,support-variable-name)
	`(:ignore)))))

(define-predicate-method (clear type-of-mixin :after) (&optional clear-database clear-rules)
  (declare (ignore clear-rules))
  (when clear-database
    (loop for object-type being the hash-values of *all-object-types*
	do (loop for object in (object-type-instances object-type)
	       do (kill object)))))

(define-predicate object-type-of (object-name type) (type-of-mixin default-predicate-model))

(define-predicate ltms:object-type-of (object-name type) (type-of-mixin ltms:ltms-predicate-model)) 

(define-predicate-model part-of-mixin () (tell-error-model ask-data-only-mixin))

(define-predicate-method (ask-data part-of-mixin) (truth-value continuation)
  (unless (or (eql truth-value +true+) (eql truth-value nil))
    (error 'model-can-only-handle-positive-queries
	    :query self
	    :model (type-of self)))
  (with-statement-destructured (parent-object child-object) self
    (cond
     ;; neither is bound
     ((and (unbound-logic-variable-p parent-object)
	   (unbound-logic-variable-p child-object))
      (map-over-object-hierarchy
       #'(lambda (object)
	   (let ((superpart (basic-object-superpart-object object)))
	     (when superpart
	       (with-unification
		   (unify parent-object superpart)
		 (unify child-object object)
		 (stack-let ((backward-support (list self +true+ (basic-object-part-predication object) '(ask-data part-of))))
			    (funcall continuation backward-support))))))
       *root*))
     ;; Parent is unbound, child must be bound but might need dereferencing
     ((unbound-logic-variable-p parent-object)
      (let* ((child-object (joshua-logic-variable-value child-object))
	     (object (basic-object-superpart-object child-object)))
	(when object
	  (with-unification
	      (unify parent-object object)
	    (stack-let ((backward-support (list self +true+ (basic-object-part-predication child-object) '(ask-data part-of))))
		       (funcall continuation backward-support))))))
     ;; child is unbound, parent must be bound but might need dereferencing
     ((unbound-logic-variable-p child-object)
      (let ((parent-object (joshua-logic-variable-value parent-object)))
	(cond 
	 ((typep parent-object 'basic-object)
	  (maphash #'(lambda (key child)
		       (declare (ignore key))
		       (with-unification
			   (unify child-object child)
			   (stack-let ((backward-support (list self +true+ (basic-object-part-predication child) 
							       '(ask-data part-of))))
				    (funcall continuation backward-support))))
		   (basic-object-subparts parent-object)))
	 ((listp parent-object)
	  (let ((path-result (follow-path parent-object)))
	    (with-unification
		(unify path-result child-object)
	      (stack-let ((backward-support (list self +true+ (basic-object-part-predication child-object) '(ask-data part-of))))
			 (funcall continuation backward-support))))))))
     (t
      ;; Both Parent and Child is now known to be instantiated but may need to be dereferenced
      ;; Cases are: a path with first variable, a path, just the parent
      (let ((child-object (joshua-logic-variable-value child-object))
	    (parent-object (joshua-logic-variable-value parent-object)))
	(cond
	 ((and (listp parent-object) (unbound-logic-variable-p (first parent-object)))
	  (when (typep child-object 'basic-object)
	    (loop for parent = child-object then next-parent
		for reverse-key in (reverse (cdr parent-object))
		for next-parent = (basic-object-superpart-object parent)
		unless (eql (subpart-named next-parent reverse-key) parent)
		return (values)
		finally (with-unification
			    (unify (first parent-object) next-parent)
			    (stack-let ((backward-support (list self +true+ (basic-object-part-predication child-object) 
								'(ask-data part-of))))
				     (funcall continuation backward-support))))))
	 ((listp parent-object)
	  (let ((path-result (follow-path parent-object)))
	    (when (eql path-result child-object)
	      (stack-let ((backward-support (list self +true+ (basic-object-part-predication child-object) '(ask-data part-of))))
			 (funcall continuation backward-support)))))
	 ;; here they are both instantiated and atomic
	 ((eql (basic-object-superpart-object child-object) parent-object)
	  (stack-let ((backward-support (list self +true+ (basic-object-part-predication child-object) '(ask-data part-of))))
		     (funcall continuation backward-support)))))))))

(define-predicate-method (expand-forward-rule-trigger part-of-mixin) (support-variable-name truth-value context bound-variables)
  (declare (ignore context))
  (unless (eql truth-value +true+)
    (error "The rule pattern ~s does not have a truth-value of true" self))
  `(:procedure
     (ask ,self
	  #'(lambda (bs) (succeed (supporting-predications-from-backward-support bs)))
	  :do-backward-rules nil :do-questions nil)
     ,support-variable-name
     ,bound-variables
     ,self
     ))


(define-predicate-model named-part-of-mixin () (tell-error-model ask-data-only-mixin))

;;; I added code here to manifest an actual predication for this relationship
;;; This was motivated by the stateful predication stuff in the attack planner
;;; which should get moved into Joshua per se.  But it then turns out that 
;;; since name-part-of relationships never change and are only true in the initial
;;; state, there's really no need for an interned predication around which to 
;;; organize a state map.  But I've left this code in place anyhow.
(define-predicate-method (ask-data named-part-of-mixin) (truth-value continuation)
  (unless (eql truth-value +true+)
    (error 'model-can-only-handle-positive-queries
	   :query self
	   :model (type-of self)))
  (with-statement-destructured (parent-object name child-object) self
    (cond
     ;; neither is bound
     ((and (unbound-logic-variable-p parent-object)
	   (unbound-logic-variable-p child-object))
      (map-over-object-hierarchy
       #'(lambda (object)
	   (let ((superpart (basic-object-superpart-object object)))
	     (when superpart
	       (with-unification
		(unify parent-object superpart)
		(unify child-object object)
		(unify name (role-name object))
		(let ((child-predication (basic-object-part-predication object)))
		  ;; (setf child-predication nil
		  ;; 	(basic-object-my-predication object) nil)
		  ;; (unless child-predication
		  ;;   (setq child-predication (make-predication `(named-part-of ,superpart ,(role-name object) ,object)))
		  ;;   (setf (basic-object-my-predication object) child-predication))
		  (stack-let ((backward-support (list self +true+ child-predication '(ask-data named-part-of))))
		    (funcall continuation backward-support)))))))
       *root*))
     ;; parent isn't bound, child must be, but needs to be dereferenced
     ((unbound-logic-variable-p parent-object)
      (let* ((child-object (joshua-logic-variable-value child-object))
	     (object (basic-object-superpart-object child-object)))
	(when object
	  (with-unification
	   (unify parent-object object)
	   (unify name (role-name child-object))
	   (let ((child-predication (basic-object-part-predication object)))
	     ;; (unless child-predication
	     ;;   (setq child-predication (make-predication `(named-part-of ,object ,(role-name child-object) ,child-object)))
	     ;;   (setf (basic-object-my-predication object) child-predication))
	     (stack-let ((backward-support (list self +true+ child-predication '(ask-data named-part-of))))
	       (funcall continuation backward-support)))))))
     ;; child isn't so parent must be, but needs to be dereferenced
     ((unbound-logic-variable-p child-object)
      (let ((parent-object (joshua-logic-variable-value parent-object)))
	(maphash #'(lambda (key child)
		     (with-unification
		      (unify name key)
		      (unify child-object child)
		      (let ((child-predication (basic-object-part-predication child)))
			;; (unless child-predication
			;;   (setq child-predication (make-predication `(named-part-of ,parent-object ,key ,child)))
			;;   (setf (basic-object-my-predication child) child-predication))
			(stack-let ((backward-support (list self +true+ child-predication '(ask-data named-part-of))))
			  (funcall continuation backward-support)))))
		 (basic-object-subparts 
		  ;; parent is either provided or a pathname
		  (if (typep parent-object 'basic-object)
		      parent-object
		    (follow-path parent-object))))))
     ;; Both Parent and Child are now known to be instantiated
     ;; Cases are: a path with first variable, a path, just the parent
     (t (let ((parent-object (joshua-logic-variable-value parent-object))
	      (child-object (joshua-logic-variable-value child-object)))
	  (cond
	   ((and (listp parent-object) (unbound-logic-variable-p (first parent-object)))
	    (when (typep child-object 'basic-object)
	      (loop for parent = child-object then next-parent
		  for reverse-key in (cons name (reverse (cdr parent-object)))
		  for next-parent = (basic-object-superpart-object parent)
		  unless (eql (subpart-named next-parent reverse-key) parent)
		  return (values)
		  finally (with-unification
			   (unify (first parent-object) next-parent)`
			   (unify name (role-name child-object))
			   (let ((child-predication (basic-object-part-predication child-object)))
			     ;; (unless child-predication
			     ;;   (setq child-predication (make-predication `(named-part-of ,parent-object ,(role-name child-object) ,child-object)))
			     ;;   (setf (basic-object-my-predication child-object) child-predication))
			     (stack-let ((backward-support (list self +true+ child-predication '(ask-data named-part-of))))
			       (funcall continuation backward-support)))))))
	   ((listp parent-object)
	    (let* ((partial-path-result (follow-path parent-object))
		   (path-result (follow-path (list partial-path-result name))))
	      (when (eql path-result child-object)
		(with-unification
		 (unify name (role-name child-object))
		 (let ((child-predication (basic-object-part-predication child-object)))
		   ;; (unless child-predication
		   ;;   (setq child-predication (make-predication `(named-part-of ,(basic-object-superpart-object child-object) ,(role-name child-object) ,child-object)))
		   ;;   (setf (basic-object-my-predication child-object) child-predication))
		   (stack-let ((backward-support (list self +true+ child-predication '(ask-data named-part-of))))
		      (funcall continuation backward-support)))))))
	   ;; here they are both instantiated and atomic
	   ((eql (basic-object-superpart-object child-object) parent-object)
	    (with-unification
	     (unify name (role-name child-object))
	     (let ((child-predication (basic-object-part-predication child-object)))
	       ;; (unless child-predication
	       ;; 	 (setq child-predication (make-predication `(named-part-of ,parent-object ,(role-name child-object) ,child-object)))
	       ;; 	 (setf (basic-object-my-predication child-object) child-predication))
	       (stack-let ((backward-support (list self +true+ child-predication '(ask-data named-part-of))))
		  (funcall continuation backward-support)))))))))))

(define-predicate-method (expand-forward-rule-trigger named-part-of-mixin) (support-variable-name truth-value context bound-variables)
  (declare (ignore context))
  (unless (eql truth-value +true+)
    (error "The rule pattern ~s does not have a truth-value of true" self))
  `(:procedure
     (ask ,self
	  #'(lambda (bs) (succeed (supporting-predications-from-backward-support bs)))
	  :do-backward-rules nil :do-questions nil)
     ,support-variable-name
     ,bound-variables
     ,self
     ))

(defvar *count* 0)
(define-predicate-method (clear part-of-mixin) (&optional clear-database clear-rules)
  (declare (ignore clear-rules))
  (when clear-database
    (maphash #'(lambda (key object)
		(declare (ignore key))
                (incf *count*)
		(kill object))
	     (basic-object-subparts *root*))
    ;; (loop for object being the hash-values of (basic-object-subparts *root*) with-key key
    ;;       do (progn key)
    ;;          (kill object))
    ))

(define-predicate part-of (superpart-object subpart-object) (part-of-mixin default-predicate-model))

(define-predicate named-part-of (superpart-object name subpart-object) 
  (named-part-of-mixin default-predicate-model))

(define-predicate ltms:part-of (superpart-object subpart-object) (part-of-mixin ltms:ltms-predicate-model))

(define-predicate ltms:named-part-of (superpart-object name subpart-object)
  (named-part-of-mixin ltms:ltms-predicate-model))


;;; EQUATED can be both ask'd and tell'd

(define-predicate-model equated-mixin () (no-variables-in-data-mixin))

(define-predicate-method (ask-data equated-mixin) (truth-value continuation)
  (unless (eql truth-value +true+)
    (error 'model-can-only-handle-positive-queries
	    :query self
	    :model (type-of self)))
  (with-statement-destructured (cell1 cell2) self
    (flet ((do-one-cell (cell other)
             #+cloe (declare (sys:downward-function))
	     (when (listp cell) (setq cell (follow-path-to-slot cell)))
	     (loop for (other-cell supporting-predication) in (slot-equal-cells cell)
		   do (with-unification
			(unify (predication-predicate self) (predication-predicate supporting-predication))
			(unify other-cell other)
			(stack-let ((backward-support (list self +true+ supporting-predication )))
			  (funcall continuation backward-support))))))
      (declare (dynamic-extent #'do-one-cell))
    (cond
      ((and (unbound-logic-variable-p cell1)
	    (unbound-logic-variable-p cell2))
       (error 'model-cant-handle-query
	       :query self
	       :model (type-of self)))
      ((unbound-logic-variable-p cell1) (do-one-cell cell2 cell1))
      ((unbound-logic-variable-p cell2) (do-one-cell cell1 cell2))
      (t (unless (typep cell2 'basic-slot)
	   (setq cell2 (follow-path-to-slot cell2)))
	 (do-one-cell cell1 cell2))))))

(define-predicate-method (expand-forward-rule-trigger equated-mixin) (support-variable-name truth-value context bound-variables)
  (declare (ignore context))
  (unless (eql truth-value +true+)
    (error "The rule pattern ~s does not have a truth-value of true" self))
  `(:procedure
    (progn
     (ask ,self
	  #'(lambda (ignore) (declare (ignore ignore)) (succeed))
	  :do-backward-rules nil :do-questions nil)
     ;; return nil so we don't succeed again by accident
     nil)
     ,support-variable-name
     ,bound-variables
     ,self))

(define-predicate-method (insert equated-mixin) ()
  (let ((statement-changed nil))
    (with-statement-destructured (cell1 cell2) self
      (when (listp cell1) (setq cell1 (follow-path-to-slot cell1) statement-changed t))
      (when (listp cell2) (setq cell2 (follow-path-to-slot cell2) statement-changed t))
      (insert-equality cell1 cell2 (if statement-changed `[,(predication-predicate self) ,cell1 ,cell2] self)))))

(define-predicate-method (uninsert equated-mixin) ()
  (with-statement-destructured (cell1 cell2) self
    (uninsert-equality cell1 cell2)))

(define-predicate-method (act-on-truth-value-change equated-mixin) (old-truth-value &optional old-state)
  (declare (ignore old-truth-value old-state))
  (when (eql (predication-truth-value self) +true+)
    (with-statement-destructured (cell1 cell2) self
      (let ((entry-for-cell2 (find cell2 (slot-equal-cells cell1) :key #'second)))
	(when entry-for-cell2
	  (loop for (value . his-pred) in (third entry-for-cell2)
		do (propagate-equality value cell2 his-pred self))
	  (setf (third entry-for-cell2) nil)))
      (let ((entry-for-cell1 (find cell1 (slot-equal-cells cell2) :key #'second)))
	(when entry-for-cell1
	  (loop for (value . his-pred) in (third entry-for-cell1)
		do (propagate-equality value cell1 his-pred self))
	  (setf (third entry-for-cell1) nil))))))

(define-predicate equated (slot1 slot2) (equated-mixin default-predicate-model))
(define-predicate ltms:equated (slot1 slot2) (equated-mixin ltms:ltms-predicate-model))



;;; Presentation Type Niceties

;;;(define-presentation-type slot-presentation ()
;;;   :history t
;;;   :description "a joshua slot"
;;;   :typep ((object)
;;;	   (typep object 'basic-slot))
;;;   :expander 'sys:expression)
;;;
;;;(define-multiple-command (com-change-slot-value :name "Change Slot Value")
;;;			 (*joshua-command-table* *joshua-only-command-table*) 
;;;    ((the-slot 'slot-presentation)
;;;     (new-value 'sys:expression :prompt "New value for this slot" :provide-default nil))
;;;   (tell `[value-of ,the-slot ,new-value]))
;;;
;;;(define-multiple-command (com-set-slot-value :name "Set Slot Value")
;;;			 (*joshua-command-table* *joshua-only-command-table*)
;;;    ((the-slot 'slot-presentation))
;;;   (tell `[value-of ,the-slot ,(accept 'sys:expression :prompt "New value for this slot" :provide-default nil)]))
;;;
;;;(dw:define-presentation-to-command-translator slot-presentation-to-change-slot-value
;;;   (slot-presentation
;;;    :documentation "Modify this slot"
;;;    :gesture :modify
;;;    :priority 10)
;;;   (the-slot)
;;;  `(com-set-slot-value ,the-slot))
;;;
;;;
;;;
;;;(define-presentation-type object-presentation ()
;;;   :history t
;;;   :description "a Joshua object"
;;;   :typep ((object)
;;;	   (typep object 'basic-object))
;;;   :expander 'sys:expression)
;;;
;;;(defmethod (show-object basic-object) (stream)
;;;  (terpri stream)
;;;  (formatting-table (stream :inter-column-spacing (* 5 (send stream :char-width)))
;;;    (formatting-column-headings (stream :underline-p t)
;;;      (formatting-cell (stream)
;;;	(write-string "Slot Name" stream))
;;;      (formatting-cell (stream)
;;;	(write-string "Slot Value" stream)))
;;;    (loop for slot-name in (all-slot-names self)
;;;	  for slot = (funcall slot-name self nil)
;;;	  for slot-has-value = (location-boundp (locf (slot-current-value slot)))
;;;	  for slot-value = (when slot-has-value
;;;			     (slot-current-value slot))
;;;	  do (formatting-row (stream)
;;;	       (formatting-cell (stream)
;;;		 (write-string (string-capitalize-words slot-name) stream))
;;;	       (formatting-cell (stream)
;;;		 (dw:with-output-as-presentation (:stream stream
;;;						  :object slot
;;;						  :type 'slot-presentation)
;;;		   (princ (if slot-has-value slot-value "Unbound") stream))))))
;;;  (princ self stream))
;;;
;;;(define-multiple-command (com-describe-joshua-object :name "Describe Object")
;;;			 (*joshua-command-table* *joshua-only-command-table*)
;;;    ((the-object 'object-presentation))
;;;   (show-object the-object *standard-output*))
;;;
;;;(dw:define-presentation-to-command-translator object-presentation-to-show-object
;;;   (object-presentation
;;;    :documentation "Describe This Object"
;;;    :gesture :control-meta-left
;;;    :priority 10)
;;;   (the-object)
;;;  `(com-describe-joshua-object ,the-object))

#-genera
(eval-when (:compile-toplevel :execute :load-toplevel) (disable-joshua))


