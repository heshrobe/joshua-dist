;;; -*- Mode: Lisp; Package: JOSHUA-INTERNALS; Syntax: ansi-common-lisp -*-
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
;;; Created 2/27/86 21:13:27 by sgr running on GROUSE at SCRC.

;;;
;;; Models and predicates supplied with basic Joshua.
;;;

(in-package :ji)

;;; Signal this flavor (or something built on it) if you can't
;;; handle a query asked of you.

(define-condition model-cant-handle-query (error) 
  ((query . #-lucid (:reader model-cant-handle-query-query :initarg :query) #+lucid nil)
   (model . #-lucid (:reader model-cant-handle-query-model :initarg :model) #+lucid nil))
  (:report (lambda (self stream)
	     (format stream "The query ~s could not be processed~&because it is more general than is supported by the ~s model."
		     (model-cant-handle-query-query self)
		     (model-cant-handle-query-model self)))))

(define-condition model-can-only-handle-positive-queries (model-cant-handle-query) 
  ((query . #-lucid (:reader model-can-only-handle-positive-queries-query :initarg :query) #+lucid nil)
   (model . #-lucid (:reader model-can-only-handle-positive-queries-model :initarg :model) #+lucid nil))
  (:report (lambda (self stream)
	     (format stream "The query ~s could not be processed~&because the ~s model only handles true queries."
		     (model-cant-handle-query-query self)
		     (model-cant-handle-query-model self)))))

(define-predicate-model false-query-error-model
    ()
  ())

(define-predicate-method (ask false-query-error-model :before) (truth-value continuation do-backward-rules do-questions)
  (declare (ignore continuation do-backward-rules do-questions))
  (unless (eql truth-value *true*)
    (error 'model-can-only-handle-positive-queries
	    :query self
	    :model (type-of self))))

;;;
;;; A model for predications that are really just data structures (like lists)
;;; and shouldn't ever be given to TELL or ASK.  These are meant to be embedded in
;;; other predications.
;;;

;;; I suppose these ought to signal separate conditions that the user could bind.

(define-predicate-model tell-error-model () ()) 

(define-predicate-method (tell tell-error-model) (statement justification)
  (declare (ignore statement justification))
  ;; cause an error if you try to TELL one of these
  (error "Predication ~S is built on TELL-ERROR-MODEL, so you can't TELL it." (type-of self)))

(define-predicate-model ask-error-model () ()) 

(define-predicate-method (ask ask-error-model) (query continuation do-rules do-questions)
  (declare (ignore query continuation do-rules do-questions))
  ;; cause an error if you try to ASK one of these
  (error "Predication ~S is built on ASK-ERROR-MODEL, so you can't ASK it." (type-of self)))
	 
(define-predicate-model error-model () (tell-error-model ask-error-model))


;;; Just a notational convenience for writing unification calls
;;; in rules
(define-predicate-model unification-model
    ()
  (default-rule-compilation-model tell-error-model)
  )

(define-predicate-method (ask unification-model) (truth-value continuation do-backward-rules do-queries)
  (declare (ignore do-backward-rules do-queries))
  (with-statement-destructured (first second) self
    (when (and (unbound-logic-variable-p first)
	       (unbound-logic-variable-p second))
      (error 'model-cant-handle-query
	  :query self
	  :model 'unification-model))
    (if (eql truth-value *true*)
	(with-unification
	    (when (unify first second)
	      (with-stack-list (just self truth-value self)
		(funcall continuation just))))
	  (block try-to-fail
	      (with-unification
		  (unify first second)
		(return-from try-to-fail nil))
	    (with-stack-list (just self truth-value self)
	      (funcall continuation just))))))

(define-predicate unify (first second)
  (unification-model)
  )

;;;
;;; The discrimination net model.
;;;

(defparameter *data-discrimination-net* (make-discrimination-net-node
					  :token '*begin-predication*)
  "Root node of the default database discrimination net.")

(define-predicate-model discrimination-net-data-mixin () ())

(define-predicate-method (insert discrimination-net-data-mixin) ()
  ;; stick it in the dn
  (discrimination-net-insert *data-discrimination-net* self))

(define-predicate-method (uninsert discrimination-net-data-mixin) ()
  ;; get it out of the dn
  (discrimination-net-uninsert *data-discrimination-net* self))

(define-predicate-method (clear discrimination-net-data-mixin) (&optional clear-database clear-rules)
  (declare (ignore clear-rules))
  ;; how to clear discrimination-net modelled predicates
  (when clear-database
    (discrimination-net-clear *data-discrimination-net*)))

(define-predicate-method (fetch discrimination-net-data-mixin) (continuation)
  ;; how to fetch data from the discrimination net.
  (discrimination-net-fetch *data-discrimination-net* self continuation))

(define-predicate-model default-predicate-model
	()
	(discrimination-net-data-mixin trivial-tms-mixin default-protocol-implementation-model))

;;;
;;; AND, OR & NOT
;;;
(define-predicate-model and-model () (default-rule-compilation-model)
  ;;needs no clear-model method, 'cause it's never really told.
  )

(define-predicate-method (say and-model) (&optional (stream *standard-output*))
  ;; how to say conjunctions
  (with-slots (predications) self
    (loop for p-tail on predications
	  as p = (car p-tail)
	  doing (say p stream)
	  while (cddr p-tail)
	  doing (write-string ", " stream)
	  finally
	    (when (cdr p-tail)
	      (write-string " and " stream)
	      (say (cadr p-tail) stream)))))
									    
(define-predicate-method (expand-forward-rule-trigger and-model) (var-name truth-value context bound-variables)
  (declare (ignore var-name bound-variables))
  (let ((statement (predication-maker-statement self)))
    (cond ((eql truth-value *true*)
	   `(:and ,@(parse-pattern-for-expand-forward-trigger statement *true* context)))
	  (t `(:or ,@(parse-pattern-for-expand-forward-trigger statement *false* context))))))

(defun parse-pattern-for-expand-forward-trigger (statement truth-value context)
  (loop with triggers = (cdr statement)
	until (null triggers)
	for (trigger . rest) = triggers
	for support-variable-name = nil
	and bound-variable-names = nil
	do (loop with stuff = rest
		 for token = (car stuff)
		 for value = (cadr stuff)
		 do (cond ((eql token :support)
			   (setq support-variable-name value
				 stuff (cddr stuff)))
			  ((eql token :bound-variables)
			   (setq bound-variable-names value
				 stuff (cddr stuff)))
			  (t (setq triggers stuff)
			     (return (values)))))
	collect (expand-forward-rule-trigger trigger
					     (when support-variable-name
					       (logic-variable-maker-name support-variable-name))
					     truth-value
					     context bound-variable-names)))

(define-predicate-method (expand-backward-rule-action and-model) (support-variable truth-value other-ask-args context)
  (declare (ignore support-variable other-ask-args))
  (let ((statement (predication-maker-statement self)))
    (cond ((eql truth-value *true*)
	   `(:and ,@(parse-pattern-for-expand-backward-action statement *true* context)))
	  (t `(:or ,@(parse-pattern-for-expand-backward-action statement *false* context))))))

(defun parse-pattern-for-expand-backward-action (statement truth-value context)
  (loop with triggers = (cdr statement)
	until (null triggers)
	for (trigger . rest) = triggers
	for support-variable-name = nil
	and do-rules = t
	and do-questions = '.do-questions.
	do (loop with stuff = rest
		 for token = (car stuff)
		 for value = (cadr stuff)
		 do (cond ((eql token :support)
			   (setq support-variable-name value
				 stuff (cddr stuff)))
			  ((eql token :do-backward-rules)
			   (setq do-rules value
				 stuff (cddr stuff)))
			  ((eql token :do-questions)
			   (setq do-questions value
				 stuff (cddr stuff)))
			  (t (setq triggers stuff)
			     (return (values)))))
	collect (expand-backward-rule-action trigger
					     (when support-variable-name
					       (logic-variable-maker-name support-variable-name))
					     truth-value
					     (list do-rules do-questions)
					     context)))

(define-predicate-method (tell and-model) (truth-value justification)
  (with-slots (predications) self
    (truth-value-case truth-value
      (*true*
       (loop for predication in predications
	     doing (tell-internal predication *true* justification)))
      (*false*
       (cond ((null predications)
	      ;; (tell [not [and]])
	      (error "[NOT [AND]] is a contradiction."))
	     ((and (consp predications) (null (rest predications)))
	      ;; (tell [not [and <single thing>])
	      (tell-internal (first predications) *false* justification))
	     (t
	      (error "I don't know how to TELL [NOT [AND <two or more things>]]."))))))
  ;; return the conjunct
  self)

(defun list-of-predications-p (thing)
  (or (null thing)
      (and (listp thing)
	   (predicationp (car thing))
	   (list-of-predications-p (cdr thing)))))

(define-predicate-method (ask and-model) (truth-value continuation do-backward-rules do-questions)
  (with-slots (predications) self
    (labels ((ask-and (preds derivations)
	       (if (null preds)
		   ;; call the AND's continuation
		   (let ((and-derivation `(,self ,truth-value and
					   ,@(reverse derivations))))
		     (funcall continuation and-derivation))
		   ;; ASK about one predication, then the rest
		   (ask-internal (first preds)
				 *true*
				 #'(lambda (derivation)
				     (let ((more-derivations `(,derivation ,@derivations)))
				       (ask-and (rest preds) more-derivations)))
				 do-backward-rules do-questions))))
      (unless (list-of-predications-p predications)
	(error
	  'model-cant-handle-query
	  :query self
	  :model 'and-model))
      (truth-value-case truth-value
	(*true*
	 (ask-and predications nil))
	(*false*
	 (loop for predication in predications
	       do (ask-internal predication
				*false*
				continuation
				do-backward-rules do-questions))))
      (values))))

;;; This model treats the telling of everything within the and as atomic
;;; as far as forward rule triggering goes.

(define-predicate-model atomic-and-model () (and-model))

(define-predicate-method (tell atomic-and-model :around) (truth-value justification)
  (declare (ignore truth-value justification))
  (with-atomic-action
    (call-next-method)))

(define-predicate-model or-model () (default-rule-compilation-model)
  ;; needs no clear-model method, 'cause it's never really told
  )

(define-predicate-method (say or-model) (&optional (stream *standard-output*))
  ;; how to say disjunctions
  (with-slots (predications) self
    (loop for p-tail on predications
	  as p = (car p-tail)
	  doing (say p stream)
	  while (cddr p-tail)
	  doing (write-string ", " stream)
	  finally
	    (when (cdr p-tail)
	      (write-string " or " stream)
	      (say (cadr p-tail) stream)))))

(define-predicate-method (expand-forward-rule-trigger or-model) (var-name truth-value context bound-variables)
  (declare (ignore var-name bound-variables))
  (let ((statement (predication-maker-statement self)))
    (cond ((eql truth-value *true*)
	   `(:or ,@(parse-pattern-for-expand-forward-trigger statement *true* context)))
	  (t `(:and ,@(parse-pattern-for-expand-forward-trigger statement *false* context))))))

(define-predicate-method (expand-backward-rule-action or-model) (support-variable-name truth-value other-ask-args context)
  (declare (ignore support-variable-name other-ask-args))
  (let ((statement (predication-maker-statement self)))
    (cond ((eql truth-value *true*)
	   `(:or ,@(parse-pattern-for-expand-backward-action statement *true* context)))
	  (t `(:and ,@(parse-pattern-for-expand-backward-action statement *false* context))))))

(define-predicate-method (tell or-model) (truth-value justification)
  (with-slots (predications) self
    (truth-value-case truth-value
      (*true*
       (cond ((null predications)
	      ;; (tell [or])
	      (error "[OR] is a contradiction."))
	     ((and (consp predications) (null (rest predications)))
	      ;; (tell [or <single thing>])
	      (tell-internal (first predications) *true* justification))
	     (t
	      (error "I don't know how to TELL [OR <two or more things>]."))))
      (*false*
       (loop for predication in predications
	     doing (tell-internal predication *false* justification)))))
  ;; return the conjunct
  self)

(define-predicate-method (ask or-model) (truth-value continuation do-backward-rules do-questions)
  (with-slots (predications) self
    (unless (list-of-predications-p predications)
      (error
	'model-cant-handle-query
	:query self
	:model 'or-model))
    (labels ((ask-and (preds derivations)
	       (if (null preds)
                   ;; call the AND's continuation
                   (with-stack-list* (and-derivation self truth-value 'or (reverse derivations))
		     (funcall continuation and-derivation))
		   ;; ASK about one predication, then the rest
		   (ask-internal (first preds)
				 *false*
				 #'(lambda (derivation)
                                     (with-stack-list* (more-derivations derivation derivations)
				       (ask-and (rest preds) more-derivations)))
				 do-backward-rules do-questions))))
      (truth-value-case truth-value
	(*true*
	 (loop for predication in predications
	       do (ask-internal predication
				*true*
				continuation
				do-backward-rules do-questions)))
	(*false*
	 (ask-and predications nil)))
      (values))))

(define-predicate-model not-model () (default-rule-compilation-model)
  )

(define-predicate-method (expand-forward-rule-trigger not-model) (name truth-value context bound-variables)
  (let ((statement (predication-maker-statement self)))
    (expand-forward-rule-trigger (cadr statement) name (negate-truth-value truth-value) context bound-variables)))

(define-predicate-method (expand-backward-rule-action not-model) (name truth-value other-ask-args context)
  (let ((statement (predication-maker-statement self)))
    (expand-backward-rule-action (cadr statement) name (negate-truth-value truth-value) other-ask-args context)))

(define-predicate-method (tell not-model) (truth-value justification)
  (with-slots (predication) self
    (tell-internal predication (negate-truth-value truth-value) justification)))

(define-predicate-method (ask not-model) (truth-value continuation do-backward-rules do-questions)
  (with-slots (predication) self
    (unless (predicationp predication)
      (error
	'model-cant-handle-query
	:query self
	:model 'not-model))
    (ask-internal predication (negate-truth-value truth-value)
		  continuation
		  do-backward-rules do-questions)))

(define-predicate and-internal (&rest predications) (and-model trivial-tms-mixin)
  #+++ignore (:conc-name and-)
  #+++ignore :readable-instance-variables
  :destructure-into-instance-variables)

(define-predicate atomic-and (&rest predications) (atomic-and-model trivial-tms-mixin)
  #+++ignore :readable-instance-variables
  :destructure-into-instance-variables)

(define-predicate or-internal (&rest predications) (or-model trivial-tms-mixin)
  #+++ignore (:conc-name or-)
  #+++ignore :readable-instance-variables
  :destructure-into-instance-variables)

(define-predicate not-internal (predication) (not-model trivial-tms-mixin)
  #+++ignore (:conc-name not-)
  #+++ignore :readable-instance-variables
  :destructure-into-instance-variables)

(defmethod not-predication ((p not-internal)) (with-slots (predication) p predication))

(define-predicate-synonym and and-internal)
(define-predicate-synonym or  or-internal)
(define-predicate-synonym not not-internal) 

;;;
;;; Stuff with funny things in functor positions has to do something
;;; else, of course.  For example, [[is not] is not [not is]] has [is
;;; not] as a functor.  Reasonable things other than symbols that might
;;; occur in functor position include variables (anything else?).
;;; Variable-functor predications probably go into their own dn?
;;;

(define-predicate-model variable-predicate-model () (default-predicate-model))

(define-predicate-method (ask variable-predicate-model) (truth-value continuation do-backward-rules do-questions)
  (maphash
    #'(lambda (predicate descriptor &aux (args-info (predicate-descriptor-args-info descriptor)))
	(when args-info
	  (catch 'ask-variable-predicate-escape
	       (let* ((arglist (loop for count below (predicate-max-args args-info)
				     collect (make-unbound-logic-variable
					       (gentemp *anonymous-prefix*)) into variable-list
				     finally
				       (unless (zerop (predicate-rest-arg args-info))
					 (let ((tailvar (make-unbound-logic-variable
							  (gentemp *anonymous-prefix*))))
					   (if (null variable-list)
					       (setq variable-list tailvar)
					       (setf (cdr (last variable-list)) tailvar))))
				       (return variable-list)))
		      (query (make-predication `(,predicate ,.arglist))))
		 (unless (typep query 'variable-predicate-model)
		   (with-unification
		     (unify query self)
		     (ask-internal query truth-value continuation do-backward-rules do-questions)))))))
    *all-predicates*)
  (values))

(define-predicate variable-predicate (&rest arguments) (variable-predicate-model trivial-tms-mixin))

;;;
;;; Functions that various example programs (hence, presumably, user programs) find useful.
;;;

(defun different-objects (object1 object2)
  ;; test that 2 things are distinct -- used as rule action to prevent
  ;; trying to put a block on top of itself and stuff like that
  (not (or (eql object1 object2)
	   ;; don't deal with unbound vars, they could later get instantiated the same
	   (unbound-logic-variable-p object1)
	   (unbound-logic-variable-p object2))))

(define-predicate-model ask-data-only-mixin () ()) ;only ask in database

(define-predicate-method (ask ask-data-only-mixin) (truth-value continuation do-backward-rules do-questions)
  (declare (ignore do-backward-rules do-questions))
  (ask-data self truth-value continuation)
  ;; make it clear that there is no interesting return value
  (values))

(define-predicate-model ask-rules-only-mixin () ())	;only ask rules

(define-predicate-method (ask ask-rules-only-mixin) (truth-value continuation do-backward-rules do-questions)
  (when do-backward-rules
    (ask-rules self truth-value continuation do-questions))
  ;; make it clear that there is no interesting return value
  (values))

(define-predicate-model ask-questions-only-mixin () ())	;only ask questions

(define-predicate-method (ask ask-questions-only-mixin) (truth-value continuation do-backward-rules do-questions)
  (declare (ignore do-backward-rules))
  (when do-questions
    (ask-questions self truth-value continuation))
  ;; make it clear that there is no interesting return value
  (values))

(define-predicate-model ask-data-and-rules-only-mixin () ())

(define-predicate-method (ask ask-data-and-rules-only-mixin) (truth-value continuation do-backward-rules do-questions)
  (ask-data self truth-value continuation)
  (when do-backward-rules
    (ask-rules self truth-value continuation do-questions))
  ;; make it clear that there is no interesting return value
  (values))

(define-predicate-model ask-rules-and-questions-only-mixin () ())

(define-predicate-method (ask ask-rules-and-questions-only-mixin) (truth-value continuation do-backward-rules do-questions)
  (when do-backward-rules
    (ask-rules self truth-value continuation do-questions))
  (when do-questions
    (ask-questions self truth-value continuation))
  ;; make it clear that there is no interesting return value
  (values))

(define-predicate-model ask-data-and-questions-only-mixin () ())

(define-predicate-method (ask ask-data-and-questions-only-mixin) (truth-value continuation do-backward-rules do-questions)
  (declare (ignore do-backward-rules))
  (ask-data self truth-value continuation)
  (when do-questions
    (ask-questions self truth-value continuation))
  ;; make it clear that there is no interesting return value
  (values))


;;; Asserted Data without variables

(define-predicate-model no-variables-in-data-mixin () ())

(define-predicate-method (data-is-guaranteed-variable-free no-variables-in-data-mixin) () t)

(define-predicate-method (tell no-variables-in-data-mixin :before) (truth-value justification)
  (declare (ignore truth-value justification))
  (unless (predication-logic-variable-free-p self)
    (error "Trying to TELL ~s which contains logic-variables but shouldn't" self)))

(define-predicate-method (write-forward-rule-full-matcher no-variables-in-data-mixin) (predication-variable environment)
  (declare (ignore predication-variable environment))
  nil)



;;;
;;; Add KNOWN, a modal operator.  I think known corresponds to the BOX operator of
;;;  modal logics.  The modal S operator (~BOX~), is [not [known [not ...]]].
;;;

(define-predicate-model known-model () (default-rule-compilation-model)
  )


(define-predicate-method (expand-forward-rule-trigger known-model) (support-variable-name truth-value context bound-variables)
  (declare (ignore context))
  (when (null support-variable-name) (setq support-variable-name (gensym)))
  (let ((query (if (eql truth-value *true*) self `(predication-maker '(not ,self)))))
    `(:procedure
       (ask ,query
	    #'(lambda (bs) 
		(succeed (supporting-predications-from-backward-support bs)))
	    :do-backward-rules nil :do-questions nil)
       ,support-variable-name
       ,bound-variables
       ,self)))

(define-predicate-method (ask known-model) (truth-value continuation do-backward-rules do-questions)
  (with-slots (predication) self
    (unless (predicationp predication)
      (error
	'model-cant-handle-query
	:query self
	:model 'known-model))
    (truth-value-case truth-value
      (*true*
       (flet ((known-continuation (derivation)
                (with-stack-list (known-derivation self truth-value 'known derivation)
		  (funcall continuation known-derivation))))
	 (ask-internal predication *true* #'known-continuation
		       do-backward-rules do-questions)
	 (handler-case (ask-internal predication *false* #'known-continuation
				     do-backward-rules do-questions)
	   (model-can-only-handle-positive-queries () nil))))
      (*false*
       (when (block try-queries
	       (flet ((unknown-continuation (b-s)
			(declare (ignore b-s))
			(return-from try-queries nil)))
		 (ask-internal predication *true* #'unknown-continuation
			       do-backward-rules do-questions)
		 (handler-case (ask-internal predication *false* #'unknown-continuation
					     do-backward-rules do-questions)
		   (model-can-only-handle-positive-queries () nil))	       
		 t))
	 (with-stack-list (unknown-derivation self truth-value 'unknown)
	   (funcall continuation unknown-derivation))))))
  (values))

(define-predicate known (predication) (known-model trivial-tms-mixin)
  :destructure-into-instance-variables)

(define-predicate-model provable-model () (default-rule-compilation-model)
  )

(define-predicate-method (ask provable-model) (truth-value continuation do-backward-rules do-questions)
  (with-slots (predication) self
    (unless (predicationp predication)
      (error
	'model-cant-handle-query
	:query self
	:model 'provable-model))
    (truth-value-case truth-value
      (*true*
       (ask-internal predication *true*
		     #'(lambda (derivation)
                         (with-stack-list (provable-derivation self truth-value 'provable derivation)
			   (funcall continuation provable-derivation)))
		     do-backward-rules do-questions))
      (*false*
       (when (block try-queries
	       (ask-internal predication *true*
			     #'(lambda (b-s)
				 (declare (ignore b-s))
				 (return-from try-queries nil))
			     do-backward-rules do-questions)
	       t)
         (with-stack-list (unprovable-derivation self truth-value 'provable)
	   (funcall continuation unprovable-derivation))))))
  (values))

(define-predicate provable (predication) (provable-model trivial-tms-mixin)
  :destructure-into-instance-variables)

;;;; General TMS stuff.

;;; TMS-CONTRADICTION
;;; The flavor that is signalled when a contradiction is detected
;;; This has all the information you'd like:
;;;   The contradictory predication if any
;;;   The unsatisfiable clause if any
;;;   All the ground support
;;;    The subset of this which are premises
;;;    The subset of this which aren't premises
;;;    It takes a little time to calculate the last two sets, but it takes much longer
;;;       to make and process the signal so it doesn't matter
;;; TMS-HARD-CONTRADICTION
;;; A hard contradiction is signalled when the only support for a contradiction are premises.
;;; This is something that a user might want to consider really wrong, so we make a specific
;;; signal for this case.

;;; The general strategy:
;;;
;;; When a contradiction is detected we signal a tms-contradiction.  There is a default
;;; handler for this signal which handles a couple of cases automatically.  This handler can
;;; be overridden by specifically handling the tms-contradiction condition.
;;;
;;; The default handler takes special action if there is exactly one non-premise supporter.
;;; In that case it unjustifys that one guy and proceeds.
;;; If all the supporters are premises it signals a tms-hard-contradiction.
;;;
;;; By condition binding either or both of these conditions you can gain
;;; whatever control you'd like.

(defvar *graphical-display-of-contradiction* nil)

(define-condition tms-contradiction (serious-condition)
 ((justification . #-lucid (:reader tms-contradiction-justification :initarg :justification) #+lucid nil)
  (contradictory-predication . #-lucid (:reader tms-contradiction-contradictory-predication :initarg :contradictory-predication)
			       #+lucid nil)			     
  (premises . #-lucid (:reader tms-contradiction-premises :initarg :premises) #+lucid nil)
  (non-premises . #-lucid (:reader tms-contradiction-non-premises :initarg :non-premises) #+lucid nil)
  (support . #-lucid (:reader tms-contradiction-support :initarg :support) #+lucid nil))
 (:report (lambda (c s)
	    (report-tms-contradiction
	     s
	     (tms-contradiction-premises c)
	     (tms-contradiction-non-premises c)
	     (tms-contradiction-justification c)
	     (tms-contradiction-contradictory-predication c)))))

(setf (get 'tms-contradiction 'tms-hard-contradiction-flavor) 'tms-hard-contradiction)

(defun tms-contradiction-hard-contradiction-flavor (condition)
  (or (get (type-of condition) 'tms-hard-contradiction-flavor)
      (error "No hard contradiction flavor found for ~s." (type-of condition))))

(define-condition tms-hard-contradiction (serious-condition)
 ((justification . #-lucid (:reader tms-hard-contradiction-justification :initarg :justification) #+lucid nil)
  (contradictory-predication . #-lucid (:reader tms-hard-contradiction-contradictory-predication :initarg :contradictory-predication)
			       #+lucid nil)
  (premises . #-lucid (:reader tms-hard-contradiction-premises :initarg :premises) #+lucid nil))
 (:report (lambda (c s)
	    (report-tms-contradiction
	     s
	     (tms-hard-contradiction-premises c)
	     nil
	     (tms-hard-contradiction-justification c)
	     (tms-hard-contradiction-contradictory-predication c)))))
		       

(defmacro without-indentation (ignore &body body)
  (declare (ignore ignore))
  `(progn ,. body))

(defun report-tms-contradiction (stream premises non-premises justification contradictory-predication)
  ;; how to report a contradiction.
  (format stream "Backtracking because:")
  (when contradictory-predication
    (fresh-line stream)
    (print-without-truth-value contradictory-predication stream)
    (format stream " is contradictory")
    (unless non-premises
      (format stream " but has only premises in its support")))
  (when justification
    (format stream "~&~S is ~:[an~;the~] unsatisfiable ~(~a~)."
	    justification contradictory-predication
	    (class-name (class-of justification))))
  (cond ((null *graphical-display-of-contradiction*)
	 ;; You can't draw graphs on all streams
	 (when non-premises
	   (format stream "~%The non-premise, primitive support underlying it is:")
	   (format-textual-list non-premises
				#'(lambda (x stream)
				    (multiple-value-bind (mnemonic predication)
					(destructure-justification x)
					(format stream "~&    The ~:(~a~) that ~a is ~:(~a~)"
						mnemonic
						(print-without-truth-value predication nil)
						(truth-value-name (predication-truth-value predication)))))
				:conjunction "and"
				:stream stream))
	 (when premises
	   (format stream "~%The premise support underlying it is:")
	   (format-textual-list premises
				#'(lambda (x stream)
				    (multiple-value-bind (mnemonic predication)
					(destructure-justification x)
					(format stream "~&    The ~:(~a~) that ~a is ~:(~a~)"
						mnemonic
						(print-without-truth-value predication nil)
						(truth-value-name (predication-truth-value predication)))
					#+genera
				      (dw:with-output-as-presentation (:stream stream
								       :object x
								       :single-box t
								       :type `((member ,@premises)))
					(format stream "~&    The ~:(~a~) that ~a is ~:(~a~)"
						mnemonic
						(print-without-truth-value predication nil)
						(truth-value-name (predication-truth-value predication))))))
				:conjunction "and"
				:stream stream)))))

(defun contradiction-restart-unjustify-subset (subset)
  (dolist (item subset) (remove-justification item))
  (values :unjustify-subset subset))

(defun contradiction-restart-interactively-unjustify-subset (premises non-premises)
  (let* ((preds (append premises non-premises))
	 (pred-count (length preds))
	 preds-to-zap)
    (when (> pred-count 1)
      (format *query-io* "~&There are ~d predications in the support:" pred-count))
    (dolist (p non-premises)
      (when (yes-or-no-p "~%Unjustify ~a? " p)
	(push p preds-to-zap)))
    (unless preds-to-zap
      (dolist (p premises)
	(when (yes-or-no-p "~%Unjustify ~a? " p)
	  (push p preds-to-zap))))
    (when (and (> pred-count 1)
	       preds-to-zap)
      (unless 
        (yes-or-no-p "~{~%  ~a~}~&OK to unjustify these? " preds-to-zap)
        (abort)))			;back to debugger cmd level
    (when (null preds-to-zap)
      (unless
       (yes-or-no-p "~%Really restart but don't unjustify anything? ")
       (abort)))			;ditto
    (list preds-to-zap)))

;;;  No global handlers in KMP CL error system, so must use this macro...
(defmacro with-automatic-unjustification (&body body)
  `(handler-bind ((tms-contradiction 'singleton-support-is-automatic-unjustify))
     ,. body))

(defun singleton-support-is-automatic-unjustify (condition-object)
  (let* ((premises (tms-contradiction-premises condition-object))
	 (non-premises (tms-contradiction-non-premises condition-object)))
    (cond ((= 1 (length non-premises))
	   ;; The real nice case, exactly one non-premise supporter, unjustify it.
	   ;; If for some reason no restart established 'round the signal,
	   ;; just enter the debugger.
	   (when (find-restart :unjustify-subset)
		 (invoke-restart :unjustify-subset non-premises))
	   nil)
	  ((null non-premises)
	   ;; If there are only premises, signal the hard-condition error
	   ;; use the one corresponding to the condition we got.
	   (error (tms-contradiction-hard-contradiction-flavor condition-object)
		   :premises premises
		   :justification (tms-contradiction-justification condition-object)
		   :contradictory-predication (tms-contradiction-contradictory-predication condition-object)))
	  ;; otherwise decline to handle and let dbg take over
	  (t nil))))

;;;; Some useful functions to use with predications that satisfy non-trivial-tms-p

(defvar *generation-counter* 0 "A generation counter used by some TMS algorithms.")

(define-predicate-model basic-tms-mixin
			((generation-mark :initform -1 :initarg :generation-mark
					  :accessor predication-generation-mark))
			()
			)

(defmethod init-plist append ((p basic-tms-mixin))
  (list :generation-mark (predication-generation-mark p)))

(define-predicate-method (nontrivial-tms-p basic-tms-mixin) ()
  ;; this one really supports the TMS protocol.
  t)

(define-predicate-method (untell basic-tms-mixin) ()
  (loop for justification in (all-justifications self)
	doing (unjustify self justification))
  (uninsert self))

(define-predicate-method (support basic-tms-mixin) (&optional filter)
  "Returns the predication leaves of the support tree for a predication"
  ;; filter is a function to apply to the unit justification to see if you want to collect it
  ;; nil means collect everything
  (let ((generation (incf *generation-counter*)))
    (labels ((trace-support-through-justification (justification)
	       (multiple-value-bind (mnemonic consequent true-support false-support unknown-support)
		   (destructure-justification justification)
		   (declare (ignore mnemonic))
		 (cond ((and (and (null true-support) (null false-support) (null unknown-support))
			     (or (null filter) (funcall filter justification)))
			(list consequent))
		       (t (loop for supporter in (append true-support false-support unknown-support)
				nconc (trace-support-through-predication supporter)
                                )))))
	     (trace-support-through-predication (predication)
	       (when (null (predication-generation-mark predication)) (setf (predication-generation-mark predication) -1))
	       (unless (= (predication-generation-mark predication) generation)
		 (setf (predication-generation-mark predication) generation)
		 (let ((current-justification (current-justification predication)))
                   (when current-justification
                     (if (and filter (funcall filter predication))
                       (list predication)
		       (trace-support-through-justification current-justification)))))))
      (trace-support-through-predication self))))

(define-predicate-method (consequences basic-tms-mixin) ()
  (let ((answer nil))
    (labels ((collect-pred (pred)
               (unless (member pred answer)
                 (push pred answer)
                 (trace-pred pred)))
             (trace-pred (pred)
               (loop for just in (all-justifications pred)
                     do (multiple-value-bind (mnemonic consequent) (destructure-justification just)
                         (declare (ignore mnemonic))
                         (when (and (not (eql consequent self))
                                    (eql just (current-justification consequent)))
                           (collect-pred consequent))))))
      (trace-pred self))
    answer))

(define-predicate-method (find-independent-support basic-tms-mixin) (assumption)
  (let ((dependents (consequences assumption)))
    (flet ((filter (thing)
             (typecase thing
               (justification (multiple-value-bind (mnemonic consequent) (destructure-justification thing)
                                (declare (ignore mnemonic))
                                (not (member consequent dependents))))
               (predication (not (member thing dependents))))))
     (delete assumption (support self #'filter)))))

(defun sort-by-truth-value (predications)
  (let (true-support false-support unknown-support)
    (loop for pred in predications
          do (truth-value-case (predication-truth-value pred)
              (*true* (push pred true-support))
              (*false* (push pred false-support))
              (*unknown* (push pred unknown-support))))
    (values true-support false-support unknown-support)))

;;; Find the primitive support with a specific mnemonic

(defun support-with-name (database-predication the-name)
  (support database-predication
	   #'(lambda (justification)
	       (if (predicationp justification)
		   nil
		 (multiple-value-bind (mnemonic consequent true-support false-support unknown-support)
		     (destructure-justification justification)
		   (declare (ignore consequent))
		   (and (eq the-name mnemonic)
			(null true-support)
			(null false-support)
			(null unknown-support)))))))

(defun assumption-support (database-predication) (support-with-name database-predication :assumption))

(defun premise-support (database-predication) (support-with-name database-predication :premise))

(defun remove-justification (justification)
  (multiple-value-bind (consequent supportee) (destructure-justification justification)
    (declare (ignore consequent))
    (unjustify supportee justification)))

(defun all-predications-p (predications)
  ;; true if arg is a list of predications.
  (and predications
       (every #'predicationp predications)))



#-genera
(defun show-joshua-database ()
  (flet ((handler (condition) 
           (declare (ignore condition))
           (throw 'ji::ask-variable-predicate-escape (values))))
    (declare (dynamic-extent handler))
    (handler-bind ((error #'handler))
      (ask (predication-maker '((logic-variable-maker ?anonymous1) . 
                                (logic-variable-maker ?anonymous2)))
           #'print-query
           :do-backward-rules nil)
      (ask (predication-maker '(not (predication-maker '((logic-variable-maker ?anonymous1) . 
                                                          (logic-variable-maker ?anonymous2)))))
           #'print-query
           :do-backward-rules nil)
      )))
