;;; -*- Mode: Lisp; Package: JI; Syntax: Ansi-common-lisp  -*-
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
;;; Created 2/25/87 18:05:42 by sgr running on GROUSE at SCRC.

;;;
;;; The protocol which all Joshua predications obey.  This is the real heart & soul of Joshua.
;;;

(in-package :ji)


(defparameter *running-rule* nil "For defaulting justifications.  Don't ever SETQ this.")
(defparameter *goal* nil "The goal being pursued by *running-rule* if running a backward rule.  Don't ever SETQ this.")

(defun succeed (&optional support)
  (declare (ignore support))
  ;; stub to prevent this from being mis-used.  The rule compiler knows when to FLET this.
  (error "You can't call SUCCEED at top level."))

;;; Perhaps this should be calling si:parse-body-declarations to merge decls.
;;; Put declarations from defgeneric in method body, somehow?  (Have to dwim on argument names.)

(def-defining-form define-protocol-function
   ;; used only by Joshua implementors, to create new elements of the protocol.
   :definer
   ((name arglist &body body &key
	  documentation
	  declaration
	  function
	  (internal-name name)
	  method-combination			;; 
	  argument-precedence-order		;;
	  generic-function-class		;; Note args to CLOS Defgeneric!!
	  method-class				;;  
	  ;; these drive define-predicate-method
	  (definer 'method-protocol-definer)
	  shower				;default flavorish thing is pretty good
	  (killer 'method-protocol-killer)
	  (function-spec-handler 'method-protocol-function-spec-handler))
    #-(or genera cloe-developer)
    (declare (ignore killer))
    (declare (ignore shower #-allegro body))
    ;; Note:  The spec says that a defgeneric declaration can only be an optimize declaration
    ;;        Allegro (or genera, cloe, mcl I guess) doesn't seem to care
    ;;        But SBCL is more specific, so I'm doing this.  It might be that I should
    ;;        do it for all lisps.
    #+sbcl
    (when declaration
      (setf (rest declaration)
	    (loop for decl in (rest declaration)
	       for (name) = decl
	       when (eql name 'optimize)
	       collect decl)))
    `(progn
       #+(or genera cloe-developer) (record-source-file-name ',name 'define-protocol-function)
       ;; define the generic function for this protocol element
       (defgeneric ,internal-name ,arglist
         #+(or genera cloe-developer)
	 (declare (zl:::sys:function-parent ,name define-protocol-function)
		  ,@(when declaration (cdr declaration)))
         #-(or genera cloe-developer)
         ,@(when declaration `((declare ,@(cdr declaration))))
	 #-mcl ,@(when documentation `((:documentation ,documentation)))
	 ,@(when argument-precedence-order
	     `((:argument-precedence-order ,argument-precedence-order)))
	 ,@(when method-combination `((:method-combination ,method-combination)))
	 ,@(when generic-function-class
	     `((:generic-function-class ,generic-function-class)))
	 ,@(when method-class `((:method-class ,method-class)))
	 )
       ;; Do this AFTER the defgeneric to shut up some compilers
       ,(when function
	  ;; The :FUNCTION option to 
	  ;; allows you to define a specially qualified method
	  ;; to act as a trampoline or perhaps last-resort.		       
	  `(defmethod ,internal-name ,(car function)
	     #+(or genera cloe-developer)
	     (declare (zl:::sys:function-parent ,name define-protocol-function))
	     ,@(when documentation `(,documentation))
	     ,@(cdr function)))
       ;; Tell Genera's function spec handler about this
       ;; for editor support primarily
       #+(or genera cloe-developer)
       (setf (get ',name 'zl:::si:definition-type-name) "Predicate Method")
       #+(or genera cloe-developer)
       (pushnew ',name zl:::si:*all-function-spec-handlers*)
       #+(or genera cloe-developer)
       (setf (get ',name 'zl:::zwei:kill-definition) ',killer)
       #+(or genera cloe-developer)
       (setf (get ',name 'zl:::sys:function-spec-handler) ',function-spec-handler)
       #-(or genera cloe-developer)
       ',function-spec-handler
       ;; add this to the list of protocol fns, as well as a translation
       (pushnew ',name *joshua-protocol-functions*)
       (pushnew '(,name . ,internal-name) *joshua-protocol-name-translations*
		:test #'(lambda (x y) (eq (car x) (car y))))
       (setf (get ',name 'protocol-definer) ',definer)
       ;; return the name
       ',name))
   :type-name "Protocol Function")



;;;
;;; Definitions of the protocol functions.
;;;

;;; TELL & ASK are top-level interfaces you probably don't want to redefine.

(define-protocol-function tell (predication truth-value justification)	;note this is internal arglist
  :documentation "Tell the database to believe a certain predication."
  #-sbcl :declaration #-sbcl(declare (values canonical-version new-or-old))
  :internal-name tell-internal)

(define-protocol-function ask (predication truth-value continuation do-backward-rules do-questions)
  :documentation "Ask the database about the state of its belief in a certain predication."
  :declaration (declare (dynamic-extent continuation) #-sbcl (values))
  :internal-name ask-internal)

(define-protocol-function ask-data (predication truth-value continuation)
  :documentation "Ask in the virtual database"
  :declaration (declare (dynamic-extent continuation)))

(define-protocol-function ask-rules (predication truth-value continuation do-questions)
  :documentation "Ask using backward chaining rules"
  :declaration (declare (dynamic-extent continuation)))

(define-protocol-function ask-questions (predication truth-value continuation)
  :documentation "Ask the user"
  :declaration (declare (dynamic-extent continuation)))

;;; INSERT, FETCH, and CLEAR-MODEL are the data-modelling part of the protocol.

(define-protocol-function insert (predication)
  :documentation "Insert predication into the virtual database."
  #-sbcl :declaration #-sbcl (declare (values database-predication new-p)))

(define-protocol-function uninsert (database-predication)
  :documentation "Remove a predication from the virtual database."
  #-sbcl :declaration #-sbcl (declare (values)))

(define-protocol-function fetch (predication continuation)
  :documentation "Fetch matching predications from the virtual database calling a continuation on each."
  :declaration (declare (dynamic-extent continuation)))

(define-protocol-function clear (model &optional clear-database undefrules)
  :documentation "Tell a model to forget everything it knows.  Should only be called by CLEAR."
  :internal-name clear-model
  :function ((model &optional clear-database undefrules)
	     (progn model clear-database undefrules)
	     ;; this guy gets called if this model doesn't
	     ;; have his own clear method.  It's the last
	     ;; resort handler and it does nothing.
	     nil)
  :definer eql-dispatched-method-protocol-definer
  :shower property-protocol-shower)

(define-protocol-function after-clear (model &optional clear-database undefrules)
  ;; this is not a method, not because it has to be around at compile time (which it isn't),
  ;; but because there's no instance to hand the generic fn to when you do (clear).  It doesn't
  ;; need to be around at compile time.
  :documentation "Tell a model to do whatever cleanup it wants to do after everything else has been done for clear"
  :internal-name after-clear
  :function ((model &optional clear-database undefrules)
	     (progn model clear-database undefrules)
	     ;; this guy gets called if this model doesn't
	     ;; have his own clear method.  It's the last
	     ;; resort handler and it does nothing.
	     nil)
  :definer eql-dispatched-method-protocol-definer
  :shower property-protocol-shower)

(define-protocol-function untell (database-predication)
  :documentation "Tell the database to remove all vestiges of a predication."
  )

;;; SAY is the presentation-modelling part of the protocol.

(define-protocol-function say (predication &optional (stream)) ;(( *standard-output*))
  :documentation "Say what the predication says, only in natural language.")


;;; The TMS-modelling part of the protocol consists of JUSTIFY, UNJUSTIFY, SUPPORT, and 
;;;   NOTICE-TRUTH-VALUE-CHANGE ACT-ON-TRUTH-VALUE-CHANGE.

(define-protocol-function certainty-factor (predication)
  :documentation "A probabilistic representation of the confidence in the predication")

(define-protocol-function justify (conclusion truth-value &optional mnemonic
				   true-support false-support unknown-support)
  :documentation "Set the truth-value and, in a TMS'd world, deal with justification.")

(define-protocol-function notice-truth-value-change (database-predication old-truth-value)
  :documentation "Gets called when the truth-value of a predication changes, to allow updating internal structures.")

(define-protocol-function act-on-truth-value-change (database-predication old-truth-value &optional old-state)
  :documentation "Gets called when the truth-value of a predication changes, to allow deductions to be made.")

(define-protocol-function unjustify (database-predication &optional justification)
  :documentation "Remove a justification from a predication, possibly causing the truth value to change")

(define-protocol-function current-justification (database-predication)
  :documentation "Returns the currently active justification of a predication")

(define-protocol-function all-justifications (database-predication)
  :documentation "Returns all justifications of a predication")

(define-protocol-function support (database-predication &optional filter)
  :documentation "Return the support structure for a predication.")

(define-protocol-function consequences (database-predication)
  :documentation "Return the consequences of a predication.")

(define-protocol-function find-independent-support (consequent assumption)
  :documentation "Returns the support of consequent that isn't dependent on assumption.")

(define-protocol-function nontrivial-tms-p (predication)
  :documentation "Whether this predication has a non-trivial TMS mixed in.")

;;; The trigger-modelling part of the protocol is MAP-TRIGGERS, ADD-TRIGGER, DELETE-TRIGGER, 
;;;  LOCATE-TRIGGER, and POSITIONS-MATCHER-CAN-SKIP.

(define-protocol-function prefetch-forward-rule-matches (predication context continuation)
  :documentation "Look for facts matching this rule trigger at rule installation time."
  :declaration (declare (dynamic-extent continuation)))

(define-protocol-function map-over-forward-rule-triggers (predication continuation)
  :documentation "Map a function over a set of forward rule triggers."
  :declaration (declare (dynamic-extent continuation)))

(define-protocol-function map-over-backward-rule-triggers (predication continuation)
  :documentation "Map a function over a set of backward rule triggers."
  :declaration (declare (dynamic-extent continuation)))

(define-protocol-function map-over-backward-question-triggers (predication continuation)
  :documentation "Map a function over a set of backward question triggers."
  :declaration (declare (dynamic-extent continuation)))

(define-protocol-function add-forward-rule-trigger (predication truth-value trigger-object context rule-name)
  :documentation "Add a forward rule trigger.")

(define-protocol-function add-backward-rule-trigger (predication truth-value trigger-object context rule-name)
  :documentation "Add a backward rule trigger.")

(define-protocol-function add-backward-question-trigger (predication truth-value trigger-object context question-name)
  :documentation "Add a backward question trigger.")

(define-protocol-function delete-forward-rule-trigger (predication truth-value rule-name context)
  :documentation "Remove a forward rule trigger.")

(define-protocol-function delete-backward-rule-trigger (predication truth-value rule-name context)
  :documentation "Remove a backward rule trigger.")

(define-protocol-function delete-backward-question-trigger (predication truth-value question-name context)
  :documentation "Remove a backward question trigger.")

(define-protocol-function locate-forward-rule-trigger (predication truth-value continuation context rule-name)
  :declaration (declare (dynamic-extent continuation))
  :documentation "Locate a forward rule trigger.")

(define-protocol-function locate-backward-rule-trigger (predication truth-value continuation context rule-name)
  :declaration (declare (dynamic-extent continuation))
  :documentation "Locate a backward rule trigger.")

(define-protocol-function locate-backward-question-trigger (predication truth-value continuation context question-name)
  :declaration (declare (dynamic-extent continuation))
  :documentation "Locate a backward question trigger.")

(defun make-standin-predication (predication-maker)
  (make-instance (predicate-is-synonym-for (predication-maker-predicate predication-maker))
		 ;; This statement isn't really valid
		 :statement predication-maker
		 ;; so turn off the code that will notice
		 :initialize nil))

(define-protocol-function write-forward-rule-full-matcher (rule-trigger predicate-variable-name environment)
  :function (((rule-trigger list) predicate-variable-name environment)
	     (write-forward-rule-full-matcher (make-standin-predication rule-trigger)
					      predicate-variable-name environment))
  :definer matcher-cache-clearing-compile-time-method-protocol-definer
  :shower property-protocol-shower
  :documentation "Called from match compiler to write a forward rule full unification matcher.")

(define-protocol-function write-forward-rule-semi-matcher (rule-trigger predicate-variable-name environment)
  :function (((rule-trigger list) predicate-variable-name environment)
	     (write-forward-rule-semi-matcher (make-standin-predication rule-trigger)
					      predicate-variable-name environment))
  :definer matcher-cache-clearing-compile-time-method-protocol-definer
  :shower property-protocol-shower
  :documentation "Called from match compiler to write a forward rule semi unification matcher.")

(define-protocol-function positions-forward-rule-matcher-can-skip (rule-trigger)
  :function (((rule-trigger list))
	     (positions-forward-rule-matcher-can-skip
	       (make-standin-predication rule-trigger)))
  :definer matcher-cache-clearing-compile-time-method-protocol-definer
  :shower property-protocol-shower
  :documentation "Called from match compiler, telling which positions semi-matcher needn't check.")

(define-protocol-function write-backward-rule-matcher (rule-trigger variables-in-trigger environment name-of-pred-to-match)
  :function (((rule-trigger list) variables-in-trigger environment name-of-pred-to-match)
	     (write-backward-rule-matcher
	       (make-standin-predication rule-trigger)
	       variables-in-trigger environment name-of-pred-to-match))
  :definer compile-time-method-protocol-definer
  :shower property-protocol-shower
  :documentation "Called from backward rule match compiler, to write the matcher"
  #-sbcl :declaration #-sbcl (declare (values form bindings used-data-stack-p)))

;;; Expand Forward Rule Trigger
;;;  return a data structure that drives the rest of the rule compiler.  This data structure represents
;;;  the Rete network in its most primitive form.  It's a non-empty lists of descriptors for Rete
;;;  trees.  Each node is either:
;;;  a match node consisting of (:match <predication> <name> <truth-value>)
;;;  an ignore node consisting of (:ignore)
;;;  an and node consisting of (:and . <other-nodes>)
;;;  an or node consisting of (:or . <other-nodes>)
;;;  or a Procedural node consisting of (:procedure code <name>)

(define-protocol-function expand-forward-rule-trigger (rule-trigger support-variable-name truth-value context bound-variables)
  :documentation "Expand a forward rule trigger for the rule compiler, returns a trigger-type, the expansion and a truth-value"
  :function ((rule-trigger support-variable-name truth-value context bound-variables)
	     (if (predication-maker-p rule-trigger)
		 (expand-forward-rule-trigger (make-standin-predication rule-trigger)
					      support-variable-name truth-value context bound-variables)
		 `(:procedure ,rule-trigger ,support-variable-name bound-variables ,rule-trigger)))
  :definer compile-time-method-protocol-definer
  :shower property-protocol-shower)

(define-protocol-function compile-forward-rule-action (action then-part rule-name environment)
  :documentation "How to compile a predication as an action of a forward rule."
  :function ((action then-part rule-name environment)
	     (if (predication-maker-p action)
		 ;; for predication-maker forms, we're generic
		 (compile-forward-rule-action (make-standin-predication action)
					      then-part rule-name environment)
		 ;; anything else is just lisp code, and we pass it through unchanged.
		 action))
  :definer compile-time-method-protocol-definer
  :shower property-protocol-shower)

;;; Expand Backward Rule Action
;;;  return a data structure that drives the rest of the rule compiler.  This data structure represents
;;;  What the If side of the rule should do in its most primitive form.  This is a nested tree of "nodes".
;;;  Each node is either:
;;;  a match node consisting of (:match <predication> <name> <truth-value> <ask-args>).  This node means: "produce code
;;;        that will ASK for matches to this pattern.  The argument to the continuation should be <name>.
;;;        The ask should be looking for predications whose truth-value is <truth-value>.  The other arguments
;;;        to ask should be <ask-args>
;;;  an and node consisting of (:and . <other-nodes>)
;;;  an or node consisting of (:or . <other-nodes>)
;;;  an ignore node consisting of (:ignore).  Means you don't have to do anything for this.
;;;  or a Procedural node consisting of (:procedure code <name>).

(define-protocol-function expand-backward-rule-action (rule-action support-variable-name truth-value other-ask-args context)
  :documentation "Expand a backward rule ation for the rule compiler, returns a trigger-type, the expansion and a truth-value"
  :function ((rule-action support-variable-name truth-value other-ask-args context)
	     (if (predication-maker-p rule-action)
		 (expand-backward-rule-action
		   (make-standin-predication rule-action)
		   support-variable-name truth-value other-ask-args context)
		`(:procedure ,rule-action ,support-variable-name)))
  :definer compile-time-method-protocol-definer
  :shower property-protocol-shower)

(define-protocol-function expand-backward-rule-trigger (rule-trigger truth-value context)
  :documentation "Expands the trigger of a backward rule, given the if-part as contextx, return a replacement trigger and an updated if-part"
  :function ((rule-trigger truth-value context)
	     (when (predication-maker-p rule-trigger)
	       (expand-backward-rule-trigger 
		(make-standin-predication rule-trigger)
		truth-value context)))
  :definer compile-time-method-protocol-definer
  :shower property-protocol-shower)

;;; a base class for extended protocol purposes
(defclass justification () 
  ())

(defgeneric destructure-justification (justification))


(define-protocol-function data-is-guaranteed-variable-free (rule-trigger)
  :function ((rule-trigger)
	     (if (predication-maker-p rule-trigger)
		(data-is-guaranteed-variable-free (make-standin-predication rule-trigger))
		(error "The argument to data-is-guaranteed-variable-free ~s is not a predication source designator"
		       rule-trigger)))
  :definer matcher-cache-clearing-compile-time-method-protocol-definer
  :shower property-protocol-shower
  :documentation "Called from match compiler, tells whether database predications of this type may contain variables")
