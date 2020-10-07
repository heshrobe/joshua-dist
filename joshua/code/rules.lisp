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

(in-package :ji)

;;;
;;; how to compile rules.
;;;

(defparameter *rule-control-structures* nil
  "Registry of rule control structures.")

(eval-when (:compile-toplevel :execute :load-toplevel)
  (proclaim '(inline rule-control-structure-p)))
(defun rule-control-structure-p (x)
  ;; whether or not you've got a rule in your hands, there
  (member x *rule-control-structures*))

;(defun rule-control-structure-compiler (rule-control-structure)
;  ;; the thing that knows how to compile rules with this kind of control structure
;  (get rule-control-structure 'rule-control-structure-compiler))

(defvar *fatal-compilation-error*)

(defun fatal-compilation-error (error &rest args)
  (apply #'warn error args)
  (setq *fatal-compilation-error* t)
  ;; return nil, so it can be used as a (bogus) form
  nil)

(def-defining-form defrule
   ;; how to define a rule
   :definer
   ((rule-name (control-structure &rest control-structure-args) if if-part then then-part
	       &environment environment)
    (progn
      (let ((*fatal-compilation-error* nil))
	(unless (symbolp rule-name)
	  (error
	   "~S is not a symbol suitable for naming a rule"
	   rule-name))
	;; first check the args
	(unless (rule-control-structure-p control-structure)
	  (error
	   "~S is not a rule control structure" control-structure))
	(unless (listp control-structure-args)
	  (error
	   "~S is not a list of keyword/value pairs" control-structure-args))
	(when (and (string-equal if 'then) (string-equal then 'if))
	  ;; if in the "wrong" order; HES claims this is cuter for backward rules.
	  ;; I disagree, but it's cheap to allow it. -- sgr
	  (rotatef if then)
	  (rotatef if-part then-part))
	(unless (string-equal if 'if)
	  (error
	    "IF token is missing or misplaced in the rule specification"))
	(unless (string-equal then 'then)
	  (error
	    "THEN token is missing or misplaced in the rule specification"))
	(let ((result (or *fatal-compilation-error*
			  ;; generate a form to compile in place of this
			  (apply #'rule-control-structure-compiler control-structure
				 rule-name if-part then-part
				 environment control-structure-args))))
	  (cond (*fatal-compilation-error*
		 (warn "Compilation of this rule skipped")
		 nil)
		(t result))))))
   :killer undefrule
   )

;;; By now you've realized that the real power is in the rule-control-structures.
(def-defining-form define-rule-control-structure
  :definer
  ((name arglist &body body)
   ;; be nice to Zmacs, put body in right place, register this, and return the name
   `(progn (defmethod rule-control-structure-compiler ((rule-type (eql ,name)) ,@arglist)
	     #+(or genera cloe-developer)
	     (declare (zl:::sys:function-parent ,name define-rule-control-structure))
	     ,@body)
	   (pushnew ',name *rule-control-structures*)
	   ',name))
  )


;;;
;;; Analyzing rule triggers and actions.
;;;

(defun analyze-trigger (trigger trigger-variable-analyses-so-far &optional name)
  ;; figure out the variable situation in this trigger.
  (loop with analyses = trigger-variable-analyses-so-far
	with named-variables = (logic-variable-makers-in-thing trigger)
	for variable in (if name (cons name named-variables) named-variables)
	for canonical = (find-corresponding-analysis variable analyses)
	if canonical
	  ;; recurrence of already-known variable, so note its occurs in another trigger
	  do (push trigger (variable-analysis-triggers-found-in canonical))
	else
	  ;; first occurrence of this variable, so construct an analysis
	  do (push (make-variable-analysis :variable variable
					   :triggers-found-in (list trigger))
		   analyses)
	finally (return analyses)))

(defun analyze-action (action action-variable-analyses-so-far trigger-variable-analyses)
  ;; figure out the variable situation in this action.
  (loop with analyses = action-variable-analyses-so-far
	for variable in (logic-variable-makers-in-thing action)
	for trigger-canonical = (find-corresponding-analysis variable trigger-variable-analyses)
	for action-canonical = (find-corresponding-analysis variable analyses)
	if trigger-canonical
	  ;; this is a trigger variable, so just note it is in this action
	  do (push action (variable-analysis-actions-found-in trigger-canonical))
	else if action-canonical
	       ;; recurrence of this action variable, so just note it is in this action
	       do (push action (variable-analysis-actions-found-in action-canonical))
	else
	  ;; first occurrence of this variable seen, so make an action analysis
	  do (push (make-variable-analysis :variable variable
					   :actions-found-in (list action))
		   analyses)
	finally (return analyses)))


;;;; Parsing up rule control structure arguments.

;;; Make backward chaining do this, too.
(defmacro with-control-structure-arguments
    ((arg-names control-structure-args
      &optional (allow-unknown-p (gensym "UNKNOWN-ARGS-") supplied-p))
     &body body
     &aux (keywords (mapcar #'(lambda (x)
				(intern (string x)
					(find-package 'keyword)))
		     arg-names)))
  ;; Parse up certain control structure args.  If allow-unknown-p is supplied, bind it
  ;; to the remaining ones.  If it's not supplied, cause an error if there are any remaining.
  ;; Execute the body in that context.
  `(let ,(loop for arg-name in arg-names
	     for keyword in keywords
	     collecting `(,arg-name (getf ,control-structure-args ,keyword)))
     (declare (ignorable ,@arg-names))
     (let ((,allow-unknown-p (copy-list ,control-structure-args)))
       (mapc #'(lambda (x) (remf ,allow-unknown-p x)) ',keywords)
       ,@(when (not supplied-p)
	   `((when ,allow-unknown-p
	       (error "Unhandled control structure arguments: ~S" ,allow-unknown-p))))
       ,@body)
     ))

;;; This subroutine is shared between backward & forward trigger writers.

(defun process-importance (importance)
  ;; convert the thing given in the rule control structure args to something we can use at runtime.
  (when (and (not (numberp importance)) (not (symbolp importance)))
    ;; neither a number nor a symbol be, so a closure I'll make of thee
    (setq importance `#'(lambda () ,importance)))
  importance)

(defun maybe-quote-importance (importance-form)
  ;; if importance-form is a list, don't quote it, since it's a L-expression that needs to be compiled.
  ;; this is called by the trigger-code writers for rules and questions when they store away
  ;; importance forms.
  (if (listp importance-form)
      ;; needs to be seen by compiler
      importance-form
      ;; compiler needn't see it
      `',importance-form))


;;;; Rule compilation for forward chaining.

(defun write-forward-rule-trigger-code (Rule-name if-part environment arguments body-variables
					&aux trigger-variable-analyses)
  ;; write some code to generate a Rete network and index the triggers appropriately
  #-sbcl (declare (values trigger-code trigger-variable-analyses))
   (let ((*forward-rule-trigger-compiler-environment* environment))
     (declare (special *forward-rule-trigger-compiler-environment*))
     ;; first, parse up the arguments to the control structure
     (with-control-structure-arguments ((importance semi-unification certainty documentatoin) arguments)
       ;; this implementation is a kludge that awaits the rest of the generic rule compiler.
       ;; its advantage over the previous implementation is that all the kludges are in the same place!
       (setq importance (process-importance importance))
       (unless (predication-maker-p if-part)
	 (error "The IF part of the forward rule ~s contains only Lisp code.~&It must contain at least one pattern." rule-name))
       (with-predication-maker-parsed (predicate the-statement) if-part
				      (when (member predicate '(or and))
					(unless (loop for form in the-statement thereis (predication-maker-p form))
					  (error "The IF part of the forward rule ~s contains only Lisp code.~&It must contain at least one pattern."
						 rule-name))))
       ;; At this point, triggers is a list of (possibly nested) predication-maker forms.
       ;; each possibly followed by a :support ?x.
       ;; Process them and make up entry structures that are dispatched on by
       ;; make-rete-network.
       (let ((real-triggers (expand-forward-rule-trigger if-part nil +true+ if-part nil)))
	 (setq if-part (convert-analysis-to-pred-maker real-triggers))
	 (labels ((find-and-analyze-trigger (trigger)
		    (case (car trigger)
		      ((:and :or) (loop for thing in (cdr trigger) doing (find-and-analyze-trigger thing)))
		      (:procedure
		       (setq trigger-variable-analyses
			 (analyze-trigger trigger trigger-variable-analyses (third trigger))))
		      ((:match :object-match)
		       (when (null (third trigger))
			 ;; this is the name of the support variable
			 (setf (third trigger)
			   ;; make all patterns have a support variable
			   (gentemp "?ANONYMOUS-")))
		       (setq trigger-variable-analyses
			 (analyze-trigger trigger trigger-variable-analyses (third trigger)))))))
	   (find-and-analyze-trigger real-triggers))
	 (let ((*forward-rule-subsidiary-functions* nil))
	   (let ((analysis (build-rete-topology real-triggers)))
	     (analyze-variable-consumption analysis body-variables)
	     (analyze-semi-ability analysis semi-unification)
	     (write-rete-procedures rule-name analysis environment)
	     (multiple-value-bind (pred-forms top-level-pred pred-mapping ignore-top-level ignorable-preds) 
		 (collect-all-predications if-part)
	       (multiple-value-bind (rete-bindings rete-body) (make-rete-network rule-name analysis pred-mapping)
		 (multiple-value-bind (final-output-map terminal-node pure-unification)
		     (follow-analysis-to-terminal analysis)
		   (make-output-env-assignments trigger-variable-analyses final-output-map)
		   (let ((lvs-in-triggers (logic-variable-makers-in-thing if-part)))
		     ;; write a function to install this rule.
		     (values `(setf (get ',rule-name 'install-triggers) 
				#'(lambda ()
				    #+(or genera cloe-developer)
				     (declare (zl:::sys:function-parent ,rule-name defrule))
				     ;; disarm any previously existing rule by this name
				     ;; now write, compile, and collect up the matchers & mergers
				     ;; record the debug-info and index the Rete network
				     ;; under each of the triggers Do this by calling out
				     ;; of line, there's nothing gained by compiling lots
				     ;; of crud in line here.
				     (let ,(loop for lv in lvs-in-triggers
					       collect `(,lv (make-unbound-logic-variable ',lv)))
				       ,@(when lvs-in-triggers
					   `((declare (ignorable ,@lvs-in-triggers))))
				       (macrolet ((known-lvs () ',lvs-in-triggers))
					 (let* (,@pred-forms ,@rete-bindings)
					   ,@(when (and ignore-top-level (not (member top-level-pred ignorable-preds)))
					       `((declare (ignore ,top-level-pred))))
					   ,@(when ignorable-preds
					       `((declare (ignorable ,@ignorable-preds))))
					   ,@rete-body
					   (install-forward-rule-triggers ',rule-name ;
									  ,(pattern-analysis-rete-node analysis)
									  ,terminal-node
									  ,(maybe-quote-importance importance)
									  ',if-part
									  ,certainty))))))
			     (nreverse *forward-rule-subsidiary-functions*)
			     ;; want the variable analyses, 'cause they contain final slot assignments.
			     trigger-variable-analyses
			     pure-unification
			     )))))))))))


(defun follow-analysis-to-terminal (analysis)
  (loop with my-analysis = analysis and links
	doing (setq my-analysis (typecase my-analysis
				  (and-group-pattern-analysis (car (and-group-pattern-analysis-sub-patterns my-analysis)))
				  (or-group-pattern-analysis (car (or-group-pattern-analysis-sub-patterns my-analysis)))
				  (otherwise my-analysis)))
	      (setq links (pattern-analysis-links my-analysis))
	until (and (null links)
		   (not (and-group-pattern-analysis-p my-analysis))
		   (not (or-group-pattern-analysis-p my-analysis)))
	finally (return (values (pattern-analysis-map my-analysis)
				(pattern-analysis-rete-node my-analysis)
				(pattern-analysis-pure-semi-unification? my-analysis)))
	when links
	  do (setq my-analysis (second (first links)))))

;;; This converts the result of expand-forward-rule-trigger
;;; into an if part equivalent to it
;;; This is in case a predication expands into something
;;; including other predications.  Good example expansion
;;; for commutative matching.
(defun convert-analysis-to-pred-maker (analysis)
  (labels ((do-next-part (part)
	     (case (first part)
	       (:or `(predication-maker '(or ,@(loop with pred and support
						   for thing in (rest part)
						   do (multiple-value-setq (pred support) (do-next-part thing))
						   collect pred
						   when support
						   append (list :support support)))))
	       (:and `(predication-maker '(and ,@(loop with pred and support
						     for thing in (rest part) 
						     do (multiple-value-setq (pred support) (do-next-part thing))
						     collect pred
						     when support
						     append (list :support support)))))
	       ((:match :object-match)
		(destructuring-bind (pred support-variable truth-value) (rest part)
		  (values (if (= truth-value +true+) pred `(predication-maker '(not ,pred)))
			  (when support-variable `(logic-variable-maker ,support-variable)))))
	       (:procedure (destructuring-bind (code support-variable bound-variables original-expression) (rest part)
			     (declare (ignore bound-variables code))
			     (values original-expression (when support-variable `(logic-variable-maker ,support-variable))))))
	     ))
    (do-next-part analysis)))

(defun collect-all-predications (if-part)
  (let ((forms nil) (mapping nil) (ignorable-preds nil))
    (labels ((do-one-piece (p-maker)
	       (cond
		((predication-maker-p p-maker)
		 (let ((name (assoc p-maker mapping)))
		   (unless name
		     (setq name (gentemp "PREDICATION-"))
		     (push (cons p-maker name) mapping)
		     (let ((predicate (predication-maker-predicate p-maker)))
		       (when (member predicate '(and or not))
			 (push name ignorable-preds)
			 (setq p-maker `(make-predication
					 (list ',predicate
					   ,@(loop for piece in (cdr (predication-maker-statement p-maker))
						 for processed-piece = (do-one-piece piece)
						 collect processed-piece)))))
		       (push (list name p-maker) forms)))
		   (setq p-maker name)))
		((logic-variable-maker-p p-maker))
		((eql p-maker :support))
		(t (setq p-maker `',p-maker)))
	       p-maker))
      (let ((top-name (do-one-piece if-part)))
	(values (nreverse forms) top-name mapping
		(member (predication-maker-predicate if-part) '(and or))
		ignorable-preds
		)))))

(defun preliminary-analysis-of-forward-rule-body (then-part)
  #-sbcl (declare (values actions variables))
  (let ((actions
	  (if (predication-maker-p then-part)
	      (with-predication-maker-parsed (predicate args) then-part
		(when (eq predicate 'or)
		  (error "Forward rules don't grok OR actions yet: ~S" then-part))
		(if (and (eq predicate 'and-internal)
			 ;; If it's backquoted anything could be going on
			 ;; so we don't try to expand it out.
			 (not (eql (first (second then-part)) 'backquote)))
		    args
		    ;; some predication other than and
		    (list then-part)))
	      ;; just a lisp form
	      (list then-part))))    
    (values actions (logic-variable-makers-in-thing actions))))

(defun write-forward-rule-body (rule-name actions then-part trigger-variable-analyses environment
				arguments computed-semi-unification
				&aux action-variable-analyses analyses-in-actions)
  ;; writes a function that presumably implements a forward rule's actions
  #-sbcl (declare (values body-code action-variable-analyses))
  (with-control-structure-arguments ((importance documentation semi-unification certainty) arguments)
    (setq computed-semi-unification (or computed-semi-unification semi-unification))
    ;; At this point, actions is a list of forms -- some lisp, some predication-maker forms.
    (loop for action in actions
	doing (setq action-variable-analyses
		(analyze-action action action-variable-analyses trigger-variable-analyses)))
    (setq analyses-in-actions (loop for t-v-a in trigger-variable-analyses
				  when (variable-analysis-actions-found-in t-v-a)
				       ;; all the action variables and some of the trigger
				       ;; variables
				  collect t-v-a into answer
				  finally (return (append answer action-variable-analyses))))
    ;; if there's a new variable introduced in the actions, then you can't 
    ;; assume semi-unification because you have an unbound variable
    (when (find-if #'(lambda (action) (null (variable-analysis-triggers-found-in action)))
		   action-variable-analyses)
      (setq computed-semi-unification nil))
    (multiple-value-bind (names bindings) (rule-variable-initializations analyses-in-actions)
      (loop
	;; it doesn't appear that these known-lvs are actually used for anything
	  ;; with old-known-lvs = (macroexpand '(known-lvs) environment)
	  ;; with new-known-lvs = (append (MAPCAR #'variable-analysis-variable analyses-in-actions) old-known-lvs)
	  for action in actions
	  ;; do (progn old-known-lvs new-known-lvs)
	  collect (compile-forward-rule-action action then-part rule-name environment)
	  into body-forms
	  finally
	    ;; actually, in the forward-chaining case, action variables
	    ;; and bindings always occur either together or not at all.
	    (when computed-semi-unification
	      (setq body-forms
		`((macrolet ((joshua-logic-variable-value (val) val))
		    ,@body-forms))))
	    (return
	      (values `(defun ,rule-name (*running-rule* .environment. *support*)
			 ,@(when documentation (list documentation))
                         #+cloe (declare (sys:function-parent ,rule-name defrule)) ;be nice to Zmacs
			 ;;so rules with no variables don't get warnings
			 (declare (ignorable .environment. *support*))
			 (let ,bindings
			   (macrolet ((known-lvs () ',names))
			     ;; bind all the trigger and action variables
			     ,@body-forms)))
		      action-variable-analyses))))))


(defparameter *forward-rules*  nil "List of all known forward rules.")

(define-rule-control-structure :forward (rule-name if-part then-part environment &rest arguments)
  ;; define forward-chaining -- return a form to be compiled in place of a forward rule
  ;; figuring out triggers and actions needs to be genericized.
  (multiple-value-bind (actions body-variables) (preliminary-analysis-of-forward-rule-body then-part)
    (multiple-value-bind (trigger-code subsidiary-functions trigger-variable-analyses semi-unification)
	;; the trigger code must be generated before the body code, since it assigns environment
	;; indices to variables that the body needs.  However, in the expansion, you almost always
	;; want to see the body code first.
	(write-forward-rule-trigger-code rule-name if-part environment arguments body-variables)
      (multiple-value-bind (body-code action-variable-analyses)
	  (write-forward-rule-body rule-name actions then-part trigger-variable-analyses
				   environment arguments semi-unification)
	(declare (ignore action-variable-analyses))
	`(progn ;; (undefrule ',rule-name)
	   #+genera (si:record-source-file-name ',rule-name 'defrule)
	   ;; put body before triggers since it's almost always what you want to see in macroexpand
	   ,body-code
	   ,trigger-code
	   ,@subsidiary-functions
	   (funcall (get ',rule-name 'install-triggers))
	   ;; register the name
	   (pushnew ',rule-name *forward-rules*)
	   ;; return the name
	   ',rule-name)))))


;;;
;;; Rule compilation for backward chaining.  The actions must be consed on the stack if
;;; they involve variables, or else the rule is not re-entrant!
;;;

(defparameter *backward-rules* nil "List of all known backward rules.")

(define-rule-control-structure :backward (rule-name if-part then-part environment &rest arguments)
  ;; define backward chaining -- return a form to be compiled in place of a backward rule
  (multiple-value-bind (trigger trigger-negated)
      (parse-backward-trigger then-part)
    (multiple-value-bind (body-code real-trigger) (write-backward-rule-body rule-name trigger trigger-negated if-part environment arguments)
      (let ((trigger-code (write-backward-rule-trigger-code rule-name real-trigger trigger-negated if-part environment arguments)))
	`(progn (undefrule ',rule-name)
		;; put body first, 'cause it's usually what we're interested in looking at.
		,body-code
		,trigger-code
		(funcall (get ',rule-name 'install-triggers))
		;; register the name
		(pushnew ',rule-name *backward-rules*)
		;; return the name
		',rule-name)))))

(defun parse-backward-trigger (then-part)
  #-sbcl (declare (values trigger trigger-negated))
  (with-predication-maker-parsed (predicate args) then-part
      ;; ---- probably ought to make sure it's well-formed
      (case predicate
	(not-internal
	  (values (first args) t))
	(otherwise
	  (values then-part nil)))))


(defun write-backward-rule-trigger-code (rule-name trigger trigger-negated if-part environment arguments)
  ;; write some code to generate the trigger mechanism for a backward rule.
  #-sbcl (declare (values trigger-code))
   (declare (ignore environment))           
   (with-control-structure-arguments ((importance documentation certainty) arguments)
     ;; this implementation is a kludge that awaits the rest of the generic rule compiler.
     ;; its advantage over the previous implementation is that all the kludges are in the same place!
     (setq importance (process-importance importance))
     (let ((predicate (predication-maker-predicate trigger)))
       (when (eq predicate 'or)
	 (error "Backward rules don't grok OR triggers yet: ~S" trigger))
       (when (eq predicate 'and)
	 (error "Backward rules don't deal with conjunctive triggers yet.")))
     ;; write a function to install this rule.
     (let ((truth-value (if trigger-negated '+false+ '+true+)))
       `(setf (get ',rule-name 'install-triggers) 
	  #'(lambda ()
	      #+(or genera cloe-developer)
	       (declare (zl:::sys:function-parent ,rule-name defrule))
	       ;; disarm any previously existing rule by this name
	       (disarm-rule-triggers ',rule-name) ;take direction arg?
	       (let* ((trigger ,trigger)
		      (entry (make-backward-trigger
			      :rule ',rule-name
			      :importance ,(maybe-quote-importance importance))))
		 ;; record the debug-info
		 (setf (rule-debug-info ',rule-name)
		   (make-rule-debug-info :name ',rule-name
					 :certainty ,certainty
					 :control :backward
					 :triggers (list (list trigger ,truth-value))
					 :context ',if-part
					 :network entry)) ;this should be called something else
		 ;; now index the trigger-object (this does the interning of variant patterns)
		 (add-backward-rule-trigger trigger ,truth-value entry ',if-part ',rule-name)))))))


(defvar *backward-support* nil)

(defun write-backward-rule-body (rule-name trigger trigger-negated if-part environment arguments)
  ;; Write the body of a backward rule.  This is more complicated than you might think,
  ;; because it has to cons all structures containing logic variables in order to be
  ;; re-entrant.
  #-sbcl (declare (values body-code))
  (with-control-structure-arguments ((importance documentation certainty) arguments)
    ;; this implementation is a kludge that awaits the rest of the generic rule compiler.
    ;; its advantage over the previous implementation is that all the kludges are in the same place!
    importance					;get rid of compiler warning
    certainty
    (multiple-value-bind (trigger if-part) (expand-backward-rule-trigger trigger trigger-negated if-part)
      (let ((actions (expand-backward-rule-action if-part nil +true+ nil if-part)))
      ;; at this point, actions is a tree of nested rule-action expressions.
      (let* (#+ignore (data-stack-p nil)
	     (variables (union (logic-variable-makers-in-thing if-part)
			       (logic-variable-makers-in-thing trigger)))
	     (bindings nil))
	(labels ((compile-head (head)
		   ;; ---- for now, just build a predication and match it.
		   ;; ---- eventually, open-code the unification
		   (multiple-value-bind (match-form new-bindings used-data-stack-p)
		       (write-backward-rule-matcher head variables environment '.goal.)
		     (declare (ignore used-data-stack-p))
		     ;; add new bindings and data stack flag; body depends on whether we're
		     ;; back-chaining on another predication or executing a piece of lisp code.
		     (setq bindings (nconc bindings new-bindings))
		     #+ignore (when used-data-stack-p (setq data-stack-p t))
		     match-form))
		 (compile-node (node continuation top-level?)
		   (case (car node)
		     (:ignore (compile-ignore-node node continuation top-level?))
		     (:procedure (compile-procedure-node node continuation top-level?))
		     (:or (compile-or-node node continuation top-level?))
		     (:and (compile-and-node node continuation top-level?))
		     (:match (compile-match-node node continuation top-level?))))
		 (continuation-variable-name-of-node (node)
		   (case (car node)
		     (:or nil)
		     (:and nil)
		     (:match (fourth node))
		     (:ignore nil)
		     (:procedure nil)))
		 (compile-ignore-node (node continuation top-level?)
		   (declare (ignore node))
		   (when top-level?
		     `(with-stack-list (rule-support .goal. .truth-value. '(rule ,rule-name))
                        (funcall ,continuation rule-support)
                        )))
		 (compile-match-node (node continuation top-level?)
		   (destructuring-bind (action truth-value junk ask-args) (cdr node)
		     (declare (ignore junk))
		     (when (null ask-args)
		       (setq ask-args '(t .do-questions.)))
		     ;; Note: Stackify thing is inlined and does nothing
		     ;; Therefore new-bindings is always NIL and
		     ;; the clause (when new-bindings ... is dead code
		     ;; SBCL's compiler figures this out and then
		     ;; marks that as dead code with a compiler note.
		     (multiple-value-bind (new-thing new-bindings used-data-stack-p)
			 (stackify-thing action environment variables)
		       (declare (ignore used-data-stack-p #+sbcl new-bindings))
		       #+ignore
		       (when used-data-stack-p (setq data-stack-p t))
		       (when top-level?
			 (setq continuation
			       (let ((support-var (gensym "SUPPORT")))
				 `#'(lambda (,support-var)
                                      (let ((*backward-support* ,support-var))
				      (with-stack-list (rule-support .goal. .truth-value. '(rule ,rule-name) ,support-var)
					(funcall ,continuation rule-support)))))))
		       (let ((body `(ask-internal ,new-thing ,truth-value ,continuation ,@ask-args)))
			 #-sbcl (when new-bindings (setq body `(stack-let* ,new-bindings ,body)))
			 body))))
		 (compile-procedure-node (node continuation top-level?)
		   (let ((lv-name (third node))
			 (body (second node))
			 (support-var (gensym)))
		     (cond ((not (calls-succeed-p body environment))
			    (cond ((not top-level?)
			           ;; no calls to succeed -- call the continuation
			           ;; note that the compiler will optimize out the funcall for us
				   `(when ,body (funcall ,continuation nil)))
				  (t `(when ,body
					(with-stack-list (rule-support .goal. .truth-value. (list 'rule ',rule-name))
					  (funcall ,continuation rule-support))))))
			   (t
			    ; some calls to succeed -- succeed either by doing (succeed) or returning
			    ;; non-NIL
			    (cond ((not top-level?)
			   	    `(flet ((succeed (&optional ,support-var)
					       #+cloe (declare (sys:downward-function))
					       ,@(when lv-name `((unify ,lv-name ,support-var)))
					       (funcall ,continuation ,support-var)
					       ;; if the last action is (succeed), don't do it again
					       nil))
                                       (declare (dynamic-extent #'succeed))
                                       (when ,body (succeed))))
				  (t `(flet ((succeed (&optional ,support-var)
                                                #+cloe (declare (sys:downward-function))
					       ,@(when lv-name `((univy ,lv-name ,support-var)))
					       (when (predicationp ,support-var)
						 (setq ,support-var (list ,support-var)))
					        (with-stack-list* (rule-support .goal. .truth-value.
										(list 'rule ',rule-name)
										,support-var)
						(funcall ,continuation rule-support))))
                                        (declare (dynamic-extent #'succeed))
					(when ,body (succeed)))))))))
		 (compile-and-node (node continuation top-level?)
		   (labels ((compile-and-list (remaining-actions support-list)
			      (if (null remaining-actions)
                                `(with-stack-list (rule-support ,@(if top-level?
                                                                    `(.goal. .truth-value. '(rule ,rule-name))
                                                                    `('and +true+ 'and))
                                                                ,@(reverse support-list))
				     (funcall ,continuation rule-support))
				  (let (next-node lv-name)
				    (loop doing (setq next-node (pop remaining-actions))
					  until (not (eql (car next-node) :ignore)))
				    (setq lv-name (continuation-variable-name-of-node next-node))
				    (compile-node next-node
						  (let ((support-var (gensym)))
						    (push support-var support-list)
						    (let ((body (compile-and-list remaining-actions support-list)))
						      `#'(lambda (,support-var)
							   #+cloe (declare (sys:downward-function))
                                                           (declare (dynamic-extent))
                                                           (let ((*backward-support* ,support-var))
                                                             ,@(when lv-name `((unify ,lv-name ,support-var)))
                                                             ,body))))
						  nil)))))
		     (compile-and-list (cdr node) nil)))
		 (compile-or-node (node continuation top-level?)
		   `(let ((.or-continuation. ,continuation))
		      ,@(loop for next-node in (cdr node)
			      with lv-name = nil
			      when (not (eql (car next-node) :ignore))
			      do (setq lv-name (continuation-variable-name-of-node next-node))
			      collect (compile-node next-node
						    (if top-level?
						       (let ((support-var (gensym)))
						          `#'(lambda (,support-var)
							       #+cloe (declare (sys:downward-function))
                                                               #+mcl(declare (dynamic-extent))
                                                               (let ((*backward-support* ,support-var))
							       ,@(when lv-name `((unify ,lv-name ,support-var)))
							       (with-stack-list (rule-support .goal. .truth-value.
                                                                                              '(rule ,rule-name)
                                                                                              ,support-var)
							         (funcall .or-continuation. rule-support)))))
							'.or-continuation.)
						    nil))))
		 (compile-body (node) (compile-node node '.continuation. t)))
	  (let ((body-form `(with-unification
			      ,(compile-head trigger)
			      ,(compile-body actions))))
	    (when bindings
	      ;; wrap the body form in a stack-let* to do the bindings
	      ;; Also, we cons all the necessary structures on the stack now,
	      ;; even if we might not need them later.  This is like the caller/callee saves
	      ;; tradeoffs in function calls in register machines.  I cons them all here,
	      ;; because if I consed them later, they might get consed many times -- once
	      ;; for each success.
	      (setq body-form `(stack-let* ,bindings ,body-form)))
	    ;; In Genera this switch would have told us to use the data-stack
	    ;; this is just here to defeat a compiler warning.
	    #+ignore
	    (when data-stack-p nil)
	    (when variables
	      (setq body-form `(with-unbound-logic-variables ,variables
				 (macrolet ((known-lvs () ',variables))
				   ,body-form))))
	    (values
	    `(defun ,rule-name (.goal. .truth-value. .continuation. .do-questions.)
	       ,@(when documentation (list documentation))
	       #+(or genera cloe-developer)
	       (declare (zl:::sys:function-parent ,rule-name defrule))
	       #-sbcl (declare (dynamic-extent .continuation.))
	       (declare (ignorable .do-questions.))
	       (when (eql .truth-value.
			  ,(if trigger-negated '+false+ '+true+))
		 (let ((*running-rule* ',rule-name)
                       (*goal* .goal.)
                       (*backward-support* nil))
		   ,body-form)))
	    trigger))))))))

;;; In allegro, this would create a new environment with 
;;; an appropriate known-lvs macro in it.
;;; There is also Pittman's god-awful doubly nested macrolet hack in jlt:mapforms
;;; (sys:augment-environment 
;;;  environment
;;;  :macro `((known-lvs (lambda (form env) 
;;; 		       (declare (ignore form env))
;;; 		       ',known-lvs))))
;;; But it turns out that we never need to let this look up 
;;; logic variable values, so we don't need to hack environments at all.

(defun calls-succeed-p (form environment)
  ;; determine whether or not this form does (succeed).
  ;; Note: This approach is specific to Allegro
  ;; There's a portable way to get the augmented environment
  ;; which is ungodly but used in the jlt code walker.
  (block called-succeed
      (jlt:mapforms
       #'(lambda (subform kind usage state)
	   (declare (ignorable kind))
	   ;; (format *trace-output* "~%Form ~a kind ~a usage ~a state ~a" subform kind usage state)
	   (cond
	    ((and (listp subform) (eql (first subform) 'logic-variable-maker))
	     ;; don't macroexpand this, there can't be a call to succeed inside of it
	     (values subform t state))
	    ((and (member usage '(jlt::eval jlt::effect))
		  (consp subform) (eq (car subform) 'succeed))
	     ;; return t when we see a call to succeed
	     (return-from called-succeed t))
	    ;; otherwise continue code-walking
	    (t (values subform nil state))))
       form
       :host-environment environment)))

;;; The default head matcher for backward chaining rules.

(define-predicate-method (write-backward-rule-matcher default-rule-compilation-model)
			 (variables-in-trigger environment name-of-pred-to-match)
  ;; ---- for now, just build a predication and match it.
  ;; ---- eventually, open-code the unification
  ;; Note: Stackify-thing is inlined, but doesn't actually do anything and so new-bindings
  ;; and use-data-stack-p are always nil
  (multiple-value-bind (new-thing new-bindings used-data-stack-p)
      (stackify-thing self environment variables-in-trigger)
    ;; add new bindings and data stack flag; body depends on whether we're
    ;; back-chaining on another predication or executing a piece of lisp code.
    (values
      `(unify-predication ,name-of-pred-to-match ,new-thing)
      new-bindings
      used-data-stack-p)))



;;;
;;; Defining backward questions.  (A little like rules, but run later.)
;;;

;;; BUG: the accept's appear to be ignoring the readtable.  You can't type variables, preds, etc.
(defun ask-default-backward-question (query truth-value what-to-funcall)
  ;; what gets called by the default question-asker.  This is what Joshua calls
  ;; for people who want an automatically-generated user-interface.
  (let* ((named-vars (reverse (named-logic-variables-in-thing query)))
	 #+ignore
	 (named-vars-names (nreverse (loop for thing in named-vars
					collect (logic-variable-name thing))))
	 )
    ;; (declare (ignore named-vars-names))
    (cond ((null named-vars)
	   ;; there are no variables in this query
	   (fresh-line *query-io*) ;can't do conditional newlines in redisplayers in 365!
	   (when 
	     (y-or-n-p "Is it ~a that ~a?"
		       (if (contains-logic-variables-p query)
			   (truth-value-case truth-value
			      (+true+ "EVER true")
			      (+false+ "ALWAYS false"))
			   (truth-value-case truth-value
			     (+true+ "true")
			     (+false+ "false")))
		       query #'say)
	     ;; user says yes
	     (funcall what-to-funcall)))
	  (t
	    ;; there are some variables here, so there could be many solutions.
	    ;; should there be a special case where there's just one variable, which just does accept,
	    ;;   instead of an accepting-values?
	    (error "can't handle this type of question yet.")))))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (proclaim '(inline question-pattern)))
(defun question-pattern (name)
  ;; figure out what the pattern is for a given question.
  (get name 'question-pattern))

(defun write-default-question-body (pattern control-structure &rest args
					    &key importance &allow-other-keys) ;for c-s args
  (declare (ignore pattern importance args))
  ;; write a question body suitable for a quick-and-dirty user interface
  (check-type control-structure (member :backward))
  `((query truth-value continuation &optional ignore)
    (declare (ignore ignore continuation))
    (ask-default-backward-question query truth-value #'succeed)))

(defun check-question-code-arg (code-arg)
  ;; verify the code arg
  (or (null code-arg)				;wants the default
      (and (consp code-arg)
	   (let ((arglist (car code-arg)))
	     (and (symbolp (first arglist))
		  (not (member (first arglist) lambda-list-keywords))
		  (not (eq (first arglist) 'ignore))
		  (symbolp (second arglist))
		  (not (member (second arglist) lambda-list-keywords))
		  (not (eq (second arglist) 'ignore))
		  (symbolp (third arglist))
		  (not (member (third arglist) lambda-list-keywords))
		  (not (eq (third arglist) 'ignore)))))))

(defvar *backward-questions* nil "Registry of backward questions.")

(def-defining-form defquestion
   :definer
   ((name (control-structure &rest control-structure-args) pattern
	  &key code context
	  &aux variables bindings body-form args
	  &environment environment
	  )
    #+genera (declare (zwei:indentation 1 1 1 2))
    ;; the body is a keyword because, in the future, there will be a
    ;; declarative question-asking language.  You'll say something like
    ;; (defquestion foo (:backward) :menu *** :picture ***) and the
    ;; system will write the :code thing for you.  first, indulge some
    ;; paranoia
    (check-type control-structure (member :backward))
    (check-type code (satisfies check-question-code-arg)
		"an arglist like (QUERY TRUTH-VALUE CONTINUATION)")
    (check-type pattern (satisfies predication-maker-p) "a PREDICATION-MAKER form")
    (with-control-structure-arguments ((importance documentation) control-structure-args)
      ;; can stackify the pattern
      (setq importance (process-importance importance))
      (multiple-value-bind (trigger trigger-negated)
	  (parse-backward-trigger pattern)
	(when (null code)
	  ;; wants us to write his user interface for him, the wimp!
	  (when trigger-negated
	    (error "I don't know how to ask negated questions -- you must supply :CODE"))
	  (setq code (apply #'write-default-question-body
			    trigger control-structure control-structure-args)))
	(setq args (car code))
	(setq variables (union (logic-variable-makers-in-thing trigger)
			       (logic-variable-makers-in-thing context)))
	;; Note: see note in write-backward-rule-matcher
	;; A consequence of that is that new-trigger-bindings is always
	;; NIL so the (setq bindings new-trigger... is dead-code
	;; Even SBCL's compiler doesn't figure that out
	(multiple-value-bind (new-trigger new-trigger-bindings)
	    (write-backward-rule-matcher trigger variables environment (first args))
	  (setq bindings new-trigger-bindings)
	  (setq body-form
		`(with-unification
		   ;; failing to unify will skip executing the rest of the body
		   ,new-trigger
		   (flet ((succeed (&optional support)
			    (with-stack-list (question-support ,(first args)
                                                               ,(second args)
                                                               '(question ,name)
                                                               support)
			      (funcall ,(third args) question-support))
			    ;; if this is the last thing, don't succeed again
			    nil))
		     ;; The body succeeds by unifying vars & doing
		     ;; (SUCCEED).  Replace logic vars by their
		     ;; corresponding lisp forms -- this code-walking 
		     ;; is essentially equivalent to what ASK does to
		     ;; its continuation.   In fact, this is copied from
		     ;; the ASK macro, so you should look there, too.
		     (when
		       (let ()
			 ,@(cdr code))
		       (succeed)))))
	  (when bindings
	    ;; use bindings generated by stackify and the variable-conser
	    (setq body-form `(stack-let* ,bindings ,body-form)))
	  (when variables
	      (setq body-form `(with-unbound-logic-variables ,variables
				 (macrolet ((known-lvs () ',variables))
				   ,body-form))))
	  ;; now ready to emit code.
	  (let ((truth-value (if trigger-negated '+false+ '+true+)))
	  `(progn #+(or genera cloe-developer)
		  (record-source-file-name ',name 'defquestion)
		  (undefquestion ',name)
		  (setf (question-info ',name)
			(make-question-info :name ',name
					    :context ',context
					    :importance ,(maybe-quote-importance importance)
					    :pattern (list ,trigger ,truth-value)))	;already quoted
		  (defun ,name ,args
		    ,@(when documentation (list documentation))
		    ;; takes 2 args -- query and continuation
		    (declare #+(or genera cloe-developer) (zl:::sys:function-parent ,name defquestion)
			     #-sbcl (dynamic-extent ,(third args)))
		    (when (eql ,(second args)
			       ,(if trigger-negated '+false+ '+true+))
		      ,body-form))
		  (add-backward-question-trigger ,trigger ,truth-value ',name ',context ',name)
		  (pushnew ',name *backward-questions*)
		  ',name))))))
   )

(defun undefquestion (name &aux (info (question-info name)))
  ;; how to undefine a question
  (when info
    ;; need a predication to index with its storage method to get rid of it.
    (handler-case 
      (delete-backward-question-trigger (first (question-info-pattern info))
					(second (question-info-pattern info))
					name
					(question-info-context info))
      (error (condition) (values nil condition)))
    (setf (question-info name) nil)
    (setq *backward-questions* (delete name *backward-questions*))
    (fmakunbound name))) 



;;;
;;; Sledge hammers to clean up the universe by removing all internal state in rules.
;;;

(defun undefrule (rule-name &aux (debug-info (rule-debug-info rule-name)))
  ;; flush a rule
  (when debug-info				;check if rule is there, first
    ;; first, make sure it won't trigger anymore.
    (disarm-rule-triggers rule-name)
    ;; second, flush its internal state
    (ecase (rule-debug-info-control debug-info)
      (:forward
	(clear-rete-network (rule-debug-info-network debug-info))
	(setq *forward-rules* (delete rule-name *forward-rules*)))
      (:backward
	;; have to do something here to remove it from the interned matcher stuff.
	(setq *backward-rules* (delete rule-name *backward-rules*))))
    ;; third, undefine it.
    (setf (rule-debug-info rule-name) nil)
    (fmakunbound rule-name)
    (remprop rule-name 'install-triggers)))

(defun clear (&optional (clear-database t) (undefrule-rules nil))
  ;; clear the database and undefine the rules, under control of the arguments.
  (clear-forward-queue)
  (flet ((clear-rule (rule-name)
	   ;; given a rule name, clear the associated Rete network.
	   ;; ("Forget everything you've ever heard.")
	   (ecase (rule-debug-info-control (rule-debug-info rule-name))
	     (:forward
	       (clear-Rete-network (rule-debug-info-network (rule-debug-info rule-name))))
	     (:backward
	       ;;nothing to do, since no state is saved.
	       nil)
	     (:mixed nil))))
    (when undefrule-rules
      ;; supposed to undefine all the rules, too (wants a semi-virgin world)
      (mapc #'undefrule *forward-rules*)
      (mapc #'undefrule *backward-rules*))
    (when (or clear-database undefrule-rules)
      ;; clear the predications out of each model
      (loop for model in *models* doing (clear-model model clear-database undefrule-rules)))
    (when clear-database
      ;; clear the rules of the state in the database
      (mapc #'clear-rule *forward-rules*)
      (mapc #'clear-rule *backward-rules*))
    (loop for model in *models* doing (after-clear model clear-database undefrule-rules))
    ;; return a harmless value
    t))
