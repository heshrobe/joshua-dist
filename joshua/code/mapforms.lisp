;;; -*- Mode: LISP; Package: jlt; Syntax: Ansi-Common-Lisp -*-
;;;>
;;;> *****************************************************************************************
;;;> ** (c) Copyright 1988 Symbolics, Inc.  All rights reserved.
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
;;;> Symbolics 3640, Symbolics 3645 (R), Symbolics 3650 (R), Symbolics 3620 (R), Symbolics 
;;;> 3610 (R), Symbolics Common Lisp (R), Symbolics-Lisp (R), Zetalisp (R), Genera (R),
;;;> Wheels, Dynamic Windows (R), Showcase, SmartStore, Semanticue, Frame-Up, Firewall (R),
;;;> MACSYMA (R), COMMON LISP MACSYMA, CL-MACSYMA (R), LISP MACHINE MACSYMA (R), MACSYMA 
;;;> Newsletter (R), Document Examiner (R), S-DYNAMICS (R), S-GEOMETRY (R), S-PAINT (R),
;;;> S-RENDER (R), "Your Next Step in Computing" (R), Ivory, Symbolics C, Symbolics Pascal,
;;;> and Symbolics Fortran are trademarks of Symbolics, Inc.
;;;> 
;;;> RESTRICTED RIGHTS LEGEND
;;;>    Use, duplication, and disclosure by the Government are subject to restrictions 
;;;> as set forth in subdivision (c)(1)(ii) of the Rights in Trademark Data and Computer 
;;;> Software Clause at FAR 52.227-7013.
;;;> 
;;;>      Symbolics, Inc.
;;;>      11 Cambridge Center
;;;>      Cambridge, Massachusetts  02142
;;;>      United States of America
;;;>      617-621-7500
;;;> *****************************************************************************************
;;;>
;;; Written May 1982 by David A. Moon for use by the Common Lisp community
;;; Revised April 1983

;;; Tools for source code analysis: code-walker

;--- Common Lisp version conversion issues:
;--- use Common Lisp condition system to signal errors, when it has one
;--- BLOCK has to be processed in a very funny way?  See (RETURN MAPFORMS)
;--- Certain symbols, e.g. SYMEVAL, aren't CL function names any more
;                      Use the #+ reader macro to dike out
;                      thing like this. --jwatkins
;--- Uses extended DEFUN syntax for defining functions as properties
;--- Depends on having LOOP of course --
;              use the CL version of LOOP from gsb. --jwatkins


;;; Interface definitions

(in-package :jlt)

;; The entry functions to this module are MAPFORMS and COPYFORMS
;;  which take a funarg, a form, and keywords, and call the funarg on parts of the form.
;;  MAPFORMS-1 and COPYFORMS-1 may be called from inside the funarg.
;; The ARG-TEMPLATE declaration is used when defining a special form.
;; The MAPFORMS property may be used for complex special forms.
;; Errors detected are of two kinds:
;;  Problems with the form being mapped over signal FORM-NOT-UNDERSTOOD
;;	These errors may always be proceeded and will do something reasonable by default.
;;	This code isn't as careful as it could be about checking for dotted lists in forms.
;;  Bugs in MAPFORMS itself, or in templates, are signalled with ERROR.


;;; KIND
;; A piece of Lisp code has a KIND, saying what it is, independent of context.
;; The following kinds are forms (they get evaluated):
;;	QUOTE - a constant (whether quoted or self-evaluating or same-evaluating)
;;		i.e. this form is guaranteed always to evaluate the same no
;;		matter how many times and in what context you evaluate it
;;      FSYMEVAL - A variable reference in function position
;;	SYMEVAL - a variable reference
;;	a list - a function combination of any sort (normal, special, or lambda)
;;	  for special forms, the list is non-empty and its cdr is the arg-template 
;;	  to be matched against the cdr of the form (see next page)
;;	  for regular function combinations, the list is NIL
;; The following kinds are not forms:
;;	SET - a variable being setqed
;;	LET - a variable being bound
;;	DECLARE - a local declaration
;;	GO - a prog tag being gone to
;;	RETURN-FROM - a block name (prog name) being returned from
;;	ARBITRARY - an arbitrary side-effect not associated with any particular piece
;;		of Lisp code.  The code passed is just the name of the special form involved.


;;; USAGE
;; The context of a piece of Lisp code is described by a usage symbol.
;; These are the usages that the MAPFORMS funarg will see.  The somewhat
;; similar usages used in arg templates are described on the next page.
;;
;; The following usages imply evaluation, and tell something about how the result
;; of the evaluation is used:
;;	EVAL - general case
;;	TEST - all that matters is whether the value is NIL
;;	EFFECT - the value is not used
;;	SMASH - the resulting object is modified (e.g. NREVERSE)
;;	PROP - the result is used as a property name
;;	FUNCTION - the result is used as a function
;;  more of these are likely to be added in the future; unrecognized usages should be
;;  assumed to imply evaluation
;;--- SMASH and PROP templates have not been put in on the many functions that would
;;--- need them.  Interlisp seems to find these useful; we could put them in someday.
;;  The KIND of a form used with one of the above usages will necessarily be
;;  one of the "form" kinds: QUOTE, SYMEVAL, or a list.
;;
;; The following usages do not imply evaluation, hence don't go with forms:
;;	QUOTE - a subform that is not evaluated
;;	SET - a variable being setq'ed
;;	LET - a variable being bound
;;	SYMEVAL - a variable used as a variable (but not a form)
;;	CALL - a function (typically inside of #')
;;	FLAVOR:GENERIC - a generic function, as found inside (FLAVOR:GENERIC FOO)
;;	GO - a prog tag being gone to
;;	RETURN-FROM - a block name being returned from
;;	DECLARE - a local declaration
;;	ARBITRARY - some arbitrary side-effect is occurring, not associated
;;			with a particular form.  The piece of Lisp code
;;			is the name of the special form involved.
;; Each of the above non-form usages has a characteristic KIND that goes with
;; it.  This is the same symbol, except for CALL where the KIND is QUOTE.


;;; ARG-TEMPLATE declaration
;;
;; An argument template is a tree which is matched against the cdr of a form.
;; Leaves of the tree are symbols or lists with special symbols in their car,
;; and usually match forms to be evaluated (sometimes they match special syntactic
;; things).  The leaves define where the forms to be evaluated are and also
;; something about how the arguments are used.  Thus many of the symbols that
;; may be used as leaves are the same as the USAGE symbols listed above.
;;
;; Possible leaves are:
;;	QUOTE - this expression is not evaluated
;;	SET - a variable appearing here is setqed
;;	LET - a variable appearing here is bound (a list is a variable and a value)
;;      LETF - Like LET, but an expression is allowed.  This is a Zetalisp feature
;;	PARALLEL-LET - like ((REPEAT LET)) but the bindings are done in parallel
;;      PARALLEL-LETF - combine the features of LETF and PARALLEL-LET
;;	SYMEVAL - a variable appearing here is used as a variable (but is not a form)
;;	CALL - this expression is not evaluated, but if it is a function it is called
;;	BODY - any number of expressions, all but the last for effect ("progn")
;;	DECLARE - any number of local declarations and documentation strings may appear here
;;	  (the funarg sees single declarations with a usage of DECLARE)
;; 	PROG - prog tags and forms evaluated for effect (a prog body)
;;	GO - this expression is not evaluated (it's a prog tag being gone to)
;;	RETURN-FROM - this expression is not evaluated (it's a block name being returned from)
;;	BLOCK - this expression is not evaluated (it's a block name being defined)
;;	EVAL - a form is evaluated
;;	TEST - a form is evaluated, but all that matters is whether the value is NIL
;;	EFFECT - a form is evaluated, but its value is not used
;; 	RETURN - a form is evaluated, and its value is also the value of the whole form
;;	SMASH - a form is evaluated and the resulting object is modified (e.g. NREVERSE)
;;	PROP - a form is evaluated and the result is used as a property name
;;	FUNCTION - a form is evaluated and the result is used as a function
;;	ARBITRARY - does not match any subform; indicates that an arbitrary
;;		side-effect occurs at this point.  This is an "escape hatch"
;;		for special forms that don't fit in to the model very well.
;;   The next three are attributes of the whole form and don't match any subforms
;;   These must appear at the front of a template
;;	COND - this form is a conditional; it doesn't necessarily evaluate all its subforms.
;;	LOOP - this form is an iteration; it may evaluate some subforms no or multiple times.
;; 	ANONYMOUS-BLOCK - indicates an unnamed prog
;;   The remaining leaves are "complex".
;;    REPEAT and ORDER match multiple subforms, the others match one.
;;	(REPEAT template template...) - the sequence of templates is repeated zero
;;		or more times, to match the length of the form
;;	(IF predicate true-template false-template) - use predicate to decide
;;		which template to use.  If predicate is atomic, it is a function
;;		applied to the matching expression, otherwise it is a form to
;;		to evaluate with EXPR bound to the matching expression.
;;	(ORDER (n template) (n template)...) - the next several subforms are matched
;;		to the templates in order.  But the order of evaluation (and hence
;;		of mapforms processing) is not left-to-right, but is according
;;		to increasing numerical order of the numbers "n".
;;		By special hair, one of the templates may be a REPEAT.
;;   The following two can really screw things up when the correspondence between
;;   what is analyzed and the original code matters.  Fortunately they aren't
;;   used currently.  They come from Interlisp.
;;	(AND template template...) - all of the templates specified apply
;;		this causes the matching expression to be analyzed multiple times
;;	(MACRO expr template) - expr and template are forms to be evaluated,
;;		with EXPR bound to the matching expression.  Use the results as
;;		the new matching expression and the new template.
;;  more of these are likely to be added in the future; unrecognized symbols should be
;;  assumed to imply evaluation
;;
;; Error if the form is longer than the template, but not vice versa (optional args).
;;
;; Example declaration for COND:
;;	(DECLARE (ARG-TEMPLATE COND (REPEAT (TEST . BODY))))


(DEFUN MAPFORMS (*MAPFORMS-FUNCTION* FORM
		 &KEY (INITIAL-STATE NIL)
		      (BOUND-VARIABLES 'NO-ENV)
		      (USAGE 'EVAL)
		      (APPLY-FUNCTION NIL)
		      (ITERATION-HOOK NIL)
		      (EXPAND-SUBSTS NIL)
		      (EXPAND-ALL-MACROS NIL)
		      (ENVIRONMENT NIL)
		      (host-environment nil)
		 &AUX (*COPYFORMS-FLAG* NIL)
		      (*MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT* ENVIRONMENT)
		      (*host-specific-environment* host-environment)
		      (*MAPFORMS-BOUND-VARIABLES* BOUND-VARIABLES)
		      (*MAPFORMS-ITERATION-HOOK* ITERATION-HOOK)
		      (*MAPFORMS-EXPAND-SUBSTS* EXPAND-SUBSTS)
		      (*MAPFORMS-BLOCK-NAMES* NIL)
		      (*MAPFORMS-GO-TAGS* NIL)
		      (*MAPFORMS-BLOCK-ALIST* NIL)
		      (*MAPFORMS-APPLY-FUNCTION* APPLY-FUNCTION)
		      (*MAPFORMS-STATE* INITIAL-STATE)
		      (*MAPFORMS-LEVEL* 0)
		      (*COPYFORMS-EXPAND-ALL-MACROS* EXPAND-ALL-MACROS))
  "Call a function on a form and all of its subforms.
  The function is called on arguments subform, kind, usage, and state,
the first two return values are new-form and flag (as with copyforms)
and its third returned value is the new state.  If the second value is
non-NIL the normal processing of this form is to be suppressed.
  STATE is initially NIL unless the :INITIAL-STATE option is specified;
the final state is returned as the value of MAPFORMS.
  KIND is a symbol or list describing the subform (which can be a form or a
variable being setq'ed or bound).
  USAGE is a symbol describing the context in which the subform appears.
The :USAGE option, defaulting to EVAL, is the usage for the top-level form.
  If the :BOUND-VARIABLES option is specified, it is the initial value
\(usually NIL) for *MAPFORMS-BOUND-VARIABLES*, the list of variables
bound around the evaluation of each form.  If :BOUND-VARIABLES is not
specified, the bookkeeping for bound variables is suppressed.
  If the :APPLY-FUNCTION option is specified, it is a function called
with the same arguments and values as the main processing function.  It sees
each non-atomic form after its arguments or subforms have been processed.
  If the :ITERATION-HOOK option is specified, it is a function called with
an argument of T when an iteration is entered and NIL when it is left.
  If the :EXPAND-SUBSTS option is specified, we look inside DEFSUBST bodies.
Normally they are just assumed to behave like functions."
  (WITH-MAPFORMS-ENVIRONMENT-TAG (*MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*)
    (COPYFORMS-1 FORM USAGE)
    *MAPFORMS-STATE*))

(DEFUN COPYFORMS (*MAPFORMS-FUNCTION* FORM
		  &KEY (BOUND-VARIABLES 'NO-ENV)
		       (USAGE 'EVAL)
		       (APPLY-FUNCTION NIL)
		       (ITERATION-HOOK NIL)
		       (EXPAND-SUBSTS NIL)
		       (EXPAND-ALL-MACROS NIL)
		       (ENVIRONMENT NIL)
		       (host-environment nil)
		  &AUX (*COPYFORMS-FLAG* T)
		       (*MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT* ENVIRONMENT)
		       (*host-specific-environment* host-environment)
		       (*MAPFORMS-APPLY-FUNCTION* APPLY-FUNCTION)
		       (*MAPFORMS-ITERATION-HOOK* ITERATION-HOOK)
		       (*MAPFORMS-EXPAND-SUBSTS* EXPAND-SUBSTS)
		       (*MAPFORMS-BOUND-VARIABLES* BOUND-VARIABLES)
		       (*MAPFORMS-BLOCK-NAMES* NIL)
		       (*MAPFORMS-GO-TAGS* NIL)
		       (*MAPFORMS-BLOCK-ALIST* NIL)
		       (*MAPFORMS-LEVEL* 0)
		       (*COPYFORMS-EXPAND-ALL-MACROS* EXPAND-ALL-MACROS))
  "Call a function on a form and all its subforms, possibly making
substitutions.  The function is called on arguments subform, kind, and usage,
and its returned value replaces the subform if it is not EQ.  If the second
value is non-NIL the normal processing of this form is to be suppressed.
Structure is copied as necessary to avoid smashing any of the original form.
  KIND is a symbol or list describing the subform (which can be a form or a variable
being setq'ed or bound).
  USAGE is a symbol describing the context in which the subform appears.  The
:USAGE option, defaulting to EVAL, is the usage for the top-level form.
  If the :EXPAND-ALL-MACROS option is specified, macro-expansions will
be copied into the result.  Otherwise, the original macro form will
remain, unless something in the expansion was modified during copying.
  If the :BOUND-VARIABLES option is specified, it is the initial value
\(usually NIL) for *MAPFORMS-BOUND-VARIABLES*, the list of variables
bound around the evaluation of each form.  If :BOUND-VARIABLES is not
specified, the bookkeeping for bound variables is suppressed.
  If the :APPLY-FUNCTION option is specified, it is a function called
with the same arguments and values as the main processing function.  It sees
each non-atomic form after its arguments or subforms have been processed.
If it substitutes a new form, the new form will be analyzed and copied.
  If the :ITERATION-HOOK option is specified, it is a function called with
an argument of T when an iteration is entered and NIL when it is left."
  (WITH-MAPFORMS-ENVIRONMENT-TAG (*MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*)
    (COPYFORMS-1 FORM USAGE)))

;;; Determine the KIND of a form (see the first page)
;;; As a second value we may return one of
;;;	LAMBDA - a lambda combination
;;;	ZL:NAMED-LAMBDA - a named-lambda combination
;;;	MACRO - a macro combination (for which there is no arg-template)
;;;	LAMBDA-MACRO - a lambda-macro combination
;;;	SYMBOL-MACRO - a symbol macro
;;;	some other atom - a special processing function, obtained from the
;;;	  property of the function name whose indicator is our second argument
;;; Return KIND and SPECIAL.
(defun classify-form (form property)
  (if (atom form)
      (cond ((constantp form) 'quote)
	    ((symbol-macro-p form *mapforms-lexical-function-environment*)
	     (values 'macro 'symbol-macro))
	    (t 'symeval))
      (let ((function (car form)))
	(cond
	  ((eq function 'quote) 'quote)
	  ((symbolp function)
	   (let ((local-definition (assoc function (env-functions *mapforms-lexical-function-environment*) :test #'eq))
		 (definition (and (fboundp function) (symbol-function function)))
		 (template (gethash function *arg-template-table*))
		 (function-property (and property (get function property))))
	     (cond
	       (local-definition
		(when (eq (car local-definition) 'special) (values nil 'macro)))
	       ((Not (fboundp function)) nil)
	       ;; The template takes precedence over an actual provided macro
	       (template
		(values template function-property))
	       ;; ok is it a macro?
	       ((macro-function function) (values 'macro 'macro))
	       ;; is it a vanilla global function
	       ;; Note: Special forms have definitions that aren't functionp in ALLEGRO
	       ;; But in SBCL this seems more complicated.  For example (macro-function 'COND) is T
	       ;; (macro-function 'if) is NIL.  (functionp #'if) is T (while in Allegro it's NIL).
	       ;; Both have templates, so we never get here for those.
	       ;; In SBCL a macro would test positive for functionp, so the order here matters
	       (#-SBCL (functionp definition) #+SBCL (not (sb-kernel::special-operator-p function)) nil)
	       ;; I don't know why the (symbol-function function) would be an array.  But it seems to
	       ;; be true for something that I've forgotten about.
	       ;; But this screws up mcl since the definition of macrolet and symbolmacrolet are in fact
	       ;; arrays for some reason.  
	       #-mcl
	       ((arrayp definition) nil)
	       ;; finally it's bound but not to a macro or a function but it has a property
	       ((and property function-property)
		(values nil function-property))
	       (t (form-not-understood form "~s is a special form but lacks an arg-template"
				       function)
		  nil))))
	  ((and (listp function) (eq (car function) 'lambda))
	   (values nil 'lambda))
	  (t (form-not-understood form "~s not understood in the function position of a form" function)
	     nil)))))



;;;; Main driving functions

;;; Process a form and its subforms, and return the new form
;;; (If not COPYFORMS, return the original form)
;;; The user function may call back into this if doing a COPYFORMS
(DEFUN COPYFORMS-1 (ORIGINAL-FORM &OPTIONAL (USAGE 'EVAL))
  ;; FORM -- the form currently being processed
  ;; NEW-FORM -- temporary to hold value returned by user processing function
  ;; ORIGINAL-FORM -- FORM before template-driven expansion of subforms.
  ;;    Note that ORIGINAL-FORM is -not- the original form before macro expansion!
  ;;    FORM and ORIGINAL-FORM are only different when FORM was consed by the LT system itself.
  ;; ORIGINAL-BEFORE-MACRO-EXPANSION -- same as ORIGINAL-FORM except that when
  ;;    a macro expansion occurs, ORIGINAL-BEFORE-MACRO-EXPANSION is not updated,
  ;;    allowing the macro expansion to be undone.
  ;;
  ;; Loop as long as new forms are substituted
  (LOOP WITH (KIND SPECIAL)
	WITH FORM = ORIGINAL-FORM
	WITH ORIGINAL-BEFORE-MACRO-EXPANSION = ORIGINAL-FORM
	WITH DONE-FLAG				;Flags considered harmful: used for two purposes, too!
	WITH NEW-FORM DO
    (MULTIPLE-VALUE-SETQ (KIND SPECIAL) (CLASSIFY-FORM FORM 'MAPFORMS))
    ;; Tell the client about this form.
    ;; It may replace the form or override normal subform processing.
    (MULTIPLE-VALUE-SETQ (NEW-FORM DONE-FLAG) (MAPFORMS-CALL FORM KIND USAGE))
    ;; Process the form accordingly, and set DONE-FLAG if loop should terminate
    (LET ((*MAPFORMS-LEVEL* (1+ *MAPFORMS-LEVEL*)))
      (COND ((NOT (EQ NEW-FORM FORM))
	     (SETQ FORM NEW-FORM)		;Again, with substituted form
	     (SETF ORIGINAL-FORM NEW-FORM)
	     (SETF ORIGINAL-BEFORE-MACRO-EXPANSION NEW-FORM))
	    (DONE-FLAG)				;Bypass normal processing
	    ((OR (EQ SPECIAL 'MACRO)		;Any kind of macro
		 (EQ SPECIAL 'SYMBOL-MACRO))
	     (LET ((EXPANSION
		     (MAPFORMS-MACROEXPAND FORM *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*)))
	       (SETQ ORIGINAL-FORM EXPANSION)
	       (SETQ FORM EXPANSION)))
	    ((EQ SPECIAL 'LAMBDA)		;Lambda-combination
	     (LET ((LAMBDA-LIST (CDAR FORM)))
	       ;; Check for lambda-list keywords that would mean abnormal
	       ;; argument evaluation.
	       (IF (MEMBER '&QUOTE LAMBDA-LIST)
		   (FORM-NOT-UNDERSTOOD FORM "&QUOTE appears in the lambda list"))
	       ;; First process the arguments.
	       (SETQ FORM (MAPFORMS-LIST ORIGINAL-FORM FORM (CDR FORM) 'EVAL 'EVAL))
	       ;; Now process the bindings and then the body
	       (MAPFORMS-RPLACA ORIGINAL-FORM FORM FORM
				(MAPFORMS-LAMBDA (CAR FORM) (CAR FORM) LAMBDA-LIST USAGE))
	       (SETQ DONE-FLAG T)))
	    (SPECIAL				;General escape
	     (SETQ FORM (FUNCALL SPECIAL ORIGINAL-FORM FORM USAGE))
	     (SETQ DONE-FLAG T))
	    ((NULL KIND)			;Ordinary function, do args
	     (SETQ FORM (MAPFORMS-LIST ORIGINAL-FORM FORM (CDR FORM) 'EVAL 'EVAL))
	     (mapforms-call (first form) 'fsymeval 'function)
	     (SETQ DONE-FLAG T))
	    ((LISTP KIND)			;Template-driven meta-eval
	     (LET ((TEMPLATE (CDR KIND)))
	       (AND (LISTP TEMPLATE)
		    (MEMBER (CAR TEMPLATE) '(COND LOOP))
		    (SETQ TEMPLATE (CDR TEMPLATE)))	;Remove flags uninteresting here
	       (SETQ FORM (MAPFORMS-TEMPLATE ORIGINAL-FORM FORM TEMPLATE USAGE))
	       (SETQ DONE-FLAG T)))
	    (T (SETQ DONE-FLAG T))))		;No subforms
    ;; Now decide whether to return what we have or process it again
    (WHEN DONE-FLAG
      (WHEN (AND (CONSP FORM)
		 (NOT (NULL *MAPFORMS-APPLY-FUNCTION*)))
	(MULTIPLE-VALUE-SETQ (NEW-FORM DONE-FLAG)
	  (MAPFORMS-CALL FORM KIND USAGE *MAPFORMS-APPLY-FUNCTION*))
	(COND ((EQ NEW-FORM FORM)
	       (SETF DONE-FLAG T))
	      (T
	       (SETF FORM NEW-FORM)
	       (SETF ORIGINAL-FORM NEW-FORM)
	       (SETF ORIGINAL-BEFORE-MACRO-EXPANSION NEW-FORM))))
      (WHEN DONE-FLAG
	(COND (*COPYFORMS-EXPAND-ALL-MACROS* (return FORM))
	      ;; If macro expansion was uninteresting, undo it
	      ((EQ FORM ORIGINAL-FORM) (return ORIGINAL-BEFORE-MACRO-EXPANSION))
	      ;; If no macro expansion was involved, form was replaced wholesale
	      ((EQ ORIGINAL-FORM ORIGINAL-BEFORE-MACRO-EXPANSION) (return FORM))
	      ;; That didn't succeed, so include the macro expansion in the result
	      (T (return FORM)))))))
  
;;; The user function may call back into this if doing a MAPFORMS
(DEFUN MAPFORMS-1 (FORM &OPTIONAL (USAGE 'EVAL))
  (COPYFORMS-1 FORM USAGE)
  *MAPFORMS-STATE*)

;;; Call the user function on this form, and return the new form
(DEFUN MAPFORMS-CALL (FORM KIND USAGE &OPTIONAL (FUNCTION *MAPFORMS-FUNCTION*) &AUX FLAG)
  (COND (*COPYFORMS-FLAG*
	 (FUNCALL FUNCTION FORM KIND USAGE))
	(T
	 (MULTIPLE-VALUE-SETQ (form FLAG *MAPFORMS-STATE*)
			      (FUNCALL FUNCTION FORM KIND USAGE *MAPFORMS-STATE*))
	 (VALUES FORM FLAG))))

;;; Process the rest of the forms in a list.  Return the original list or a copy
;;; of it with substitutions made.
(DEFUN MAPFORMS-LIST (ORIGINAL-LIST CURRENT-LIST TAIL-TO-DO ALL-BUT-LAST-USAGE LAST-USAGE)
  (LOOP FOR TAIL ON TAIL-TO-DO DO
    (MAPFORMS-RPLACA ORIGINAL-LIST CURRENT-LIST
		TAIL (COPYFORMS-1 (CAR TAIL) (IF (CDR TAIL) ALL-BUT-LAST-USAGE LAST-USAGE))))
  CURRENT-LIST)

;;; Pass over documentation strings and local declarations, and return two values,
;;; and save the declarations in *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT* and in
;;; LOCAL-DECLARATIONS.
;;;	New value of CURRENT-LIST
;;;	New value of TAIL
(DEFUN MAPFORMS-DECLARE (ORIGINAL-LIST CURRENT-LIST TAIL &AUX (DECLARATIONS NIL) EXP)
  (LOOP DOING
    (COND ((NULL TAIL) (RETURN))
	  ((AND (CDR TAIL) (STRINGP (CAR TAIL))))	;Doc string
	  ((AND (LISTP (CAR TAIL))
		;; I have nuked out the form which macroexpanded before
		;; looking for a declaration.
		(LISTP (SETQ EXP (CAR TAIL)))
		(EQ (CAR EXP) 'DECLARE))
	   (LOOP WITH ORIGINAL = (CAR TAIL)		;Map over each declaration
		 WITH CURRENT = EXP
		 FOR DCLS ON (CDR EXP)
		 AS DCL = (CAR DCLS)
		 DO (MAPFORMS-RPLACA ORIGINAL CURRENT
				     DCLS (SETQ DCL (MAPFORMS-CALL DCL 'DECLARE 'DECLARE)))
		    (WHEN (LISTP DCL)
		      (CASE (CAR DCL)
			((INLINE NOTINLINE)
			 (DOLIST (FUN (CDR DCL))
			   (LET ((ENTRY (ASSOC FUN (ENV-FUNCTIONS *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*))))
			     (PUSH (IF ENTRY
				       `(,FUN ,(SECOND ENTRY) INLINE ,(CAR DCL) ,@(CDDR ENTRY))
				       `(,FUN NIL INLINE ,(CAR DCL)))
				   (ENV-FUNCTIONS *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*)))))
			((IGNORE TYPE DYNAMIC-EXTENT FTYPE FUNCTION))))
		 FINALLY (MAPFORMS-RPLACA ORIGINAL-LIST CURRENT-LIST TAIL CURRENT)))
	  (T (RETURN)))
    (POP TAIL))
  (SETF LOCAL-DECLARATIONS (APPEND DECLARATIONS LOCAL-DECLARATIONS))
  (PUSH DECLARATIONS (ENV-DECLARATIONS *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*))
  (VALUES CURRENT-LIST TAIL))

(DEFUN CORRESPONDING-TAIL (TAIL OLD-LIST NEW-LIST)
  (IF (EQ OLD-LIST NEW-LIST)
      TAIL
      (LOOP FOR NEW-TAIL ON NEW-LIST
	    FOR OLD-TAIL ON OLD-LIST
	    WHEN (EQ OLD-TAIL TAIL)
	      RETURN NEW-TAIL
	    FINALLY (ERROR "Tail not found."))))

;;; Process a lambda-expression, or any function body
(DEFUN MAPFORMS-LAMBDA (ORIGINAL-LAMBDA LAMBDA ARGS-AND-BODY USAGE)
  (PROG2
   (WHEN *MAPFORMS-ITERATION-HOOK*
	 (FUNCALL *MAPFORMS-ITERATION-HOOK* 'LAMBDA lambda))
   (LET ((*MAPFORMS-BOUND-VARIABLES* *MAPFORMS-BOUND-VARIABLES*)
	 (LOCAL-DECLARATIONS LOCAL-DECLARATIONS)
	 (BODY (CDR ARGS-AND-BODY)))
	(WITH-NEW-INTERPRETER-ENVIRONMENT
	    (MULTIPLE-VALUE-BIND (NEW-LAMBDA NEW-BODY)
				 (MAPFORMS-DECLARE ORIGINAL-LAMBDA LAMBDA BODY)
				 (SETF ARGS-AND-BODY (CORRESPONDING-TAIL ARGS-AND-BODY LAMBDA NEW-LAMBDA))
				 (SETF LAMBDA NEW-LAMBDA)
				 (SETF BODY NEW-BODY))
	  (LET ((BODY-LAMBDA LAMBDA))
	       (MAPFORMS-RPLACA ORIGINAL-LAMBDA LAMBDA ARGS-AND-BODY
				(LOOP WITH LAMBDA-LIST = (CAR ARGS-AND-BODY)
				      WITH ORIGINAL-LAMBDA-LIST = LAMBDA-LIST
				      WITH ALLOW-SUPPLIED-P = NIL
				      WITH ALLOW-INITIALIZATION = NIL
				      FOR LL ON LAMBDA-LIST
				      FOR (L) = LL DO
				      (COND ((MEMBER L LAMBDA-LIST-KEYWORDS)
					     (when *mapforms-iteration-hook*
					       (funcall *mapforms-iteration-hook* 'lambda-keyword l))
					     (CASE L
						   ((&OPTIONAL)
						    (SETQ ALLOW-SUPPLIED-P T)
						    (SETF ALLOW-INITIALIZATION T))
						   ((&KEY)
						    (SETQ ALLOW-SUPPLIED-P '&KEY)
						    (SETF ALLOW-INITIALIZATION T))
						   ((&REST)
						    (SETQ ALLOW-SUPPLIED-P NIL)
						    (SETF ALLOW-INITIALIZATION NIL))
						   ((&AUX)
						    (SETQ ALLOW-SUPPLIED-P NIL)
						    (SETF ALLOW-INITIALIZATION T))))
					    (T
					     (WHEN *MAPFORMS-ITERATION-HOOK*
						   (FUNCALL *MAPFORMS-ITERATION-HOOK* 'start-binding
							    lambda-list
							    ))
					     (MAPFORMS-RPLACA
					      ORIGINAL-LAMBDA-LIST LAMBDA-LIST LL
					      (IF (OR ALLOW-INITIALIZATION
						      (ATOM L))
						  (MAPFORMS-BIND-SINGLE
						   LL NIL ALLOW-SUPPLIED-P LAMBDA)
						  (MAPFORMS-RPLACA
						   L L L (MAPFORMS-BIND-SINGLE L NIL NIL LAMBDA))))
					     (WHEN *MAPFORMS-ITERATION-HOOK*
						   (FUNCALL *MAPFORMS-ITERATION-HOOK* 'end-binding
							    lambda-list))))
				      FINALLY (RETURN LAMBDA-LIST)))
	       (SETF BODY (CORRESPONDING-TAIL BODY BODY-LAMBDA LAMBDA)))
	  (when *MAPFORMS-ITERATION-HOOK*
	    (funcall *MAPFORMS-ITERATION-HOOK* 'lambda-bindings-exit lambda)
	    (funcall *MAPFORMS-ITERATION-HOOK* 'body-start body))
	  (MAPFORMS-CALL (MAPFORMS-LIST ORIGINAL-LAMBDA LAMBDA  BODY 'EFFECT USAGE)
			 'QUOTE 'CALL)
	  (when *MAPFORMS-ITERATION-HOOK*
	    (funcall *MAPFORMS-ITERATION-HOOK* 'body-end body))
	  )
	lambda)
   (WHEN *MAPFORMS-ITERATION-HOOK*
	 (FUNCALL *MAPFORMS-ITERATION-HOOK* 'lambda-end lambda))))

(DEFUN MAPFORMS-BIND-SINGLE (BINDING-LIST PARALLEL-BINDING-P ALLOW-SUPPLIED-P CONTAINING-FORM)
  (LET ((*MAPFORMS-LOCATOR-START* BINDING-LIST)
	(*MAPFORMS-LOCATOR-END* (CDR BINDING-LIST)))
    (MAPFORMS-BIND (CAR BINDING-LIST) PARALLEL-BINDING-P ALLOW-SUPPLIED-P CONTAINING-FORM)))

;; Process a single binding
;; which may be VAR, (VAR), or (VAR VAL)
;; ALLOW-SUPPLIED-P is NIL normally
;;		       T to allow (var val flag-var)
;;		       IGNORE to allow (var val . anything)
;;		       &KEY to allow ((keyword var) ...) syntax too
;;--- Doesn't handle separate LOCAL-DECLARATIONS for the variable and the init form
(DEFUN MAPFORMS-BIND (BIND PARALLEL-BINDING-P ALLOW-SUPPLIED-P CONTAINING-FORM
		      &AUX (ORIGINAL-BIND BIND) (VAR1 NIL) (VAR2 NIL))
  (COND ((SYMBOLP BIND)
	 (SETQ BIND (SETQ VAR1 (MAPFORMS-CALL BIND 'LET 'LET))))
	((ATOM BIND)
	 (FORM-NOT-UNDERSTOOD CONTAINING-FORM
			      "~S appears where a bound variable should be" BIND))
	((AND (NOT (SYMBOLP (CAR BIND)))
	      (OR (NOT (EQ ALLOW-SUPPLIED-P '&KEY))
		  (NOT (LISTP (CAR BIND)))
		  (NOT (SYMBOLP (CAAR BIND)))))
	 (FORM-NOT-UNDERSTOOD CONTAINING-FORM
			      "~S appears where a bound variable should be" (CAR BIND)))
	(T
 	 (IF (LISTP (CAR BIND))
	     (LET ((CAR (CAR BIND)))
	       (MAPFORMS-RPLACA (CAR ORIGINAL-BIND) CAR CAR
				(SETQ VAR1 (MAPFORMS-CALL (CAR CAR) 'LET 'LET)))
	       (MAPFORMS-RPLACA ORIGINAL-BIND BIND BIND CAR))
	     (MAPFORMS-RPLACA ORIGINAL-BIND BIND BIND
			      (SETQ VAR1 (MAPFORMS-CALL (CAR BIND) 'LET 'LET))))
	 (WHEN (CDR BIND)
	   ;; Init form or default value for optional argument
	   (MAPFORMS-RPLACA ORIGINAL-BIND BIND (CDR BIND) (COPYFORMS-1 (CADR BIND) 'EVAL))
	   (COND ((NULL (CDDR BIND)))
		 ((OR (CDDDR BIND) (NOT ALLOW-SUPPLIED-P))
		  (FORM-NOT-UNDERSTOOD CONTAINING-FORM
				       "~S is too long to be a list of variable and value"
				       BIND))
		 ((EQ ALLOW-SUPPLIED-P 'IGNORE))
		 ((NOT (SYMBOLP (CADDR BIND)))
		  (FORM-NOT-UNDERSTOOD CONTAINING-FORM
				       "~S appears where a supplied-p variable should be"
				       (CADDR BIND)))
		 (T ;; Optional argument supplied-p-flag variable
		  (MAPFORMS-RPLACA ORIGINAL-BIND BIND (CDDR BIND)
				   (SETQ VAR2 (MAPFORMS-CALL (CADDR BIND) 'LET 'LET))))))))
  (COND (PARALLEL-BINDING-P
	 (WHEN VAR1 (PUSH VAR1 *MAPFORMS-PARALLEL-BINDS*))
	 (WHEN VAR2 (PUSH VAR2 *MAPFORMS-PARALLEL-BINDS*)))
	(T
	 (LET ((BOUND-VARIABLES-P (Not (eql *MAPFORMS-BOUND-VARIABLES* 'NO-ENV))))
	   (WHEN VAR1
	     (WHEN BOUND-VARIABLES-P (PUSH VAR1 *MAPFORMS-BOUND-VARIABLES*))
	     (PUSH (LIST VAR1 NIL)
		   (ENV-VARIABLES *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*)))
	   (WHEN VAR2
	     (WHEN BOUND-VARIABLES-P (PUSH VAR2 *MAPFORMS-BOUND-VARIABLES*))
	     (PUSH (LIST VAR2 NIL)
		   (ENV-VARIABLES *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*))))))
  BIND)

;;; Template-directed driving function

(DEFUN MAPFORMS-TEMPLATE (ORIGINAL-FORM *MAPFORMS-TEMPLATE-FORM*
					TEMPLATE *MAPFORMS-TEMPLATE-USAGE*)
  (MAPFORMS-RPLACD ORIGINAL-FORM *MAPFORMS-TEMPLATE-FORM*
		   *MAPFORMS-TEMPLATE-FORM*
		   (LET ((*MAPFORMS-BOUND-VARIABLES* *MAPFORMS-BOUND-VARIABLES*))
		     (MAPFORMS-TEMPLATE-1 (CDR *MAPFORMS-TEMPLATE-FORM*) TEMPLATE)))
  *MAPFORMS-TEMPLATE-FORM*)

;;; May return a modified version of ARGL, which the caller rplac's into his (sub)form
;;; This function is recursive in the car direction and iterative in the cdr
;;; ARGL is some piece of the original form (initially the cdr), not necessarily a list

(DEFUN MAPFORMS-TEMPLATE-1 (ORIGINAL-ARGL TEMPLATE)
  (LOOP WITH CURRENT-ARGL = ORIGINAL-ARGL
	WITH TAIL = NIL
	WITH ARGL = ORIGINAL-ARGL DO
    (COND ((NULL TEMPLATE)
	   (IF ARGL
	       (FORM-NOT-UNDERSTOOD *MAPFORMS-TEMPLATE-FORM*
				    "~S are extra arguments not allowed for by the template"
				    ARGL))
	   (LOOP-FINISH))
	  ;; The following template items match single subforms
	  ((MEMBER TEMPLATE '(QUOTE GO RETURN-FROM SET SYMEVAL SYMBOL-VALUE))
	   (SETQ ARGL (MAPFORMS-CALL ARGL TEMPLATE TEMPLATE))
	   (LOOP-FINISH))
	  ((EQ TEMPLATE 'LET)
	   (WHEN *MAPFORMS-ITERATION-HOOK*
		 (FUNCALL *MAPFORMS-ITERATION-HOOK* 'let-binding-enter argl)
		 (FUNCALL *MAPFORMS-ITERATION-HOOK* 'start-binding (first argl)))
	   (SETQ ARGL (MAPFORMS-BIND ARGL NIL NIL *MAPFORMS-TEMPLATE-FORM*))
	   (WHEN *MAPFORMS-ITERATION-HOOK*
		 (FUNCALL *MAPFORMS-ITERATION-HOOK* 'end-binding (first argl))
		 (FUNCALL *MAPFORMS-ITERATION-HOOK* 'let-binding-exit argl))
	   (LOOP-FINISH))
	  ((EQ TEMPLATE 'PARALLEL-LET)
	   (LET ((*MAPFORMS-PARALLEL-BINDS* NIL))
		(WHEN *MAPFORMS-ITERATION-HOOK*
		      (FUNCALL *MAPFORMS-ITERATION-HOOK* 'let-binding-enter argl))
	     (LOOP WHILE ARGL DO
		(WHEN *MAPFORMS-ITERATION-HOOK*
		      (FUNCALL *MAPFORMS-ITERATION-HOOK* 'start-binding (first argl)))
	       (MAPFORMS-RPLACA ORIGINAL-ARGL CURRENT-ARGL ARGL
				(MAPFORMS-BIND-SINGLE ARGL T NIL *MAPFORMS-TEMPLATE-FORM*))
	       (WHEN *MAPFORMS-ITERATION-HOOK*
		     (FUNCALL *MAPFORMS-ITERATION-HOOK* 'end-binding (first argl)))
	       (SETQ TAIL ARGL ARGL (CDR ARGL)))
	     (WHEN *MAPFORMS-ITERATION-HOOK*
		   (FUNCALL *MAPFORMS-ITERATION-HOOK* 'let-bindings-exit argl))
	     (DOLIST (VAR *MAPFORMS-PARALLEL-BINDS*)
	       (PUSH (LIST VAR NIL)
		     (ENV-VARIABLES *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*)))
	     (UNLESS (EQ *MAPFORMS-BOUND-VARIABLES* 'NO-ENV)
	       (SETQ *MAPFORMS-BOUND-VARIABLES*
		     (NCONC *MAPFORMS-PARALLEL-BINDS* *MAPFORMS-BOUND-VARIABLES*))))
	   (LOOP-FINISH))
	  ((EQ TEMPLATE 'CALL)
	   (SETQ ARGL (MAPFORMS-TEMPLATE-CALL ARGL))
	   (LOOP-FINISH))
	  ((ATOM TEMPLATE)
	   (CASE TEMPLATE
	     ((BODY)
	      (let ((answer nil))
		(when *mapforms-iteration-hook*
		  (funcall *mapforms-iteration-hook* 'body-start argl))
		(setq answer
		  (MAPFORMS-LIST ORIGINAL-ARGL CURRENT-ARGL
					   ARGL 'EFFECT *MAPFORMS-TEMPLATE-USAGE*))
		(when *mapforms-iteration-hook*
		  (funcall *mapforms-iteration-hook* 'body-end argl))
		(return answer)))
	     ((PROG)
	      (LET ((*MAPFORMS-GO-TAGS*
		      (AND (NOT (EQ *MAPFORMS-BOUND-VARIABLES* 'NO-ENV))
			   (NCONC (LOOP FOR STMT IN ARGL
					WHEN (ATOM STMT) COLLECT STMT)
				  *MAPFORMS-GO-TAGS*)))
		    (ITERATION NIL))
		(LOOP FOR TAIL ON ARGL AS STMT = (CAR TAIL) DO
		      (cond
		       ((ATOM STMT)
			;; First tag is start of possibly iterated code
			;; We aren't smart enough to worry about tags reached
			;; only by forward branches.
			(mapforms-call stmt 'tag 'tag)
			(UNLESS ITERATION
				(WHEN *MAPFORMS-ITERATION-HOOK*
				      (FUNCALL *MAPFORMS-ITERATION-HOOK* T t))
				(SETQ ITERATION T)))
		       ;; Lists are forms evaluated for effect
		       (t
			(MAPFORMS-RPLACA ORIGINAL-ARGL CURRENT-ARGL
					 TAIL (COPYFORMS-1 STMT 'EFFECT)))))
		(AND ITERATION
		     *MAPFORMS-ITERATION-HOOK*
		     (FUNCALL *MAPFORMS-ITERATION-HOOK* NIL nil))
		(RETURN CURRENT-ARGL)))
	     ((RETURN)
	      (SETQ ARGL (COPYFORMS-1 ARGL *MAPFORMS-TEMPLATE-USAGE*))
	      (LOOP-FINISH))
	     ((EVAL TEST EFFECT SMASH PROP FUNCTION)
	      (when (and (eql template 'test) *MAPFORMS-ITERATION-HOOK*)
		(FUNCALL *MAPFORMS-ITERATION-HOOK* 'test-entry argl))
	      (SETQ ARGL (COPYFORMS-1 ARGL TEMPLATE))
	      (when (and (eql template 'test) *MAPFORMS-ITERATION-HOOK*)
		(FUNCALL *MAPFORMS-ITERATION-HOOK* 'test-exit argl))
	      (LOOP-FINISH))
	     (OTHERWISE
	      (ERROR "Malformed template: ~S trying to match ~S in a ~S-form"
		     TEMPLATE ARGL (CAR *MAPFORMS-TEMPLATE-FORM*)))))
	  ((EQ (CAR TEMPLATE) 'AND)
	   (DOLIST (TEMPLATE (CDR TEMPLATE))
	     (SETQ ARGL (MAPFORMS-TEMPLATE-1 ARGL TEMPLATE)))
	   (LOOP-FINISH))
	  ((member (CAR TEMPLATE) '(IF #+cloe-developer 'zl:::cltl::if))
	   (SETQ ARGL (MAPFORMS-TEMPLATE-1 ARGL
			(IF (IF (ATOM (SECOND TEMPLATE)) (FUNCALL (SECOND TEMPLATE) ARGL)
				(LET ((EXPR ARGL))
				  (DECLARE (SPECIAL EXPR))
				  (EVAL (SECOND TEMPLATE))))
			    (THIRD TEMPLATE)
			    (FOURTH TEMPLATE))))
	   (LOOP-FINISH))
	  ((eql (CAR TEMPLATE) 'MACRO)
	   (LET ((EXPR ARGL))
	     (DECLARE (SPECIAL EXPR))
	     (SETQ ARGL (MAPFORMS-TEMPLATE-1 (EVAL (SECOND TEMPLATE))
					     (EVAL (THIRD TEMPLATE)))))
	   (LOOP-FINISH))

	  ;; The following template items match a variable number of subforms (or none)
	  ;; COND and LOOP should have been POP'ed off before we ever get here
	  ((EQ (CAR TEMPLATE) 'DECLARE)
	   (MULTIPLE-VALUE-SETQ (CURRENT-ARGL ARGL)
	     (MAPFORMS-DECLARE ORIGINAL-ARGL CURRENT-ARGL ARGL))
	   (SETQ TEMPLATE (CDR TEMPLATE)))
	  ((EQ (CAR TEMPLATE) 'BLOCK)
	   (MAPFORMS-RPLACD ORIGINAL-ARGL ARGL ARGL
			    (MAPFORMS-BLOCK (CAR ARGL) (CDR ARGL) (CDR TEMPLATE)))
	   (LOOP-FINISH))
	  ((EQ (CAR TEMPLATE) 'ANONYMOUS-BLOCK)
	   (SETQ ARGL (MAPFORMS-BLOCK NIL ARGL (CDR TEMPLATE)))
	   (LOOP-FINISH))
	  ((EQ (CAR TEMPLATE) 'ARBITRARY)
	   (MAPFORMS-CALL (CAR *MAPFORMS-TEMPLATE-FORM*) 'ARBITRARY 'ARBITRARY)
	   (SETQ TEMPLATE (CDR TEMPLATE)))
	  ((NULL ARGL) (LOOP-FINISH))
	  ((AND (LISTP (CAR TEMPLATE))
		(EQ (CAAR TEMPLATE) 'REPEAT))
	   (LOOP REPEAT (MAPFORMS-REPEAT-CHECK TEMPLATE ARGL (CDAR TEMPLATE)) DO
	     (LOOP FOR TEM IN (CDAR TEMPLATE) DO
	       (MAPFORMS-RPLACA ORIGINAL-ARGL CURRENT-ARGL
				ARGL (LET ((*MAPFORMS-LOCATOR-START* ARGL)
					   (*MAPFORMS-LOCATOR-END* (CDR ARGL)))
				       (MAPFORMS-TEMPLATE-1 (CAR ARGL) TEM)))
	       (SETQ TAIL ARGL ARGL (CDR ARGL))))
	   (SETQ TEMPLATE (CDR TEMPLATE)))
	  ((AND (LISTP (CAR TEMPLATE))
		(EQ (CAAR TEMPLATE) 'ORDER))
	   ;; First match up templates with forms, special-casing REPEAT
	   ;; Each element of FORMS is a form, or a list of forms to repeat through
	   ;; Each element of QUEUE is a list (priority template cons-of-FORMS)
	   (LOOP FOR X IN (CDAR TEMPLATE) WITH L = ARGL
		 AS N = (FIRST X) AND TEM = (SECOND X)
		 COLLECT
		   (COND ((AND (LISTP TEM) (EQ (CAR TEM) 'REPEAT))
			  (LET ((REPEAT (MAPFORMS-REPEAT-CHECK TEMPLATE ARGL
							       (CDR TEM) (CDDAR TEMPLATE))))
			    (SETQ TEM (CDR X))	;((REPEAT t t...))
			    (LDIFF L (SETQ L (NTHCDR (* REPEAT (LIST-LENGTH (CDAR TEM)))
						     L)))))
			 (L (POP L))
			 (T (FORM-NOT-UNDERSTOOD *MAPFORMS-TEMPLATE-FORM*
					"Wrong length list: matching ~S to template ~S"
					ARGL TEMPLATE)
			    NIL))
		   INTO FORMS
		COLLECT (LIST N TEM (LAST FORMS)) INTO QUEUE
		FINALLY
	    ;; Process the forms and templates in evaluation order
	    (LOOP FOR (N TEM FORM-LOC) IN (SORT QUEUE #'< :KEY #'CAR) DO
	      (Progn n)
	      (RPLACA FORM-LOC (MAPFORMS-TEMPLATE-1 (CAR FORM-LOC) TEM)))
	    ;; Store the resulting forms back into ARGL
	    (LOOP FOR (N TEM) IN (CDAR TEMPLATE) AND FORM IN FORMS DO
	      (progn n)
	      (COND ((AND (LISTP TEM) (EQ (CAR TEM) 'REPEAT))
		     (DOLIST (FORM FORM)
		       (MAPFORMS-RPLACA ORIGINAL-ARGL CURRENT-ARGL ARGL FORM)
		       (SETQ TAIL ARGL ARGL (CDR ARGL))))
		    (T (MAPFORMS-RPLACA ORIGINAL-ARGL CURRENT-ARGL ARGL FORM)
		       (SETQ TAIL ARGL ARGL (CDR ARGL))))))
	   (SETQ TEMPLATE (CDR TEMPLATE)))

	  ;; Not a leaf.  Destructure into the car and cdr of the trees.
	  (T (MAPFORMS-RPLACA ORIGINAL-ARGL CURRENT-ARGL
			      ARGL (MAPFORMS-TEMPLATE-1 (CAR ARGL) (CAR TEMPLATE)))
	     (SETQ TAIL ARGL ARGL (CDR ARGL))
	     (SETQ TEMPLATE (CDR TEMPLATE))))
    FINALLY (RETURN (COND ((NULL TAIL) ARGL)
			  (T (MAPFORMS-RPLACD ORIGINAL-ARGL CURRENT-ARGL TAIL ARGL)
			     CURRENT-ARGL)))))

;;; This must agree both with what can appear in the car of a form
;;; and with what the FUNCTION special form does.

(DEFUN MAPFORMS-TEMPLATE-CALL (FN)
  (TYPECASE FN
    (SYMBOL
      (MAPFORMS-CALL FN 'QUOTE 'CALL))
    (LIST					;Lambda-expression, -macro, or function-spec
      (COND ((MEMBER (CAR FN) '(LAMBDA SUBST #+CLOE-DEVELOPER ZL:::CL:LAMBDA #+genera lisp:lambda))
	     (LET ((ARGS-AND-BODY (CDR (CASE (CAR FN)
					 ((LAMBDA #+CLOE-DEVELOPER ZL:::CL:LAMBDA #+genera lisp:lambda) FN)
					 (OTHERWISE (CDR FN))))))
	       (MAPFORMS-LAMBDA FN FN ARGS-AND-BODY 'EVAL)))
	    ((eql (car fn) 'setf)
	     (mapforms-call fn 'quote 'call))
	    (T (FORM-NOT-UNDERSTOOD FN "~S is an invalid function." FN))))
    (COMPILED-FUNCTION	;Functional constants
     (MAPFORMS-CALL FN 'QUOTE 'CALL))
    (OTHERWISE
      (FORM-NOT-UNDERSTOOD FN "~S is an invalid function." FN))))

;;; Call the template processor inside a binding of *MAPFORMS-BLOCK-ALIST*
;;; with a new pair on the front.
(DEFUN MAPFORMS-BLOCK (NAME BODY TEMPLATE)
  (WITH-STACK-LIST* (PAIR NAME *MAPFORMS-TEMPLATE-USAGE*)
    (WITH-STACK-LIST* (*MAPFORMS-BLOCK-ALIST* PAIR *MAPFORMS-BLOCK-ALIST*)
      (LET ((*MAPFORMS-BLOCK-NAMES* (AND (NOT (EQ *MAPFORMS-BOUND-VARIABLES* 'NO-ENV))
					 (CONS NAME *MAPFORMS-BLOCK-NAMES*))))
	(MAPFORMS-TEMPLATE-1 BODY TEMPLATE)))))

;;; Decide how much of the form a REPEAT should match (return number of repetitions)
(DEFUN MAPFORMS-REPEAT-CHECK (TEMPLATE ARGL SUBTEMPLATE &OPTIONAL MORE-TEMPLATE)
  ;; Some error checking because these templates are so hairy
  (DOLIST (HAIR '(DECLARE BLOCK ANONYMOUS-BLOCK LOOP COND ARBITRARY))
    (AND (MEMBER HAIR (CDR TEMPLATE))
	 (ERROR "Malformed template for ~S: ~S can't figure out how ~@
		 much of ~S to match because there is a ~S to its right."
		(CAR *MAPFORMS-TEMPLATE-FORM*) (CAAR TEMPLATE) ARGL HAIR)))
  (DOLIST (HAIR '(REPEAT ORDER))
    (AND (CAREFUL-ASSOC HAIR (CDR TEMPLATE))
	 (ERROR "Malformed template for ~S: ~S can't figure out how ~@
		 much of ~S to match because there is a ~S to its right."
		(CAR *MAPFORMS-TEMPLATE-FORM*) (CAAR TEMPLATE) ARGL HAIR)))
  (DOLIST (HAIR '(DECLARE BLOCK ANONYMOUS-BLOCK LOOP COND ARBITRARY))
    (AND (MEMBER HAIR SUBTEMPLATE)
	 (ERROR "Malformed template for ~S: ~S can't figure out how ~@
		 much of ~S to match because there is a ~S in the repeated part."
		(CAR *MAPFORMS-TEMPLATE-FORM*) (CAAR TEMPLATE) ARGL HAIR)))
  (DOLIST (HAIR '(REPEAT ORDER))
    (AND (CAREFUL-ASSOC HAIR SUBTEMPLATE)
	 (ERROR "Malformed template for ~S: ~S can't figure out how ~@
		 much of ~S to match because there is a ~S in the repeated part."
		(CAR *MAPFORMS-TEMPLATE-FORM*) (CAAR TEMPLATE) ARGL HAIR)))
  ;; Decide number of repetitions
  (LET ((TLEN (LIST-LENGTH SUBTEMPLATE))	;Number of repeated items
	(LEN (- (LIST-LENGTH ARGL)		;Number of matching args
		(LIST-LENGTH (CDR TEMPLATE)) 
		(LIST-LENGTH MORE-TEMPLATE))))
    (OR (ZEROP (MOD LEN TLEN))
	(FORM-NOT-UNDERSTOOD *MAPFORMS-TEMPLATE-FORM*
			     "Wrong length list: matching ~S to template ~S leaves ~D extra"
			     ARGL TEMPLATE (MOD LEN TLEN)))
    (FLOOR LEN TLEN)))

;I can't use any Common Lisp functions for this, because L may be a "dotted"
;list rather than a true list.
(DEFUN CAREFUL-ASSOC (X L)
  (DO ((L L (CDR L)))
      ((ATOM L) NIL)
    (AND (LISTP (CAR L))
	 (EQL (CAAR L) X)
	 (RETURN (CAR L)))))

;;; Error reporting

;All errors are signalled by calling this function, which signals
;the condition of the same name.  Normally goes to the debugger,
;but the caller of MAPFORMS may establish a handler.
;This function might work differently in other Lisp implementations.

;;This will work in straight Common Lisp
(DEFUN FORM-NOT-UNDERSTOOD (FORM FORMAT-STRING &REST FORMAT-ARGS)
  (ERROR "~:[MAPFORMS~;COPYFORMS~] was unable to understand the form ~S.~%~1{~}"
	  *COPYFORMS-FLAG* FORM FORMAT-STRING FORMAT-ARGS))


;;; The non-Symbolics version
(DEFMACRO DEFPROPFUN ((SYMBOL PROPERTY-NAME) (&REST ARGS) &BODY BODY)
  `(SETF (GET ',SYMBOL ',PROPERTY-NAME)
	 #'(LAMBDA ,(COPY-LIST ARGS)
		    ,@BODY)))

(DEFPROPFUN (MACROLET MAPFORMS) (ORIGINAL-FORM FORM USAGE)
  (LET ((ENVIRONMENT *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*))
    (declare (ignore environment))
    (WITH-NEW-INTERPRETER-ENVIRONMENT
      (LOOP FOR MACRO IN (CADR FORM)
	    DO (PUSH `(special ,MACRO) (ENV-FUNCTIONS *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*)))
      (eval `(macrolet ,(cadr form)
	       (macrolet ((.scootch-it. (&environment e)
			    (let ((*host-specific-environment* e))
			      `',(MAPFORMS-LIST ',ORIGINAL-FORM ',FORM ',(CDDR FORM) 'EFFECT ',USAGE))))
		 (.scootch-it.)))))))

(Defpropfun (SYMBOL-MACROLET MAPFORMS) (ORIGINAL-FORM FORM USAGE)
  (LET ((*MAPFORMS-BOUND-VARIABLES* *MAPFORMS-BOUND-VARIABLES*)
	(LOCAL-DECLARATIONS LOCAL-DECLARATIONS)
	(BODY (CDDR FORM)))
    (MULTIPLE-VALUE-BIND (DECLARATIONS SPECIALS his-BODY DOCUMENTATION DEBUGGING-INFO VAR-DCLS)
	(PARSE-BODY-DECLARATIONS BODY)
      (declare (ignore DECLARATIONS SPECIALS his-BODY DOCUMENTATION DEBUGGING-INFO))
      (WITH-NEW-INTERPRETER-ENVIRONMENT
	(MULTIPLE-VALUE-SETQ (FORM BODY) (MAPFORMS-DECLARE ORIGINAL-FORM FORM BODY))
	(DOLIST (BINDING (SECOND FORM))
	  (LET* ((VAR (POP BINDING))
		 (EXPANSION (POP BINDING)))
	    (LET ((DCL (ASSOC VAR VAR-DCLS)))
	      (WHEN DCL
		(LET ((TYPE (ASSOC 'TYPE (REST DCL))))
		  (WHEN TYPE
		    (SETF EXPANSION `(THE ,(CDR TYPE) ,EXPANSION))))))
	    (PUSH `(,VAR ,EXPANSION T)
		  (ENV-VARIABLES *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*))
	    (UNLESS (EQ *MAPFORMS-BOUND-VARIABLES* 'NO-ENV)
	      (PUSH VAR *MAPFORMS-BOUND-VARIABLES*))))
	(eval `(symbol-macrolet ,(cadr form)
		 (macrolet ((.scootch-it. (&environment e)
 			      (let ((*host-specific-environment* e))
				`',(MAPFORMS-LIST ',ORIGINAL-FORM ',FORM ',BODY 'EFFECT ',USAGE))))
		   (.scootch-it.))))))))

(DEFPROPFUN (FLET MAPFORMS) (ORIGINAL-FORM FORM USAGE)
  (MAPFORMS-FLET-LABELS ORIGINAL-FORM FORM USAGE nil t))

(DEFPROPFUN (LABELS MAPFORMS) (ORIGINAL-FORM FORM USAGE)
  (MAPFORMS-FLET-LABELS ORIGINAL-FORM FORM USAGE t t))

(DEFPROPFUN (GENERIC-FLET MAPFORMS) (ORIGINAL-FORM FORM USAGE)
  (MAPFORMS-FLET-LABELS ORIGINAL-FORM FORM USAGE nil nil))

(DEFPROPFUN (GENERIC-LABELS MAPFORMS) (ORIGINAL-FORM FORM USAGE)
  (MAPFORMS-FLET-LABELS ORIGINAL-FORM FORM USAGE t nil))

(DEFUN MAPFORMS-FLET-LABELS (ORIGINAL-FORM FORM USAGE parallel? standard-syntax?)
  (WITH-NEW-INTERPRETER-ENVIRONMENT
      (FLET ((WALK-DEFINITIONS ()
              (LOOP WITH FUNCL = (CADR FORM)
		    WITH CURRENT-FUNCL = FUNCL
		    FOR DEFS ON FUNCL
		    AS (LAMBDA) = DEFS
		    DO (WHEN *MAPFORMS-ITERATION-HOOK*
			     (FUNCALL *MAPFORMS-ITERATION-HOOK* 'internal-function-start 
				      lambda))
		    (MAPFORMS-RPLACA
		     FUNCL CURRENT-FUNCL DEFS
		     (cond
		      (standard-syntax?
		       (MAPFORMS-LAMBDA LAMBDA LAMBDA (CDR LAMBDA) 'EVAL))
		      (t
		       (LOOP WITH ORIGINAL-LAMBDA = LAMBDA
			     FOR OPTIONS ON (CDDR LAMBDA)
			     AS (OPTION) = OPTIONS
			     WHEN (AND (CONSP OPTION)
				       (EQ (FIRST OPTION) :METHOD))
			     DO (MAPFORMS-RPLACA
				 ORIGINAL-LAMBDA LAMBDA OPTIONS
				 (MAPFORMS-LAMBDA OPTION OPTION (CDR OPTION) `EVAL))
			     FINALLY (RETURN LAMBDA)))))
		     (WHEN *MAPFORMS-ITERATION-HOOK*
			   (FUNCALL *MAPFORMS-ITERATION-HOOK* 'internal-function-end
				    lambda))
		    FINALLY (MAPFORMS-RPLACA ORIGINAL-FORM FORM (CDR FORM) CURRENT-FUNCL)))
	     (ADD-DEFINITIONS-TO-ENVIRONMENT ()
               (LOOP FOR (FUNCTION) IN (CADR FORM)
		     DO (PUSH (LIST FUNCTION
				    #'(LAMBDA (&REST IGNORE)
					      (declare (ignore ignore))
					      (ERROR "Can't call lexical function ~S at compile time."
						     FUNCTION))) ;Get it?
			      (ENV-FUNCTIONS *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*))
		     (WHEN *MAPFORMS-ITERATION-HOOK*
			       (FUNCALL *MAPFORMS-ITERATION-HOOK* 'internal-function-definition
					function))
		     COLLECT FUNCTION)))
	    (WHEN *MAPFORMS-ITERATION-HOOK*
		  (FUNCALL *MAPFORMS-ITERATION-HOOK* 'start-flet-labels (car original-form)))
	    (cond
	     ((not parallel?)
	      (WALK-DEFINITIONS)
	      (ADD-DEFINITIONS-TO-ENVIRONMENT))
	     (t
	      (ADD-DEFINITIONS-TO-ENVIRONMENT)
	      (WALK-DEFINITIONS))))
    (WHEN *MAPFORMS-ITERATION-HOOK*
	  (FUNCALL *MAPFORMS-ITERATION-HOOK* 'body-start original-form))
    (LET ((BODY (CDDR FORM)))
	 (cond
	  (standard-syntax?
	   (MULTIPLE-VALUE-SETQ (FORM BODY)
				     (MAPFORMS-DECLARE ORIGINAL-FORM FORM BODY))))
	 (MAPFORMS-LIST ORIGINAL-FORM FORM BODY 'EFFECT USAGE))
    (WHEN *MAPFORMS-ITERATION-HOOK*
	  (FUNCALL *MAPFORMS-ITERATION-HOOK* 'body-end original-form))
    ))

(DEFPROPFUN (COMPILER-LET MAPFORMS) (IGNORE FORM USAGE)
  (declare (ignore ignore))
  (LET ((NEW-BODY (COMPILER-LET-INTERNAL (CADR FORM) (CDDR FORM) #'COPYFORMS-1 USAGE)))
    ;; If the body was altered, build a new whole form to contain it, else return original
    (IF (NULL (CDDDR FORM))
	(IF (EQ NEW-BODY (CADDR FORM))
	    FORM
	    `(COMPILER-LET ,(CADR FORM) ,NEW-BODY))
	;; Take back apart the progn 'compile built by compiler-let-internal
	(COND ((EQ (CDDR NEW-BODY) (CDDR FORM))
	       FORM)
	      ((AND (LISTP NEW-BODY)
		    (EQ (CAR NEW-BODY) 'PROGN)
		    (EQUAL (CADR NEW-BODY) ''COMPILE))
	       `(COMPILER-LET ,(CADR FORM) . ,(CDDR NEW-BODY)))
	      (T `(COMPILER-LET ,(CADR FORM) ,NEW-BODY))))))

(DEFVAR COMPILER-LET-VARIABLES NIL)
(DEFVAR COMPILER-LET-VALUES NIL)

(DEFUN COMPILER-LET-INTERNAL (BINDLIST BODY PROCESSING-FUNCTION &REST ADDITIONAL-ARGS)
  (LET ((VARS (MAPCAR #'(LAMBDA (X) (IF (ATOM X) X (CAR X))) BINDLIST))
	(VALS (MAPCAR #'(LAMBDA (X) (IF (ATOM X) NIL (EVAL (CADR X)))) BINDLIST)))
    (PROGV VARS VALS
      (LET ((COMPILER-LET-VALUES
	      (NCONC VALS (LOOP FOR OLD-VAR IN COMPILER-LET-VARIABLES
				AND OLD-VAL IN COMPILER-LET-VALUES
				UNLESS (member OLD-VAR VARS) COLLECT OLD-VAL)))
	    (COMPILER-LET-VARIABLES
	      (NCONC VARS (LOOP FOR OLD-VAR IN COMPILER-LET-VARIABLES
				UNLESS (member OLD-VAR VARS) COLLECT OLD-VAR))))
	(apply PROCESSING-FUNCTION
	       (IF (CDR BODY)
		   `(PROGN 'COMPILE . ,BODY)
		   (CAR BODY))
	       ADDITIONAL-ARGS)))))

(DEFPROPFUN (RETURN-FROM MAPFORMS) (ORIGINAL-FORM FORM USAGE)
  (MAPFORMS-RPLACA ORIGINAL-FORM FORM (CDR FORM)
		   (MAPFORMS-CALL (CADR FORM) 'RETURN-FROM 'RETURN-FROM))
  (SETQ USAGE (OR (CDR (ASSOC (CADR FORM) *MAPFORMS-BLOCK-ALIST*)) 'EVAL))
  (MAPFORMS-LIST ORIGINAL-FORM FORM (CDDR FORM) USAGE USAGE))

;Procedural because of MAPFORMS-LAMBDA
(DEFPROPFUN (MACRO MAPFORMS) (ORIGINAL-FORM FORM USAGE)
  (MAPFORMS-LAMBDA ORIGINAL-FORM FORM (CDDR FORM) USAGE))

(defpropfun (lambda mapforms) (original-form form usage)
  (MAPFORMS-LAMBDA ORIGINAL-FORM FORM (CDR FORM) USAGE))

;The template is ((REPEAT (ORDER (2 SET) (1 EVAL)))), which is too hard to
;implement nonprocedurally because of the ORDER inside the REPEAT
(DEFPROPFUN (SETQ MAPFORMS) (ORIGINAL-FORM FORM USAGE)
  (DECLARE (IGNORE USAGE))
  (LOOP WITH TAIL = (CDR FORM) WHILE TAIL
	AS NEW-VAL = (COPYFORMS-1 (CADR TAIL) 'EVAL)
	AS NEW-VAR = (LET ((*MAPFORMS-LOCATOR-START* TAIL)
			   (*MAPFORMS-LOCATOR-END* (CDDR TAIL)))
		       (MAPFORMS-CALL (CAR TAIL) 'SET 'SET))
	DO (MAPFORMS-RPLACA ORIGINAL-FORM FORM TAIL NEW-VAR)
	   (SETQ TAIL (CDR TAIL))
	   (MAPFORMS-RPLACA ORIGINAL-FORM FORM TAIL NEW-VAL)
	   (SETQ TAIL (CDR TAIL)))
  FORM)

(DEFPROPFUN (SETQ MAPFORMS) (ORIGINAL-FORM FORM USAGE)
  ;; If there are any symbol macros, then the SETQ has to be rearranged.
  (cond
    ((LOOP FOR TAIL ON (CDR FORM) BY #'CDDR
	   THEREIS (SYMBOL-MACRO-P (CAR TAIL) *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*))
     (LET* ((CHANGEDP *COPYFORMS-EXPAND-ALL-MACROS*)
	    (NEW-FORMS
	      (LOOP FOR TAIL ON (CDR FORM) BY #'CDDR
		    AS (VAR VAL . MORE) = TAIL
		    COLLECT
		      (IF (NOT (SYMBOL-MACRO-P VAR *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*))
			  (LET ((NEW-VAL (COPYFORMS-1 VAL 'EVAL))
				(NEW-VAR (LET ((*MAPFORMS-LOCATOR-START* TAIL)
					       (*MAPFORMS-LOCATOR-END* MORE))
					   (MAPFORMS-CALL VAR 'SET 'SET))))
			    (WHEN (OR (Not (EQ NEW-VAL VAL))
				      (Not (EQ NEW-VAR VAR)))
			      (SETQ CHANGEDP T))
			    `(SETQ ,NEW-VAR ,NEW-VAL))
                          (let ((expansion-of-var
                                 (MAPFORMS-MACROEXPAND var *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*)))
			    (LET* ((SETF-FORM `(SETF ,expansion-of-VAR ,VAL))
				   (NEW-FORM (COPYFORMS-1 SETF-FORM (IF MORE 'EFFECT USAGE))))
			      (WHEN (not (eq SETF-FORM NEW-FORM))
			        (SETQ CHANGEDP T)
			        (WHEN (AND (NOT *COPYFORMS-EXPAND-ALL-MACROS*)
					   (CONSP (LAST NEW-FORM))
					   (= (LENGTH NEW-FORM) 3)
					   (EQ (FIRST NEW-FORM) (FIRST SETF-FORM))
					   (EQ (SECOND NEW-FORM) (SECOND SETF-FORM)))
				  ;; Only the VAL part changed, not the VAR, so unexpand VAR
				  (SETQ NEW-FORM `(SETQ ,VAR ,(THIRD NEW-FORM)))))
			      NEW-FORM))))))
       (COND ((NOT CHANGEDP) FORM)
	     ((NULL (REST NEW-FORMS)) (FIRST NEW-FORMS))
	     (T (CONS 'PROGN NEW-FORMS)))))
    (t ;; No symbol macros
     (LOOP WITH TAIL = (CDR FORM) WHILE TAIL
	   AS NEW-VAL = (COPYFORMS-1 (CADR TAIL) 'EVAL)
	   AS NEW-VAR = (LET ((*MAPFORMS-LOCATOR-START* TAIL)
			      (*MAPFORMS-LOCATOR-END* (CDDR TAIL)))
			  (MAPFORMS-CALL (CAR TAIL) 'SET 'SET))
	   DO (MAPFORMS-RPLACA ORIGINAL-FORM FORM TAIL NEW-VAR)
	      (SETQ TAIL (CDR TAIL))
	      (MAPFORMS-RPLACA ORIGINAL-FORM FORM TAIL NEW-VAL)
	      (SETQ TAIL (CDR TAIL)))
     FORM)))

;;; This is required in Genera because even Ansi Loop expands into this
;;; in some cases.
#+genera
(defpropfun (sys:VARIABLE-LOCATION MAPFORMS) (ORIGINAL-FORM FORM IGNORE)
  (declare (ignore ignore))
  (LET ((VAR (SECOND FORM)))
    (COND ((NOT (SYMBOL-MACRO-P VAR *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*))
	   ;; Obviously this comment is wrong!
	   ;;
	   ;; This would count as a SET because the variable could
	   ;; potentially be set indirectly through the locative
	   ;; produced, however we already assume that arbitrary
	   ;; side-effects always affect local variables.  So count it
	   ;; as a SYMEVAL: that we don't assume a side-effect just
	   ;; from computing the location; the side-effect is deferred
	   ;; until somebody actually does something unpredictable
	   ;; with that location.  This matters!
	   (MAPFORMS-RPLACA ORIGINAL-FORM FORM (CDR FORM) (MAPFORMS-CALL VAR 'SET 'SET))
	   FORM)
	  (T
	   (LET* ((LOCF-FORM `(scl:LOCF ,VAR))
		  (NEW-FORM (COPYFORMS-1 LOCF-FORM 'EVAL)))
	     (IF (OR *COPYFORMS-EXPAND-ALL-MACROS*
		     (scl:NEQ NEW-FORM LOCF-FORM))
		 NEW-FORM
		 FORM))))))





;;;; Support function stolen from Genera

(defun parse-body-declarations (body
				&optional
				  (lambda-list nil lambda-list-p)
				&aux (specials nil) (unspecials nil) (declarations nil)
				  (documentation nil) (debugging-info nil)
				  (var-dcls nil) (fun-dcls nil))
  (declare (ignore lambda-list))
  #-sbcl(declare (values declarations specials body documentation debugging-info var-dcls
			 unspecials fun-dcls))
  (labels ((add-var-dcl (kind value &rest vars)
                        (dolist (var vars)
                          (cond ((symbolp var)
                                 (let ((entry (assoc var var-dcls)))
                                   (unless entry
                                     (push (setq entry (list var)) var-dcls))
                                   (let ((already (assoc kind (cdr entry))))
                                     (if already
                                       (warn nil "duplicate ~s declarations for variable ~s." kind var)
                                       (push (cons kind value) (cdr entry))))))
                                (t
                                 (warn "~s is not a valid variable name, a ~s declaration is being ignored."
                                       var kind)))))
           (add-fun-dcl (kind value &rest funs)
                        (dolist (fun funs)
                          (cond ((symbolp fun)
                                 (let ((entry (assoc fun fun-dcls)))
                                   (unless entry
                                     (push (setq entry (list fun)) fun-dcls))
                                   (let ((already (assoc kind (cdr entry))))
                                     (if already
                                       (warn nil "duplicate ~s declarations for function ~s." kind fun)
                                       (push (cons kind value) (cdr entry))))))
                                (t
                                 (warn "~s is not a valid function name, a ~s declaration is being ignored."
                                       fun kind)))))
           (add-var-or-fun-dcl (kind value &rest names)
                               (dolist (name names)
                                 (if (and (listp name)
                                          (= (length name) 2)
                                          (eql (first name) 'function))
                                   (add-fun-dcl kind value (second name))
                                   (add-var-dcl kind value name)))))	     
    (loop until (null body)
          as form = (car body)
          do ;; nuked out code which expanded macros looking for decls
          (cond ((and lambda-list-p (stringp form) (cdr body))
                 (unless documentation		;only remember the first documentation string
                   (setq documentation form)))
                ((and (listp form)
                      (eq (car form) 'declare))
                 (dolist (declaration (cdr form))
                   (cond ((eq (car declaration) 'special)
                          (setq specials (append (cdr declaration) specials))
                          (apply #'add-var-dcl 'special t (cdr declaration)))
                         ((eq (car declaration) 'unspecial)
                          (setq unspecials (append (cdr declaration) unspecials))
                          (apply #'add-var-dcl 'special nil (cdr declaration)))
                         ((eq (car declaration) 'ignore)
                          (apply #'add-var-or-fun-dcl 'ignore t (cdr declaration)))
                         ((eq (car declaration) 'ignorable)
                          (apply #'add-var-or-fun-dcl 'ignore 'ignorable (cdr declaration)))
                         ((eq (car declaration) 'type)
                          (apply #'add-var-dcl 'type (second declaration) (cddr declaration)))
                         ((type-name-p (car declaration))
                          (apply #'add-var-dcl 'type (first declaration) (cdr declaration)))
                         ((eq (car declaration) 'dynamic-extent)
                          (apply #'add-var-or-fun-dcl 'dynamic-extent t (cdr declaration)))
                         ((eq (car declaration) 'inline)
                          (apply #'add-fun-dcl 'inline 'inline
                                 (cdr declaration)))
                         ((eq (car declaration) 'notinline)
                          (apply #'add-fun-dcl 'inline 'notinline
                                 (cdr declaration)))
                         ((eq (car declaration) 'ftype)
                          (apply #'add-fun-dcl 'ftype (second declaration)
                                 (cddr declaration)))
                         ((eq (car declaration) 'function)
                          (add-fun-dcl 'ftype `(function ,@(cddr declaration))
                                       (second declaration)))
                         (t (push declaration declarations)))))
                (t (return)))
	 (pop body)))
  ;;Note: isn't this whole form dead-code?
  ;; arglist is never consumed after the setf forms
  ;; I'm not sure how this got here but the original genera code doesn't
  ;; have anything like this.  Instead inside the when it has a bunch of stuff about
  ;; turning dynamic-extent declarations into downward funarg declarations
  ;; (when lambda-list-p
  ;;   (let ((arglist (assoc 'arglist debugging-info)))
  ;;     (if arglist
  ;;       (setf arglist (cdr arglist))
  ;;       (setf arglist lambda-list))))
  (values (nreverse declarations) specials body documentation debugging-info var-dcls
          unspecials fun-dcls))

(defun type-name-p (type-name &optional env)
  (or (get-properties (symbol-plist type-name)
		      '(typep deftype type-expand defstruct-description named-structure-invoke))
      (find-class type-name nil env)))



(defun used-variables (form &key already-known from-set)
  (let ((vars already-known))
    (mapforms
      #'(lambda (form kind usage state)
	  ;; (format t "~&Form ~a Kind ~s Usage ~s Bound ~a" Form kind usage jlt::*MAPFORMS-BOUND-VARIABLES*)
	  (declare (ignore state))
	  (when (and (eql kind 'symeval) (eql usage 'eval))
	    (Unless (member form *MAPFORMS-BOUND-VARIABLES*)
	      (when (or (eql from-set :any) (member form from-set))
		(pushnew form vars))))
	  (values form nil))
      form
      :bound-variables already-known)
    vars))

(defun used-functions (form)
  (let ((vars nil))
    (mapforms
      #'(lambda (form kind usage state)
	  (when (and (eql kind 'fsymeval) (eql usage 'function))
	    (Unless (assoc form (env-functions *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*))
	      (pushnew form vars)))
	  (values state nil))
      form
      :bound-variables nil)
    vars))

(DEFUN mapforms-trace (FORM)
  (MAPFORMS #'(LAMBDA (FORM KIND USAGE IGNORE)
		(declare (ignore ignore))
		(FORMAT T "~&~S for ~S kind ~S" FORM USAGE Kind))
	    FORM))

(DEFUN PRINT-SUBFORMS (FORM)
  (MAPFORMS #'(LAMBDA (FORM KIND USAGE IGNORE)
		(declare (ignore ignore))
		(UNLESS (MEMBER KIND *MAPFORMS-NON-FORM-KINDS*)
		  (FORMAT T "~&~S for ~S" FORM USAGE)))
	    FORM))
