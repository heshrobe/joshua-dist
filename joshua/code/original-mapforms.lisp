;;; -*- Mode:LISP; Package:Language-Tools; Syntax:Common-Lisp -*-
;;;>
;;;> *****************************************************************************************
;;;> ** (c) Copyright 1998-1982 Symbolics, Inc.  All rights reserved.
;;;> ** Portions of font library Copyright (c) 1984 Bitstream, Inc.  All Rights Reserved.
;;;>
;;;>    The software, data, and information contained herein are proprietary to,
;;;> and comprise valuable trade secrets of, Symbolics, Inc., which intends 
;;;> to keep such software, data, and information confidential and to preserve them
;;;> as trade secrets.  They are given in confidence by Symbolics pursuant 
;;;> to a written license agreement, and may be used, copied, transmitted, and stored
;;;> only in accordance with the terms of such license.
;;;> 
;;;> Symbolics, Symbolics 3600, Symbolics 3675, Symbolics 3630, Symbolics 3640,
;;;> Symbolics 3645, Symbolics 3650, Symbolics 3653, Symbolics 3620, Symbolics 3610,
;;;> Zetalisp, Open Genera, Virtual Lisp Machine, VLM, Wheels, Dynamic Windows,
;;;> SmartStore, Semanticue, Frame-Up, Firewall, Document Examiner,
;;;> Delivery Document Examiner, "Your Next Step in Computing", Ivory, MacIvory,
;;;> MacIvory model 1, MacIvory model 2, MacIvory model 3, XL400, XL1200, XL1201,
;;;> Symbolics UX400S, Symbolics UX1200S, NXP1000, Symbolics C, Symbolics Pascal,
;;;> Symbolics Prolog, Symbolics Fortran, CLOE, CLOE Application Generator,
;;;> CLOE Developer, CLOE Runtime, Common Lisp Developer, Symbolics Concordia,
;;;> Joshua, Statice, and Minima are trademarks of Symbolics, Inc.
;;;> 
;;;> Symbolics 3670, Symbolics Common Lisp, Symbolics-Lisp, and Genera are registered
;;;> trademarks of Symbolics, Inc.
;;;>
;;;> GOVERNMENT PURPOSE RIGHTS LEGEND
;;;> 
;;;>      Contract No.: various
;;;>      Contractor Name: Symbolics, Inc.
;;;>      Contractor Address: c/o Ropes & Gray
;;;> 			 One International Place
;;;> 			 Boston, Massachusetts 02110-2624
;;;>      Expiration Date: 2/27/2018
;;;>      
;;;> The Government's rights to use, modify, reproduce, release, perform, display or
;;;> disclose this software are restricted by paragraph (b)(2) of the "Rights in
;;;> Noncommercial Computer Software and Noncommercial Computer Software Documentation"
;;;> contained in the above identified contracts.  No restrictions apply after the
;;;> expiration date shown above.  Any reproduction of the software or portions thereof
;;;> marked with this legend must also reproduce the markings.  Questions regarding
;;;> the Government's rights may be referred to the AS&T Contracts Office of the
;;;> National Reconnaissance Office, Chantilly, Virginia 20151-1715.
;;;> 
;;;>      Symbolics, Inc.
;;;>      c/o Ropes & Gray
;;;>      One International Place
;;;>      Boston, Massachusetts 02110-2624
;;;>      781-937-7655
;;;>
;;;> *****************************************************************************************
;;;>
;;; Written May 1982 by David A. Moon for use by the Common Lisp community
;;; Revised April 1983

;;; Tools for source code analysis: code-walker

;--- Common Lisp version conversion issues:
;--- new DECLARE not hacked yet
;--- use Common Lisp condition system to signal errors, when it has one
;--- BLOCK has to be processed in a very funny way?  See (RETURN MAPFORMS)
;--- Certain symbols, e.g. SYMEVAL, aren't CL function names any more
;--- Uses extended DEFUN syntax for defining functions as properties
;--- Depends on having LOOP of course

;;; Interface definitions

;; All symbols that are part of the interface are exported from the LT package

(EXPORT '(MAPFORMS COPYFORMS MAPFORMS-1 COPYFORMS-1			;Functions
	  CALL BODY TEST EFFECT SMASH PROP ARG-TEMPLATE REPEAT EXPR	;Template things
	  PARALLEL-LET ANONYMOUS-BLOCK ORDER ARBITRARY
          LETF PARALLEL-LETF
	  BLOCK QUOTE SYMEVAL SET LET DECLARE GO RETURN-FROM		;Template things that
	  EVAL FUNCTION PROG RETURN COND LOOP				; are already global
	  FORM-NOT-UNDERSTOOD						;Condition
	  *MAPFORMS-BOUND-VARIABLES* *MAPFORMS-BLOCK-NAMES*		;Variables
	  *MAPFORMS-GO-TAGS* *MAPFORMS-NON-FORM-KINDS*
	  *MAPFORMS-LEVEL* *MAPFORMS-LOCATOR-START* *MAPFORMS-LOCATOR-END*))

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
;;	SYMEVAL - a variable reference
;;	a list - a function combination of any sort (normal, special, or lambda)
;;	  for special forms, the list is non-empty and its cdr is the arg-template 
;;		to be matched against the cdr of the form (see next page)
;;	  for regular function combinations, the list is NIL
;;        for macros of all sorts, the list is NIL.  This includes regular macros,
;;        symbol-macros, lambda-macros, and lambda-macro calls.
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
;;	MACROEXPAND - the form is being macroexpanded during expansion of another macro
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
;; For IF (with multi-else feature):
;;	(DECLARE (ARG-TEMPLATE COND TEST RETURN . BODY))

(DEFPROP ARG-TEMPLATE T SI:DEBUG-INFO)

;;; The following variables are likely to be used by the user.

;;; This variable contains a list of the variables bound around the current form
;;; or the symbol NO-ENV if we were not asked to keep track of that
(DEFVAR *MAPFORMS-BOUND-VARIABLES*)

;;; If bound variables maintained, this list of block names extant is maintained
(DEFVAR *MAPFORMS-BLOCK-NAMES*)

;;; If bound variables maintained, this list of go tags extant is maintained
(DEFVAR *MAPFORMS-GO-TAGS*)

;;; Recursion level of mapping, allows user program to understand nesting in parallel.
(DEFVAR *MAPFORMS-LEVEL*)

;;; These two mark the limits of the part of the form actually being processed, for
;;; example the single LET or SETQ clause
(DEFVAR *MAPFORMS-LOCATOR-START*)
(DEFVAR *MAPFORMS-LOCATOR-END*)

;;; The KIND symbols that correspond to non-form Lisp code fragments
(DEFPARAMETER *MAPFORMS-NON-FORM-KINDS* '(SET LET DECLARE GO RETURN-FROM ARBITRARY))

;;; The user may call back into MAPFORMS-1 or COPYFORMS-1 when bypassing
;;; normal processing.  Don't forget to return a second value of T.

;;; Mapforms/Copyforms top level

;;; The following variables are bound at entry to MAPFORMS or COPYFORMS

;;; This variable contains an a-list of the block names defined around the current form.
;;; The cdr of each entry is the USAGE of that block.
;;; This list is a stack list, and hence must not be squirelled away
(DEFVAR *MAPFORMS-BLOCK-ALIST*)

(DEFVAR *MAPFORMS-FUNCTION*)			;Function being mapped
(DEFVAR *MAPFORMS-STATE*)			;Holds state returned by user function
(DEFVAR *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*);An interpreter environment
						;for tracking MACROLET's and FLET's 
(DEFVAR *COPYFORMS-FLAG*)			;T if copying/transforming subforms
(DEFVAR *MAPFORMS-APPLY-FUNCTION*)		;Post-processing function
(DEFVAR *MAPFORMS-ITERATION-HOOK*)		;:ITERATION-HOOK function
(DEFVAR *MAPFORMS-EXPAND-SUBSTS*)		;:EXPAND-SUBSTS flag
(DEFVAR *MAPFORMS-PARALLEL-BINDS*)		;Side-effect from MAPFORMS-BIND
(DEFVAR *COPYFORMS-EXPAND-ALL-MACROS* NIL)	;T to copy macro expansions
		;(needs a top-level value, but it doesn't matter what it is!)
(DEFVAR *COPYFORMS-BACK-TRANSLATE-MACROS* NIL)	;:BACK-TRANSLATE-MACROS flag

;;; Supporting macros

(DEFMACRO WITH-NEW-INTERPRETER-ENVIRONMENT (&BODY BODY)
  `(LET ((ENV *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*))
     (ZL:SI:WITH-INTERPRETER-ENVIRONMENT
       (*MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*
	 *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*
	 (POP ENV) (POP ENV) (POP ENV) (POP ENV) (POP ENV))
       ,@BODY)))

;;; Record the current MAPFORMS incarnation in the declarations part of the environment
(DEFMACRO WITH-MAPFORMS-ENVIRONMENT-TAG ((ENV) &BODY BODY)
  (LET ((TEMP (GENSYM))
	(NEW-DCLS (GENSYM)))
    `(LET ((,TEMP ,ENV))
       (STACK-LET ((,NEW-DCLS `(((MAPFORMS-ENVIRONMENT-TAG ,(%STACK-FRAME-POINTER)))
				,@(SI:ENV-DECLARATIONS ,TEMP))))
	 (ZL:SI:WITH-INTERPRETER-ENVIRONMENT
	   (,ENV ,ENV (POP ,TEMP) (POP ,TEMP) (POP ,TEMP) (POP ,TEMP) ,NEW-DCLS)
	   ,@BODY)))))

;;; This macro allows substituting for some element of a list being
;;; mapped down, without smashing anything yet with minimal consing.
;;;	ORIGINAL-LIST - the original, uncopied list
;;;	CURRENT-LIST - that or a copy of it (must be a variable)
;;;	TAIL - must be a tail of CURRENT-LIST, its car is to be changed
;;; If TAIL is a variable, it is setq'ed to the corresponding tail of
;;; the copy if a copy is made.
(DEFMACRO MAPFORMS-RPLACA (ORIGINAL-LIST CURRENT-LIST TAIL NEWCAR &ENVIRONMENT ENV)
  (OR (SYMBOLP CURRENT-LIST) (ERROR "~S not a variable" CURRENT-LIST))
  (ONCE-ONLY (NEWCAR &ENVIRONMENT ENV)
    `(COND ((NEQ (CAR ,TAIL) ,NEWCAR)
	    (RPLACA (IF (EQ ,ORIGINAL-LIST ,CURRENT-LIST)
			(MULTIPLE-VALUE-SETQ (,(AND (SYMBOLP TAIL) TAIL) ,CURRENT-LIST)
			  (MAPFORMS-RPLACA-COPY ,TAIL ,CURRENT-LIST))
			,TAIL)
		    ,NEWCAR)))))

(DEFUN MAPFORMS-RPLACA-COPY (TAIL LIST)
  (LOOP WITH NEW-LIST = (COPY-LIST LIST)
	FOR NEW-TAIL ON NEW-LIST AND OLD-TAIL ON LIST
	WHEN (EQ OLD-TAIL TAIL)
	  RETURN (VALUES NEW-TAIL NEW-LIST)
	FINALLY (ERROR "~S is not a tail of ~S" TAIL LIST)))

;;; Same for cdr.
;;; Never stores back into TAIL (since of course it doesn't copy list beyond it)
;;; We assume that a given tail will only be rplacd'ed once
(DEFMACRO MAPFORMS-RPLACD (ORIGINAL-LIST CURRENT-LIST TAIL NEWCDR &ENVIRONMENT ENV)
  (OR (SYMBOLP CURRENT-LIST) (ERROR "~S not a variable" CURRENT-LIST))
  (ONCE-ONLY (NEWCDR &ENVIRONMENT ENV)
    `(COND ((NEQ (CDR ,TAIL) ,NEWCDR)
	    (RPLACD (IF (EQ ,ORIGINAL-LIST ,CURRENT-LIST)
			(MULTIPLE-VALUE-SETQ (NIL ,CURRENT-LIST)
			  (MAPFORMS-RPLACD-COPY ,TAIL ,CURRENT-LIST))
			,TAIL)
		    ,NEWCDR)))))

;Copy list through tail, but not into (cdr tail)
(DEFUN MAPFORMS-RPLACD-COPY (TAIL LIST)
  (LET* ((ORIGINAL-TAIL TAIL)
	 (ORIGINAL-LIST LIST)
	 (NEW-HEAD (NCONS (CAR LIST)))
	 (NEW-TAIL NEW-HEAD))
    (LOOP DO
      (WHEN (ATOM LIST)
	(ERROR "~S is not a tail of ~S" ORIGINAL-TAIL ORIGINAL-LIST))
      (RPLACD NEW-TAIL (CDR LIST))
      (WHEN (EQ LIST TAIL)
	(RETURN (VALUES NEW-TAIL NEW-HEAD)))
      (SETQ LIST (CDR LIST))
      (RPLACD NEW-TAIL (SETQ NEW-TAIL (NCONS (CAR LIST)))))))

(DEFUN MAPFORMS (*MAPFORMS-FUNCTION* FORM
		 &KEY (INITIAL-STATE NIL)
		      (BOUND-VARIABLES 'NO-ENV)
		      (USAGE 'EVAL)
		      (APPLY-FUNCTION NIL)
		      (ITERATION-HOOK NIL)
		      (EXPAND-SUBSTS NIL)
		      (ENVIRONMENT NIL)
		 &AUX (*COPYFORMS-FLAG* NIL)
		      (*MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT* ENVIRONMENT)
		      (*MAPFORMS-BOUND-VARIABLES* BOUND-VARIABLES)
		      (*MAPFORMS-ITERATION-HOOK* ITERATION-HOOK)
		      (*MAPFORMS-EXPAND-SUBSTS* EXPAND-SUBSTS)
		      (*MAPFORMS-BLOCK-NAMES* NIL)
		      (*MAPFORMS-GO-TAGS* NIL)
		      (*MAPFORMS-BLOCK-ALIST* NIL)
		      (*MAPFORMS-APPLY-FUNCTION* APPLY-FUNCTION)
		      (*MAPFORMS-STATE* INITIAL-STATE)
		      (*MAPFORMS-LEVEL* 0))
  (DECLARE (SYS:DOWNWARD-FUNARG *MAPFORMS-FUNCTION*))
  "Call a function on a form and all of its subforms.
  The function is called on arguments subform, kind, usage, and state,
and its first returned value is the new state.  If the second value is
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
		       (BACK-TRANSLATE-MACROS NIL)
		       (ENVIRONMENT NIL)
		  &AUX (*COPYFORMS-FLAG* T)
		       (*MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT* ENVIRONMENT)
		       (*MAPFORMS-APPLY-FUNCTION* APPLY-FUNCTION)
		       (*MAPFORMS-ITERATION-HOOK* ITERATION-HOOK)
		       (*MAPFORMS-EXPAND-SUBSTS* EXPAND-SUBSTS)
		       (*MAPFORMS-BOUND-VARIABLES* BOUND-VARIABLES)
		       (*MAPFORMS-BLOCK-NAMES* NIL)
		       (*MAPFORMS-GO-TAGS* NIL)
		       (*MAPFORMS-BLOCK-ALIST* NIL)
		       (*MAPFORMS-LEVEL* 0)
		       (*COPYFORMS-EXPAND-ALL-MACROS* EXPAND-ALL-MACROS)
		       (*COPYFORMS-BACK-TRANSLATE-MACROS* BACK-TRANSLATE-MACROS))
  (DECLARE (SYS:DOWNWARD-FUNARG *MAPFORMS-FUNCTION*))
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
  If the :BACK-TRANSLATE-MACROS option is specified, then if something in
the expansion of a macro was modified while copying, and that thing
can be located in the macro invocation, then the result is the modified
macro invocation instead of the modified expansion.
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
;;;	MACRO - a macro combination (for which there is no arg-template)
;;;	LAMBDA-MACRO - a lambda-macro combination
;;;	SYMBOL-MACRO - a symbol macro
;;;	some other atom - a special processing function, obtained from the
;;;	  property of the function name whose indicator is our second argument
(DEFUN CLASSIFY-FORM (FORM PROPERTY &AUX FCN TEM)
  (DECLARE (VALUES KIND SPECIAL))
  (COND ((ATOM FORM)
	 (COND ((CONSTANTP FORM *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*)
		'QUOTE)
	       ((SYMBOL-MACRO-P FORM *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*)
		(VALUES NIL 'SYMBOL-MACRO))
	       (T
		'SYMEVAL)))
	((EQ (SETQ FCN (CAR FORM)) 'QUOTE)
	 'QUOTE)
	((SYMBOLP FCN)
	 (LET ((DEF (OR (SI:LOCAL-FUNCTION-DEFINITION
			  FCN *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*)
			(SI:DECLARED-DEFINITION FCN))))
	   (COND ((NULL DEF) NIL)
		 ((AND (LISTP DEF)
		       (EQ (FIRST DEF) 'FLAVOR:DEFUN-IN-FLAVOR))
		  NIL)
		 ((SETQ TEM (ASSOC 'ARG-TEMPLATE (DEBUGGING-INFO DEF)))
		  (VALUES TEM (AND PROPERTY (GET FCN PROPERTY))))
		 ((MACRO-FUNCTION DEF)
		  (VALUES NIL 'MACRO))
		 ((FUNCTIONP DEF NIL)
		  NIL)
		 ((ARRAYP DEF)
		  NIL)
		 ((AND PROPERTY (SETQ TEM (GET FCN PROPERTY)))
		  (VALUES NIL TEM))
		 ((SPECIAL-FORM-P DEF)
		  (FORM-NOT-UNDERSTOOD FORM "~S is a special form but lacks an arg-template"
				       FCN)
		  NIL)
		 (T NIL))))
	((SI:INTERPRETED-LAMBDA-P FCN)
	 (VALUES NIL 'LAMBDA))
	((LAMBDA-MACRO-CALL-P FCN)
	 (VALUES NIL 'LAMBDA-MACRO))
	(T (FORM-NOT-UNDERSTOOD FORM "~S not understood in the function position of a form"
				FCN)
	   NIL)))

(DEFUN VARIABLEP (X &OPTIONAL ENV)
  (AND (SYMBOLP X)
       (NOT (MEMBER X LAMBDA-LIST-KEYWORDS))
       (NOT (CONSTANTP X ENV))			;better in Common Lisp
       (NOT (SYMBOL-MACRO-P X ENV))))

;GET-PROPERTIES is an absolutely miserable replacement for GETL
(DEFUN PROPERTYP (SYMBOL INDICATOR)
  (LOOP FOR (I V) ON (SYMBOL-PLIST SYMBOL) BY 'CDDR
	DO (IGNORE V)
	THEREIS (EQ I INDICATOR)))

;;; Main driving functions

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
		 (EQ SPECIAL 'LAMBDA-MACRO)
		 (EQ SPECIAL 'SYMBOL-MACRO))
	     (LET ((EXPANSION
		     (MAPFORMS-MACROEXPAND FORM *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*)))
	       (SETQ ORIGINAL-FORM EXPANSION)
	       (SETQ FORM EXPANSION)))
	    ((AND (EQ KIND NIL) (EQ SPECIAL NIL)
		  *MAPFORMS-EXPAND-SUBSTS*
		  (SI:ENV-DECLARATIONS-INLINE-P (FIRST FORM)
						*MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*)
		  (LET ((METHOD
			  (FIND-INLINE-FORM-METHOD (FIRST FORM)
						   *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*)))
		    (WHEN METHOD
		      (LET ((EXPANSION
			      (EXPAND-INLINE-FORM METHOD FORM
						  *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*)))
			(WHEN (NOT (EQ EXPANSION FORM))
			  (SETF ORIGINAL-FORM EXPANSION)
			  (SETF ORIGINAL-BEFORE-MACRO-EXPANSION EXPANSION)
			  (SETF FORM EXPANSION)
			  T))))))
	    ((EQ SPECIAL 'LAMBDA)		;Lambda-combination
	     ;; First process the arguments.
	     (SETQ FORM (MAPFORMS-LIST ORIGINAL-FORM FORM (CDR FORM) 'EVAL 'EVAL))
	     (LET* ((ORIGINAL-LAMBDA (CAR ORIGINAL-FORM))
		    (LAMBDA (CAR FORM))
		    (LAMBDA-LIST-AND-BODY (SI:INTERPRETED-LAMBDA-LAMBDA-LIST-AND-BODY LAMBDA)))
	       ;; Replace the lambda with its decoded version.
	       (MAPFORMS-RPLACA ORIGINAL-LAMBDA LAMBDA LAMBDA 'FUTURE-COMMON-LISP:LAMBDA)
	       (MAPFORMS-RPLACD ORIGINAL-LAMBDA LAMBDA LAMBDA LAMBDA-LIST-AND-BODY)
	       ;; Now process the bindings and then the body
	       (MAPFORMS-RPLACA ORIGINAL-FORM FORM FORM
				(MAPFORMS-LAMBDA ORIGINAL-LAMBDA LAMBDA LAMBDA-LIST-AND-BODY USAGE))
	       (SETQ DONE-FLAG T)))
	    (SPECIAL				;General escape
	     (SETQ FORM (FUNCALL SPECIAL ORIGINAL-FORM FORM USAGE))
	     (SETQ DONE-FLAG T))
	    ((NULL KIND)			;Ordinary function, do args
	     (SETQ FORM (MAPFORMS-LIST ORIGINAL-FORM FORM (CDR FORM) 'EVAL 'EVAL))
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
	(RETURN (COND (*COPYFORMS-EXPAND-ALL-MACROS* FORM)
		      ;; If macro expansion was uninteresting, undo it
		      ((EQ FORM ORIGINAL-FORM) ORIGINAL-BEFORE-MACRO-EXPANSION)
		      ;; If no macro expansion was involved, form was replaced wholesale
		      ((EQ ORIGINAL-FORM ORIGINAL-BEFORE-MACRO-EXPANSION) FORM)
		      ;; Try to map this change back into the unexpanded macro
		      ((AND *COPYFORMS-BACK-TRANSLATE-MACROS*
			    (BACK-TRANSLATE-MACRO FORM ORIGINAL-FORM
						  ORIGINAL-BEFORE-MACRO-EXPANSION)))
		      ;; That didn't succeed, so include the macro expansion in the result
		      (T FORM)))))))

;;; The user function may call back into this if doing a MAPFORMS
(DEFUN MAPFORMS-1 (FORM &OPTIONAL (USAGE 'EVAL))
  (COPYFORMS-1 FORM USAGE)
  *MAPFORMS-STATE*)

(DEFUN GET-MAPFORMS-ENVIRONMENT-TAG (ENV)
  (CADAR (FIND 'MAPFORMS-ENVIRONMENT-TAG (SI:ENV-DECLARATIONS ENV) :KEY #'CAAR)))

;;; All expansion of macros, lambda-macros, and symbol-macros by MAPFORMS goes
;;; through here.  This is so that macros that are expanded explicitly by other macro
;;; expander functions get seen by MAPFORMS.  For example, SETF macroexpands its
;;; -place- argument.  The immediate need for this is so that ANNOTATE-FORM can note
;;; them as free variables (symbol-macros) or functions (regular macros).
(DEFUN MAPFORMS-MACROEXPAND (FORM ENV)
  (LET* ((FIRST T)
	 (ORIGINAL-TAG (GET-MAPFORMS-ENVIRONMENT-TAG ENV))
	 (ORIGINAL-HOOK *MACROEXPAND-HOOK*)
	 (*MACROEXPAND-HOOK*
	   #'(LAMBDA (FCN FORM ENV)
	       (DECLARE (SYS:DOWNWARD-FUNCTION))
	       (BLOCK NIL
		 ;; First check that this macroexpansion is part of this MAPFORMS and not
		 ;; something unrelated, such as a macroexpansion in the body of a macro
		 ;; that has not been compiled, or a macroexpansion during a recursive
		 ;; call to MAPFORMS from the body of a macro.
		 (WHEN (EQ (GET-MAPFORMS-ENVIRONMENT-TAG ENV) ORIGINAL-TAG)
		   (IF FIRST
		       ;; The first macroexpansion has already been through MAPFORMS-CALL
		       (SETQ FIRST NIL)
		       ;; This must be a recursive macroexpansion, let the client know about it
		       (LET ((*MACROEXPAND-HOOK* ORIGINAL-HOOK))
			 (UNLESS (EQ FORM (SETQ FORM (MAPFORMS-CALL FORM NIL 'MACROEXPAND)))
			   ;; Client has substituted a different form, so abort expansion
			   (RETURN FORM)))))
		 ;; Continue with normal macroexpansion processing
		 (FUNCALL ORIGINAL-HOOK FCN FORM ENV)))))
    ;; Expand this macro, lambda-macro, or symbol-macro with the above hook installed
    (MACROEXPAND-1 FORM ENV)))

;;; Call the user function on this form, and return the new form
(DEFUN MAPFORMS-CALL (FORM KIND USAGE &OPTIONAL (FUNCTION *MAPFORMS-FUNCTION*) &AUX FLAG)
  (COND (*COPYFORMS-FLAG*
	 (FUNCALL FUNCTION FORM KIND USAGE))
	(T
	 (MULTIPLE-VALUE-SETQ (*MAPFORMS-STATE* FLAG)
	   (FUNCALL FUNCTION FORM KIND USAGE *MAPFORMS-STATE*))
	 (VALUES FORM FLAG))))

;;; For each difference between NEW and OLD, if we can find that part of OLD in
;;; BEFORE-MACRO-EXPANSION, replace it with the corresponding part of NEW.
;;; This assumes that list EQness can be used for form identity.
;;; I.e. a macro will not create lists out of whole cloth that are EQ to subforms,
;;; and source code does not have the same EQ list in two non-equivalent places.
(DEFUN BACK-TRANSLATE-MACRO (NEW OLD BEFORE-MACRO-EXPANSION &AUX (SUBSTITUTIONS NIL))
  (LABELS ((REPAIR (NEW OLD)
	     (DECLARE (VALUES FOUND))
	     ;; Damnable displacing macros
	     (WHEN (AND (CONSP NEW) (EQ (FIRST NEW) 'SI:DISPLACED))
	       (SETQ NEW (SECOND NEW)))
	     (WHEN (AND (CONSP OLD) (EQ (FIRST OLD) 'SI:DISPLACED))
	       (SETQ OLD (SECOND OLD)))
	     (COND ((EQ NEW OLD) T)			;nothing to do
		   ((LET ((ELEM (ASSOC OLD SUBSTITUTIONS)))
		      (EQ (CDR ELEM) NEW))		;this happens when something appears
							;once in the macro invocation but
		    T)					;twice in the macro expansion
		   ((OR (ATOM OLD) (ATOM NEW)) NIL)	;only trust EQ of conses
		   (T (LET ((FOUND NIL))
			(LABELS ((TRAVERSE (BEFORE)	;try the substitution in this tree
				   (DECLARE (VALUES AFTER))
				   (COND ((ATOM BEFORE) BEFORE)
					 ((EQ BEFORE OLD)
					  (PUSH (CONS OLD NEW) SUBSTITUTIONS)
					  (SETQ FOUND T)
					  NEW)
					 (T
					  ;; Damnable displacing macros
					  (WHEN (EQ (FIRST BEFORE) 'SI:DISPLACED)
					    (SETQ BEFORE (SECOND BEFORE)))
					  (LOOP WITH ORIGINAL = BEFORE
						WITH IN-SYNCH = NIL
						WITH OLD = OLD WITH NEW = NEW
						FOR TAIL ON BEFORE WHILE (CONSP TAIL) DO
					    ;; Traverse recursively and update BEFORE
					    (MAPFORMS-RPLACA ORIGINAL BEFORE TAIL
							     (TRAVERSE (CAR TAIL)))
					    ;; The list in BEFORE may only share a sublist
					    ;; with the list in OLD and NEW, as for example
					    ;; in SIGNAL-PROCEED-CASE.  If we find at least
					    ;; one similarity followed by the difference,
					    ;; that's reliable enough for government work.
					    ;; Try to get OLD and NEW in synch with TAIL.
					    (UNLESS IN-SYNCH
					      (LOOP FOR O = OLD THEN (CDR O)
						    FOR N = NEW THEN (CDR N)
						    UNTIL (OR (ATOM O) (ATOM N))
						    DO (WHEN (EQ (CAR O) (CAR TAIL))
							 (SETQ OLD O NEW N)
							 (RETURN (SETQ IN-SYNCH T)))
						    WHILE (EQUAL (CAR O) (CAR N))))
					    ;; If we're in synch, then we can compare subforms
					    (WHEN (AND IN-SYNCH (CONSP OLD) (CONSP NEW))
					      (COND ((EQ (CAR OLD) (CAR TAIL))
						     (UNLESS (EQ (CAR OLD) (CAR NEW))
						       (WHEN (EQUAL (CDR OLD) (CDR NEW))
							 (SETQ FOUND T))
						       (PUSH (CONS (CAR OLD) (CAR NEW))
							     SUBSTITUTIONS)
						       (MAPFORMS-RPLACA ORIGINAL BEFORE TAIL
									(CAR NEW)))
						     (SETQ OLD (CDR OLD) NEW (CDR NEW)))
						    (T (SETQ IN-SYNCH NIL)))))
					  BEFORE))))
			  ;; Try to substitute for the entire difference
			  (SETQ BEFORE-MACRO-EXPANSION (TRAVERSE BEFORE-MACRO-EXPANSION))
			  (OR FOUND
			      ;; That failed, try to substitute by parts
			      (LOOP DOING
				;; If any difference can't be located in BEFORE,
				;; the whole thing fails
				(UNLESS (REPAIR (POP NEW) (POP OLD)) (RETURN NIL))
				(WHEN (EQ NEW OLD) (RETURN T))
				;; If the two lists are of different lengths, fail
				(WHEN (OR (ATOM NEW) (ATOM OLD)) (RETURN NIL))))))))))
    ;; If we're able to make the substitution, return the modified third argument
    ;; If we fail to make the substitution, return NIL
    (AND (REPAIR NEW OLD)
	 BEFORE-MACRO-EXPANSION)))

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
  (DECLARE (VALUES CURRENT-LIST TAIL))
  (LOOP DOING
    (COND ((NULL TAIL) (RETURN))
	  ((AND (CDR TAIL) (STRINGP (CAR TAIL))))	;Doc string
	  ((AND (LISTP (CAR TAIL))
		(LISTP (SETQ EXP (MACROEXPAND (CAR TAIL)
					      *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*
					      T)))
		(EQ (CAR EXP) 'DECLARE))
	   (LOOP WITH ORIGINAL = (CAR TAIL)	;Map over each declaration
		 WITH CURRENT = EXP
		 FOR DCLS ON (CDR EXP)
		 AS DCL = (CAR DCLS)
		 DO (MAPFORMS-RPLACA ORIGINAL CURRENT
				     DCLS (SETQ DCL (MAPFORMS-CALL DCL 'DECLARE 'DECLARE)))
		    (WHEN (LISTP DCL)
		      (CASE (CAR DCL)
			((SYS:INSTANCE-VARIABLES)
			 (DOLIST (VAR (CDDR DCL))
			   (PUSH (LIST VAR NIL)
				 (SI:ENV-VARIABLES *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*))
			   (UNLESS (EQ *MAPFORMS-BOUND-VARIABLES* 'NO-ENV)
			     (PUSH VAR *MAPFORMS-BOUND-VARIABLES*))))
			((FLAVOR:LOCAL-FUNCTIONS)
			 (SETF (SI:ENV-FUNCTIONS *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*)
			       (APPEND (CDR DCL) (SI:ENV-FUNCTIONS
						   *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*))))
			((SPECIAL ZL:UNSPECIAL)
			 (WHEN SI:*SPECIAL-DECLARATIONS-ARE-PERVASIVE*
			   (PUSH DCL DECLARATIONS)))
			((INLINE NOTINLINE)
			 (DOLIST (FUN (CDR DCL))
			   (LET ((ENTRY (ASSOC FUN (SI:ENV-FUNCTIONS *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*))))
			     (PUSH (IF ENTRY
				       `(,FUN ,(SECOND ENTRY) INLINE ,(CAR DCL) ,@(CDDR ENTRY))
				       `(,FUN NIL INLINE ,(CAR DCL)))
				   (SI:ENV-FUNCTIONS *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*)))))
			((IGNORE TYPE COMPILER:VARIABLE-INLINABLE
				 FUTURE-COMMON-LISP:DYNAMIC-EXTENT FTYPE FUNCTION
				 SYS:ARRAY-REGISTER SYS:ARRAY-REGISTER-1D SYS:LOGIC-VARIABLE))
			(OTHERWISE
			  (UNLESS (CLI::TYPE-NAME-P (CAR DCL) *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*)
			    (PUSH DCL DECLARATIONS)))))
			 
		 FINALLY (MAPFORMS-RPLACA ORIGINAL-LIST CURRENT-LIST TAIL CURRENT)))
	  (T (RETURN)))				;Start of real body
    (POP TAIL))
  (SETF LOCAL-DECLARATIONS (APPEND DECLARATIONS LOCAL-DECLARATIONS))
  (PUSH DECLARATIONS (ZL:SI:ENV-DECLARATIONS *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*))
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
      (FUNCALL *MAPFORMS-ITERATION-HOOK* 'LAMBDA))
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
				    (MAPFORMS-RPLACA
				      ORIGINAL-LAMBDA-LIST LAMBDA-LIST LL
				      (IF (OR ALLOW-INITIALIZATION
					      (ATOM L))
					  (MAPFORMS-BIND-SINGLE
					    LL NIL ALLOW-SUPPLIED-P LAMBDA)
					  (MAPFORMS-RPLACA
					    L L L (MAPFORMS-BIND-SINGLE L NIL NIL LAMBDA))))))
				 FINALLY (RETURN LAMBDA-LIST)))
	  (SETF BODY (CORRESPONDING-TAIL BODY BODY-LAMBDA LAMBDA)))
	(MAPFORMS-CALL (MAPFORMS-LIST ORIGINAL-LAMBDA LAMBDA BODY 'EFFECT USAGE)
		       'QUOTE 'CALL)))
    (WHEN *MAPFORMS-ITERATION-HOOK*
      (FUNCALL *MAPFORMS-ITERATION-HOOK* NIL))))

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
	      (OR (NEQ ALLOW-SUPPLIED-P '&KEY)
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
	 (LET ((BOUND-VARIABLES-P (NEQ *MAPFORMS-BOUND-VARIABLES* 'NO-ENV)))
	   (WHEN VAR1
	     (WHEN BOUND-VARIABLES-P (PUSH VAR1 *MAPFORMS-BOUND-VARIABLES*))
	     (PUSH (LIST VAR1 NIL)
		   (SI:ENV-VARIABLES *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*)))
	   (WHEN VAR2
	     (WHEN BOUND-VARIABLES-P (PUSH VAR2 *MAPFORMS-BOUND-VARIABLES*))
	     (PUSH (LIST VAR2 NIL)
		   (SI:ENV-VARIABLES *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*))))))
  BIND)

(DEFUN MAPFORMS-LETF-BIND (BIND PARALLEL-BINDING-P CONTAINING-FORM &AUX (ORIGINAL-BIND BIND))
  (COND ((CDDR BIND)
	 (FORM-NOT-UNDERSTOOD CONTAINING-FORM "Invalid LETF clause: ~S" BIND))
	((OR (ATOM BIND) (ATOM (CAR BIND)))
	 (MAPFORMS-BIND BIND PARALLEL-BINDING-P NIL CONTAINING-FORM))
	(T
	 (MAPFORMS-RPLACA
	   ORIGINAL-BIND
	   BIND
	   BIND
	   (LET ((*COPYFORMS-EXPAND-ALL-MACROS* NIL))	;Don't lose DEFLOCFs
	     (COPYFORMS-1 (CAR BIND) 'EVAL)))
	 (WHEN (CDR BIND)
	   (MAPFORMS-RPLACA ORIGINAL-BIND BIND (CDR BIND) (COPYFORMS-1 (CADR BIND) 'EVAL)))))
  BIND)

;;; Template-directed driving function

(DEFVAR *MAPFORMS-TEMPLATE-USAGE*)	;USAGE of the whole form being processed
(DEFVAR *MAPFORMS-TEMPLATE-FORM*)	;Original of the whole form

(DEFUN MAPFORMS-TEMPLATE (ORIGINAL-FORM *MAPFORMS-TEMPLATE-FORM*
					TEMPLATE *MAPFORMS-TEMPLATE-USAGE*)
  (MAPFORMS-RPLACD ORIGINAL-FORM *MAPFORMS-TEMPLATE-FORM*
		   *MAPFORMS-TEMPLATE-FORM*
		   (WITH-NEW-INTERPRETER-ENVIRONMENT
		     (LET ((*MAPFORMS-BOUND-VARIABLES* *MAPFORMS-BOUND-VARIABLES*)
			   (LOCAL-DECLARATIONS LOCAL-DECLARATIONS))
		       (MAPFORMS-TEMPLATE-1 (CDR *MAPFORMS-TEMPLATE-FORM*) TEMPLATE))))
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
	  ((MEMBER TEMPLATE '(QUOTE GO RETURN-FROM SET SYMEVAL))
	   (SETQ ARGL (MAPFORMS-CALL ARGL TEMPLATE TEMPLATE))
	   (LOOP-FINISH))
	  ;; This is also a single subform, but the KIND is QUOTE.
	  #+Symbolics
	  ((EQ TEMPLATE 'FLAVOR:GENERIC)
	   (SETQ ARGL (MAPFORMS-CALL ARGL 'QUOTE TEMPLATE))
	   (LOOP-FINISH))
	  ((EQ TEMPLATE 'LET)
	   (SETQ ARGL (MAPFORMS-BIND ARGL NIL NIL *MAPFORMS-TEMPLATE-FORM*))
	   (LOOP-FINISH))
	  ((EQ TEMPLATE 'LETF)
	   (SETQ ARGL (MAPFORMS-LETF-BIND ARGL NIL *MAPFORMS-TEMPLATE-FORM*))
	   (LOOP-FINISH))
	  ((EQ TEMPLATE 'PARALLEL-LET)
	   (LET ((*MAPFORMS-PARALLEL-BINDS* NIL))
	     (LOOP WHILE ARGL DO
	       (MAPFORMS-RPLACA ORIGINAL-ARGL CURRENT-ARGL ARGL
				(MAPFORMS-BIND-SINGLE ARGL T NIL *MAPFORMS-TEMPLATE-FORM*))
	       (SETQ TAIL ARGL ARGL (CDR ARGL)))
	     (DOLIST (VAR *MAPFORMS-PARALLEL-BINDS*)
	       (PUSH (LIST VAR NIL)
		     (SI:ENV-VARIABLES *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*)))
	     (UNLESS (EQ *MAPFORMS-BOUND-VARIABLES* 'NO-ENV)
	       (SETQ *MAPFORMS-BOUND-VARIABLES*
		     (NCONC *MAPFORMS-PARALLEL-BINDS* *MAPFORMS-BOUND-VARIABLES*))))
	   (LOOP-FINISH))
	  ((EQ TEMPLATE 'PARALLEL-LETF)
	   (LET ((*MAPFORMS-PARALLEL-BINDS* NIL))
	     (LOOP WHILE ARGL DO
		   (MAPFORMS-RPLACA
		     ORIGINAL-ARGL CURRENT-ARGL ARGL
		     (MAPFORMS-LETF-BIND (CAR ARGL) T *MAPFORMS-TEMPLATE-FORM*))
		   (SETQ TAIL ARGL ARGL (CDR ARGL)))
	     (SETQ *MAPFORMS-BOUND-VARIABLES*
		   (NCONC *MAPFORMS-PARALLEL-BINDS* *MAPFORMS-BOUND-VARIABLES*)))
	   (LOOP-FINISH))
	  ((EQ TEMPLATE 'CALL)
	   (SETQ ARGL (MAPFORMS-TEMPLATE-CALL ARGL))
	   (LOOP-FINISH))
	  ((ATOM TEMPLATE)
	   (CASE TEMPLATE
	     ((BODY)
	      (RETURN (MAPFORMS-LIST ORIGINAL-ARGL CURRENT-ARGL
				     ARGL 'EFFECT *MAPFORMS-TEMPLATE-USAGE*)))
	     ((PROG)
	      (LET ((*MAPFORMS-GO-TAGS*
		      (AND (NEQ *MAPFORMS-BOUND-VARIABLES* 'NO-ENV)
			   (NCONC (LOOP FOR STMT IN ARGL
					WHEN (ATOM STMT) COLLECT STMT)
				  *MAPFORMS-GO-TAGS*)))
		    (ITERATION NIL))
		(LOOP FOR TAIL ON ARGL AS STMT = (CAR TAIL) DO
		  (IF (ATOM STMT)
		      ;; First tag is start of possibly iterated code
		      ;; We aren't smart enough to worry about tags reached
		      ;; only by forward branches.
		      (UNLESS ITERATION
			(WHEN *MAPFORMS-ITERATION-HOOK*
			  (FUNCALL *MAPFORMS-ITERATION-HOOK* T))
			(SETQ ITERATION T))
		      ;; Lists are forms evaluated for effect
		      (MAPFORMS-RPLACA ORIGINAL-ARGL CURRENT-ARGL
				       TAIL (COPYFORMS-1 STMT 'EFFECT))))
		(AND ITERATION
		     *MAPFORMS-ITERATION-HOOK*
		     (FUNCALL *MAPFORMS-ITERATION-HOOK* NIL))
		(RETURN CURRENT-ARGL)))
	     ((RETURN)
	      (SETQ ARGL (COPYFORMS-1 ARGL *MAPFORMS-TEMPLATE-USAGE*))
	      (LOOP-FINISH))
	     ((EVAL TEST EFFECT SMASH PROP FUNCTION)
	      (SETQ ARGL (COPYFORMS-1 ARGL TEMPLATE))
	      (LOOP-FINISH))
	     (OTHERWISE
	      (ERROR "Malformed template: ~S trying to match ~S in a ~S-form"
		     TEMPLATE ARGL (CAR *MAPFORMS-TEMPLATE-FORM*)))))
	  ((EQ (CAR TEMPLATE) 'AND)
	   (DOLIST (TEMPLATE (CDR TEMPLATE))
	     (SETQ ARGL (MAPFORMS-TEMPLATE-1 ARGL TEMPLATE)))
	   (LOOP-FINISH))
	  ((EQ (CAR TEMPLATE) 'IF)
	   (SETQ ARGL (MAPFORMS-TEMPLATE-1 ARGL
			(IF (IF (ATOM (SECOND TEMPLATE)) (FUNCALL (SECOND TEMPLATE) ARGL)
				(LET ((EXPR ARGL))
				  (DECLARE (SPECIAL EXPR))
				  (EVAL (SECOND TEMPLATE))))
			    (THIRD TEMPLATE)
			    (FOURTH TEMPLATE))))
	   (LOOP-FINISH))
	  ((EQ (CAR TEMPLATE) 'MACRO)
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
	      (IGNORE N)
	      (RPLACA FORM-LOC (MAPFORMS-TEMPLATE-1 (CAR FORM-LOC) TEM)))
	    ;; Store the resulting forms back into ARGL
	    (LOOP FOR (N TEM) IN (CDAR TEMPLATE) AND FORM IN FORMS DO
	      (IGNORE N)
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
      (COND ((SI:INTERPRETED-LAMBDA-P FN)
	     (MAPFORMS-LAMBDA FN FN (SI:INTERPRETED-LAMBDA-LAMBDA-LIST-AND-BODY FN) 'EVAL))
	    ((VALIDATE-FUNCTION-SPEC FN)
	     (MAPFORMS-CALL FN 'QUOTE 'CALL))
	    ((LAMBDA-MACRO-CALL-P FN)
	     (MAPFORMS-TEMPLATE-CALL
	       (LAMBDA-MACRO-EXPAND FN *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*)))
	    (T (FORM-NOT-UNDERSTOOD FN "~S is an invalid function." FN))))
    ((OR COMPILED-FUNCTION DYNAMIC-CLOSURE LEXICAL-CLOSURE)	;Functional constants
     (MAPFORMS-CALL FN 'QUOTE 'CALL))
    (OTHERWISE
      (IF (VALIDATE-FUNCTION-SPEC FN)
	  (MAPFORMS-CALL FN 'QUOTE 'CALL)
	  (FORM-NOT-UNDERSTOOD FN "~S is an invalid function." FN)))))

;;; Call the template processor inside a binding of *MAPFORMS-BLOCK-ALIST*
;;; with a new pair on the front.
(DEFUN MAPFORMS-BLOCK (NAME BODY TEMPLATE)
  (WITH-STACK-LIST* (PAIR NAME *MAPFORMS-TEMPLATE-USAGE*)
    (WITH-STACK-LIST* (*MAPFORMS-BLOCK-ALIST* PAIR *MAPFORMS-BLOCK-ALIST*)
      (LET ((*MAPFORMS-BLOCK-NAMES* (AND (NEQ *MAPFORMS-BOUND-VARIABLES* 'NO-ENV)
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

#+LISPM (PROGN 'COMPILE		;Common Lisp doesn't have conditions yet

;Flavor definition put first to defeat signal compiler warning
(DEFFLAVOR FORM-NOT-UNDERSTOOD (FORM FORMAT-STRING FORMAT-ARGS COPYFORMS-FLAG)
	   (DBG:NO-ACTION-MIXIN ZL:ERROR)
  (:INITABLE-INSTANCE-VARIABLES FORM FORMAT-STRING FORMAT-ARGS)
  (:GETTABLE-INSTANCE-VARIABLES FORM))

;All errors are signalled by calling this function, which signals
;the condition of the same name.  Normally goes to the debugger,
;but the caller of MAPFORMS may establish a handler.
;This function might work differently in other Lisp implementations.
(DEFUN FORM-NOT-UNDERSTOOD (FORM FORMAT-STRING &REST FORMAT-ARGS)
  (SIGNAL 'FORM-NOT-UNDERSTOOD :FORM FORM
			       :FORMAT-STRING FORMAT-STRING
			       :FORMAT-ARGS (COPY-LIST FORMAT-ARGS)
			       :PROCEED-TYPES '(:NO-ACTION)))

(DEFPROP FORM-NOT-UNDERSTOOD T :ERROR-REPORTER)

(DEFMETHOD (MAKE-INSTANCE FORM-NOT-UNDERSTOOD) (&REST IGNORE)
  (SETQ COPYFORMS-FLAG *COPYFORMS-FLAG*))

(DEFMETHOD (:REPORT FORM-NOT-UNDERSTOOD) (STREAM)
  (FORMAT STREAM "~:[MAPFORMS~;COPYFORMS~] was unable to understand the form ~S.~%~1{~:}"
	  COPYFORMS-FLAG FORM FORMAT-STRING FORMAT-ARGS))

(COMPILE-FLAVOR-METHODS FORM-NOT-UNDERSTOOD)
);#+LISPM

#-LISPM				;This will work in straight Common Lisp
(DEFUN FORM-NOT-UNDERSTOOD (FORM FORMAT-STRING &REST FORMAT-ARGS)
  (ERROR "~:[MAPFORMS~;COPYFORMS~] was unable to understand the form ~S.~%~1{~}"
	  COPYFORMS-FLAG FORM FORMAT-STRING FORMAT-ARGS))

;;; Knowledge of special forms that has to be procedural

(DEFUN (MACROLET MAPFORMS) (ORIGINAL-FORM FORM USAGE)
  (WITH-NEW-INTERPRETER-ENVIRONMENT
    ;; I think it is improper to walk the expanders --BSG
    (LOOP FOR MACRO IN (CADR FORM)
	  DO (PUSH (SI:DIGEST-MACROLET-MACRO MACRO NIL)
		   (SI:ENV-FUNCTIONS *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*)))
    (MAPFORMS-LIST ORIGINAL-FORM FORM (CDDR FORM) 'EFFECT USAGE)))

(DEFUN (FUTURE-COMMON-LISP:MACROLET MAPFORMS) (ORIGINAL-FORM FORM USAGE)
  (LET ((ENVIRONMENT *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*))
    (WITH-NEW-INTERPRETER-ENVIRONMENT
      ;; I agree it is improper to walk the expanders --Cyphers
      (LOOP FOR MACRO IN (CADR FORM)
	    DO (PUSH (SI:DIGEST-MACROLET-MACRO MACRO ENVIRONMENT)
		     (SI:ENV-FUNCTIONS *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*)))
      (MAPFORMS-LIST ORIGINAL-FORM FORM (CDDR FORM) 'EFFECT USAGE))))

(DEFUN (FUTURE-COMMON-LISP:FLET MAPFORMS) (ORIGINAL-FORM FORM USAGE)
  (MAPFORMS-FLET-LABELS ORIGINAL-FORM FORM USAGE))

(DEFUN (FUTURE-COMMON-LISP:LABELS MAPFORMS) (ORIGINAL-FORM FORM USAGE)
  (MAPFORMS-FLET-LABELS ORIGINAL-FORM FORM USAGE))

(DEFUN (CLOS:GENERIC-FLET MAPFORMS) (ORIGINAL-FORM FORM USAGE)
  (MAPFORMS-FLET-LABELS ORIGINAL-FORM FORM USAGE))

(DEFUN (CLOS:GENERIC-LABELS MAPFORMS) (ORIGINAL-FORM FORM USAGE)
  (MAPFORMS-FLET-LABELS ORIGINAL-FORM FORM USAGE))

(DEFUN MAPFORMS-FLET-LABELS (ORIGINAL-FORM FORM USAGE)
  (WITH-NEW-INTERPRETER-ENVIRONMENT
    (FLET ((WALK-DEFINITIONS ()
	     (LOOP WITH FUNCL = (CADR FORM)
		   WITH CURRENT-FUNCL = FUNCL
		   FOR DEFS ON FUNCL
		   AS (LAMBDA) = DEFS
		   DO (MAPFORMS-RPLACA
			FUNCL CURRENT-FUNCL DEFS
			(ECASE (CAR ORIGINAL-FORM)
			  ((FUTURE-COMMON-LISP:FLET FUTURE-COMMON-LISP:LABELS)
			   (MAPFORMS-LAMBDA LAMBDA LAMBDA (CDR LAMBDA) 'EVAL))
			  ((CLOS:GENERIC-FLET CLOS:GENERIC-LABELS)
			    (LOOP WITH ORIGINAL-LAMBDA = LAMBDA
				  FOR OPTIONS ON (CDDR LAMBDA)
				  AS (OPTION) = OPTIONS
				  WHEN (AND (CONSP OPTION)
					    (EQ (FIRST OPTION) :METHOD))
				    DO (MAPFORMS-RPLACA
					 ORIGINAL-LAMBDA LAMBDA OPTIONS
					 (MAPFORMS-LAMBDA OPTION OPTION (CDR OPTION) `EVAL))
				  FINALLY
				    (RETURN LAMBDA)))))
		   FINALLY (MAPFORMS-RPLACA ORIGINAL-FORM FORM (CDR FORM) CURRENT-FUNCL)))
	   (ADD-DEFINITIONS-TO-ENVIRONMENT ()
	     (LOOP FOR (FUNCTION) IN (CADR FORM)
		   DO (PUSH (ZL:SI:FAKE-FUNCTION-BINDING FUNCTION)
			    (ZL:SI:ENV-FUNCTIONS *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*))
		   COLLECT FUNCTION)))
      (ECASE (CAR ORIGINAL-FORM)
	((FUTURE-COMMON-LISP:LABELS CLOS:GENERIC-LABELS)
	 (ADD-DEFINITIONS-TO-ENVIRONMENT)
	 (WALK-DEFINITIONS))
	((FUTURE-COMMON-LISP:FLET CLOS:GENERIC-FLET)
	 (WALK-DEFINITIONS)
	 (ADD-DEFINITIONS-TO-ENVIRONMENT))))
    (LET ((BODY (CDDR FORM)))
      (ECASE (CAR ORIGINAL-FORM)
	((FUTURE-COMMON-LISP:FLET FUTURE-COMMON-LISP:LABELS)
	 (MULTIPLE-VALUE-SETQ (FORM BODY)
	   (MAPFORMS-DECLARE ORIGINAL-FORM FORM BODY))))
      (MAPFORMS-LIST ORIGINAL-FORM FORM BODY 'EFFECT USAGE))))

;Must be procedural to get bindings wrapped around it
(DEFUN (COMPILER-LET MAPFORMS) (IGNORE FORM USAGE)
  (LET ((NEW-BODY (SYS:COMPILER-LET-INTERNAL (CADR FORM) (CDDR FORM) #'COPYFORMS-1 USAGE)))
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

(DEFUN (RETURN-FROM MAPFORMS) (ORIGINAL-FORM FORM USAGE)
  (MAPFORMS-RPLACA ORIGINAL-FORM FORM (CDR FORM)
		   (MAPFORMS-CALL (CADR FORM) 'RETURN-FROM 'RETURN-FROM))
  (SETQ USAGE (OR (CDR (ASSOC (CADR FORM) *MAPFORMS-BLOCK-ALIST*)) 'EVAL))
  (MAPFORMS-LIST ORIGINAL-FORM FORM (CDDR FORM) USAGE USAGE))

;Procedural because of MAPFORMS-LAMBDA
(DEFUN (MACRO MAPFORMS) (ORIGINAL-FORM FORM USAGE)
  (MAPFORMS-LAMBDA ORIGINAL-FORM FORM (CDDR FORM) USAGE))

;The template is ((REPEAT (ORDER (2 SET) (1 EVAL)))), which is too hard to
;implement nonprocedurally because of the ORDER inside the REPEAT
(DEFUN (SETQ MAPFORMS) (ORIGINAL-FORM FORM USAGE)
  ;; If there are any symbol macros, then the SETQ has to be rearranged.
  (WHEN (LOOP FOR TAIL ON (CDR FORM) BY 'CDDR
	      THEREIS (SYMBOL-MACRO-P (CAR TAIL) *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*))
    (LET* ((CHANGEDP *COPYFORMS-EXPAND-ALL-MACROS*)
	   (NEW-FORMS
	     (LOOP FOR TAIL ON (CDR FORM) BY 'CDDR
		   AS (VAR VAL . MORE) = TAIL
		   COLLECT
		     (IF (NOT (SYMBOL-MACRO-P VAR *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*))
			 (LET ((NEW-VAL (COPYFORMS-1 VAL 'EVAL))
			       (NEW-VAR (LET ((*MAPFORMS-LOCATOR-START* TAIL)
					      (*MAPFORMS-LOCATOR-END* MORE))
					  (MAPFORMS-CALL VAR 'SET 'SET))))
			   (WHEN (OR (NEQ NEW-VAL VAL)
				     (NEQ NEW-VAR VAR))
			     (SETQ CHANGEDP T))
			   `(SETQ ,NEW-VAR ,NEW-VAL))
			 (LET* ((SETF-FORM `(SETF ,VAR ,VAL))
				(NEW-FORM (COPYFORMS-1 SETF-FORM (IF MORE 'EFFECT USAGE))))
			   (WHEN (NEQ SETF-FORM NEW-FORM)
			     (SETQ CHANGEDP T)
			     (WHEN (AND (NOT *COPYFORMS-EXPAND-ALL-MACROS*)
					(CONSP (LAST NEW-FORM))
					(= (LENGTH NEW-FORM) 3)
					(EQ (FIRST NEW-FORM) (FIRST SETF-FORM))
					(EQ (SECOND NEW-FORM) (SECOND SETF-FORM)))
			       ;; Only the VAL part changed, not the VAR, so unexpand VAR
			       (SETQ NEW-FORM `(SETQ ,VAR ,(THIRD NEW-FORM)))))
			   NEW-FORM)))))
      (RETURN-FROM SETQ
	(COND ((NOT CHANGEDP) FORM)
	      ((NULL (REST NEW-FORMS)) (FIRST NEW-FORMS))
	      (T (CONS 'PROGN NEW-FORMS))))))
  ;; No symbol macros
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

;;; The ARG-TEMPLATE would be sufficient if it were not for symbol-macros
(DEFUN (ZL:MULTIPLE-VALUE MAPFORMS) (ORIGINAL-FORM FORM USAGE)
  ;; If there are any symbol macros, then their expansions have to be used
  (COND ((LOOP FOR VAR IN (SECOND FORM)
	       THEREIS (SYMBOL-MACRO-P VAR *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*))
	 (LET* ((TEMPS (LOOP REPEAT (LENGTH (SECOND FORM)) COLLECT (GENSYM)))
		(EXPANSION `(MULTIPLE-VALUE-BIND (,@TEMPS)
				,(THIRD FORM)
			      ,@(LOOP FOR VAR IN (SECOND FORM)
				      FOR TEMP IN TEMPS
				      COLLECT (IF (NULL VAR) TEMP
						  `(SETQ ,VAR ,TEMP)))
			      ,(FIRST TEMPS)))
		(NEW-FORM (COPYFORMS-1 EXPANSION USAGE)))
	   (COND (*COPYFORMS-EXPAND-ALL-MACROS*
		  ;; Keep the expanded symbol macros
		  NEW-FORM)
		 ((EQ NEW-FORM EXPANSION)
		  ;; Nothing changed, so unexpand
		  FORM)
		 ((AND (CONSP (LAST NEW-FORM))
		       (= (LENGTH NEW-FORM) (LENGTH EXPANSION))
		       (EQ (FIRST NEW-FORM) (FIRST EXPANSION))
		       (EQUAL (SECOND NEW-FORM) (SECOND EXPANSION))
		       (EQUAL (CDDDR NEW-FORM) (CDDDR EXPANSION)))
		  ;; Only the (THIRD FORM) part changed, not the VARs, so unexpand the rest
		  (MAPFORMS-RPLACA ORIGINAL-FORM FORM (CDDR FORM) (THIRD NEW-FORM))
		  FORM)
		 (T NEW-FORM))))
	(T
	 ;; No symbol macros
	 (MAPFORMS-RPLACA ORIGINAL-FORM FORM (CDDR FORM) (COPYFORMS-1 (THIRD FORM) 'EVAL))
	 (LET* ((ORIGINAL-VARS (SECOND FORM))
		(VARS ORIGINAL-VARS))
	   (LOOP FOR TAIL ON VARS DO
	     (WHEN (CAR TAIL)	;NIL means IGNORE
	       (MAPFORMS-RPLACA ORIGINAL-VARS VARS TAIL (MAPFORMS-CALL (CAR TAIL) 'SET 'SET))))
	   (MAPFORMS-RPLACA ORIGINAL-FORM FORM (CDR FORM) VARS))
	 FORM)))

;This should really just point to (:PROPERTY SETQ MAPFORMS) but I'm too lazy to define
;a new syntax for mapforms.

;The template is ((REPEAT (ORDER (2 SET) (1 EVAL)))), which is too hard to
;implement nonprocedurally because of the ORDER inside the REPEAT
(DEFUN (SI:ADVISE-SETQ MAPFORMS) (ORIGINAL-FORM FORM IGNORE)
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

(DEFUN (FUTURE-COMMON-LISP:SYMBOL-MACROLET MAPFORMS) (ORIGINAL-FORM FORM USAGE)
  (LET ((*MAPFORMS-BOUND-VARIABLES* *MAPFORMS-BOUND-VARIABLES*)
	(LOCAL-DECLARATIONS LOCAL-DECLARATIONS)
	(BODY (CDDR FORM)))
    (MULTIPLE-VALUE-BIND (IGNORE IGNORE IGNORE IGNORE IGNORE VAR-DCLS)
	(ZL:SI:PARSE-BODY-DECLARATIONS BODY *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*)
      (WITH-NEW-INTERPRETER-ENVIRONMENT
	(MULTIPLE-VALUE-SETQ (FORM BODY)
	  (MAPFORMS-DECLARE ORIGINAL-FORM FORM BODY))
	(DOLIST (BINDING (SECOND FORM))
	  (LET* ((VAR (POP BINDING))
		 (EXPANSION (POP BINDING)))
	    (SI:CHECK-SYMBOL-SETABILITY VAR 'FUTURE-COMMON-LISP:SYMBOL-MACROLET)
	    (LET ((DCL (ASSOC VAR VAR-DCLS)))
	      (WHEN DCL
		(LET ((TYPE (ASSOC 'TYPE (REST DCL))))
		  (WHEN TYPE
		    (SETF EXPANSION `(THE ,(CDR TYPE) ,EXPANSION))))))
	    (PUSH `(,VAR ,EXPANSION T)
		  (SI:ENV-VARIABLES *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*))
	    (UNLESS (EQ *MAPFORMS-BOUND-VARIABLES* 'NO-ENV)
	      (PUSH VAR *MAPFORMS-BOUND-VARIABLES*))))
	(MAPFORMS-LIST ORIGINAL-FORM FORM BODY 'EFFECT USAGE)))))

(DEFUN (VARIABLE-LOCATION MAPFORMS) (ORIGINAL-FORM FORM IGNORE)
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
	   (LET* ((LOCF-FORM `(LOCF ,VAR))
		  (NEW-FORM (COPYFORMS-1 LOCF-FORM 'EVAL)))
	     (IF (OR *COPYFORMS-EXPAND-ALL-MACROS*
		     (NEQ NEW-FORM LOCF-FORM))
		 NEW-FORM
		 FORM))))))
