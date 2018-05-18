;;; -*- Mode: LISP; Package: jlt; Syntax: Ansi-common-lisp  -*-
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

(in-package "JLT")

(DEFVAR *IGNORE* NIL)
;;; Interface definitions

(setf (get 'ARG-TEMPLATE 'DEBUG-INFO) t)

;;; For purposes of building a Common lisp version we build a hash table containing
;;; the templates for the special forms. This kludge is brought to you by lack of
;;; agreement inthe community. 
(defvar *ARG-TEMPLATE-TABLE*)

;;; Create a table for arg templates. This is based upon the LISPMs version.
;;; Although it is cumbersome, it is probably best to maintain compatibility with
;;; the LISPMs storage of the arg-template on the p-list of the symbol.
(defun create-arg-template-table ()
  (setq *ARG-TEMPLATE-TABLE* (make-hash-table))
  (mapcar #'(lambda (template)
	      (setf (gethash (first template) *ARG-TEMPLATE-TABLE*)
		    (second template)))
	  '((and (arg-template cond (repeat return)))
	    (block (arg-template block . body))
	    (catch (arg-template eval #|arbitrary|# . body))
	    (cond  (arg-template cond (repeat (test . body))))
	    #+genera
	    (compiler:invisible-references (ARG-TEMPLATE QUOTE . BODY))
	    (compiler-let (arg-template))
            #+MCL
            (CCL::DEBIND (ARG-TEMPLATE (ORDER (2 ((REPEAT LET)))
                                             (1 EVAL))
                                      DECLARE . BODY))
	    (declare (arg-template . quote))
	    (eval-when (arg-template quote (repeat return)))
	    (flet (arg-template))
	    #+genera
	    (lisp:function (arg-template call))
	    #-genera
	    (function (arg-template call))
	    (go (arg-template go))
	    (if (arg-template cond test return return))
	    #+genera
	    (lisp:if (arg-template cond test return . body))
	    #+cloe-developer
	    (zl:::cltl::if (arg-template cond test return . body))
	    (labels (arg-template))
	    (lambda (arg-template ))
	    (let (arg-template parallel-let declare . body))
	    (let* (arg-template ((repeat let)) declare . body))
	    (locally (declare . body))
	    (multiple-value-list (arg-template eval))
	    (multiple-value-bind (arg-template (order (2 ((repeat (if null quote let))))
						      (1 eval))
				  declare . body))
	    (multiple-value-setq (arg-template (order (2 ((repeat (if null quote set))))
						      (1 eval))))
	    (multiple-value-call (arg-template #|arbitrary|# (repeat eval)))
	    (multiple-value-prog1 (arg-template return (repeat effect)))
	    (or (arg-template cond (repeat return)))
	    (progn (arg-template . body))
	    (progv (arg-template eval eval #|arbitrary|# . body))
	    (quote (arg-template quote))
	    (return (arg-template . body))
	    (return-from  (arg-template return-from . body))
	    ; (setq (arg-template (repeat (order (2 set) (1 eval)))))
	    (setq (arg-template (repeat set eval)))  ; not exactly right but works
	    


	    (tagbody (arg-template loop . prog))
	    (throw (arg-template eval eval))
	    (unwind-protect (arg-template return (repeat effect)))
	    (with-stack-list (ARG-TEMPLATE ((ORDER (2 LET) (1 (REPEAT EVAL)))) . BODY))
	    (with-stack-list* (ARG-TEMPLATE ((ORDER (2 LET) (1 (REPEAT EVAL)))) . BODY))
	    )))

(create-arg-template-table)

(DEFVAR *MAPFORMS-TEMPLATE-USAGE*)	;USAGE of the whole form being processed
(DEFVAR *MAPFORMS-TEMPLATE-FORM*)	;Original of the whole form



;;; the following variables are likely to be used by the user.

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
(defvar *host-specific-environment*)		;An environment in form that this
						;implementation represents one.
(DEFVAR *COPYFORMS-FLAG*)			;T if copying/transforming subforms
(DEFVAR *MAPFORMS-APPLY-FUNCTION*)		;Post-processing function
(DEFVAR *MAPFORMS-ITERATION-HOOK*)		;:ITERATION-HOOK function
(DEFVAR *MAPFORMS-EXPAND-SUBSTS*)		;:EXPAND-SUBSTS flag
(DEFVAR *MAPFORMS-PARALLEL-BINDS*)		;Side-effect from MAPFORMS-BIND
(DEFVAR *COPYFORMS-EXPAND-ALL-MACROS* NIL)	;T to copy macro expansions
                                                ;(needs a top-level value, but it doesn't matter what it is!)

;LOCAL-DECLARATIONS is a list of local declarations.
;Each local declaration is a list starting with an atom which says
;what type of declaration it is.  The meaning of the rest of the
;list depends on the type of declaration.
;The compiler is interested only in SPECIAL and UNSPECIAL declarations,
;for which the rest of the list contains the symbols being declared,
;and MACRO declarations, which look like (DEF symbol MACRO LAMBDA args ..body...),
;and ARGLIST declarations, which specify arglists to go in the debugging info
;(to override the actual arglist of the function, for user information)
;which look like (ARGLIST FOO &OPTIONAL BAR ...), etc.

(DEFVAR LOCAL-DECLARATIONS NIL)



;;;; Environments

(defstruct (environment (:type list) (:conc-name env-))
  variables
  functions
  blocks
  tagbodies
  declarations
  evacuation
  parent)

(defmacro with-interpreter-environment
	  ((new-env old-env new-vars new-fns new-blks new-tags new-dcls)
	   &body body)
  `(with-stack-list (,new-env ,new-vars ,new-fns ,new-blks ,new-tags ,new-dcls nil ,old-env)
     ;; evaluate the body, presumably it uses new-env
     . ,body))

(defmacro with-new-interpreter-environment (&body body)
  `(let ((env *mapforms-lexical-function-environment*))
     (with-interpreter-environment
       (*mapforms-lexical-function-environment*
	 *mapforms-lexical-function-environment*
	 (pop env) (pop env) (pop env) (pop env) (pop env))
       ,@body)))

;;; record the current mapforms incarnation in the declarations part of the environment
;;; we use a counter.  genera used the stack frame pointer.
(defvar *mapforms-environment-counter* 0)

(defmacro with-mapforms-environment-tag ((env) &body body)
  (let ((temp (gensym))
	(new-dcls (gensym)))
    `(let ((,temp ,env))
       (stack-let ((,new-dcls `(((mapforms-environment-tag ,(incf *mapforms-environment-counter*)))
				,@(env-declarations ,temp))))
	 (with-interpreter-environment
	   (,env ,env (pop ,temp) (pop ,temp) (pop ,temp) (pop ,temp) ,new-dcls)
	   ,@body)))))



;;;; Getting Information from the environments

(defun get-mapforms-environment-tag (env)
  (cadar (find 'mapforms-environment-tag (env-declarations env) :key #'caar)))

(defun symbol-macro-p (name &optional env)
  (let ((env-value (cdr (assoc name (env-variables env)))))
    (when env-value
      (return-from symbol-macro-p
	(if (and (consp env-value)
		 (cdr env-value))
	    (values t (car env-value))
	    nil))))
  ;; Implementation specific code to determine if something is a global
  ;; symbol-macro needs to be here
  ;;  (let ((default '#.(gensym)))
  ;;    (let ((form (get name 'sys:symbol-macro default)))
  ;;      (when (not (eq form default))
  ;;	(return-from symbol-macro-p (values t form)))))
  nil)

;;; This may require special casing for each implementation
(defun macro-p (function &optional env)
  (flet ((local-function-definition (function)
	   (second (assoc function (env-functions env) :test #'equal))))
    (when (symbolp function)
      (let ((temp (local-function-definition function)))
	(if temp
	    ;; If we've bound it in our bookkeeping environment
	    ;; The if our binding is a macro return it otherwise
	    ;; nil.
	    (when (and (listp temp)
		       (eq (first function) 'special))
	      (second function))
	    ;; use the real world's global definition
	    (macro-function function nil))
	))))



;;;; Our own macroexpand which understands our environments

;;; All expansion of macros, symbol-macros by MAPFORMS goes
;;; through here.  This is so that macros that are expanded explicitly by other macro
;;; expander functions get seen by MAPFORMS.  For example, SETF macroexpands its
;;; -place- argument.  The immediate need for this is so that ANNOTATE-FORM can note
;;; them as free variables (symbol-macros) or functions (regular macros).

(defun mapforms-macroexpand (form env)
  (let* ((first t)
	 (original-tag (get-mapforms-environment-tag env))
	 (original-hook *macroexpand-hook*))
    (flet ((new-hook (fcn form host-env)
	     (block nil
	       ;; first check that this macroexpansion is part of this mapforms and not
	       ;; something unrelated, such as a macroexpansion in the body of a macro
	       ;; that has not been compiled, or a macroexpansion during a recursive
	       ;; call to mapforms from the body of a macro.
	       (when (eq (get-mapforms-environment-tag env) original-tag)
		 (if first
		     ;; the first macroexpansion has already been through mapforms-call
		     (setq first nil)
		     ;; this must be a recursive macroexpansion, let the client know about it
		     (let ((*macroexpand-hook* original-hook))
		       (unless (eq form (setq form (mapforms-call form nil 'macroexpand)))
			 ;; client has substituted a different form, so abort expansion
			 (return form)))))
	       ;; continue with normal macroexpansion processing
	       (funcall original-hook fcn form HOST-env))))
      (declare (dynamic-extent #'new-hook))
      (let ((*macroexpand-hook* #'new-hook))
	;; expand this macro, lambda-macro, or symbol-macro with the above hook installed
	(cond ((symbolp form)
	       (if (symbol-macro-p form env)
		   (macroexpand-1 form *host-specific-environment*)
		   (values form nil)))
	      ((not (listp form))
	       (values form nil))
	      (t (if (macro-p (first form) env)
		     (macroexpand-1 form *host-specific-environment*)
		     (values form nil))))))))



;;;; Supporting macros

;;; This macro allows substituting for some element of a list being
;;; mapped down, without smashing anything yet with minimal consing.
;;;	ORIGINAL-LIST - the original, uncopied list
;;;	CURRENT-LIST - that or a copy of it (must be a variable)
;;;	TAIL - must be a tail of CURRENT-LIST, its car is to be changed
;;; If TAIL is a variable, it is setq'ed to the corresponding tail of
;;; the copy if a copy is made.
;;;
;;; We need a ONCE-ONLY form here.
;;; Create code that is body, possibly with a lambda wrapped around it to make
;;; sure that the forms assigned to the listed variables only get evaluated once.

;;; This appears not to be true.
;;;;;; Gensymbol is used to generate a new Symbol. We use this insted of gensym 
;;;;;; because gensym is "stickey". This is used by MAPFORMS-RPLACA and MAPFORMS-RPLACD.
;;;
;;;(defvar *gensymbol-count* -1)
;;;
;;;;;; A non-stickey gensym.
;;;
;;;(defun gensymbol (name)
;;;   (make-symbol (format nil "~A~D" name (incf *gensymbol-count*))))

(DEFMACRO MAPFORMS-RPLACA (ORIGINAL-LIST CURRENT-LIST TAIL NEWCAR)
  (OR (SYMBOLP CURRENT-LIST) (ERROR "~S not a variable" CURRENT-LIST))
  (LET ((NAME (GENSYM "G")))
    `(LET ((,NAME ,NEWCAR))
       (COND ((NOT (EQ (CAR ,TAIL) ,NAME))
	      (RPLACA (IF (EQ ,ORIGINAL-LIST ,CURRENT-LIST)
			  (MULTIPLE-VALUE-SETQ (,(IF (SYMBOLP TAIL) TAIL '*IGNORE*)
						,CURRENT-LIST)
			    (MAPFORMS-RPLACA-COPY ,TAIL ,CURRENT-LIST))
			  ,TAIL)
		      ,NAME))))))

(DEFUN MAPFORMS-RPLACA-COPY (TAIL LIST)
  (LOOP WITH NEW-LIST = (COPY-LIST LIST)
	FOR NEW-TAIL ON NEW-LIST AND OLD-TAIL ON LIST
	WHEN (EQ OLD-TAIL TAIL)
	  RETURN (VALUES NEW-TAIL NEW-LIST)
	FINALLY (ERROR "~S is not a tail of ~S" TAIL LIST)))

;;; Same for cdr.
;;; Never stores back into TAIL (since of course it doesn't copy list beyond it)
;;; We assume that a given tail will only be rplacd'ed once
;;;
;;; We need a ONCE-ONLY form here.

(DEFMACRO MAPFORMS-RPLACD (ORIGINAL-LIST CURRENT-LIST TAIL NEWCDR)
  (OR (SYMBOLP CURRENT-LIST) (ERROR "~S not a variable" CURRENT-LIST))
  (LET ((NAME (gensym "G")))
    `(LET ((,NAME ,NEWCDR))
       (COND ((NOT (EQ (CDR ,TAIL) ,NAME))
	      (RPLACD (IF (EQ ,ORIGINAL-LIST ,CURRENT-LIST)
			  (MULTIPLE-VALUE-SETQ (*IGNORE* ,CURRENT-LIST)
					       (MAPFORMS-RPLACD-COPY ,TAIL ,CURRENT-LIST))
			  ,TAIL)
		      ,NAME))))))

;Copy list through tail, but not into (cdr tail)
(DEFUN MAPFORMS-RPLACD-COPY (TAIL LIST)
  (LET* ((ORIGINAL-TAIL TAIL)
	 (ORIGINAL-LIST LIST)
	 (NEW-HEAD (CONS (CAR LIST) NIL))
	 (NEW-TAIL NEW-HEAD))
    (LOOP DO
      (WHEN (ATOM LIST)
	(ERROR "~S is not a tail of ~S" ORIGINAL-TAIL ORIGINAL-LIST))
      (RPLACD NEW-TAIL (CDR LIST))
      (WHEN (EQ LIST TAIL)
	(RETURN (VALUES NEW-TAIL NEW-HEAD)))
      (SETQ LIST (CDR LIST))
      (RPLACD NEW-TAIL (SETQ NEW-TAIL (CONS (CAR LIST) NIL))))))
