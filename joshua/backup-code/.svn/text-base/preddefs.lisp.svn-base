;;; -*- Mode: Lisp; Package: JI; Syntax: Ansi-common-lisp -*-
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
;;; Created 2/25/87 17:43:48 by sgr running on GROUSE at SCRC.

;;;
;;; Defs file for the predications part of Joshua.  Don't put random non-rule-compiler-or-predication-
;;;   related things here.
;;;

;;;
;;; Instrumentation.
;;;

(in-package :ji)

(defparameter *joshua-meters* nil "List of all the Joshua meters that Peek knows about.")

(eval-when (compile eval load)
	   (proclaim '(inline peek-meter-initial-value)))
(defun peek-meter-initial-value (name)
  ;; where the initial value of the meter gets stashed.
  (get name 'peek-meter-initial-value))

(def-defining-form define-joshua-meter
  :definer
  ((name initial-value)
   ;; define a Joshua meter
   `(progn
      (defparameter ,name ,initial-value)
      (setf (get ',name 'peek-meter-initial-value) ',initial-value)
      (pushnew ',name *joshua-meters*)))
  .
  NIL)

(defun clear-joshua-meters ()
  ;; clear all the joshua meters
  (loop for meter in *joshua-meters*
	doing (set meter  (peek-meter-initial-value meter))))

(define-joshua-meter *match-count*            0)
(define-joshua-meter *semi-match-count*       0)
(define-joshua-meter *successful-match-count* 0)
(define-joshua-meter *merge-count*            0)
(define-joshua-meter *semi-merge-count*       0)
(define-joshua-meter *successful-merge-count* 0)
(define-joshua-meter *forward-fire-count*     0)
(define-joshua-meter *backward-fire-count*    0)
(define-joshua-meter *stack-collector-overflows* 0)




(defstruct (discrimination-net-node

	     (:print-function
	       (lambda (self stream depth)
		 (declare (ignore depth))
		 (format stream "#<~a ~a>"
			 (type-of self) (discrimination-net-node-token self)))))
  ;; Structure of a node in the dn.
  (token nil)					;token you have to have to get into this node.  Root has noise here.
  (var-link nil)				;pointer to child that indexes simple variables
  (seg-var-link nil)				;pointer to child that indexes segment variables
  (info-or-table nil))				;predications or nil for terminal nodes; table or nil for non-terminal nodes



;;;
;;; Information kept around for debugging, recompilation, and so on.
;;; Here so it gets compiled early.
;;;

(defstruct (rule-debug-info
	     (:print-function
	       ;; print the name of the rule
	       (lambda (self stream depth)
		 (declare (ignore depth))
		 (format stream "#<~a ~s>"
			 (type-of self) (rule-debug-info-name self)))))
  (name nil)
  (control nil)
  (triggers nil)
  (network nil)
  (context nil)
  (certainty nil))

(eval-when (compile eval load)
  (proclaim '(inline rule-debug-info))
  (defun rule-debug-info (rule-name)
    (get rule-name 'rule-debug-info)))

(defun rule-certainty-factor (rule-name)
  (rule-debug-info-certainty (rule-debug-info rule-name)))

(defsetf rule-debug-info (rule-name) (val)
	 `(setf (get ,rule-name 'rule-debug-info) ,val))

(defvar *rule-depth* 0 "Used for indenting Joshua trace messages.")


;#+genera
;;;  Contains nothing dependent, but useless without INDENTING-OUTPUT.
;(defun joshua-trace-message-indentor (stream)
;  ;; prefix a trace message with vertical-bars, to make it easier to follow
;  (loop repeat *rule-depth*
;	doing (write-char #\| stream)
;	      (write-char #\Space stream)))

(defmacro format-trace (format-string &rest args)
  
  ;; prints message on *trace-output*, indented properly.
  `(progn (format *trace-output* ,format-string ,@args)
	  t))

(defmacro with-another-rule (&body body)
  ;; keep rule trace messages indented properly
  `(let ((*rule-depth* (1+ *rule-depth*)))
     ,@body)) 

;;;
;;; Truth values.  Here so they'll get open-coded.
;;;

;;; Bit 0 is for true, bit 1 is for false.  If both are on, that's contradictory.
(eval-when (eval load compile) ;used in macroexpanders of macros used below
(defconstant *unknown*       0)
(defconstant *true*          1)
(defconstant *false*         2)
(defconstant *contradictory* 3) ;for the TMS
)

(defparameter *truth-value-table* '#(*unknown* *true* *false* *contradictory*)
  "For figuring out the name of a truth value")

(defmacro truth-value-case (key &rest clauses)
  `(let ((.select-key. ,key))
     (cond ,@(mapcar #'(lambda (c)
			 (let ((k (car c)))
			   (if (and (symbolp k)
				    (or (eq k 't)
					(string= k "OTHERWISE")))
			       `(t ,.(cdr c))
			       `((eql .select-key. ,(car c))
				 ,.(cdr c)))))
		     clauses))))

(defun truth-value-name (truth-value)
  ;; return the symbol that names a truth-value
  (aref *truth-value-table* truth-value))

(defun negate-truth-value (truth-value &optional (if-unknown *unknown*))
  ;; negation only defined for true, false and unknown
  (truth-value-case truth-value
    (*true* *false*)
    (*false* *true*)
    (*unknown* if-unknown)
    (otherwise
     (check-type truth-value (integer 0 2)
		 "a truth value of true, false, or unknown"))))

;;;
;;; Tools for analyzing the usage of variables within a rule.
;;;


(defstruct (variable-analysis
	     (:print-function
	       (lambda (self stream depth)
		 ;; print these in slightly less barbaric fashion than standard
		 (declare (ignore depth))
		 (format stream "#<~a of ~s>"
			 (type-of self) (variable-analysis-variable self)))))
  ;; analysis of how a variable is used in a rule
  (variable nil)					;; The Lisp variable
  (triggers-found-in nil)
  (actions-found-in nil)
  ;; position in final environment; not initial or internal ones.
  (env-position nil)
  )

(defun find-corresponding-analysis (symbol variable-analyses)
  ;; find the variable analysis corresponding
  ;; to this logic-variable NAME.
  (find symbol variable-analyses :key #'variable-analysis-variable))

(eval-when (compile eval load)
  (proclaim '(inline trigger-variable-analysis-p))
  (defun trigger-variable-analysis-p (variable-analysis)
    (variable-analysis-triggers-found-in variable-analysis)))

(eval-when (compile eval load)
  (proclaim '(inline action-variable-analysis-p))
  (defun action-variable-analysis-p (variable-analysis)
    (not (trigger-variable-analysis-p variable-analysis)))) 

(defun rule-variable-initializations (variable-analyses)
  ;; tell each variable analysis what lisp variable is its vicar, and
  ;; Return a list of bindings to initialize those lisp variables.
  (loop for analysis in variable-analyses
	for name = (variable-analysis-variable analysis)
	collect name into names
	if (trigger-variable-analysis-p analysis)
	  ;; this variable is a trigger variable, so we can refer to it directly.
	  collect `(,name (lookup-in-environment .environment. ,(variable-analysis-env-position analysis)))
	    into forms
	else
	  ;; this variable is an action variable, so collect a binding and put the name
	  ;; in the analysis
	  collect `(,name (make-unbound-logic-variable ',name)) into forms
	finally (return (values names forms))
	   ))

(defun make-output-env-assignments (trigger-variable-analyses map)
  ;; make some assignments of variables to output environment slots, and return the map.
  (loop with output-map = map
	for (var . position) in output-map
	for analysis = (find-corresponding-analysis var trigger-variable-analyses)
	unless analysis
	  do (error "Can't make environment slot assignments, because ~S doesn't have ~
                     an analysis in ~{~s~^, ~}."
		    var trigger-variable-analyses)
	doing (setf (variable-analysis-env-position analysis) position)
	finally (return output-map)))

;;;
;;; Extensions to stack-let so we can cons predications on the stack.
;;; This version also fixes things up so we can cons lists on the stack.  Meter that, I suppose.
;;;

;;; Test cases
;(with-data-stack-list (foo 'a 'b 'c) (print foo) (describe-list-conses foo))
;(with-data-stack-list* (foo 'b 'c) (print foo) (describe-list-conses foo))
;(stack-let ((foo (data-stack-list* 'a 'b 'c))) (print foo) (describe-list-conses foo))

;;; Test cases
;(with-stack-predication (foo '(is-on a b c)) (print foo))
;(with-stack-predication (foo (list 'a 'b 'c)) (print foo))
;(with-stack-predication (foo (list* 'a 'b 'c)) (print foo)) -- doesn't fire here

;;; Bug:  STACK-LET is too smart; it's going in and stack-consing the list before the make-predication optimizer sees it.
;;;       Got around this by judiciously not telling it about data-stack-list and data-stack-list*.  However, it still
;;;       wont' optimize (make-predication (list ...) :stack) and so on.

(defvar *debugging-with-stack-predication* nil)

;;;  Dummy version for the bootless and unhorsed...
(defmacro with-stack-predication ((var structure-form &optional area
				       &rest make-predication-keywords) &body body)
  (declare (ignore area))
  `(let ((,var (make-predication ,structure-form ,.make-predication-keywords)))
     ,.body))

;;;#+genera
;;;(defmacro with-stack-predication ((var structure-form &optional area &rest make-predication-keywords) &body body)
;;;  ;; this assumes you've done a sys:with-data-stack on your own.
;;;  (unless (member area '(nil :stack))
;;;    (error "You can't specify an area for WITH-STACK-PREDICATION, unless it's NIL or :STACK."))
;;;  (let* ((unoptimized `(make-predication ,structure-form :stack ,@make-predication-keywords))
;;;	 (kludge (funcall (first (get 'make-predication 'compiler:optimizers)) unoptimized)))
;;;    ;; The nonsense above is a crude attempt to figure out whether or not the make-predication optimizer
;;;    ;; can fire on this kind of thing.  If it does, the thing it optimizes into won't cons.  If it doesn't,
;;;    ;; we need to stack-let the structure to prevent the consing that would otherwise happen.
;;;    (cond ((eq kludge unoptimized)
;;;	   ;; the optimizer didn't fire, so let's try to avoid consing the structure arg, at least
;;;	   (when *debugging-with-stack-predication*
;;;	     ;;(break "~&unoptimized: ~S" structure-form)
;;;	     (warn "Not optimizing WITH-STACK-PREDICATION of ~S." unoptimized))
;;;	   (let ((list-var (sys:gensymbol "STACK-PRED-LIST")))
;;;	     `(stack-let ((,list-var ,structure-form))
;;;		;; don't use stack-let here, 'cause that's what called us!
;;;		(let ((,var (make-predication ,list-var :stack ,@make-predication-keywords)))
;;;		  ,@body))))
;;;	  (t
;;;	    ;; use the unoptimized version, which doesn't cons
;;;	    `(let ((,var ,kludge))
;;;	       ,@body)))))
;;;
;;;
;;;#+genera
;;;(defun data-stack-list (&rest elements)
;;;  ;; assumes you've already done a with-data-stack
;;;  (let* ((length (length elements))
;;;	 (array (sys:make-stack-array length :type 'sys:art-q-list)))
;;;    (loop for i below length
;;;	  for element in elements
;;;	  doing (setf (aref array i) element))
;;;    (g-l-p array)))
;;;
;;;#+genera
;;;(defun data-stack-list* (&rest elements)
;;;  ;; assumes you've already done a with-data-stack
;;;  (let* ((length (length elements))
;;;	(array (sys:make-stack-array length :type 'sys:art-q-list)))
;;;    (loop for i below length
;;;	  for element in elements
;;;	  doing (setf (aref array i) element))
;;;    (setf (sys:%p-cdr-code (locf (aref array (- length 2)))) sys:cdr-normal)
;;;    (g-l-p array)))
;;;
;;;#+genera
;;;(defun extend-stack-let ()
;;;  ;; extend this, cleaning up previous attempts
;;;  (setq si:*stack-let-operations*
;;;	(delete-if #'(lambda (x) (member (car x) '(make-predication)))
;;;		   si:*stack-let-operations*))
;;;  ;; You extend STACK-LET* and friends by pushing the appropriate frob onto
;;;  ;; SI:*STACK-LET-OPERATIONS*.  The format is
;;;  ;; (<constructor> <min-args> <max-args> <wrapping-stack-constructor>).
;;;  ;; See the other values on that list for examples.
;;;  (push '(make-predication 1 nil with-stack-predication) si:*stack-let-operations*)
;;;;  (push '(data-stack-list 0 nil with-data-stack-list) si:*stack-let-operations*)
;;;;  (push '(data-stack-list* 0 nil with-data-stack-list*) si:*stack-let-operations*)
;;;  )
;;;
;;;#+genera
;;;(extend-stack-let) ;do it now (i.e., (eval load) time)

;;;
;;; Some data structures used by the question-asking mechanism.
;;;

(defstruct (question-info
	     (:print-function
	       (lambda (self stream depth)
		 (declare (ignore depth))
		 (format stream "#<~a for ~s>" (type-of self) (question-info-name self)))))
  ;; structure describing this question's attributes.
  (name nil)
  (importance nil)
  (pattern nil)
  (context nil))

(eval-when (compile eval load)
  (proclaim '(inline question-info))
  (defun question-info (name)
    (get name 'question-info)))

(defsetf question-info (name) (val)
  `(setf (get ,name 'question-info) ,val))

;;;  This was in Ptypes-and-commands.  I moved it here.  -Wev

(defmacro map-over-database-predications (predication-pattern function)
  `(ask ,predication-pattern
	#'(lambda (backward-support)
	    (when (and (consp backward-support)
		       (consp (rest backward-support))
		       (consp (rest (rest backward-support))))
	      (let ((database-predication (ask-database-predication backward-support)))
		(when (predicationp database-predication)
		  (funcall ,function database-predication)))))
	:do-backward-rules nil
	:do-questions nil))
