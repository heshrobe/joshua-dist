;;; -*- Mode: LISP; Package: JOSHUA-INTERNALS; Syntax: Ansi-common-lisp -*-
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
;;;> Symbolics 3640, Symbolics 3645 (R), Symbolics 3650 (R), Symbolics 3620 (R), Symbolics 
;;;> 3610 (R), Symbolics Common Lisp (R), Symbolics-Lisp (R), Zetalisp (R), Genera (R),
;;;> Wheels, Dynamic Windows (R), Showcase, SmartStore (R), Semanticue (R), Frame-Up (R),
;;;> Firewall (R), MACSYMA (R), COMMON LISP MACSYMA, CL-MACSYMA (R), LISP MACHINE
;;;> MACSYMA (R), MACSYMA Newsletter (R), Document Examiner (R), S-DYNAMICS (R),
;;;> S-GEOMETRY (R), S-PAINT (R), S-RENDER (R), "Your Nextjoshua/code/ Step in Computing" (R), Ivory,
;;;> Symbolics C, Symbolics Pascal, Symbolics Prolog, Symbolics Fortran, CLOE, Joshua,
;;;> Concordia, and Statice are trademarks of Symbolics, Inc.
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
;;; Created Friday the eighteenth of October, 1985; 9:01:43 pm by sgr on GROUSE at SCRC

;;;
;;; Reader hackery for Joshua.
;;;

(in-package :ji)

(defvar *joshua-readtable* nil ;defined below
  "Readtable for Joshua.")

(defparameter *joshua-variable-char* #\?
  "Logic variables begin with this character.")

(defparameter *joshua-open-predication-char* #\[
  "This opens a predication.")

(defparameter *joshua-close-predication-char* #\]
  "This closes a predication.")

(defparameter *joshua-open-predication-maker-char* #\{)
(defparameter *joshua-close-predication-maker-char* #\})

(defparameter *joshua-list-close-chars*
	      (list *joshua-close-predication-char* #\))
  "Characters which close list-like structures.")

;;; Backquote handling

(defparameter *backquote-comma-flag* (make-symbol ","))
(defparameter *backquote-comma-dot-flag* (make-symbol ",."))
(defparameter *backquote-comma-atsign-flag* (make-symbol ",@"))

(defmacro backquote (code) (expand-backquote code))

(defun expand-backquote (code)
  (let ((*backquote-depth* 1))
    (declare (special *backquote-depth*))
    (labels
      ((handle-atom (code)
	 (cond ((null code) (values nil nil))
	       ;; don't quote self-evaluating atoms
	       ((and (atom code)
		     (or (not (symbolp code))
			 (or (member code '(t nil)) (keywordp code)
			     (and (constantp code) (eq (symbol-value code) code)))))
		(values t code))
	       (t (values 'quote code))))
       (handle-comma (code)
	 (let ((*backquote-depth* (1- *backquote-depth*)))
	   (declare (special *backquote-depth*))
	   (cond
	     ((> *backquote-depth* 0) (handle-comma-under-backquote code))
	     ((atom code)
	      (cond ((null code)
		     (values nil nil))
		    ((and (atom code)
			  (or (not (symbolp code))
			      (or (member code '(t nil)) (keywordp code)
				  (and (constantp code) (eq (symbol-value code) code)))))
		     (values t code))
		    (t (values '*backquote-comma-flag* code))))
	     ((eq (car code) 'quote)
	      (values (car code) (cadr code)))
	     ((member (car code) '(append list list* nconc))
	      (values (car code) (cdr code)))
	     ((eq (car code) 'cons)
	      (values 'list* (cdr code)))
	     (t (values '*backquote-comma-flag* code)))))
       (handle-comma-under-backquote (code)
	 (multiple-value-bind (flag stuff) (handle-term code)
	     (cond ((or (eql flag '*backquote-comma-atsign-flag*)
			(eql flag '*backquote-comma-dot-flag*))
		    (values '*backquote-comma-dot-flag*
			    (list 'mapcar #'(lambda (thing) (list '*backquote-comma-flag* thing)) stuff)))
		   (t
		    (values
		      'list
		      (let ((packaged-stuff (package-up-for-top-level flag stuff)))
			(list (list 'quote '*backquote-comma-flag*) packaged-stuff)))))))
       (handle-comma-atsign (code)
	 (let ((*backquote-depth* (1- *backquote-depth*)))
	   (declare (special *backquote-depth*))
	   (if (> *backquote-depth* 0)
	       (multiple-value-bind (flag stuff) (handle-term code)
		 (cond ((or (eql flag '*backquote-comma-atsign-flag*)
			    (eql flag '*backquote-comma-dot-flag*))
			(values '*backquote-comma-atsign-flag*
				(list 'mapcar #'(lambda (thing) (list '*backquote-comma-atsign-flag* thing)) stuff)))
		       (t
			(values
			  'list
			  (let ((packaged-stuff (package-up-for-top-level flag stuff)))
			    (list (list 'quote '*backquote-comma-atsign-flag*) packaged-stuff))))))
	       (values '*backquote-comma-atsign-flag* code))))
       (handle-comma-dot (code)
	 (let ((*backquote-depth* (1- *backquote-depth*)))
	   (declare (special *backquote-depth*))
	   (if (> *backquote-depth* 0)
	       (multiple-value-bind (flag stuff) (handle-term code)
		 (let ((packaged-stuff (package-up-for-top-level flag stuff)))
		   (values
		     'list
		     (list (list 'quote '*backquote-comma-dot-flag*) packaged-stuff))))
	       (values '*backquote-comma-dot-flag* code))))
       (handle-backquote (code)
	 (let ((*backquote-depth* (1+ *backquote-depth*)))
	   (declare (special *backquote-depth*))
	   (multiple-value-bind (flag stuff) (handle-term (second code))
	     (let ((packaged-stuff (package-up-for-top-level flag stuff)))
	       (values 'list `('backquote ,packaged-stuff))))))
       (handle-list (code)
	 (multiple-value-bind (carflag car) (handle-term (car code))
	   (multiple-value-bind (cdrflag cdr) (handle-term (cdr code))
	     (cond ((eq carflag '*backquote-comma-atsign-flag*)
		    (values
		      'append
		      (cond ((eq cdrflag 'append)
			     (cons car cdr))
			    (t (list car (Package-up-for-top-level cdrflag cdr))))))
		   ((eq carflag '*backquote-comma-dot-flag*)
		    (values
		      'nconc
		      (cond ((eq cdrflag 'nconc)
			     (cons car cdr))
			    (t (list car (Package-up-for-top-level cdrflag cdr))))))
		   ((null cdrflag)
		    (cond ((member carflag '(quote t nil))
			   (values 'quote (list car)))
			  (t (values
			       'list
			       (list (Package-up-for-top-level carflag car))))))
		   ((member cdrflag '(quote t))
		    (cond ((member carflag '(quote t nil))
			   (values 'quote (cons car cdr)))
			  (t (values 'list*
				     (list (Package-up-for-top-level carflag car)
					   (Package-up-for-top-level cdrflag cdr))))))
		   (t (setq car (Package-up-for-top-level carflag car))
		      (if (member cdrflag '(list list*))
			  (values cdrflag (cons car cdr))
			  (values 'list* (list car (Package-up-for-top-level cdrflag cdr)))))))))
       (handle-term (code)
	 (cond ((atom code) (handle-atom code))
	       ((eq (car code) '*backquote-comma-flag*)
		(handle-comma (cadr code)))
	       ((eq (car code) '*backquote-comma-atsign-flag*)
		(handle-comma-atsign (cadr code)))
	       ((eq (car code) '*backquote-comma-dot-flag*)
		(handle-comma-dot (cadr code)))
	       #+genera
	       ((and (scl:variable-boundp si:*read-circularity-unresolved-labels*)
		     (member code si:*read-circularity-unresolved-labels*))
		(values t code))
	       ((eq (car code) 'backquote) (handle-backquote code))
	       (t (handle-list code))))
       (Package-up-for-top-level (flag thing)
	 (cond ((or (eq flag '*backquote-comma-flag*) (member flag '(t nil))) thing)
	       ((eq flag 'quote) (list 'quote thing))
	       ((eq flag 'list*)
		(cond ((null (cddr thing))
		       (cons 'cons thing))
		      (t (cons 'list* thing))))
	       (t (cons (cdr (assoc flag `((cons . cons)
					   (list . list)
					   (append . append)
					   (nconc . nconc))))
			thing)))))
      (multiple-value-bind (flag stuff) (handle-term code)
	(Package-up-for-top-level flag stuff)))))



(defun joshua-top-level-backquote-reader (stream char)
  (if (char-equal #\[ (peek-char 't stream))
      (joshua-backquote-bracket-reader stream char)
      (joshua-normal-backquote-reader stream char)))

(defvar *backquote-count* 0)

(defun joshua-normal-backquote-reader (stream ignore)
  (declare (ignore ignore))
  (let* ((*backquote-count* (1+ *backquote-count*))
	 (the-statement (read stream t nil t)))
    `(backquote ,the-statement)))

(defun joshua-backquote-bracket-reader (stream char)
  ;; Called when we read a backquote which is immediately followed by a
  ;; left bracket.  We've read the backquote and checked with peek-char
  ;; that the next token is the left bracket.  In this case we handle
  ;; the backquote reading ourselve by duplicating xr-bq-macro and then
  ;; we return a predication-maker form.
  (declare (ignore char))
  (let ((*backquote-count* (1+ *backquote-count*)))
    ;;  first do a read-char for effect to get rid of the bracket
    (read-char stream)
    ;; then read the (possibly dotted) list and do backquote magic on it
    (let ((the-statement (read-delimited-dotted-list *joshua-close-predication-char* stream t)))
      `(predication-maker (backquote ,the-statement)))))

(defun joshua-comma-reader (stream char)
  (declare (ignore char))
  (unless (> *backquote-count* 0)
    (error "comma not inside a backquote"))
  (let ((c (read-char stream nil))
	(*backquote-count* (1- *backquote-count*)))
    (macrolet ((readrec (x) `(read ,x t nil t)))
    (cond ((null c)
	   (error "end of file reached while reading a comma inside a backquote"))
	  ((char-equal c #\@)
	   (list '*backquote-comma-atsign-flag* (readrec stream)))
	  ((char-equal c #\.)
	   (list '*backquote-comma-dot-flag* (readrec stream)))
	  (t (unread-char c stream)
	     (list '*backquote-comma-flag* (readrec stream)))))))


;;;
;;; Source code representations of Joshua logic variables and predications.
;;;

;;; 
;;; Design notes:
;;;
;;; In the implementation until recently, there was a special variable called *known-lvs* which is 
;;; supposed to always be bound to the already seen logic-variables that lexically scope where we
;;; are in execution or compilation.  This was kept current using Compiler-let to rebind *known-lvs*
;;; as new logic-variables were introduced (by ask or rule matching for example).
;;; This approach works for compiled code but not for interpreted.
;;; This isn't surprising given the description above that we are
;;; using dynamic binding of *known-lvs* to keep track of lexical context.

;;; The failure case occurs in interpreted code.  Consider Rule-1
;;; which has a pattern (call it pattern-1) in the If-part which
;;; triggers Rule-2 to satisfy this goal.  Rule-2 succeeds and calls
;;; it's continuation, which is code that tries to satisfy the next
;;; pattern (call it pattern-2).  Pattern-2 wants to execute in an environment that corresponds to its
;;; lexical scope.  But *Known-lvs* is really part of this environment and it was last bound by Rule-2
;;; with stuff having nothing to do with pattern-2's scope in Rule-1.

;;; Oddly enough, we haven't seen this bug for all of the 15 or so
;;; years of this code's life, because we always run compiled.
;;; Because the compiler walks the code for a rule all at once, dynamic scope winds up
;;; being the same as lexical scope.  The variable *known-lvs* needs to be special
;;; because it has to have a value in the global environment so that
;;; top level forms can compare their set of logical-variables to that of
;;; the enclosing scope.  And it goes downhill from there.
;;;
;;; The fix is to pass info along as part of Lisp's environment
;;; (either compiler or interpreter environment) and to extend the
;;; environment with updated bindings for Known-lvs (no longer a
;;; special variable).  Alas Cltl-2 never adopted the environment
;;; proposal, although each implementation has some variant of it.

;;; However, you don't really need the equivalent of
;;; Augment-Environment, because there's a portable way to do an
;;; equivalent using macrolet and macroexpand and carefully managing
;;; the &environment argument's to these.  Macrolet creates a binding
;;; of a new macro, called known-lvs, which macroexpands to NIL in the
;;; global environment and which macroexpands to the set of lexically
;;; seen logic-variables in each nested environment.
;;; So you just have to be careful to pass along the environment.
;;;  This also eliminates the use of the deprecated compiler-let.

;;; Where this is used:
;;; 1) Known-lvs is used in reformulating the syntax for a
;;; predication (called a predication maker) so that it appropriately
;;; backquotes and commifies the logic-variables inside.  In
;;; particular, all syntax for references to logic-variables
;;; (logic-variable-maker) must be commified.  Tell is a special case
;;; of this.
;;;
;;; 2) Ask binds logic-variables and the continuation needs to know
;;; which logic-variables are already bound, so that if there's
;;; another ask inside it doesn't bind more.
;;;
;;; 3) Backward chaining rules: we compile them by finding all the
;;; logic-variables in both the if-part and then-part and then binding
;;; them at the top-level.  This needs to be communicated to the code
;;; that generates the matcher and body.  Also, in compiling the body,
;;; uses the code-walker to see if there's any calls to succeed in the body.
;;; The code walker needs to be invoked with the right environment so
;;; that when it macroexpands logic-variables they don't complain
;;; about being out of scope.  Actually, nothing cares whether they
;;; are or not, but the code walker does macroexpand them at the moment.
;;; At the moment, I use augment-environment in this case only, but either
;;; a) I can stop the code walker from expanding the
;;; logic-variable-maker macros (which would be appropriate) or
;;; b) I can use the binding trick in the code walker (an evaluation
;;; of a doubly nested macrolet to get the right environment set up).
;;;
;;; 4) Forward chaining rules: The pattern matchers for the If-part
;;; are all compiled independently and at top-level, since they become
;;; match-nodes of the rete network.  The then-part needs to be passed
;;; the right environment, but that works trivially using macrolet.

;;; So in summary, the macrolet trick should work for both compiled and
;;; interpreted code and should handle all the cases we need.  There
;;; should be no references to *known-lvs* left in the code.


;;; there are no known-lvs in the global scope
(defmacro known-lvs () nil)

;;; Logic-variable-makers are the things in source
;;; code used to indicate that a logic variable is intended.

(defun make-logic-variable-maker (name)
  `(logic-variable-maker ,name))

(defun logic-variable-maker-p (form)
  (and (consp form) (eq (car form) 'logic-variable-maker)))

(deftype logic-variable-maker ()
  '(satisfies logic-variable-maker-p))

(defun logic-variable-maker-name (lv-maker)
  (second lv-maker))

(defun predication-maker-p (form)
  (and (consp form) (eq (car form) 'predication-maker)))

(deftype predication-maker ()
  '(satisfies predication-maker-p))

(defun predication-maker-statement (form)
  (second (second form)))

(defun predication-maker-predicate (form)
  (let ((statement-container (second form)))
    (if (member (car statement-container) '(quote backquote))
	;; this is the "nice" case of strictly quoted stuff
	(first (second statement-container))
	;; it's one of the Messy xr-bq-?? stuff
	(error "Trying to find predicate of predication-maker which I don't understand~&~s"
	       form))))

(defmacro with-predication-maker-destructured (arglist statement &body body)
  (when (null statement)
    (setq statement 'self))
  `(destructuring-bind ,arglist (cdr (predication-maker-statement ,statement))
     ,@body))

(defmacro logic-variable-maker (name &environment env)
  (let ((known-lvs (macroexpand '(known-lvs) env)))
    (unless (member name known-lvs)
      (warn "The logic variable ~s has been used as a free reference. ~a" 
	    name known-lvs))
    `(joshua-logic-variable-value ,name)))

;;; This macro converts what the reader produces into what is meant by
;;; that.

;;; To allow simple parsing of rule patterns, the input produced by the
;;; reader is a direct structural analog of what the user typed unless
;;; the input used explicit backquoting and commifying.

;;; If you eval the results of this macro expansion you'll get the
;;; predication structure that you expect.  This macro "commifies"
;;; logic-variables as well as predication-creating forms used inside
;;; the top-level predication creation form.

(defmacro predication-maker (&body form &environment env)
  (let ((new-lvs nil) (all-lvs (macroexpand '(known-lvs) env)))
    (labels
      ((collect-lv (lv)
	 (let ((lv-name (logic-variable-maker-name lv)))
	   (when (not (member lv-name all-lvs))
	     (push lv-name new-lvs)
	     (push lv-name all-lvs))))
       ;; This gets called to walk over quoted forms it finds
       ;; logic-variable and forces them to be evaluated.
       (do-away-with-lvs (form)
	 (cond ((atom form) form)
	       ((logic-variable-maker-p form)
		(collect-lv form)
		(list '*backquote-comma-flag* form))		
	       (t (loop for sublist = form then (cdr sublist)
			while (and (consp sublist)
				   (not (logic-variable-maker-p sublist)))
			for thing = (car sublist)
			collect (do-away-with-lvs thing) into answer
			finally (when sublist
				  (setq answer (nconc answer (do-away-with-lvs sublist))))
				(return answer)))))
       ;; A code walker which converts quoted logic-variable-makers to
       ;; evaluated ones and which turns predication-maker into
       ;; make-predication.
       (walker (form)
	 (jlt:copyforms
	   #'(lambda (subform kind usage)
	       ;; (format t "~&Subform ~s~% Kind ~s Usage ~s" subform kind usage)
	       (declare (ignore usage))
	       (cond ((predication-maker-p subform)
		      ;; make predication form
		      (values `(make-predication ,(walker (second subform)) ,@(cddr subform)) t))
		     ((logic-variable-maker-p subform)
		      ;; a non-quoted logic variable
		      ;; just collect it
		      (collect-lv subform)
		      ;; this says that I handled it.
		      (values subform t))
		     ;; logic-variable in tail
		     ((and (consp subform)
			   (logic-variable-maker-p (cdr subform)))
		      ;; With lvs being a list (lv x) the tail variable looks like a
		      ;; two long list in the tail.  This catches it before confusion
		      ;; sets in.
		      (collect-lv (cdr subform))
		      (values (cons (walker (car subform)) (cdr subform))
			      #+++ignore	; from the dec version
		              (cons (walker (car subform))
				    (walker (cdr subform)))
			      t))
		     ;; being of quoted context
		     ((and (eq kind 'quote) (consp subform) (eq (car subform) 'quote))
		      (let ((quoted-stuff (second subform)))
			(values (expand-backquote (do-away-with-lvs quoted-stuff)) t)))
		     ;; anything else
		     (t (values subform nil))))
	   form
	   :expand-all-macros t
	   :environment (if (boundp 'jlt:*MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT*)
			    jlt:*mapforms-lexical-function-environment*
			    nil)))
       ;; This one is used to preprocess the form, forcing all nested
       ;; predications to be commifed.
       (commify-predications (form &optional (pred-count 0) (backquote-count 0))
	 (cond ((predication-maker-p form)
		(let ((matrix (cond ((member (first (second form)) '(quote backquote))
				     ;; The following shouldn't be written with backquote,
				     ;; in case it gets read with the backquoter herein.
				     ;; When that happens, the "comma" below doesn't work.
				     ;;`(predication-maker
				     ;;  (backquote
				     ;;   ,(commify-predications (second (second form))
				     ;;				 (1+ pred-count)
				     ;;				 (1+ backquote-count))))
				     (list 'predication-maker
					   (list 'backquote
						 (commify-predications
						   (second (second form))
						   (1+ pred-count)
						   (1+ backquote-count)))))
				    (t `(predication-maker
					  ,(commify-predications (second form)
								 (1+ pred-count)
								 backquote-count))))))
		  (when (and (not (zerop pred-count)) (not (zerop backquote-count)))
		    (setq matrix (list '*backquote-comma-flag* matrix)))
		  matrix))
	       ((atom form) form)
	       ((member (car form) '(*backquote-comma-flag* *backquote-comma-atsign-flag* *backquote-comma-dot-flag*))
		(list (car form)
		      (commify-predications (cadr form) pred-count (1- backquote-count))))
	       (t (loop for sublist = form then (cdr sublist)
			while (consp sublist)
			for thing = (car sublist)
			collect (commify-predications thing pred-count backquote-count) into answer
			finally (if (null sublist)
				    (return answer)
				    (return (nconc answer
						   (commify-predications sublist pred-count backquote-count)))))))))
      (let ((form (walker (commify-predications `(predication-maker ,@form)))))
	(if new-lvs
	    `(let ,(mapcar #'(lambda (lv) `(,lv (make-unbound-logic-variable ',lv)))
		    new-lvs)
	       (macrolet ((known-lvs () ',all-lvs))
		 ,form))
	    form)))))

;;; this is the boxed structure for holding a logic variable's pname
;;; and value cell.

;;; On non-Genera machines, it's unbound when its value cell refers to
;;; the structure itself.

(defstruct (joshua-logic-variable
	     (:print-function
	       (lambda (self stream depth)
		 (declare (ignore depth))
		 ;; This might like to print the pointer too, but it's imp-dependent...
		 (if (eql (joshua-logic-variable-value-cell self) self)
		     (format stream "#<~:[unbound ~;~]LV named ~a~@[ ~o~]>"
			     (not (eq (joshua-logic-variable-value-cell self) self))
			     (joshua-logic-variable-name self)
			     nil)
		   (let ((value (joshua-logic-variable-value self)))
		     (princ value stream))))))
  name
  value-cell)

(eval-when (compile eval load)
  (proclaim '(inline joshua-logic-variable-unbound-p))
  (defun joshua-logic-variable-unbound-p (x)
    (eq (joshua-logic-variable-value-cell x) x))
  )

(deftype unbound-logic-variable ()
  '(and joshua-logic-variable
	(satisfies joshua-logic-variable-unbound-p)))

(eval-when (compile eval load)
(proclaim '(inline unbound-logic-variable-p))
(defun unbound-logic-variable-p (x)
  ;; Sort of a tautology, but what the heck.
  (typep x 'unbound-logic-variable))
)


;;; On bootless and unhorsed LISPs (i.e. those without invisible
;;; pointers), we must "dereference" LVs the hard way.  This operator
;;; is used in expansions of LOGIC-VARIABLE-MAKER.
;;;    Should this be INLINE?
(eval-when (compile eval load)
(proclaim '(inline joshua-logic-variable-value))
(defun joshua-logic-variable-value (jlv &aux val)
  (loop
    (cond ((not (joshua-logic-variable-p jlv))
	   (return jlv))			;bound or not LV => return value.
	  ((eq (setq val (joshua-logic-variable-value-cell jlv)) jlv)
	   (return jlv))			;unbound => return the structure itself.
	  (t (setq jlv val))))))		;indirect => chase down the chain.

(defun set-joshua-logic-variable-value (jlv new-value &aux val)
  (loop
    (cond ((or (eq (setq val (joshua-logic-variable-value-cell jlv)) jlv)
	       (not (joshua-logic-variable-p val)))
	   (return (setf (joshua-logic-variable-value-cell jlv) new-value)))
	  (t (setq jlv val)))))

(defsetf joshua-logic-variable-value set-joshua-logic-variable-value)

(defun make-unbound-logic-variable (name)
  (let ((variable-structure (make-joshua-logic-variable :name name :value-cell nil)))
    (setf (joshua-logic-variable-value-cell variable-structure) variable-structure)))

(defun joshua-logic-variable-makunbound (lv)
  (check-type lv joshua-logic-variable)
  (setf (joshua-logic-variable-value-cell lv) lv))

(eval-when (compile eval load)
  (proclaim '(inline logic-variable-name))
  (defun logic-variable-name (logic-variable)
    (joshua-logic-variable-name logic-variable)))

(defun compare-logic-variables (lv1 lv2)
  ;; function suitable for comparing logic variables.  Logic-variables-same-name-p
  ;; would be true of distinct anonymous variables, so this is better.
  (let ((name1 (logic-variable-name lv1)))
    (if name1
	;; if at least one is named, compare the names
	(eq name1 (logic-variable-name lv2))
	;; otherwise use eql, because at least one is anonymous
	(eql lv1 lv2))))


(defmacro with-unbound-logic-variables (variable-list &body body)
  ;; execute body with lisp variables bound to unbound logic variables.
  `(let ,(mapcar #'(lambda (variable)
		     `(,variable (make-unbound-logic-variable ',variable)))
		 variable-list)
     ,@body))


(eval-when (compile eval load)
  (proclaim '(inline white-space-p))
  (defun white-space-p (char &optional ignore)
    (declare (ignore ignore))
    "Whether this char is a whitespace char"
    (member char '(#\space #\tab #\newline #\page #\return #\linefeed))))

#+genera
(defun read-delimited-dotted-list (char &optional stream recursive-p)
  (cond ((null stream) (setq stream zl:standard-input))
	((eq stream t) (setq stream zl:terminal-io)))
  (setq char (code-char (char-code char)))
  (if recursive-p
      (si:read-list-internal stream char nil)
      ;; if the stream is interactive and we are not already inside the input editor,
      ;; then invoke it.  also start a new circular-list environment.
      (scl:with-input-editing (stream :end-activation)
	(let ((si:*read-circularity* nil))
	  (si:read-list-internal stream char nil)))))

;;; This is kind of rough, since we can't see the readtable's syntax entries for chars...
#-genera
(defun read-delimited-dotted-list (limit-char &optional stream recursive-p)
  ;; Like CL:READ-DELIMITED-LIST but handles consing-dot.
  (loop with things = (cons (read stream t nil t) nil)
	and things-last and char
	initially (setq things-last things)
	doing
    (setq char (peek-char t stream t nil recursive-p))
	until (eql char limit-char)
	doing
    (cond 
      ((eql char #\.)
       (read-char stream t nil t)		; waste the dot.
       (let ((next-char (peek-char nil stream t nil recursive-p)))
	 (labels ((consing-dot-after-tail ()
		    (loop as next-char = (peek-char t stream t nil recursive-p)
			  until (char= (peek-char t stream t nil recursive-p)
				       limit-char)
			  doing (progn next-char)	;meaning ignore
		      (multiple-value-bind (this-thing read-something-p)
			  (read-maybe-nothing stream t nil t)
			(declare (ignore this-thing))
			(when read-something-p
			  (error "On ~s, expected list delimiter \"~c\" after dot tail."
				 stream limit-char)))
			  finally
			    (read-char stream t nil t)	;waste delimiter we've already peeked
			    (return-from read-delimited-dotted-list things)))
		  (handle-consing-dot ()
		    (let ((next-char (peek-char t stream t nil recursive-p)))
		      (when (char= next-char limit-char)
			(error "On ~s, delimiter \"~c\" read after consing dot." stream next-char))
		      (loop 
			(multiple-value-bind (this-thing read-something-p)
			    (read-maybe-nothing stream t nil t)
			  (when read-something-p
			    (setf (cdr things-last) this-thing)
			    (return))))
		      (consing-dot-after-tail))))
	   ;; Sure would be nice if we could ask the readtable whether a char
	   ;; is whitespace or not...
	 (if (member next-char '(#\space #\tab #\newline #\return #\linefeed #\page))
	     ;; dot-whitespace = 
	     (handle-consing-dot)
	     ;; maybe a funny thing...
	     (multiple-value-bind (fn ntp) (get-macro-character next-char)
	       (if (and fn (not ntp))		;macro thing.
		   (multiple-value-bind (this-thing read-something-p)
		       (read-maybe-nothing stream t nil t)
		     (if read-something-p
			 ;; Got a tail.   ".(" might land us here.
			 (progn (setf (cdr things-last) this-thing)
				(consing-dot-after-tail))
			 ;; Was a comment.  Handle following like ordinary dot tail.
			 (handle-consing-dot)))
		   ;; Not a macro char.  Read out token beginning with a dot.
		   (loop as t-char = (read-char stream)
			 until (or (member t-char
					   ;; see "sure would be nice" above.
					   '(#\space #\tab #\newline #\return
					     #\linefeed #\page))
				   (multiple-value-bind (fn ntp)
				       (get-macro-character t-char)
				     (and fn (not ntp))))
			 collecting t-char into charlist
			 finally
			   (setf (cdr things-last)
				 (cons (read-from-string
					 (coerce (cons #\. charlist) 'string))
				       nil)))))))))
      (t
       (multiple-value-bind (this-thing read-something-p)
	   (read-maybe-nothing stream t nil t)
	 (when read-something-p
	   (setf (cdr things-last)
		 (cons this-thing nil))))))
    (when (consp (cdr things-last))
      (setq things-last (cdr things-last)))
	finally
	   (read-char stream t nil t) ; waste the delimiter.
	   (return things)))

(defun read-maybe-nothing (&optional input-stream (eof-errorp t) eof-value recursive-p)
  (declare (values thing read-something-p))
  (let* ((first-char (peek-char t input-stream eof-errorp eof-value recursive-p))
	 (macro-fn (get-macro-character first-char)))
    (if macro-fn
	(let ((vals (multiple-value-list (funcall macro-fn
						  (cond ((null input-stream)
							 *standard-input*)
							((eq input-stream 't)
							 *terminal-io*)
							(t input-stream))
						  (read-char input-stream eof-errorp
							     eof-value recursive-p)))))
	  (if (null vals)
	      nil
	      (values (first vals) t)))
	(values (read input-stream eof-errorp eof-value recursive-p)
		t))))

(defun read-predication (stream char)
  "Reader for Joshua predications,
   called when we read a left-bracket not immediately preceded by a backquote"
  (declare (ignore char))
  `(predication-maker
    ',(read-delimited-dotted-list *joshua-close-predication-char* stream t)))

(defparameter *temp* nil)
(defun read-predication-maker (stream char)
  "Reader for Joshua predication makers, i.e. backquoting the whole form
   called when we read a left-bracket not immediately preceded by a backquote"
  (declare (ignore char))
  (let* ((*backquote-count* (1+ *backquote-count*))
	 (inside (read-delimited-dotted-list *joshua-close-predication-maker-char* stream t)))
    (setq *temp* inside)
    (list 'backquote (list 'predication-maker (list 'quote inside)))))

(defparameter *anonymous-prefix* (concatenate 'string (string *joshua-variable-char*) "ANONYMOUS"))

(defun read-variable (stream char)
  "Reader for Interning Joshua variables."
  (declare (ignore char))
  ;; peek at the very next character
  (let ((next (peek-char nil stream nil #\space nil)))
    (cond ((or (white-space-p next)
	       (char-equal next *joshua-close-predication-char*)	;common case
	       (char-equal next #\)))		;another one
	   ;; this variable is anonymous
	   ;; Make more robust -- assume anonymous if following token not a symbol.
	   ;; Instead of whitespace-p, want to ask if whitespace or something that can't begin a symbol.
	   (make-logic-variable-maker (gentemp *anonymous-prefix*)))
	  ((symbolp (setq next (read stream t nil t)))
	   `(logic-variable-maker
	      ,(intern (concatenate 'string (string *joshua-variable-char*) (string next)))))
	  (t ;; not a symbol
	    (error "Expected symbol (or anonymous) in variable name, got ~S instead."
		   next)))))

;;;
;;; How to construct a Joshua readtable.
;;;

(defun make-joshua-readtable ()
  "Makes a readtable for Joshua."
  ;; Yes, it costs a few extra cycles to use ' instead of #', but this
  ;; way we get patches properly
  ;; first make a standard CL readtable
  (let* ((new-readtable (copy-readtable nil)))
    ;; tell this readtable where it came from ("Daddy, where did I come from?")
    #+(or genera cloe-developer)
    (setf (zl:::si:readtable-appropriate-file-syntax new-readtable) 
          #+cloe-developer :joshua-cloe #+genera :joshua)
    #+(or genera cloe-developer)
    (setf (zl:::si:readtable-name new-readtable) "Standard-Joshua")
    ;; now add our modifications to it
    ;; since it's just a constant recognized by other reader functions in context.
    (set-macro-character *joshua-variable-char*
			 #'read-variable ;+++pr
			 t
			 ;;non-terminating, i.e., only activates at beginning of token
			 new-readtable)
    ; handle equiv sign also for compat.
    ;; (set-macro-character (code-char 30) #'read-variable t new-readtable)
    (set-macro-character *joshua-open-predication-char*
			 #'read-predication ;+++pr
			 nil			;terminating (like parentheses)
			 new-readtable)
    (set-syntax-from-char *joshua-close-predication-char* #\) new-readtable)
    
    (set-macro-character *joshua-open-predication-maker-char*
			 #'read-predication-maker
			 nil			;terminating (like parentheses)
			 new-readtable)
    (set-syntax-from-char *joshua-close-predication-maker-char* #\) new-readtable)
    
    (set-macro-character #\, #'joshua-comma-reader nil new-readtable)
    (set-macro-character #\` #'joshua-top-level-backquote-reader nil new-readtable)
    new-readtable))

(defun install-new-joshua-readtable (&optional (new-readtable (make-joshua-readtable)))
  "Creates a new Joshua readtable and installs it."
  (setq *joshua-readtable* new-readtable)	;put it in the standard place
  ;; innocuous value
  t)						      

(install-new-joshua-readtable)

;;; In Allegro's Emacs interface this is needed to let you put in a readtable
;;; property in the mode-line.
#+allegro
(setf (excl::named-readtable :joshua) *joshua-readtable*)

;;;
;;; Another lisp syntax.  Now you can do :Set Lisp Context Joshua and win.
;;;

#+cloe-developer
(zl:::si:define-lisp-syntax :joshua-cloe
  :readtable *joshua-readtable*
  :packages-must-use '(("cloe-joshua" "cloe") ("cloe-joshua" "lisp"))
  :default-package-use-list '("cloe-joshua" "cloe")
  :user-package "cloe-joshua-user"
  :reasonable-packages ()
  :inherits-from :cloe
  :relative-names (zl:::si:lisp-syntax-relative-names (zl:::si:find-lisp-syntax :cloe))
		  #||'(("user" "cloe-joshua-user"))||#)

#+genera
(sys:define-lisp-syntax :joshua
  :readtable *joshua-readtable*
  :packages-must-use '(("joshua" "common-lisp"))
  :default-package-use-list '("joshua" "common-lisp")
  :user-package "joshua-user"
  :reasonable-packages ()
  :inherits-from :ansi-common-lisp
  :relative-names (si:lisp-syntax-relative-names (si:find-lisp-syntax :ansi-common-lisp)))

;;;
;;; We need a way of establishing a Lisp context. Here we define two
;;; functions: one to set up the context tobe joshua. the other to
;;; return to what was there before. 
;;;
;;; Should we establish a way back to a standard CL?

(defvar *old-readtable* nil
  "This variable is a place holder for the old readtable.")

(defvar *old-package* nil
  "This variable is a place holder for the old package.")

;;; Set up a way to use Joshua. We need to establish the readtable and
;;; know about the packages. 

(defmacro with-joshua-readtable (&body body)
  `(let ((*readtable* *joshua-readtable*))
     ,@body))

(defun enable-joshua ()
  "This function sets up the joshua readtable and establishes the
packages JI, JU, and Joshua as the current context."
  (unless (eql *readtable* *joshua-readtable*)
    (setq *old-readtable* *readtable*
	  *old-package* *package*)
    (setq *readtable* *joshua-readtable*)
    (let ((pul (package-use-list *package*)))
      (unless (or (eq *package* (find-package "JI"))
		  (eq *package* (find-package "JU"))
		  (eq *package* (find-package "JOSHUA"))
		  (member (find-package "JOSHUA") pul)
		  (member (find-package "JI") pul)
		  (member (find-package "JU") pul))
	;; package cannot possibly be a Joshua package, so set it to JU
	(format *error-output* 
                "~& Notice: Package ~A is not a Joshua package. Joshua-User will be used instead." 
                (package-name *package*))
	(setf *package* (find-package "JU"))))))

(defun disable-joshua ()
  "This function returns us to where we were before setting up the
Joshua readtable and the JI, JU, and Joshua packages. "
  (when (and *old-readtable* (eql *readtable* *joshua-readtable*))
    (setf *readtable* *old-readtable*
	  *package* *old-package*)))

#+mcl
(defun compile-joshua-file (&rest stuff)
  (let ((*readtable* *joshua-readtable*))
    (apply #'clim-ds::compile-lisp-file stuff)))

#+mcl
(defun load-joshua-file (&rest stuff)
  (let ((*readtable* *joshua-readtable*))
    (apply #'clim-ds::load-lisp-file stuff)))

#+mcl
(pushnew `(:joshua ,@clim-ds::lisp-file-types compile-joshua-file load-joshua-file)
         clim-ds::*language-descriptions* :test #'equal)

;;;;;;;;;;;;;;;;;

#+(or mcl allegro)
(eval-when (:compile-toplevel :execute :load-toplevel)

(defun joshua-predication-printer (stream object)
  (let ((object (cadr object)))
    (case (car object)
      (backquote (write-string "`[" stream)
                 (loop for first-time = t then nil
                       for thing in (cadr object)
                       unless first-time do (write-char #\space stream)
		     do #+mcl (ccl::write-1 thing stream)
			#+allegro (write thing :stream stream)
                       ))
      (otherwise (write-string "[" stream)
                 (loop for first-time = t then nil 
                       for thing in (cadr object)
                       unless first-time do (write-char #\space stream)
		     do #+mcl (ccl::write-1 thing stream)
			#+allegro (write thing :stream stream))))
    (write-string "]" stream)))

(defun joshua-backquote-printer (stream object) (format stream "`~w" (cadr object)))

(defun joshua-variable-printer (stream object)
  (let ((name (second object)))
    (let ((position (search "anonymous" (string name) :test #'char-equal)))
    (if (and position (= 1 position))
      (write-string "?" stream)
      (write-string (string name) stream)))))

(defun joshua-comma-printer (stream object) (format stream ",~w" (cadr object)))
(defun joshua-comma-dot-printer (stream object) (format stream ",.~w" (cadr object)))
(defun joshua-comma-atsign-printer (stream object) (format stream ",@~w" (cadr object)))

(set-pprint-dispatch '(cons (member predication-maker)) #'joshua-predication-printer)
(set-pprint-dispatch '(cons (member backquote)) #'joshua-backquote-printer)
(set-pprint-dispatch '(cons (member logic-variable-maker)) #'joshua-variable-printer)
(set-pprint-dispatch '(cons (member *backquote-comma-flag*)) #'joshua-comma-printer)
(set-pprint-dispatch '(cons (member *backquote-comma-dot-flag*)) #'joshua-comma-dot-printer)
(set-pprint-dispatch '(cons (member *backquote-comma-atsign-flag*)) #'joshua-comma-atsign-printer)
 )
