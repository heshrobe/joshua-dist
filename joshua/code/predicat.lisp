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
;;; Created 2/14/86 15:46:16 by sgr running on GROUSE at SCRC.

;;;
;;; Define the notion of a predication.
;;;

(in-package :ji)


#+genera
;;; Define a new invisible frame type; a rug under which we can sweep our dirty laundry,
;;; to mix a m-4.
(pushnew 'joshua-internals dbg:*all-invisible-frame-types*)

(defgeneric install-predicate-args-in-map (predication)
  ;; not part of the protocol!
  (:documentation
    "Internal method to keep the statement consistent in the face of recompilation."))

(defvar *support* nil
  ;; we need a set of tms primitives to keep from having to tell users about this.
  "Current value of support structure.  The Default Justification.")

;;;
;;; First, some substrate for defining protocol functions.  Joshua protocol function specs
;;; look like (<name> <model/pred> . <keywords>) where <name> is the name of a protocol function (SAY,
;;; TELL, ASK, etc.), <model/pred> is the model or predicate you're talking about, and
;;; <keywords> is a bunch of options.  <keywords> can be nothing, in which case you're talking
;;; about a primary method.  It can also be flavorish things like :BEFORE, :AFTER, :CASE :FOO, etc.
;;; Finally, it can be :WHOPPER, :WHOPPER-SUBST, or :WRAPPER with the obvious meaning.
;;;

(defvar *joshua-protocol-functions* nil "All the specs for elements of the protocol.")

;;;  The whole business of fspecs here is troublesome.
;;;  The code appears to depend on it.   Should a bunch of FSPEC stuff be in
;;;  BORROWINGS.lisp, then?

(eval-when (:compile-toplevel :execute :load-toplevel)
  (proclaim '(inline joshua-fspec-protocol))
  (defun joshua-fspec-protocol (fspec)
    (first fspec)))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (proclaim '(inline joshua-fspec-model))
  (defun joshua-fspec-model (fspec)
    (second fspec)))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (proclaim '(inline joshua-fspec-method-keywords))
  (defun joshua-fspec-method-keywords (fspec)
    (cddr fspec)))

(defun joshua-protocol-function-spec-p (x)
  ;; check function-specs for reasonableness
  ;; (tell <model> :after) or (tell <model>) or (tell <model> :around)
  (and (consp x)
       (consp (cdr x)) ;length at least 2
       (member (joshua-fspec-protocol x) *joshua-protocol-functions*)
       (symbolp (joshua-fspec-model x))
       (or (null (joshua-fspec-method-keywords x))
	   (member (car (joshua-fspec-method-keywords x))
		   '(:before :after :around)))
       ;; leaving this in causes an I loop -- calling this calls the handler, which
       ;; does a check-type that calls this!
       ;;(sys:validate-function-spec x)
       ))

(deftype joshua-protocol-function-spec ()
  ;; good for error-checking in define-predicate-method
  '(satisfies joshua-protocol-function-spec-p))

;;;
;;; Some parts of the protocol have internal and external names, e.g., TELL and JI::TELL-INTERNAL.
;;; This is a translation mechanism to get from one to the other.
;;;

(defvar *joshua-protocol-name-translations* nil "Alist of (visible-name . internal-name) for protocol.")

(defun protocol-internal-name (external-name)
  ;; translate from external to internal name
  (cdr (assoc external-name *joshua-protocol-name-translations*)))

(defun protocol-external-name (internal-name)
  ;; translate from internal to external name
  (car (rassoc internal-name *joshua-protocol-name-translations*)))

;;;
;;; Sometimes the system will convert function-spec operations to operations
;;;  on a symbol that presumably names the function spec.  This undoes that in the
;;;  cases where it matters.
;;;

#+(or genera cloe-developer)
(defvar *function-spec-translations*
	'((zl:::zl:plist   . zl:::si:function-spec-plist)
	  (zl:::zl:get     . zl:::si:function-spec-get)
	  (zl:::zl:putprop . zl:::si:function-spec-putprop)
	  (zl:::zl:remprop . zl:::si:function-spec-remprop))
  "System occasionally gets overenthusiastic about symbols; this translates back to fspec operators.")

#+(or genera cloe-developer)
(defun translate-to-fspec-operation (operation)
  ;; translate an operation like zl:plist to si:function-spec-plist
  (let ((translated (cdr (assoc operation *function-spec-translations*))))
    (if translated
	translated
	operation)))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (proclaim '(inline predicate-synonyms))
  (defun predicate-synonyms (predicate)
    (get predicate 'predicate-synonyms)))

(defsetf predicate-synonyms (pred) (val)
	 `(setf (get ,pred 'predicate-synonyms) ,val))

;;; this should be called canonical-predicate or something like that
(eval-when (:compile-toplevel :execute :load-toplevel)
  (proclaim '(inline predicate-is-synonym-for))
  (defun predicate-is-synonym-for (synonym-predicate)
    (cond ((symbolp synonym-predicate)
	   (or (get synonym-predicate 'predicate-is-synonym-for)
	       synonym-predicate))
	  ((unbound-logic-variable-p synonym-predicate) 'variable-predicate)
	  ((logic-variable-maker-p synonym-predicate) 'variable-predicate)
	  (t synonym-predicate))))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (proclaim '(inline set-predicate-is-synonym-for))
  (defun set-predicate-is-synonym-for (synonym-predicate new-value)
    (setf (get synonym-predicate 'predicate-is-synonym-for) new-value)))

(defsetf predicate-is-synonym-for set-predicate-is-synonym-for)

(defun record-predicate-synonyms (new-name old-name)
  ;; note the synonym relationship both ways.
  (check-type new-name symbol)
  (check-type old-name symbol)
  (push new-name (predicate-synonyms old-name))
  (setf (predicate-is-synonym-for new-name) old-name))

(defun undefine-predicate-synonym (name)
  ;; how to undo a define-predicate-synonym
  (check-type name symbol)
  (let ((real-pred (predicate-is-synonym-for name)))
    (setf (predicate-synonyms real-pred) (delete name (predicate-synonyms real-pred))))
  (remprop name 'predicate-is-synonym-for))

;;; With Clos all predicate methods can be real clos methods, using
;;; EQL dispatching as well as class dispatching.
;;; These four functions cover all cases that we have now.

(defun method-protocol-definer (fspec args &rest body)
  ;; How we usually define handlers (as methods) for Joshua protocol functions.
  (check-type fspec joshua-protocol-function-spec)
  ;; translate to the internal name, e.g., tell > ji::tell-internal
  (let* ((method-keywords (joshua-fspec-method-keywords fspec))
	 (protocol (protocol-internal-name (joshua-fspec-protocol fspec)))
	 (model (joshua-fspec-model fspec))
         (names (mapcar #'slot-definition-name (CLASS-instance-slots (find-class model)))))
    ;; (format t "~&Model ~a Names ~a" model names)
    (multiple-value-bind (decs real-body) (find-body-declarations body nil)
      (let ((used-ivs nil))
	(loop for form in real-body
	      do (setq used-ivs (jlt:used-variables form :already-known used-ivs :from-set names)))
	`(eval-when (:compile-toplevel :execute :load-toplevel)
	   #+(or genera cloe-developer)
	   (record-source-file-name ',fspec 'define-predicate-method)
	   (defmethod ,protocol ,@method-keywords ((self ,model) ,@args)
	    ,@decs
	    #+(or genera cloe-developer)
	    (declare (zl:::sys:function-parent ,fspec define-predicate-method))
	    (with-slots ,used-ivs self
	     ,@real-body)))))))

(defun eql-dispatched-method-protocol-definer (fspec args &rest body)
  ;; How we usually define handlers (as methods) for Joshua protocol functions.
  (check-type fspec joshua-protocol-function-spec)
  ;; translate to the internal name, e.g., tell > ji::tell-internal
  (let* ((method-keywords (joshua-fspec-method-keywords fspec))
	 (protocol (protocol-internal-name (joshua-fspec-protocol fspec)))
	 (model (joshua-fspec-model fspec)))
    (multiple-value-bind (decs real-body) (find-body-declarations body nil)
      `(defmethod ,protocol ,@method-keywords ((self (eql ',model)) ,@args)
	 ,@decs
	 #+(or genera cloe-developer)
	 (declare (zl:::sys:function-parent ,fspec define-predicate-method))
         ,@real-body))))

(defun matcher-cache-clearing-compile-time-method-protocol-definer (fspec args &rest body)
  (declare (special *matcher-cache*))
  (when (boundp '*matcher-cache*) (clrhash *matcher-cache*))
  (apply #'compile-time-method-protocol-definer fspec args body))

(defun compile-time-method-protocol-definer (fspec args &rest body)
  ;; How we usually define handlers (as methods) for Joshua protocol functions.
  (check-type fspec joshua-protocol-function-spec)
  ;; translate to the internal name, e.g., tell > ji::tell-internal
  (let* ((method-keywords (joshua-fspec-method-keywords fspec))
	 (protocol (protocol-internal-name (joshua-fspec-protocol fspec)))
	 (model (joshua-fspec-model    fspec)))
    (multiple-value-bind (decs real-body) (find-body-declarations body nil)
      `(eval-when (:compile-toplevel :execute :load-toplevel)
	 (defmethod ,protocol ,@method-keywords ((self ,model) ,@args)
	   ,@decs
	   #+(or genera cloe-developer)
	   (declare (zl:::sys:function-parent ,fspec define-predicate-method))
	   ;; Make self be the predication-maker passed into
	   ;; me by the called in the statement slot
	   (let ((self (predication-statement self)))
	     (declare (ignorable self))
	     ,@real-body))))))


;;; Method shower is the flavor system's, 'cause that's too wonderful for words.

#+(or genera cloe-developer)
(defun method-protocol-killer (fspec)
  ;; how to undefine a protocol element
  (check-type fspec joshua-protocol-function-spec)
  (zl:::scl::fundefine fspec))

;;; This is only for Genera Editor Support.

#+(or genera cloe-developer)
(defun method-protocol-function-spec-handler (operation fspec &rest args)
  ;; translate the fspec into the type of function that implements it
  ;; A comment in SYS:SYS;FSPEC says we have to handle these operations:
  ;; FDEFINE definition
  ;; FDEFINEDP
  ;; FDEFINITION
  ;; FDEFINITION-LOCATION
  ;; DEFINITION-HAS-LOCATION-P
  ;; FUNDEFINE
  ;; FUNCTION-PARENT
  ;; COMPILER-FDEFINEDP -- returns T if will be fdefinedp at run time
  ;; GET indicator
  ;; PUTPROP value indicator
  ;; PLIST retrieves the entire PLIST, which for some function specs
  ;;   will also include things put on with regular PUTPROP.
  ;; ---- Added by sgr from examining the source ---
  ;; REMPROP
  ;; VALIDATE-FUNCTION-SPEC
  ;;
  ;; SYS:FUNCTION-SPEC-DEFAULT-HANDLER is also interesting.
  (when (and (not (joshua-protocol-function-spec-p fspec))
	     (eq operation 'zl:::sys:validate-function-spec))
    ;; sometimes dw will call validate-function-spec on, e.g., an ASK
    ;; form, thinking it's a function spec.  this makes that safe, but
    ;; keeps the error checking for other operations.
    (return-from method-protocol-function-spec-handler nil))
  (check-type fspec joshua-protocol-function-spec)
  ;; translate to internal name, e.g., tell -> ji::tell-internal
  (let* ((method-keywords (joshua-fspec-method-keywords fspec))
	 (protocol (protocol-internal-name (joshua-fspec-protocol fspec)))
	 (model (joshua-fspec-model fspec))
	 (rest-of-fspec-arglist (loop for token in (cdr (zl:::scl::arglist protocol))
				      until (member token zl:::scl:lambda-list-keywords)
				      collect t)))
    ;; It doesn't make sense to pass on the function-parent operation,
    ;; as the define-predicate-method is usually the function-parent of
    ;; the internal method. If we pass it on we effectively declare the
    ;; predicate method the function-parent of itself, which confuses m-.
    (unless (eq operation 'zl:::sys:function-parent)
      (apply (translate-to-fspec-operation operation)
	     `(method ,protocol
		      (, model ,@rest-of-fspec-arglist)
		      ,@method-keywords)
	     args))))

;;;
;;; These are for some of the protocol elements that are implemented as
;;; property-list functions.
;;;

(eval-when (:compile-toplevel :execute :load-toplevel)
  (proclaim '(inline no-method-keywords))
  (defun no-method-keywords (x)
    (null (cddr x))))

(deftype joshua-plist-function-spec ()
  ;; type for plist fspecs
  `(and joshua-protocol-function-spec
	(satisfies no-method-keywords)))

;;;
;;; Now the flavor this is all about!
;;;

;;;
;;; Stuff to manipulate the bits instance variable of a predication.
;;;

;;; DEFBITFIELDS is defined in BORROWINGS, although it isn't one strictly speaking.
(defbitfields predication-bits
  (truth-value   +unknown+ :byte (byte 2 0))	;the current state in the database
  (has-been-in-database 0  :byte (byte 1 2))	;tells whether truth-value is interesting
  (has-logic-variables 0   :byte (byte 1 3))	;set by the make-instance method
  (spare               0   :byte (byte 1 4))
  (ive-been-in-before  0   :byte (byte 1 5))
  (tms-bits            0   :byte (byte 6 6))
  (ive-been-untold     0   :byte (byte 1 12)))

(defgeneric init-plist (predication)
  (:method-combination append))

(defclass predication ()
  ;; STATEMENT is a list representing the statement that the
  ;; predication makes.  It's used in looping over the args.
  ;; BITS is a fixnum's worth of bits; see the predication-bits defstruct.
  ((statement :reader predication-statement :initarg :statement)
   (bits :accessor predication-bits :initform (make-predication-bits)
	 :initarg :bits))
  )

(defmethod init-plist append ((p predication))
  (list :statement (predication-statement p)
	:bits (predication-bits p)))


;;;
;;; Here 'cause it needs to be around for macroexpansion of ASK's.
;;;

(eval-when (:compile-toplevel :execute :load-toplevel)
  (proclaim '(inline predicationp))
  (defun predicationp (object) (typep object 'predication)))


;;; Want a (probably (initialize-instance :after)) method to change
;;; prematurely dotted tails into anonymi.

(defmethod predication-predicate ((self predication))
  ;; how to get at the predicate of a predication abstractly.
  (car (predication-statement self)))


(defmethod describe-object :after ((self predication) stream)
  ;; arrange for this to happen after the usual describe stuff from flavor:vanilla
  ;; so that mere humans (like me!) can read the bits field.
  (with-slots (bits) self
    (format
      stream
       "~&~S has bits #O~O: Truth value ~a, ~[no ~;~]logic variables,~& has~[ not~;~] been in the database, has~[ not~;~] been in before, has~[ not~;~] been UNTELL'd,~& TMS bits are #O~O."
      self bits
      (truth-value-name (predication-bits-truth-value bits))
      (predication-bits-has-logic-variables bits)
      (predication-bits-has-been-in-database bits)
      (predication-bits-ive-been-in-before bits)
      (predication-bits-ive-been-untold bits)
      (predication-bits-tms-bits bits))))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (proclaim '(inline predication-logic-variable-free-p))
  (defun predication-logic-variable-free-p (predication)
    (zerop (predication-bits-has-logic-variables (predication-bits predication)))))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (proclaim '(inline predication-truth-value))
  (defun predication-truth-value (predication)
    (predication-bits-truth-value (predication-bits predication))))

(defmethod tms-bits ((self predication))
  (with-slots (BITS) self
  ;; give the tms-bits; used by TMS writers
  (predication-bits-tms-bits bits)))

(defmethod (setf tms-bits) (new-value (self predication))
  (with-slots (BITS) self
  ;; set the TMS bits field; used by TMS writers
  (setf (predication-bits-tms-bits bits) new-value)))

(defmethod been-in-before-p ((self predication))
  (with-slots (BITS) self
  ;; wheter or not this predication's been in before
    (not (zerop (predication-bits-ive-been-in-before bits)))))

(defmethod not-been-in-before-p ((self predication))
  (with-slots (BITS) self
    ;; wheter or not this predication's been in before
     (zerop (predication-bits-ive-been-in-before bits))))

(defmethod (setf been-in-before-p) (new-value (self predication))
  (with-slots (BITS) self
  ;; set the ive-been-in-before flag
  (setf (predication-bits-ive-been-in-before bits) (if new-value 1 0))))

(defun has-been-in-database-p (predication)
  (and (predicationp predication)
       (not (zerop
	      (predication-bits-has-been-in-database
	       (predication-bits predication))))))

(defmethod (setf been-in-database-p) (new-value (predication predication))
  (setf (predication-bits-has-been-in-database (predication-bits predication)) (if new-value 1 0))
  )

;;; This is used to examine source code, not objects in the execution
;;; environment.  It is used by tell, ask, and the rule compilers.

(defun logic-variable-makers-in-thing (thing)
  ;; Return list of all variables in a predication, list, or variable.
  ;; Good for analyzing rule trigger patterns.
  ;; Deals properly with embedded lists & predications.
  (let ((variable-list nil))
    (labels ((collect-variables (thing)
	       (loop for rest = (if (predication-maker-p thing)
				    ;; convert thing to a list we can loop over, or just an atom
				    (second thing)	;predication
				    thing)	;list or atom
			      then (cdr rest)
		     ;; stop when it's not a list
		     while (and (consp rest) (not (logic-variable-maker-p rest)))
		     for next = (car rest)
		     doing (cond
			     ((logic-variable-maker-p next)
			       ;; found a variable, so remember it
			       (pushnew (logic-variable-maker-name next) variable-list))
			     ((or (listp next) (predication-maker-p next))
			      ;; recurse over lists and predications
			      (collect-variables next)))
		     finally
		       (cond
			 ((logic-variable-maker-p rest)
			   ;; found a variable, so remember it
			   (pushnew (logic-variable-maker-name rest) variable-list))
			 ((predication-maker-p rest)
			   ;; recurse over lists and predications
			   (collect-variables rest))))))
      (collect-variables thing)
      variable-list)))

;;; This produces the variables in some order, which is used later.
;;; Use it to find variables, so you can later construct the maps.
(defun logic-variables-in-thing (thing)
  ;; Return list of all variables in a predication, list, or variable.
  ;; Good for analyzing rule trigger patterns.
  ;; Deals properly with embedded lists & predications.
  (let ((variable-list nil))
    (labels ((collect-variables (thing)
	       (loop for rest = (if (predicationp thing)
				    ;; convert thing to a list we can loop over, or just an atom
				    (predication-statement thing)	;predication
				    thing)	;list or atom
			      then (cdr rest)
		     ;; stop when it's not a list
		     while (consp rest)
		     for next = (car rest)
		     doing (typecase next
			     (unbound-logic-variable
			       ;; found a variable, so remember it
			       (pushnew next variable-list :test #'compare-logic-variables))
			     ((or list predication)
			      ;; recurse over lists and predications
			      (collect-variables next)))
		     finally
		       (typecase rest
			 (unbound-logic-variable
			   ;; found a variable, so remember it
			   (pushnew rest variable-list :test #'compare-logic-variables))
			 (predication
			   ;; recurse over lists and predications
			   (collect-variables rest))))))
      (collect-variables thing)
      variable-list)))

(defun named-logic-variables-in-thing (thing)
  ;; like variables-in-thing, but ignores anonymous variables.
  (loop for variable in (logic-variables-in-thing thing)
	when (logic-variable-name variable) collect variable))

(defun contains-logic-variables-p (thing)
  ;; like variables-in-thing, except you only want a yes-or-no answer
  (typecase thing
    (unbound-logic-variable t)			;*****   ??
    (cons
      ;; check for lv's in a list
      (loop for items = thing then (cdr items)
	    while (consp items)
	    thereis (contains-logic-variables-p (car items))
	    ;; check tail variables
	    finally (return (contains-logic-variables-p items))))
    (predication
      ;; check for lv's in a predication
      (if (predication-logic-variable-free-p thing)
	  ;; known in advance to be variable-free.
	  nil
	  ;; have to check now, since some vars might have been unified.
	  (contains-logic-variables-p (predication-statement thing))))
    (otherwise
      ;; not a structure that Joshua should take apart and not a lv, so NIL
      nil)))

(defmethod change-of-class ((self predication))
  ;; re-initialize the map to the new arguments
  (install-predicate-args-in-map self))

(defmethod initialize-instance :after ((self predication) &key (initialize t)
				       &allow-other-keys )
  ;; (declare (ignore ignore))
  (when initialize
    (with-slots (STATEMENT BITS) self
      (install-predicate-args-in-map self)
      (labels ((list-has-logic-variables-p (list)
		 (loop for items = list then (cdr items)	;"on" doesn't deal w/ tail vars
		       while (consp items)
		       doing (typecase (car items)
			       (unbound-logic-variable (return t))
			       (predication (unless (predication-logic-variable-free-p (car items))
					      (return t)))
			       (cons (when (list-has-logic-variables-p (car items))
				       (return t))))
		       finally
			 (when (typep items 'unbound-logic-variable) (return t)))))
	(setf (predication-bits-has-logic-variables bits)
	      (if (list-has-logic-variables-p statement) 1 0))))))

;;; BUG: this should record the entire bits field.
(defmethod make-load-form ((self predication) #+(or mcl allegro sbcl) &optional #+(or mcl allegro sbcl) environment)
  #+(or mcl allegro sbcl) (declare (ignore environment))
  (with-slots (STATEMENT) self
    ;; Makes an instance of the right flavor.  Relies on make-instance method, above,
    ;; for destructuring and such.  Indexing is somebody else's job.
    `(make-predication ',(copy-list statement))))

(defvar *print-predication-top-level-p* t)

;;; used to be a defun-method
(defun print-predication-internal (predication stream)
  ;; subroutine shared by a couple of print-thingys.
  (let ((*print-readably* t))
  (with-slots (statement) predication
    (let* ((*print-predication-top-level-p* nil))
      (labels ((print-dereferenced-list (stuff open close)
		 (write-char open stream)			;opening bracket
		 (loop for rest-args = stuff then (cdr rest-args)
		       for first = t then nil
		       while (consp rest-args)
		       do (unless first (write-char #\space stream))
			  (let ((val (joshua-logic-variable-value (car rest-args))))
			    (cond
			      ((joshua-logic-variable-p val)	;meaning unbound at this point
			       (write-string (string (joshua-logic-variable-name val)) stream))
			      ((listp val)
			       (print-dereferenced-list val #\( #\)))
			      (t
				(prin1 val stream))))
		       finally
			 (when rest-args
			   ;; there is a tail variable
			   (write-string " . " stream)
			   (let ((val (joshua-logic-variable-value rest-args)))
			     (if (joshua-logic-variable-p val)
				 (write-string (string (joshua-logic-variable-name val)))
				 (prin1 val stream))))
			 (write-char close stream))))
	(when statement
	  (print-dereferenced-list statement
				   *joshua-open-predication-char*
				   *joshua-close-predication-char*)))))))

(defmethod print-object ((self predication) stream)
  (with-slots (BITS) self
  ;; how to print a predication with the ugly-printer
  (when (and (not (zerop (predication-bits-has-been-in-database bits)))
	     *print-predication-top-level-p*)
    (truth-value-case (predication-bits-truth-value bits)
      (+true+)
      (+false+ (write-char #\~ stream))
      (+unknown+ (write-char #\? stream))
      (+contradictory+ (write-string "<>" stream))))
  (print-predication-internal self stream)))

(defmethod print-without-truth-value ((self predication) &optional (stream *standard-output*))
  ;; Print the predication as if it were +TRUE+.  If stream is NIL, print to a string
   (if (null stream)
	;; wants to print to a string
	(with-output-to-string (stream)
	  (print-predication-internal self stream))
	;; wants to print to a real stream
	(print-predication-internal self stream)))

(defun make-predication (statement &optional area)
  ;; make a predication
  (declare (ignore area))
  (let* ((predicate (car statement))
	 (real-predicate (predicate-is-synonym-for predicate)))
    (if (find-class real-predicate)
	(make-instance real-predicate :statement statement)
	(error "~S is an undefined predicate" predicate))))

(defun make-predication-form-p (form)
  (and (consp form) (eq (car form) 'make-predication)))

(defmacro with-predication-maker-parsed ((predicate-var args) form &body body)
  ;; parse up a form like (make-predication '(...)).  Used in rule
  ;; compiler, in the places where it has to be generic over forms,
  ;; instead of instances.
  ;; (declare (ignore env))
  `(let ((form ,form))
	(check-type form (satisfies predication-maker-p)
		    "a predication source designator.")
	(multiple-value-bind (,predicate-var ,args)
			     (parse-predication-maker form)
			     ,@body)))

;;; Need :format-self version of this.

;;; Backquotage of [] expressions results in a make-predication form, not an predication,
;;; since it depends on run-time data.  Thus we have to be able to pretty-print calls
;;; to make-predication.

(defvar *print-make-predication-forms-prettily* nil)

;;;
;;; Here's the machinery for defining predicates and their associated models.
;;;

;;;
;;; First some subroutines for writing some of the methods, processing args, and so on.
;;;

(defun length-dotted (possibly-dotted-list)
  ;; "length" and last cons of possibly-dotted-list
  #-sbcl(declare (values length dotted-tail))
  (loop initially (when (atom possibly-dotted-list) (return (values 0 possibly-dotted-list)))
	for cons = possibly-dotted-list then (cdr cons) ;faster than "on"
	counting cons into length
	while (consp (cdr cons))
	finally (return (values length (cdr cons)))))

(defun check-predication-args-all-required (statement name number-args)
  ;; check args for predications whose args are all required.  Don't care about value, just error check.
  (multiple-value-bind (length tail) (length-dotted (cdr statement))
    (cond (tail
	    ;; bad tail
	    (error "Dotted tail ~S given to predicate ~S, which has no &REST argument: ~S"
		   tail name statement))
	  ((/= length number-args)
	   ;; wrong number args
	   (error "~:[More than~;Fewer than~] ~D arg~:P supplied to predicate ~S: ~S"
		  (< length number-args) number-args name statement)))))

(defun check-predication-args-optional-no-rest (statement name min-args max-args defaulter)
  ;; check args for predications with some &OPTIONAL args, but no &REST arg.  May return new map.
  #-sbcl(declare (dynamic-extent defaulter))
  (multiple-value-bind (length tail) (length-dotted (cdr statement))
    (cond (tail
	    ;; bad tail
	    (error "Dotted tail ~S given to predicate ~S, which has no &REST argument: ~S"
		   tail name statement))
	  ((<= min-args length max-args)
	   ;; acceptable number of args, check out defaulting.
	   (if (or (= length max-args) (null defaulter))
	       ;; all args supplied, or all defaults are NIL, so no need to re-cons
	       statement
	       ;; too few args and non-trivial defaulting, so re-cons statement
	       (apply defaulter statement)))
	  ((< length min-args)
	   ;; too few args
	   (error "Fewer than ~D arg~:P supplied to predicate ~S: ~S" min-args name statement))
	  (t
	   ;; too many args
	   (error "More than ~D arg~:P supplied to predicate ~S: ~S" max-args name statement)))))


(defun check-predication-args-no-optional-rest (statement name min-args rest-defaulter)
  ;; check args for predications with &REST arg, but no &OPTIONAL args.
  #-sbcl (declare (dynamic-extent rest-defaulter))
  (multiple-value-bind (length tail) (length-dotted (cdr statement))
    (cond ((< length min-args)
	   ;; too few args
	   (error "Fewer than ~D arg~:P supplied to predicate ~S: ~S" min-args name statement))
	  ((and (= length min-args) (null tail) rest-defaulter)
	   ;; exactly right # positional args supplied, but no tail for &REST and there is a default
	   (append statement (funcall rest-defaulter)))
	  (t
	    ;; either enough args supplied, or there's a tail, or the &REST default is NIL
	    statement))))

(defun check-predication-args-optional-rest (statement name min-args max-args
							  full-defaulter rest-defaulter)
  ;; check args for predications with both &OPTIONAL and &REST args
  #-sbcl(declare (dynamic-extent full-defaulter rest-defaulter))
  (multiple-value-bind (length tail) (length-dotted (cdr statement))
    (cond ((< max-args length)
	   ;; there are plenty of args to go around, no defaulting necessary
	   statement)
	  ((= length max-args)
	   ;; just enough to fill positional slots
	   (if (or tail (null rest-defaulter))
	       ;; either a tail was supplied, or default &REST arg is NIL
	       statement
	       ;; have to default the &REST arg
	       (append statement (funcall rest-defaulter))))
	  ((< length min-args)
	   ;; too few args
	   (error "Fewer than ~D arg~:P supplied to predicate ~S: ~S" min-args name statement))
	  (tail
	    ;; prematurely supplied tail variable
	    (error "Dotted tail in predication provided before &REST arg position: ~S"
		   statement))
	  (full-defaulter
	    ;; non-trivial default for some &OPTIONAL arg or the &REST arg
	    (apply full-defaulter statement))
	  (t
	    ;; all defaults are trivial, so no need to re-cons
	    statement))))

;;; Assume out-of-place tail variables replaced already by sufficient anonymous variables
(defun write-argument-mapping-method-for-predicate
       (name required-arg-specs optional-arg-specs rest-arg-spec destructure-ivs fixed-arglist)
  ;; write a method that defaults unsupplied arguments for which the
  ;; user specified defaults, and installs forwarding pointers to the
  ;; argument map in instance variables of destructured args.
  (flet ((write-defaulting-code ()
	   ;; write the code that checks the number of args and ensures defaults are enforced
	   (cond ((and (null optional-arg-specs) (null rest-arg-spec))
		  ;; all args required, so no defaulting -- just check number
		  `(check-predication-args-all-required
		     .statement. ',name ,(length required-arg-specs)))
		 ((and optional-arg-specs (null rest-arg-spec))
		  ;; some &optionals, no &rest -- therefore no tail variable
		  `(setq .statement.
			 ;; will re-cons argument map if defaulting dictates it has to.
			 ,(if (some #'cadr optional-arg-specs)
			      `(flet ((continuation (predicate-name ,@fixed-arglist)
					;; how to default args for this predication
					(list predicate-name
					      ,@required-arg-specs
					      ,@(mapcar #'car optional-arg-specs))))
				 (declare (dynamic-extent #'continuation))
				 (check-predication-args-optional-no-rest
				   .statement. ',name
				   ,(length required-arg-specs)
				   ,(+ (length required-arg-specs) (length optional-arg-specs))
				   #'continuation))
			      `(check-predication-args-optional-no-rest
				 .statement. ',name
				 ,(length required-arg-specs)
				 ,(+ (length required-arg-specs) (length optional-arg-specs))
				 nil))))
		 ((and (null optional-arg-specs) rest-arg-spec)
		  ;; rest, but no optionals -- might be a tail variable
		  `(setq .statement.
			 ;; will re-cons .statement. if defaulting dictates it has to.
			 ,(if (cadr rest-arg-spec)
			      `(flet ((continuation () ,(cadr rest-arg-spec)))
				 (declare (dynamic-extent #'continuation))
				 (check-predication-args-no-optional-rest
				   .statement. ',name
				   ,(length required-arg-specs)
				   #'continuation))
			      `(check-predication-args-no-optional-rest
				 .statement. ',name
				 ,(length required-arg-specs)
				 nil))))
		 (t
		   ;; both optionals and rest -- might be a tail variable
		  (let* ((rest-defaulting (cadr rest-arg-spec))
			 (optional-defaulting (some #'cadr optional-arg-specs))
			 (any-defaulting (or rest-defaulting optional-defaulting))
			 (basic-statement
			   `(check-predication-args-optional-rest
			      .statement. ',name
			      ,(length required-arg-specs)
			      ,(+ (length required-arg-specs) (length optional-arg-specs))
			      ;; there is a nontrivial default somewhere (&OPTIONAL or &REST)
			      ,(when any-defaulting `#'continuation)
			      ,(when rest-defaulting `#'rest-continuation))))
		    (when any-defaulting
		      (setq basic-statement
			    `(flet (,(when rest-defaulting `(rest-continuation () ,(cadr rest-arg-spec)))
				    ,(when any-defaulting `(continuation (predicate-name ,@fixed-arglist)
							    (list* predicate-name
								   ,@required-arg-specs
								   ,@(mapcar #'car optional-arg-specs)
								   (or ,(car rest-arg-spec) ,(cadr rest-arg-spec))))))
			       (declare (dynamic-extent ,(when rest-defaulting `#'rest-continuation)
							,(when any-defaulting `#'continuation)))
			       ,basic-statement)))
		    ;; will re-cons statement if defaulting dictates it has to.
		   `(setq .statement. ,basic-statement)))))
	 (write-destructuring-code ()
	   ;; write code for destructuring elements of argument map into instance variables
	   (when destructure-ivs
	     (let* ((positional-arg-names
		      (append required-arg-specs (mapcar #'car optional-arg-specs)))
		    (locations
		      (sort (loop for name in destructure-ivs
				  for pos = (position name positional-arg-names)
				  when pos collect (cons name pos))	;deal with "&" words
			    #'< :key #'cdr))
		    (rest-arg-position (when (member (car rest-arg-spec) destructure-ivs)
					 (length positional-arg-names))))
	       (when rest-arg-position
		 (setq locations (nconc locations
					(list (cons (car rest-arg-spec) rest-arg-position)))))
	       `((let ((map (cdr .statement.)))	;flush predicate
		   ,@(loop with current-position = 0
			   for (name . name-pos) in locations
			   when (/= current-position name-pos)
			     collect
			       `(setq map (nthcdr ,(- name-pos current-position) map))
			   do (setq current-position name-pos)
			   if (and rest-arg-position (= name-pos rest-arg-position))
			     collect `(setq ,(car rest-arg-spec) map)
			   else
			     collect `(if (consp map)
					  (setq ,name (car map))
					  ;; we've run off the end of the map, and are
					  ;; trying to destructure an &optional whose
					  ;; default was NIL, anyway.
					  (setq ,name nil)))))))))
    `((defmethod install-predicate-args-in-map ((self ,name))
	;; this will always exist, to check the right number of arguments.
	#+(or genera cloe-developer)
	(declare (zl:::sys:function-parent ,name define-predicate))
	(with-slots ((.statement. statement) ,@destructure-ivs) self
	  ,(write-defaulting-code)
	  ;; this will only exist if you use :destructure-into-instance-variables.
	  ,@(write-destructuring-code))))))

(defmacro with-statement-destructured (arglist predication &body body)
  ;; useful for spreading some args into local variables if you don't want to
  ;; do :DESTRUCTURE-INTO-INSTANCE-VARIABLES.
  `(destructuring-bind ,arglist (cdr ,(if (or (null predication)
					      (eq predication 'self))
					  '(slot-value self 'statement)
					  `(predication-statement ,predication)))
     ,@body))

(defun process-predicate-args (args)
  #-sbcl(declare (values fixed-arglist required-arg-specs optional-arg-specs rest-arg-spec))
  ;; &REST and &OPTIONAL as in CL lambda-lists are also allowed, with the usual interpretations.
  ;;
  ;; Return values: a "fixed" arglist, i.e., one in which the &rest arg is not defaulted.
  ;;                names of required arguments
  ;;                names of optional arguments & their defaults
  ;;                name of rest argument
  ;;                rest arg default value
  (flet ((is-acceptable-arg-name (thing)
	   ;; thing must be either a proper symbol or a logic variable
	   (and (symbolp thing)
		(not (member thing lambda-list-keywords)))))
    (loop with context = 'required ;required, optional, rest, or rest-in-peace
	  with rest-arg-spec = nil
	  for arg in args
	  when (eq context 'rest-in-peace)
	    ;; found garbage after rest
	    do (error "Too much stuff after an &REST in predicate arglist ~S" args)
	  ;; now collect things into a "fixed" arglist, which has the rest arg undefaulted
	  ;; and logic variables replaced by their names.
	  if (consp arg)
	    if (eq context 'rest)
	      ;; collect just rest arg, not default
	      collect (first arg) into fixed-arglist
	    else ;loop indentation bug puts this in wrong place, but code is right
	      ;; collect defaulted arg
	      collect (list (first arg) (second arg)) into fixed-arglist
	  ;; collect undefaulted arg or lambda-keyword
	  else collect arg into fixed-arglist
	  if (eq arg '&optional)
	    ;; found &optional marker
	    do (unless (eq context 'required)
		 (error "&OPTIONAL in bad position in predicate arglist ~S; current context is ~A"
			args context))
	       (setq context 'optional)
	  else if (eq arg '&rest)
		 ;; found &rest marker
		 do (setq context 'rest)
	  else if (and (is-acceptable-arg-name arg) (eq context 'required))
		 ;; required variable
		 collect arg into required-arg-specs
	  else if (and (is-acceptable-arg-name arg) (eq context 'optional))
		 ;; undefaulted optional variable.  NIL is the "default default".
		 collect (list arg nil) into optional-arg-specs
	  else if (and (is-acceptable-arg-name arg) (eq context 'rest))
		 ;; undefaulted rest variable.  NIL is the "default default".
		 do (setq rest-arg-spec (list arg nil)
			  context       'rest-in-peace)
	  else if (and (consp arg)
		       (= (length arg) 2.)
		       (is-acceptable-arg-name (first arg))
		       (eq context 'rest))
		 ;; defaulted rest variable
		 do (setq rest-arg-spec (list (first arg) (second arg))
			  context       'rest-in-peace)
	  else if (and (consp arg)
		       (= (length arg) 2.)
		       (is-acceptable-arg-name (first arg))
		       (eq context 'optional))
		 ;; defaulted optional arg
		 collect (list (first arg) (second arg)) into optional-arg-specs
	  else do (error "Bad item ~S in predicate arglist; should be arg-symbol, &OPTIONAL, ~
                          (arg-symbol default), or &REST." arg)
	  finally (return (values fixed-arglist required-arg-specs optional-arg-specs rest-arg-spec)))))

;;;
;;; Now define some places to stash info about the predicate.
;;; We use a hash-table of all predicates
;;;

(defvar *all-predicates* (make-hash-table))

;;; The following stolen from SYSDEF.lisp
(eval-when (:compile-toplevel :execute :load-toplevel)
  ;; This is required for SBCL which insists that defconstant maintains eql'ness
  ;; if it happends more than once.  Since the system-definition compiles and then
  ;; loads each file, we do see this twice during compilation.  There is a specific
  ;; error thrown with an appropriate restart, but this is simpler.
  (defmacro define-constant (name value &optional doc)
    `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
       ,@(when doc (list doc))))
  (define-constant +arg-desc-max-args+ (byte 8 0)) ;sys:%%arg-desc-max-args
  (define-constant +arg-desc-min-args+ (byte 8 8)) ;sys:%%arg-desc-min-args
  (define-constant +arg-desc-rest-arg+ (byte 1 16)) ;sys:%%arg-desc-rest-arg
  )

(defstruct (predicate-descriptor)
  args-info
  arglist)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (proclaim '(inline predicate-max-args))
  (defun predicate-max-args (args-info)
    (ldb +arg-desc-max-args+ args-info)))

(define-setf-method predicate-max-args  (x)
  (defbitfields-setf-expander x `',+arg-desc-max-args+ 'predicate-max-args))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (proclaim '(inline predicate-min-args))
  (defun predicate-min-args (args-info)
    (ldb +arg-desc-min-args+ args-info)))

(define-setf-method predicate-min-args (x)
  (defbitfields-setf-expander x `',+arg-desc-min-args+ 'predicate-min-args))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (proclaim '(inline predicate-rest-arg))
  (defun predicate-rest-arg (args-info)
    (ldb +arg-desc-rest-arg+ args-info)))

(define-setf-method predicate-rest-arg (x)
  (defbitfields-setf-expander x `',+arg-desc-rest-arg+ 'predicate-rest-arg))

(defun find-predicate-arglist (predicate)
  ;; given the name, find the arglist or nil if none.
  (let ((synonym (predicate-is-synonym-for predicate)))
    (let ((descriptor (gethash (or synonym predicate) *all-predicates*)))
      (when descriptor
	(values (predicate-descriptor-arglist descriptor) t)))))

#+allegro
(def-fwrapper wrap-arglist (symbol)
  (handler-case (call-next-fwrapper)
    (error nil
      (or
       (find-predicate-arglist symbol)
       (error "~s  is not a function, macro or predicate" symbol)))
    (:no-error (answer &optional flag) (values answer flag))))

#+allegro
(fwrap 'excl:arglist 'wrap-arglist-1 'wrap-arglist)

;;; I think this would work in Emacs without autodoc, but apparently we get autodoc
#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-arglist-of-predication-wrapper  (original-function function-name)
    (let ((predicate-arglist (find-predicate-arglist function-name)))
      (if (null predicate-arglist)
          (funcall original-function function-name)
          predicate-arglist)))

  (sb-int:encapsulate 'sb-introspect:function-lambda-list 'joshua 'get-arglist-of-predication-wrapper)

  (defun valid-operator-symbol-p-wrapper (original-function symbol)
    (if (find-predicate-arglist symbol)
        t
        (funcall original-function symbol)))

  (sb-int:encapsulate 'swank::valid-operator-symbol-p 'joshua 'valid-operator-symbol-p-wrapper)

  (defun extra-keywords-wrapper (original-function operator args)
    (if (find-predicate-arglist operator)
        nil
        (funcall original-function operator args)))

  (sb-int:encapsulate 'swank::extra-keywords 'joshua 'extra-keywords-wrapper))





(defmethod virtual-data-p ((self predication))
  (with-slots (BITS) self
  (or (not (zerop (predication-bits-has-logic-variables bits)))
      #+genera (si:in-stack-or-temporary-area self))))

(defmethod update-after-copying ((self predication) new-predication) new-predication)

(defun copy-object-if-necessary (object)
  (let ((variables nil))
    (labels ((copy (object)
	       (setq object (joshua-logic-variable-value object))
	       (typecase object
		 (predication
		   (if (virtual-data-p object)
		       (let ((new-object (make-predication (copy (predication-statement object)) nil)))
			 (update-after-copying object new-object)
			 new-object)
		       object))
		 (cons
		   ;; just copy the list, 'cause you don't know what bound variables
		   ;; might be lurking -- but be nice and cdr-code the copy
		   ;; ---- someday maybe look for logic variables, like copy-unified-value
		   ;; ---- tries to, so we won't copy lists unnecessarily
		   (multiple-value-bind (length-to-copy tail)
		       (loop for object-cons = object then (cdr object-cons)
			     while (consp object-cons)
			     count t into length
			     finally (return (values length object-cons)))
		     (let ((copy (make-list (if tail (1+ length-to-copy) length-to-copy))))
		       (loop for copy-cons = copy then (cdr copy-cons)
			     for object-cons = object then (cdr object-cons)
			     repeat (1- length-to-copy)
			     do (setf (car copy-cons) (copy (car object-cons)))
			     finally (setf (car copy-cons) (copy (car object-cons)))
				     (when tail
				       (setf (cdr copy-cons) (copy tail))))
		       copy)))
		 (unbound-logic-variable
		   (let ((new-variable-assoc (assoc object variables :test #'eq)))
		     (if new-variable-assoc
			 (cdr new-variable-assoc)
			 (let ((new-variable (make-unbound-logic-variable
					       (logic-variable-name object))))
			   (push (cons object new-variable) variables)
			   new-variable))))
		 (otherwise object))))
      (copy object))))

;;;
;;; Now the guys who are the programmer-visible tip of this iceberg.
;;;

(eval-when (:load-toplevel :execute)
(defparameter *models* nil "List of all known models."))

(defun undefine-predicate-model (name)
  ;; remove this model
  (setq *models* (delete name *models*))
  (setf (find-class name) nil))

(def-defining-form define-predicate-model
  :definer
  ((name slot-specs components &body options)
   "Define a model."				;
   #+genera (declare (zwei:indentation 1 7 3 1))	;same as defflavor
   ;; force it to be built on predication
   `(progn
      ;; Lucid's Lisp 4.0.2 evaluates defclass in the compile environment
      ;; This is wrong but it's what they do.  This patches around
      ;; all the stupid class defined twice warnings that their bug creates.
      #+lucid (defclass ,name ,components ,slot-specs ,@options)
      (eval-when (:load-toplevel :execute :compile-toplevel)
	(defclass ,name ,components ,slot-specs ,@options)
	(pushnew ',name *models*))
      #+(or genera cloe-developer)
      (record-source-file-name ',name 'define-predicate-model)
      ))
  :killer undefine-predicate-model
  :type-name "Predicate Model")


(defun undefine-predicate (name)
  ;; how to kill a define-predicate form
  ;; remove name from hash-table of all predicates
  (remhash name *all-predicates*)
  ;; clobber the flavor
  (when (find-class name nil)
    (setf (find-class name) nil)))

(def-defining-form define-predicate
   :definer
   ((name args &optional (model-and-other-components '(default-predicate-model))
	  &body options
	  &aux alternative-arglist constructor-name destructure-ivs)
    "Define a predicate."
    #+genera (declare (zwei:indentation 1 7 3 1))	;same as defclass
    (progn constructor-name)
    (multiple-value-bind (fixed-arglist required-arg-specs optional-arg-specs rest-arg-spec)
	(process-predicate-args args)
      (setq options (copy-list options))	;so we can smash it
      (flet ((maybe-car (x) (if (consp x) (car x) x)))	; deal with option syntax
	;; figure out what arguments he wants spread into instance variables.
	(setq destructure-ivs (find :destructure-into-instance-variables
				    options :key #'maybe-car))
	(setq options (delete :destructure-into-instance-variables options :key #'maybe-car))
	(cond ((listp destructure-ivs)		;includes nil, but that's ok
	       ;; flush the keyword and cdr is list of what he wants destructured
	       (setq destructure-ivs (cdr destructure-ivs)))
	      ((eq destructure-ivs :destructure-into-instance-variables)
	       ;; wants them all destructured
	       (setq destructure-ivs
		     (append required-arg-specs (mapcar #'maybe-car optional-arg-specs)))
	       (when rest-arg-spec
		 ;; put rest arg in place where it'll be in natural position
		 (setq destructure-ivs (nconc destructure-ivs (list (first rest-arg-spec))))))
	      (t
		(error "Bad :DESTRUCTURE-INTO-INSTANCE-VARIABLES option: ~S" destructure-ivs)))
	;; see if the :arglist option was given, and remove it if so
	(setq alternative-arglist (second (find :arglist options :key #'maybe-car)))
	(when alternative-arglist (setq options (delete :arglist options :key #'maybe-car)))
	;; see if the :area-keyword option was given, and parse it if so
	(setq options (delete :area-keyword options :key #'maybe-car))
	)
      (unless (member 'predication model-and-other-components)
	(setq model-and-other-components (append model-and-other-components '(predication))))
      ;; error check for undefined class in model-and-other-components
      ;; to avoid later potential type redefinition problems
      (loop for class-name in model-and-other-components do
	(when (not (find-class class-name nil))
	  (error "~s is not the name of a class or predicate model" class-name)))
      ;; now we grok the implications  of the arglist
      (let* ((argument-mapping-methods (write-argument-mapping-method-for-predicate
					 name required-arg-specs optional-arg-specs
					 rest-arg-spec destructure-ivs fixed-arglist)))
	`(progn
	   #+(or genera cloe-developer)
	   (record-source-file-name ',name 'define-predicate)
	   ;; Lucid does the defclass at compile time whether you ask it to or not.
	   #+lucid (defclass ,name ,model-and-other-components ,destructure-ivs)
	   #-lucid
	   (eval-when (:compile-toplevel :load-toplevel :execute)
	     (defclass ,name ,model-and-other-components ,destructure-ivs))
	   ;; write method that keeps the statement consistent
	   ;; used in looping over args to a predication
	   ,@argument-mapping-methods
	   ;; set up some debug-info for the class...
	   (setup-predicate-args-info ',name
				      ,(length required-arg-specs)
				      ,(length optional-arg-specs)
				      ,(not (null rest-arg-spec))
				      ',(if alternative-arglist
					    alternative-arglist args))
	   ',name)
	)))
   :killer undefine-predicate)

(defun setup-predicate-args-info (predicate-name number-of-required number-of-optional rest-arg arg-list)
  (let* ((args-info 0))
    ;; bash the arity stuff into args-info
    (setf (predicate-max-args args-info) (+ number-of-required number-of-optional))
    (setf (predicate-min-args args-info) number-of-required)
    (setf (predicate-rest-arg args-info) (if rest-arg 1 0))
    (setf (gethash predicate-name *all-predicates*)
	  (make-predicate-descriptor :args-info args-info
				     :arglist arg-list))))

;;; Note that it's a bug to put methods on synonyms, they should go on either a model or the
;;; canonical version.
(def-defining-form define-predicate-synonym
   :definer
   ((new-name old-name)
    ;; define new-name to be a synonym for old-name, without defflavoring new-name.
    ;; Keeps you out of trouble with globalized symbols, like AND.
    `(progn #+(or genera cloe-developer)
	    (record-source-file-name ',new-name 'define-predicate-synonym)
	    ;; flush existing definition, if any
	    (undefine-predicate ',new-name)
	    ;; keep track of the synonym relationship
	    (record-predicate-synonyms ',new-name ',old-name)
	    ;; return the name
	    ',new-name))
   )

(defun parse-predication-maker (form)
  ;; extract the predicate from things predication-maker forms
  #-sbcl(declare (values predicate args))
  (destructuring-bind (predicate . args) (predication-maker-statement form)
    (values (predicate-is-synonym-for predicate) args)))


#+(or genera cloe-developer)
(defun undefine-predicate-method (method-spec)
  ;; define-predicate-method is data-driven; depends on what part of the
  ;; protocol is being hacked.  see the define-protocol-function of the
  ;; protocol element you're interested in.
  (check-type method-spec joshua-protocol-function-spec "a Joshua protocol function-spec")
  (funcall (get (joshua-fspec-protocol method-spec) 'zl:::zwei:kill-definition) method-spec))

;;;
;;; How users define handlers for a model.
;;;

(def-defining-form define-predicate-method
  :definer
  (((protocol-function flavor &rest options) args &body body)
   ;; define-predicate-method is data-driven; depends on what part of
   ;; the protocol is being hacked.  see the define-protocol-function
   ;; of the protocol element you're interested in.
   (let ((fspec `(,protocol-function ,flavor ,@options)))
     (check-type fspec joshua-protocol-function-spec "a Joshua protocol function-spec")
     (apply (get (joshua-fspec-protocol fspec) 'protocol-definer) fspec args body)))
  :killer undefine-predicate-method
  :type-name "Predicate Method"
  )




(eval-when (:compile-toplevel :execute :load-toplevel)
  (proclaim '(inline stackify-thing))
  (defun stackify-thing (thing env all-lvs)
    (declare (ignore env all-lvs)
	     #-sbcl(dynamic-extent thing))
    thing))


;;; Here are the macro versions of TELL and ASK.  They have positional
;;; rather than keyword arguments.

(defmacro tell (predication &key justification)
  ;; convert to positional form, defaulting correctly.
  #-sbcl(declare (values canonical-version new-or-old))
  `(tell-internal ,predication +true+ , justification))


;;; Data-stack frobozzery elided for the bootless and unhorsed...
(defmacro ask (query continuation &key (do-backward-rules t) (do-questions nil) &environment env)
  (let* ((known-lvs (macroexpand '(known-lvs) env))
	 (new-lvs-in-thing (set-difference (logic-variable-makers-in-thing query)
					   known-lvs))
	 (all-lvs (union new-lvs-in-thing known-lvs)))
    (let ((new-form
	    (if (and (listp continuation)
		     (eql (first continuation) 'function)
		     (listp (second continuation))
		     (eql (first (second continuation)) 'lambda))
		(destructuring-bind (lambda-word args &body body) (second continuation)
		  (declare (ignore lambda-word))
		  `(flet ((continuation ,args
			    #+(or cloe genera) (declare (sys:downward-function))
			    ,@body))
		     (declare (dynamic-extent #'continuation))
		     (ask-internal ,query +true+ #'continuation ,do-backward-rules ,do-questions)))
		`(ask-internal ,query +true+ ,continuation ,do-backward-rules ,do-questions))))
      (when new-lvs-in-thing
	(setq new-form `(with-unbound-logic-variables ,new-lvs-in-thing
			 (macrolet ((known-lvs () ',all-lvs))
				       ,new-form))))
      ;; return the form.
      new-form)))

(defmacro binding-lvs (variable-list &body body &environment env)
  (let* ((known-lvs (macroexpand '(known-lvs) env))
	 (new-lvs-in-thing (set-difference (logic-variable-makers-in-thing variable-list) known-lvs))
	 (all-lvs (union new-lvs-in-thing known-lvs))
	 (new-form body))
    (when new-lvs-in-thing
      (setq new-form `(with-unbound-logic-variables ,new-lvs-in-thing
			 (macrolet ((known-lvs () ',all-lvs))
			   ,@new-form))))
    new-form))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (proclaim '(inline ask-query))
  (defun ask-query (backward-support)
    (first backward-support)))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (proclaim '(inline ask-query-truth-value))
  (defun ask-query-truth-value (backward-support)
    (second backward-support)))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (proclaim '(inline ask-database-predication))
  (defun ask-database-predication (backward-support)
    (third backward-support)))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (proclaim '(inline ask-derivation))
  (defun ask-derivation (backward-support)
    (rest (rest backward-support))))

;;; A number of functions to use in the continuation to ask.  These two
;;; are just for convenience, allowing the the user to say (ask [...]
;;; #'print-query)

(defun print-query-internal (backward-support stream)
  (truth-value-case (ask-query-truth-value backward-support)
    (+true+
     (prin1 (ask-query backward-support) stream))
    (+false+
     (write-string "[not " stream)
     (print-without-truth-value (ask-query backward-support) stream)
     (write-string "]" stream))))

(defun print-query (backward-support &optional (stream *standard-output*))
  (check-type backward-support cons "backward-support from a query")
  (terpri stream)
  (print-query-internal backward-support stream)
  nil)

(defun say-query (backward-support &optional (stream *standard-output*))
  (check-type backward-support cons "backward-support from a query")
  (fresh-line stream)
  (say (ask-query backward-support) stream))

;;; This applies a function to each element in the backward-support.
;(defun map-over-derivation (derivation function)
;  (declare (sys:downward-funarg function))
;  (labels ((map-over-support (support)
;	     (cond ((consp support)
;		    (funcall function (car support))
;		    (map nil #'map-over-support (cdr support)))
;		   (t
;		      (funcall support-continuation support)))))
;    (loop for derivation-item in derivation
;	  do
;      (map-over-support support-item))))

;;; Derivation is always a list.
;;;  The first element is always the (unified) query.  Almost always the query is positive.
;;;  The second is always the truth value of the query.
;;;  Possibilities for the rest are:
;;;  (<query> <t/f> <database predication>)
;;;  (<query> <t> AND <conjunct1 derivation> <conjunct2 derivation> ...)
;;;  (<query> <t/f> (RULE <rule name>) <conjunct1 derivation> <conjunct2 derivation> ...)
;;;  (<query> <t/f> (QUESTION <question name>) <succeed argument>)
;;;  (<query> <t/f> KNOWN <derivation>)
;;;  (<query> <t/f> PROVABLE <derivation>)
;;;
(defvar *pqr-indent* 0)
(defun print-query-results (backward-support &key (stream *standard-output*) (printer #'prin1))
  (flet ((print-indent ()   (loop repeat *pqr-indent* do (write-char #\space))))
  (terpri stream)
  (print-indent)
  (let ((*pqr-indent* (+ 2 *pqr-indent*)))
  (let ((b-s backward-support))	;pcl destructuring-bind lossage
    (typecase backward-support
      (cons
	(destructuring-bind (query truth-value type . rest) b-s
	  (declare (ignore query))
	  (print-query-internal backward-support stream)
	  (princ " succeeded" stream)
	  (PROGN
	    (terpri stream)
            (PRINT-INDENT)
	    (etypecase type
	      (predication
		(print-without-truth-value type stream)
		(write-string " was " stream)
		(princ (truth-value-name truth-value) stream)
		(write-string " in the database" stream))
	      (cons
		(princ "It was derived from " stream)
		(princ (case (first type)
			 (rule "rule")
			 (question "question")
			 (otherwise (first type)))
		       stream)
		(princ " " stream)
		(princ (second type) stream)
		;; rules can have support of nil when the if part is trivial
		(when (first rest)
		  (print-query-results (first rest) :stream stream))
		(loop for sub-support in (rest rest)
		      do (when sub-support
                           (print-indent)
			   (print-query-results sub-support :stream stream))))
	      (symbol
		(ecase type
		  ((and or)
		   (cond ((null rest)
			  (princ "it's true by convention" stream))
			 (t
			  (PROGN
			    (print-query-results (first rest) :stream stream)
			    (loop for sub-support in (rest rest)
				  do (fresh-line stream)
				     (print-query-results sub-support :stream stream))))))
		  ((known provable)
		   (truth-value-case truth-value
		     (+true+
		      (PROGN
			(print-query-results (first rest) :stream stream)))
		     (+false+
		      (princ "it was not " stream)
		      (princ type stream))))))))))
      (otherwise
	(funcall printer backward-support stream)))))))



(defun expand-do-queries (name varspecs body)
  ;; expander function for the two macros that follow.
  (loop for (var query . options) in (reverse varspecs)
	for continuation-body = body then (list form)
	for continuation = `#'(lambda (,var) ,@continuation-body)
	for form = `(ask ,query ,continuation ,@options)
	;; enclose in a (block nil ...) to allow return to be used in the body.
	finally (return `(block ,name ,form))))

;;; This is a good model to give to users to explain how backward rules compile.
(defmacro do-queries* (varspecs &body body)
  ;; iterative syntax like do-list and so on, for people who don't like
  ;; continuation-passing style.
  (expand-do-queries nil varspecs body))

(defmacro do-queries-named* (name varspecs &body body)
  ;; like do-queries*, but has a named block so return-from makes sense
  (expand-do-queries name varspecs body))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (proclaim '(inline ask-question))
  (defun ask-question (question predication truth-value continuation)
    (funcall question predication truth-value continuation)))

(defmacro ask* (predication &body body)
  (let ((just (gensym "JUST-")))
    `(ask ,predication
	  #'(lambda (,just)
	      (declare (ignore ,just))
	      ,@body))))
