;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-ENV; Base: 10; Lowercase: Yes -*-

;;; Copyright (c) 1994-2000, Scott McKay.
;;; Copyright (c) 2001-2003, Scott McKay and Howard Shrobe.
;;; All rights reserved.  No warranty is expressed or implied.
;;; See COPYRIGHT for full copyright and terms of use.

(in-package :clim-env)

;;; Presentation types for the class and generic function browsers

(define-presentation-type generic-function-specializer (generic-function parameter-name)
  :options ((display-as-specializer nil))
  :history nil)

(define-presentation-method accept
    ((type generic-function-specializer) stream (view textual-view) &key)
  (let* ((what (with-accept-help
		   ((:subhelp #+++ignore (:subhelp :append)
		     #'(lambda (stream action string-so-far)
			 (declare (ignore action string-so-far))
			 (format stream "How do you wish to specialize the ~A argument to ~A (~{~:(~A~)~^, ~})?"
			   parameter-name
			   (function-name generic-function)
			   '(:object :class)))))
		 (accept '(member :object :class)
			 :stream stream
			 :prompt (format nil "argument ~A" (symbol-name parameter-name))
			 :provide-default nil
			 :additional-delimiter-gestures '(#\space))))
	 thing)
    (flet ((eat-delimiter ()
	     (unless (eql (stream-read-gesture stream) #\space)
	       (simple-parse-error "You must type a space after ~A" what))))
      (declare (dynamic-extent #'eat-delimiter))
      (setq thing
	    (case what
	      (:object
		(eat-delimiter)
		(with-accept-help
		    ((:subhelp #+++ignore (:subhelp :append)
		      #'(lambda (stream action string-so-far)
			  (declare (ignore action string-so-far))
			  (format stream
			      "An object on which to specialize the ~A argument"
			    parameter-name
			    (generic-function-name generic-function)))))
		  (multiple-value-bind (exp type)
		      (accept 'expression
			  :stream stream
			  :prompt nil
			  :provide-default nil)
		    (values (if (symbolp exp) exp (eval exp))
			    type))))
	      (:class
		(eat-delimiter)
		(let* ((subprompt (format nil "the ~A argument to ~A"
				    parameter-name
				    (generic-function-name generic-function)))
		       (class (with-accept-help
				  ((:subhelp #+++ignore (:subhelp :append)
				    #'(lambda (stream action string-so-far)
					(declare (ignore action string-so-far))
					(format stream "A class on which ~A is specialized" subprompt))))
				(accept 'class
					:stream stream
					:prompt nil))))
		  class)))))
    (list what thing)))

(define-presentation-method present
    (object (type generic-function-specializer) stream (view textual-view) &key)
  (if display-as-specializer
    (case (car object)
      (:object (format stream "(~S ~S)" 'eql (second object)))
      (:class (present (second object) 'class :stream stream)))
    (progn
      (format stream "~@(~A~)" (car object))
      (case (car object)
	(:object
	 (write-char #\space stream)
	 (prin1 (second object) stream))
	(:class
	 (write-char #\space stream)
	 (present (second object) 'class :stream stream))))))

(define-presentation-method presentation-typep
    (object (type generic-function-specializer))
  (and (listp object)
       (null (cddr object))
       (case (first object)
	 (:class (typep (second object) 'class))
	 (:object t)
	 (otherwise nil))))


(define-presentation-type generic-function-specializers (generic-function)
  :options ((prompt nil)
	    (display-as-specializer nil)))

(define-presentation-method accept
    ((type generic-function-specializers) stream (view textual-view) &key)
  ;;--- It would sure be nice if we could flush the trailing "Class T" stuff
  (let* ((required (let ((required nil))
		     (map-over-lambda-list
		       (generic-function-lambda-list generic-function)
		       #'(lambda (args type)
			   (when (eq type :required)
			     (push (car args) required))))
                     (nreverse required)))
         (ptypes (mapcar #'(lambda (arg)
			     `(generic-function-specializer ,generic-function ,arg))
			  required)))
    (accept `((sequence-enumerated ,@ptypes) :separator #\,)
            :stream stream
            :prompt nil)))

(define-presentation-method present
    (object (type generic-function-specializers) stream (view textual-view) &key)
  (format-textual-list 
    object #'(lambda (spec stream)
	       (present spec `((generic-function-specializer ,generic-function #:any)
			       :display-as-specializer ,display-as-specializer)
			:stream stream))
    :stream stream :separator ", "))

(define-presentation-method presentation-typep
    (object (type generic-function-specializers))
  (with-presentation-type-parameters (generic-function-specializers type)
    (and (listp object)
	 (every #'(lambda (x) (presentation-typep x `(generic-function-specializer ,generic-function))) object))))



;;; Utilities for the class and generic function browsers

;; Given the kind of "matching" keyword arguments for the show commands,
;; create a predicate suitable for use as the matching function.
;;  - If the value of KEYWORD-ARG is :ALL then the predicate #'true is used
;;    so that everything will match.
;;  - If the value of KEYWORD-ARG is :NONE then the predicate #'false is used
;;    so that nothing will match.
;;  - If the value of KEYWORD-ARG is a string, then a predicate is constructed
;;    that will succeed if the string is a substring of the value returned when
;;    NAME-KEY is applied to the thing.
;;  - Otherwise KEYWORD-ARG should be a predicate to use.
(defmacro get-matcher-function (keyword-arg name-key)
  `(let ((keyword-arg ,keyword-arg))
     (cond ((eql keyword-arg :all) #'true)
	   ((eql keyword-arg :none) #'false)
	   ((stringp keyword-arg)
	    #'(lambda (thing)
		(search keyword-arg (string (funcall ,name-key thing))
			:test #'char-equal)))
	   (t keyword-arg))))

(defmethod class-name-string-for-matching ((class standard-class))
  (string (class-name class)))

(defun parse-generic-function-name (generic-function)
  (let ((name (generic-function-name generic-function)))
    (cond ((symbolp name) (values name ""))
	  ((listp name)
	   (values (second name) (first name)))
	  (t ""))))

(defun generic-function-sort-name-predicate (gf1 gf2)
  (let ((name1 (parse-generic-function-name gf1))
	(name2 (parse-generic-function-name gf2)))
    (if (string-equal name1 name2)
      (let ((name1 (generic-function-name gf1))
	    (name2 (generic-function-name gf2)))
	(cond ((symbolp name1) t)
	      ((symbolp name2) nil)
	      (t (string-lessp (car name1) (car name2)))))
      (string-lessp name1 name2))))

;; NB: this clobbers METHODS -- copy it if you care
(defun sort-generic-function-methods (generic-function methods &key sort)
  (if sort
    (sort methods (make-method-sort-predicate generic-function sort))
    methods))

(defun make-method-sort-predicate (generic-function sort)
  (ecase sort
    (:alphabetical
      #'(lambda (method1 method2)
	  (labels ((asp (m1-specs m2-specs)
		     (let (spec1 spec2)
		       (cond ((or (null m1-specs)
				  (null m2-specs))
			      t)			;preserve status quo
			     ((eql-method-specializer-p (setq spec1 (car m1-specs)))
			      t)			;EQL's first
			     ((eql-method-specializer-p (setq spec2 (car m2-specs)))
			      nil)
			     ((eql spec1 spec2)
			      (asp (cdr m1-specs) (cdr m2-specs)))
			     (t (string-lessp (class-name spec1)
					      (class-name spec2)))))))
	    (declare (dynamic-extent #'asp))
	    (asp (method-specializers method1)
		 (method-specializers method2)))))
    (:heuristic
      (let ((precedence-positions
	      (mapcar #'(lambda (method-arg)
			  (position method-arg
				    (generic-function-lambda-list generic-function)))
		      (generic-function-argument-precedence-order generic-function)))) ; MCL non-MOP!
	;; Least specific first
	#'(lambda (method1 method2)
	    (labels ((msp (positions)
		       (if (null positions)
			 (let ((qualifier-length1 (length (method-qualifiers method1)))
			       (qualifier-length2 (length (method-qualifiers method2))))
			   ;; Methods have the same specializers.
			   ;; Sort one with fewest qualifiers first.
			   (cond ((< qualifier-length1 qualifier-length2)
				  t)	;less qualified first
				 ((> qualifier-length1 qualifier-length2)
				  nil)
				 (t t))) ;preserve status quo
			 (macrolet ((method-specializer (method)
				      `(nth (car positions)
					    (method-specializers ,method)))
				    (depth-metric (class)
				      `(length (class-precedence-list ,class))))
			   (let* ((spec1 (method-specializer method1))
				  (spec2 (method-specializer method2))
				  (eql1 (eql-method-specializer-p spec1))
				  (eql2 (eql-method-specializer-p spec2))
				  depth1 depth2)
			     ;; Is spec1 less specific than spec2?
			     (cond ((and eql1 eql2)
				    (msp (cdr positions)))
				   (eql1 nil)
				   (eql2 t)
				   ((eql spec1 spec2)
				    (msp (cdr positions)))
				   ((< (setq depth1 (depth-metric spec1))
				       (setq depth2 (depth-metric spec2)))
				    t)	;deeper one is less general
				   ((> depth1 depth2)
				    nil)
				   (t (string-lessp (class-name spec1)
						    (class-name spec2)))))))))
	      (declare (dynamic-extent #'msp))
	      (msp precedence-positions)))))))

(defmacro with-emphasis ((stream emphasize) &body body &environment env)
  (if (constantp emphasize #+(or Genera Allegro) env)
    (ecase emphasize
      (:interesting `(progn ,@body))
      (:boring `(progn 
		  (write-char #\[ ,stream)
		  (multiple-value-prog1
		      (progn ,@body)
		    (write-char #\] ,stream)))))
    `(case ,emphasize
       (:interesting (progn ,@body))
       (:boring (progn 
		  (write-char #\[ ,stream)
		  (multiple-value-prog1
		      (progn ,@body)
		    (write-char #\] ,stream)))))))

(defun maybe-finalize-inheritance (class)
  #+Genera (unless (clos-internals::class-finalized-p class)
	     (clos-internals::finalize-inheritance class))
  #+Lispworks (clos::ensure-class-finalized class)
  #+CCL (declare (ignore class))		; FOR NOW
  #+Allegro (unless (clos:class-finalized-p class)
	      (clos:finalize-inheritance class)))

(defun eql-method-specializer-p (spec)
  (and (listp spec)
       (eql (car spec) 'eql)
       (consp (cdr spec))
       (null (cddr spec))
       (values t (second spec))))

(defun equivalent-specializers-p (spec1 spec2)
  (or (eql spec1 spec2)
      (multiple-value-bind (eql1-p object1)
	  (eql-method-specializer-p spec1)
	(multiple-value-bind (eql2-p object2)
	    (eql-method-specializer-p spec2)
	  (and eql1-p eql2-p
	       (eql object1 object2))))))

(defun equivalent-specializer-lists-p (spec1 spec2)
  (and (= (length spec1) (length spec2))
       (every #'equivalent-specializers-p spec1 spec2)))

(defun class-init-keywords (class &key direct)
  (declare (values keywords allow-other-keys-p))
  #+Allegro (unless (clos:class-finalized-p class)
	      (clos:finalize-inheritance class))
  (if direct
    (let ((initialization-keywords nil)
	  (allow-other-keys-p nil))
      (flet ((check-method (gf &rest specializers)
	       (declare (dynamic-extent specializers))
	       (dolist (method (generic-function-methods gf))
		 (when (equivalent-specializer-lists-p
			(method-specializers method) specializers)
		   (multiple-value-bind (keywords allow)
		       (function-keywords method)
		     (when allow
		       (setf allow-other-keys-p t))
		     (dolist (keyword keywords)
		       (pushnew keyword initialization-keywords)))))))
	(declare (dynamic-extent #'check-method))
	(let ((metaclass `(eql ,class)))
	  (check-method (symbol-function 'make-instance) metaclass)
	  (check-method (symbol-function 'allocate-instance) metaclass))
	(check-method (symbol-function 'initialize-instance) class)
	(check-method (symbol-function 'shared-initialize) class t))
      (dolist (slot (class-direct-slots class)
		(dolist (initarg (slot-definition-initargs slot))
		  (pushnew initarg initialization-keywords))))
      (values initialization-keywords allow-other-keys-p))
    (values (class-make-instance-keywords class)
	    (class-make-instance-allow-other-keys class))))

(defun class-make-instance-keywords (class)
  #+Genera (clos-internals::class-make-instance-keywords class)
  #+Lispworks (let ((initargs nil))
                ;;--- This will just have to do for the time being
                (dolist (slot (clos:class-slots class))
	          (dolist (initarg (clos:slot-definition-initargs slot))
	            (pushnew initarg initargs)))
                initargs)
  #+Allegro (excl::compute-valid-make-instance-initargs class)
  #+CCL (ccl::class-make-instance-initargs class))

(defun class-make-instance-allow-other-keys (class)
  #+Genera (ldb-test clos-internals::%%standard-class-make-instance-allow-other-keys-p
		     (clos-internals::class-bits class))
  #-Genera nil)

#+Genera
(defun effective-method-method-evaluation-order (effective-method)
  (let ((methods-reverse-evaluation-order nil))
    (labels ((walker (form kind usage state)
	       (declare (ignore usage)
			(values new-state suppress-processing))
	       (if (and (consp form)
			(listp kind)
			(eql (first form) 'clos:call-method))
		 (progn
		   (push (second form) methods-reverse-evaluation-order)
		   (dolist (next-method (if (cdddr form)
					  (getf (cddr form) ':next-methods)
					  (third form)))
		     (cond ((typep next-method 'method)
			    (push next-method methods-reverse-evaluation-order))
			   ((listp next-method)
			    (lt:mapforms-1 next-method))
			   (t (warn "Unknown next-method ~s" next-method)) ))
		   (values state t))
		 (values state nil))) )
      (declare (dynamic-extent #'walker))
      (lt:mapforms #'walker effective-method))
    (nreverse methods-reverse-evaluation-order))) 


;;; CLOS-related commands that can be used anywhere

(define-command (com-show-class-superclasses :command-table clos :name t)
    ((class 'class
	    :display-default nil
	    :prompt "class"
	    :gesture nil)
     &key
     (duplicates 'boolean
		 :default nil :mentioned-default t
		 :documentation "Show duplicate occurrences of classes in the hierarchy.")
     (matching '(token-or-type (:all :none) string)
	       :prompt "Superclasses matching"
	       :default :all
	       :documentation "Show superclasses whose names contain this substring."))
  (with-frame-standard-output (stream)
    (document-class-and-superclasses class
      :match-superclass (get-matcher-function matching #'class-name)
      :show-duplicate-classes duplicates
      :exclude-classes '(t standard-object structure-object)
      :stream stream)))

(define-command (com-show-class-subclasses :command-table clos :name t)
    ((class 'class
	    :display-default nil
	    :prompt "class"
	    :gesture nil)
     &key
     (duplicates 'boolean
		 :default nil :mentioned-default t
		 :documentation "Show duplicate occurrences of classes within the hierarchy.")
     (matching '(token-or-type (:all :none) string)
	       :default :all
	       :documentation "Show subclasses whose names contain this substring.")
     (levels '(token-or-type (:all) (integer 1 *))
	     :default :all
	     :prompt "number"
	     :display-default nil
	     :documentation "Number of levels of subclasses to show."))
  (with-frame-standard-output (stream)
    (document-class-and-subclasses class
      :match-subclasses (get-matcher-function matching #'class-name-string-for-matching)
      :show-duplicate-classes duplicates
      :depth-limit (if (numberp levels) levels nil)
      :stream stream)))

(define-command (com-show-class-slots :command-table clos :name t)
    ((class 'class
	    :display-default nil
	    :prompt "class"
	    :gesture nil)
     &key
     (detailed 'boolean
	       :default nil :mentioned-default t
	       :documentation "Include information about accessors, etc.")
     (direct 'boolean
	     :default nil :mentioned-default t
	     :documentation "Only show direct slots (those defined on this class).")
     (sort '(member :alphabetical :class)
	   :default :alphabetical
	   :documentation "Sort the display alphabetically by slot name, or by location in superclass hierarchy.")
     (matching '(token-or-type (:all) string)
	       :prompt "Slots matching"
	       :default :all
	       :documentation "Show slots whose names contain this substring."))
  #+Allegro (unless (clos:class-finalized-p class)
	      (clos:finalize-inheritance class))
  (with-frame-standard-output (stream)
    (let ((matcher (get-matcher-function matching #'slot-definition-name)))
      (if (eql sort :alphabetical)
	(let* ((the-class class)
	       (the-slots)
	       #+CCL (slot-class-alist)) ; needed if NOT direct
	  (if direct
	    (dolist (slot (class-direct-slots the-class))
	      (when (funcall matcher slot)
		(pushnew slot the-slots :key #'slot-definition-name)))
	    (dolist (class (class-precedence-list the-class))
	      (dolist (slot (class-direct-slots class))
		(when (funcall matcher slot)
		  (pushnew slot the-slots :key #'slot-definition-name)
		  #+CCL (pushnew (list slot class) slot-class-alist
				 :key #'(lambda (slot-class-pair)
					  (slot-definition-name (first slot-class-pair))))))))
	  (fresh-line stream)
	  (format stream "~D ~:[slot~2:*~P~;direct slot~2:*~P~] of "
	    (length the-slots) direct)
	  (present the-class 'class :stream stream)
	  (if (eql matching :all)	
	    nil
	    (format stream " matching ~S" matching))
	  (cond (the-slots
		 (format stream ":~%")
		 (setq the-slots (sort the-slots #'string-lessp :key #'slot-definition-name))
		 (indenting-output (stream '(4 :character))
		   (if detailed
		     (dolist (slot the-slots)
		       (document-slot slot #+CCL (if direct 
						   class
						   (second (assoc slot slot-class-alist)))
				      :highlight-name t :verbose t
				      :stream stream))
		     (filling-output (stream)
		       (format-textual-list
			the-slots #'(lambda (slot stream)
				      (prin1 (slot-definition-name slot) stream))
			:stream stream)))))
		(t 
		 (write-char #\. stream))))
	(document-class-and-superclasses class 
	  :match-superclass (if direct #'false #'true)
	  :match-slots matcher
	  :match-methods #'false
	  :slot-description (if detailed :verbose :terse)
	  :exclude-classes '(t standard-object structure-object)
	  :show-duplicate-classes nil
	  :stream stream)))))

(define-command (com-show-class-initargs :command-table clos :name t)
    ((class 'class
	    :display-default nil
	    :prompt "class"
	    :gesture nil)
     &key
     (detailed 'boolean
	       :default nil :mentioned-default t
	       :documentation "Show affected the slots and defaults.")
     (direct 'boolean
	     :default nil :mentioned-default t
	     :documentation "Only show initializations defined on this class.")
     (sort '(member :alphabetical :class)
	   :default :alphabetical
	   :documentation "Sort the display alphabetically by initarg, or by location in superclass hierarchy.")
     (matching '(token-or-type (:all) string)
	       :prompt "Initargs matching"
	       :default :all
	       :documentation "Only show initargs whose names contain this substring."))
  (maybe-finalize-inheritance class)
  (with-frame-standard-output (stream)
    (if (eql sort :alphabetical)
      (if detailed
	(document-class-initargs class
	  :match (get-matcher-function matching #'identity)
	  :direct direct
	  :stream stream)
	(multiple-value-bind (initargs allow-other-keys-p)
	    (class-init-keywords class :direct direct)
	  (setf initargs (nconc (sort (copy-list initargs) #'string-lessp)
				(and allow-other-keys-p '(&allow-other-keys))))
	  (cond (initargs
		 (format-textual-list
		  initargs #'(lambda (initarg stream) (prin1 initarg stream))
		  :stream stream))
		(t
		 (fresh-line stream)
		 (format stream "No ~:[~;direct ~]initargs of ~/presentation/"
		   direct (list class 'class))))))
      (document-class-and-superclasses class
	:match-superclass (if direct #'false #'true)
	:match-default-initargs (get-matcher-function matching #'car)
	:match-init-keywords (get-matcher-function matching #'identity)
	:match-slots #'false
	:match-methods #'(lambda (method)
			   (let (keywords)
			     (and (let ((gf (method-generic-function method)))
				    (or (eql gf (symbol-function 'initialize-instance))
					(eql gf (symbol-function 'shared-initialize))))
				  (setq keywords (function-keywords method))
				  (some (get-matcher-function matching #'identity)
					keywords))))
	:exclude-classes '(t standard-object structure-object)
	:show-duplicate-classes nil
	:stream stream))))

(define-command (com-show-class-methods :command-table clos :name t)
    ((class 'class
	    :display-default nil
	    :prompt "class"
	    :gesture nil)
     &key
     (direct 'boolean
	     :default nil :mentioned-default t
	     :documentation "Don't show methods inherited from superclasses.")
     (stop-at 'class			;we could be more restrictive...
	      :default (car (last (class-precedence-list class) 2))
	      :documentation "Only look as far upwards as this class.")
     (matching 'string
	       :prompt "Generic functions matching"
	       :default ""
	       :documentation "Show generic functions whose names contain this substring."))
  (with-frame-standard-output (stream)
    (let ((methods nil)
	  (interesting-types nil))
      (labels ((collect-methods-for-class (class)
		 (dolist (method (specializer-direct-methods class))
		   (when (search matching 
				 (string (parse-generic-function-name
					   (method-generic-function method)))
				 :test #'char-equal)
		     (pushnew method methods)))))
	(declare (dynamic-extent #'collect-methods-for-class))
	(when direct (setq stop-at class))
	(dolist (class (class-precedence-list class))
	  (push class interesting-types)
	  (collect-methods-for-class class)
	  (when (eql class stop-at) 
	    (return)))
	(when methods
	  ;; This should probably sort by generic function and then for
	  ;; methods in each generic function bucket, sort them again
	  (setq methods (sort methods #'generic-function-sort-name-predicate
			      :key #'method-generic-function))
	  (indenting-output (stream '(2 :character))
	    (dolist (method methods)
	      ;; Might eventually be worth having a :detailed argument or something
	      ;; which allows the user to select between the ":only-these-types"
	      ;; behavior and the "specializers-for-abbreviation" behavior.
	      (document-method method
			       :only-these-types interesting-types
			       ;:specializers-for-abbreviation (list class)
			       :stream stream))))))))

(define-command (com-show-generic-function :command-table clos :name t)
    ((generic-function 'generic-function
		       :display-default nil
		       :prompt "generic function name"
		       :gesture nil)
     &key
     (classes '(member-alist (("Yes" . t) ("No" . nil) ("by class" . :class)))
	      :default nil :mentioned-default t
	      :documentation "List classes which define methods for this generic function.")
     (methods '(member-alist (("Yes" . t) ("No" . nil) ("Detailed" . :detailed)))
	      :default t :mentioned-default t
	      :documentation "List methods defined on this generic function.")
     (sort '(member :alphabetical :heuristic)
	   :default nil :mentioned-default :alphabetical
	   :documentation "Sort the methods by specializer names (in argument precedence order)."))
  (with-frame-standard-output (stream)
    (let ((methods-list (copy-list (generic-function-methods generic-function))))
      (document-generic-function generic-function
	:stream stream)
      (fresh-line stream)
      (when classes
	(labels ((classes-specializing-generic-function (generic-function methods)
		   (declare (ignore generic-function))
		   (let ((classes nil))
		     (dolist (m methods)
		       (dolist (specializer (method-specializers m))
			 (unless (listp specializer)
			   (setq classes (adjoin specializer classes)))))
		     (setq classes (sort classes #'class-name-string-lessp 
					 :key #'class-name))
		     classes))
		 (class-name-string-lessp (cn1 cn2)
		   (cond ((and (consp cn1) (consp cn2))
			  (if (string-equal (first cn1) (first cn2))
			    (string-lessp (second cn1) (second cn2))
			    (string-lessp (first cn1) (first cn2))))
			 ((consp cn1)
			  (string-lessp (first cn1) cn2))
			 ((consp cn2)
			  (string-lessp cn1 (first cn2)))
			 (t
			  (string-lessp cn1 cn2)))))
	  (declare (dynamic-extent #'classes-specializing-generic-function 
				   #'class-name-string-lessp))
	  (let* ((defined-on-classes
		   (classes-specializing-generic-function generic-function methods-list))
		 (length (length defined-on-classes)))
	    (format stream "~%There ~@? with methods defined for ~/presentation/"
	      (if (= length 1) "is ~D class" "are ~D classes") length
	      (list generic-function 'generic-function))
	    (fresh-line stream)
	    (when defined-on-classes
	      (indenting-output (stream '(2 :character))
		(format-items defined-on-classes
			      :presentation-type 'class
			      :stream stream))))))
      (when methods
	(let* ((methods-list
		 (sort-generic-function-methods
		   generic-function methods-list :sort sort))
	       (length (length methods-list)))
	  (unless sort (setq sort :alphabetical))
	  (format stream "~%There ~:[are~;is~] ~D method~:P defined for ~/presentation/"
	    (= length 1) length
	    (list generic-function 'generic-function))
	  (fresh-line stream)
	  (indenting-output (stream '(2 :character))
	    (dolist (method methods-list)
	      (document-method method
		:only-these-types :all
		:show-generic-function-name nil
		:stream stream))))))))

(define-command (com-show-effective-method :command-table clos :name t)
    ((generic-function 'generic-function
		       :prompt "generic function"
		       :display-default nil)
     (specializers `(generic-function-specializers ,generic-function)
		   :prompt nil)
     &key
     (code 'boolean
	   :default t
	   :documentation "Show LISP code illustrating the behavior of the effective method"))
  (with-frame-standard-output (stream)
    (let ((applicable-methods (find-applicable-methods generic-function specializers))
	  effective-method
	  method-evaluation-order)
      (when applicable-methods
        #+Genera
	(setq effective-method 
		(clos-internals::compute-effective-method-1
		  generic-function applicable-methods
		  #'(lambda (type position)
		      (let ((thing (nth position specializers)))
			(ecase type
			  (class
			    (case (car thing)
			      (:object (class-of (second thing)))
			      (:class (second thing))))
			  (eql
			    (case (car thing)
			      (:object (values t (second thing)))
			      (:class nil)))))))
	      method-evaluation-order
		(effective-method-method-evaluation-order effective-method))
        #-Genera
        (setq effective-method 
	      (compute-effective-method
	        generic-function
		(generic-function-method-combination generic-function)
		applicable-methods)))
      (fresh-line stream)
      (format stream "There ~:[are~;is~] ~D method~:P applicable to ~S on ~/presentation/."
	(= (length applicable-methods) 1) (length applicable-methods)
	(generic-function-name generic-function)
	(list specializers `((generic-function-specializers ,generic-function)
			     :display-as-specializer t)))
      (fresh-line stream)
      (indenting-output (stream '(2 :character))
	(dolist (method method-evaluation-order)
	  (document-method method
	    :only-these-types :all
	    :show-generic-function-name nil
	    :stream stream)))
      (when code
	(cond (effective-method
               (fresh-line stream)
	       (format stream "The code for the effective method for ~/presentation/ is:~%"
		 (list generic-function 'generic-function))
	       (indenting-output (stream '(2 :character))
		 (let ((*print-pretty* t))
		   (format stream "~S" effective-method))))
	      (t
               (fresh-line stream)
	       (write-string "No effective method." stream)))))))

;;---*** What's the equivalent under Allegro for 'clos::compute-applicable-methods-from-classes'?
#+Lispworks
(defun find-applicable-methods (generic-function specializers)
  (block object-based-arglist
    (let ((object-arglist
           ;; Objects or standard classes that support CLASS-PROTOTYPE
           ;; mean we can compute applicable methods based on objects
           (mapcar #'(lambda (specializer)
                       (destructuring-bind (type object) specializer
                         (cond ((eq type :object)
                                object)
                               ((typep object 'standard-class)
                                (class-prototype object))
                               (t
                                (return-from object-based-arglist)))))
                   specializers)))
      (return-from find-applicable-methods
        (compute-applicable-methods generic-function object-arglist))))
  (block class-based-arglist
    (let ((class-arglist
           ;; All classes means we can compute applicable methods based on classes
           (mapcar #'(lambda (specializer)
                       (destructuring-bind (type object) specializer
                         (cond ((eq type :class)
                                object)
                               (t
                                (return-from class-based-arglist)))))
                   specializers)))
      (return-from find-applicable-methods
        (clos::compute-applicable-methods-from-classes generic-function class-arglist))))
  nil)

(define-presentation-to-command-translator edit-class
    (class com-edit-definition editing
     :gesture :edit)
    (object)
  (list (class-name object) :type 'defclass))

(define-presentation-to-command-translator edit-generic-function
    (generic-function com-edit-definition editing
     :gesture :edit)
    (object)
  (list (generic-function-name object) :type 'defun)) 


;; Describe a class and the superclasses from which it inheritsfrom.
;; CLASS is a CLOS class object.
;; The :MATCH-xxx keyword arguments are predicates which are applied to
;; their respective objects to determine if they should be described.
(defun document-class-and-superclasses
    (class &key (match-superclass #'true)
		(match-init-keywords #'false)
		(match-default-initargs #'false)
		(match-slots #'false)
		(match-methods #'false)
		(slot-description :terse)
		(exclude-classes nil)
		show-duplicate-classes
		(stream *standard-output*))
  #+Allegro (unless (clos:class-finalized-p class)
	      (clos:finalize-inheritance class))
  (let* ((precedence-list (class-precedence-list class))
	 (found-slots nil)		;slots which have been detailed
	 (found-default-initargs nil)	;default initargs which have been detailed
	 (dont-describe-these-classes (mapcar #'find-class exclude-classes))
	 ;; Don't bother with detailed descriptions of the above classes.
	 (last-class-printed (make-array (length precedence-list)))
	 ;; This vector remembers the last class printed at each indentation level
	 ;; it needs to be as long as the depth of the superclass tree
	 (duplicate-queue nil)
	 ;; ((class level) (class level) ...) queue of duplicates waiting to be printed
	 (indentation-per-level 2))
    (macrolet ((place-of-precedence (class)
		 ;; Determining if the current position in the tree is the place
		 ;; from which the class gets included
		 `(let ((this-is-the-place (eql ,class (car precedence-list))))
		    (when this-is-the-place (pop precedence-list))
		    this-is-the-place)))
      (labels ((superclass-matches-p (class)
		 ;; Returns T if any of the superclasses (not necessarily direct) of
		 ;; CLASS satisfy the MATCH-SUPERCLASSES predicate
		 (dolist (c (class-direct-superclasses class) nil)
		   (if (funcall match-superclass c)
		     (return-from superclass-matches-p t)
		     (when (superclass-matches-p c)
		       (return-from superclass-matches-p t)))))
	       (walk-superclasses (class stream level)
		 ;; Travel the class hierarchy and describe classes when and as appropriate
		 (cond ((place-of-precedence class)
			(when (or (funcall match-superclass class)
				  (superclass-matches-p class))
			  (dequeue-duplicate-classes stream)
			  (fresh-line stream)
			  (indenting-output (stream (list (* level indentation-per-level) :character)
					     ;; Defeat spurious fresh line
					     :move-cursor nil)
			    (with-emphasis (stream :interesting)
			      (present class 'class :stream stream))
			    (unless (member class dont-describe-these-classes)
			      (show-class-description-detailed class stream))
			    (setf (aref last-class-printed level) class))
			  (dolist (c (class-direct-superclasses class))
			    (walk-superclasses c stream (1+ level)))))
		       ((and show-duplicate-classes
			     (not (eql class (find-class 't)))
			     (not (eql class (find-class 'standard-object)))
			     (not (eql class (find-class 'structure-object))))
			(when (or (funcall match-superclass class)
				  (superclass-matches-p class))
			  (enqueue-duplicate-class class stream level)))))
	       (walk-superclasses-again (class stream level &rest trail)
		 (declare (dynamic-extent trail))
		 (cond ((not (eql class (car precedence-list)))
			;; We didn't find the starting class yet, walk up the tree
			(dolist (c (class-direct-superclasses class))
			  (apply #'walk-superclasses-again c stream (1+ level) class trail)))
		       ((or (funcall match-superclass class)
			    (superclass-matches-p class))
			;; We found the next class to do, first show how we got here
			(loop for level from 0 and class in (reverse trail)
			      with equal = t doing
			  (unless (eql class (aref last-class-printed level))
			    (setq equal nil))
			  (unless equal
			    (show-duplicate-class class stream level)))
			(walk-superclasses class stream level))
		       (t
			;; Skip this class
			(pop precedence-list))))
	       (enqueue-duplicate-class (class stream level)
		 (declare (ignore stream))
		 (push (list class level) duplicate-queue))
	       (dequeue-duplicate-classes (stream)
		 (loop for (class level) in (nreverse duplicate-queue)
		       do (show-duplicate-class class stream level))
		 (setq duplicate-queue nil))
	       (show-duplicate-class (class stream level)
		 (fresh-line stream)
		 (indenting-output (stream (list (* level indentation-per-level) :character))
		   (with-emphasis (stream :boring)
		     (present class 'class :stream stream))
		   (if (member class precedence-list)
		     (write-string " (below)" stream)
		     (write-string " (above)" stream))
		   (setf (aref last-class-printed level) class)))
	       (show-class-description-detailed (class stream)
		 ;; Output a detailed description of CLASS, including
		 ;; descriptions of default initargs and slots if requested
		 (fresh-line stream)
		 (multiple-value-bind (hd default-initargs slots)
		     (document-class class
				     :match-init-keywords match-init-keywords
				     :match-default-initargs match-default-initargs
				     :match-slots match-slots
				     :match-methods match-methods
				     :ignore-slots found-slots
				     :ignore-default-initargs found-default-initargs
				     :slot-description slot-description
				     :additional-indentation 4
				     :stream stream)
		   (declare (ignore hd))
		   (setq found-slots (append slots found-slots)
			 found-default-initargs (append default-initargs
							found-default-initargs)))))
	(declare (dynamic-extent #'superclass-matches-p
				 #'walk-superclasses #'walk-superclasses-again
				 #'enqueue-duplicate-class #'dequeue-duplicate-classes
				 #'show-duplicate-class
				 #'show-class-description-detailed))
	(walk-superclasses class stream 0)
	(loop while (some match-superclass precedence-list)
	      do (setq duplicate-queue nil) ;suppress trailing duplicates
	         (walk-superclasses-again class stream 0))))))

;; Analogous to DOCUMENT-CLASS-AND-SUPERCLASSES
(defun document-class-and-subclasses 
    (class &key (depth-limit nil)
		(match-subclasses #'true)
		(match-init-keywords #'false)
		(match-default-initargs #'false)
		(match-slots #'false)
		(match-methods #'false)
		(slot-description :terse)
		show-duplicate-classes
		(stream *standard-output*))
  (let ((the-class class)
	(classes-already-detailed nil))
    #+Allegro (unless (clos:class-finalized-p class)
		(clos:finalize-inheritance class))
    (macrolet ((class-already-detailed-p (class)
		 `(member ,class classes-already-detailed))
	       (show-class-name (class stream &optional distinction)
		 `(progn 
		    (fresh-line ,stream)
		    (with-emphasis (,stream ,distinction)
		      (present ,class 'class :stream ,stream)))))
      (labels ((check-ancestry (class)
		 (not (class-already-detailed-p class)))
	       (subclass-matches-p (class predicate)
		 (declare (values class-matches branch-matches))
		 (let ((class-matches (funcall predicate class)))
		   (labels ((subclass-matches-helper (class)
			      (dolist (c (class-direct-subclasses class) nil)
				(if (funcall predicate c)
				  (return-from subclass-matches-p (values class-matches t))
				  (subclass-matches-helper c)))))
		     (subclass-matches-helper class)
		     (values class-matches nil))))
	       (walk-subclasses (class stream depth)
		 (let ((have-detailed nil))
		   ;; HAVE-DETAILED is a flag used to indicate where to print the
		   ;; ellipsis (...) when abbreviating for depth limit
		   (if (check-ancestry class)
		     (multiple-value-bind (class-matches branch-matches)
			 (subclass-matches-p class match-subclasses)
		       (if class-matches
			 (setq have-detailed (show-class-detailed class stream))
			 (when branch-matches
			   (show-class-name class stream :interesting)))
		       (when branch-matches
			 (let ((subclasses (class-direct-subclasses class)))
			   (if (and subclasses depth-limit (>= depth depth-limit))
			     (cond (have-detailed
				    (fresh-line stream)
				    (write-string "..." stream))
				   (t (write-string " ..." stream)))
			     (indenting-output (stream '(2 :character)
						;; Defeat spurious fresh line
						:move-cursor nil)
			       (dolist (c subclasses)
				 (walk-subclasses c stream (1+ depth))))))))
		     (when show-duplicate-classes
		       (show-class-brief class stream)))))
	       (show-class-detailed (class stream)
		 (push class classes-already-detailed)
		 (show-class-name class stream :interesting)
		 (fresh-line stream)
		 (document-class class
				 :match-init-keywords match-init-keywords
				 :match-default-initargs match-default-initargs
				 :match-slots match-slots
				 :match-methods match-methods
				 :slot-description slot-description
				 :additional-indentation 4
				 :stream stream))
	       (show-class-brief (class stream)
		 (show-class-name class stream :boring)
		 (if (class-already-detailed-p class)
		   (write-string " (above)" stream)
		   (write-string " (below)" stream))))
	(declare (dynamic-extent #'check-ancestry #'subclass-matches-p
				 #'walk-subclasses
				 #'show-class-detailed #'show-class-brief))
	(walk-subclasses the-class stream 0)))))

(defun document-class (class &key (match-init-keywords #'false)
				  (match-default-initargs #'false)
				  (match-slots #'false)
				  (match-methods #'false)
				  (slot-description :terse)
				  ignore-slots
				  ignore-default-initargs
				  (additional-indentation 0)
				  (stream *standard-output*))
  (declare (values have-detailed found-default-initargs found-slots))
  #+Allegro (unless (clos:class-finalized-p class)
	      (clos:finalize-inheritance class)) 
  (let ((have-detailed nil)
	(found-slots
	  (ignore-errors
	    (sort (remove-if-not match-slots
				 (set-difference (class-direct-slots class)
						 ignore-slots
						 :key #'slot-definition-name))
		  #'string-lessp
		  :key #'slot-definition-name)))
	(found-initargs
	  (ignore-errors
	    (multiple-value-bind (initargs allow-other-keys-p)
		(class-init-keywords class :direct t)
	      (nconc (sort (delete-if-not match-init-keywords (copy-list initargs))
			   #'string-lessp)
		     (and allow-other-keys-p '(&allow-other-keys))))))
	(found-default-initargs
	  (ignore-errors
	    (sort (remove-if-not match-default-initargs
				 (set-difference
				   (class-direct-default-initargs class)
				   ignore-default-initargs
				   :key #'car))
		  #'string-lessp
		  :key #'car))))
    ;; Initargs stuff
    (when found-initargs
      (fresh-line stream)
      (write-string "Init keywords: " stream)
      (format-textual-list
	found-initargs #'(lambda (initarg stream) (prin1 initarg stream))
	:stream stream)
      (setq have-detailed t))
    (when found-default-initargs
      (fresh-line stream)
      (write-string "Default initargs:" stream)
      (indenting-output (stream (list (+ 2 additional-indentation) :character))
	(dolist (initarg found-default-initargs)
          (fresh-line stream)
	  (format stream "~S ~S"
	    (first initarg) (second initarg))))
      (setq have-detailed t))
    ;; Slots stuff
    (when found-slots
      (fresh-line stream)
      (write-string "Slots:" stream)
      (indenting-output (stream (list (+ 2 additional-indentation) :character))
	(dolist (slot found-slots)
	  (document-slot 
	    slot
            #+CCL class
	    :highlight-name t 
	    :verbose (eql slot-description :verbose)
	    :stream stream)))
      (setq have-detailed t))
    ;; Methods stuff
    (unless (eql match-methods #'false)
      (let ((methods (remove-if-not match-methods
				    (specializer-direct-methods class))))
	(when methods
          (fresh-line stream)
	  (write-string "Methods:" stream)
	  (indenting-output (stream (list (+ 2 additional-indentation) :character))
	    (dolist (method methods)
	      (document-method method 
	        :specializers-for-abbreviation (list class)
		:only-these-types nil
		:stream stream)))
	  (setq have-detailed t))))
    (values have-detailed found-default-initargs found-slots)))

;; Describe a CLOS slot.
;; SLOT is a slot-definition object.
;; HIGHLIGHT-NAME is a flag indicating that the name of the slot should be highlighted.
;; VERBOSE is a flag indicating that an extensive description is wanted, including type,
;; allocation, readers, writers, and initializers.
(defun document-slot (slot 
                      #+CCL class
                      &key highlight-name verbose 
			   (stream *standard-output*))
  (let* ((slot-name (slot-definition-name slot)))
    (fresh-line stream)
    (if highlight-name
      (with-emphasis (stream :interesting)
	(format stream "~S " slot-name))
      (with-emphasis (stream :boring)
	(format stream "~S " slot-name)))
    (when verbose 
      (format stream "~((~A allocation)~)"
	(slot-definition-allocation #+MCL class slot))
      (let ((type (slot-definition-type slot))
	    (initform (slot-definition-initform slot))
	    (initargs (slot-definition-initargs slot))
	    (readers #-CCL (slot-definition-readers slot)
                     #+CCL (ccl:slot-readers class slot-name))
	    (writers #-CCL (slot-definition-writers slot)
                     #+CCL (ccl:slot-writers class slot-name))
	    #+Genera (locators (clos:slot-definition-locators slot)))
	(when (or (not (eql type t))
		  initform initargs readers writers #+Genera locators)
	  (indenting-output (stream '(4 :character))
	    (unless (eql type t)
              (fresh-line stream)
	      (format stream "Type:     ~S" type))
	    (when initform
              (fresh-line stream)
	      (write-string "Initform: " stream)
	      ;;--- Fix this
	      (progn ;scl:abbreviating-output (stream :height 1)
		(prin1 initform stream)))
	    (let ((format-miracle "~*~:[~;~%~2:*~A~{ ~S~}~]"))
	      (format stream format-miracle "Initargs:" initargs)
	      (format stream format-miracle "Readers: " readers)
	      (format stream format-miracle "Writers: " writers)
	      #+Genera
	      (format stream format-miracle "Locators: " locators))))))
    slot-name))

;; Because DCLOS defines a "!" dispatch macro char...
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *dclos-readtable* (copy-readtable))
  (setq *readtable* (copy-readtable nil)))

;; Describe a CLOS method.
;; Prints the generic function name, the method qualifiers in {} brackets
;; and the types of the specializers.  Any of the specializers which match
;; SPECIALIZERS-FOR-ABBREVIATION are printed as an exclamation point rather than
;; having their names typed out.
(defun document-method (method &key specializers-for-abbreviation
				    only-these-types
				    brief
				    (show-generic-function-name t)
				    (stream *standard-output*))
  (fresh-line stream)
  (let ((method-spec #+Genera (clos-internals::function-spec-object method)
                     #-Genera (function-name (method-function method))))
    (with-output-as-presentation (stream method (class-name (class-of method)))
      (with-output-as-presentation (stream method-spec 'function-spec)
	(when show-generic-function-name
	  (present (method-generic-function method) 'generic-function :stream stream)
	  (write-char #\space stream))
	(flet ((method-argument-specializer-type-alist
		   (method &key abbreviate (only-these :all))
		 (do ((specializers (method-specializers method) (cdr specializers))
		      (ll (generic-function-lambda-list
			    (method-generic-function method)) (cdr ll))
		      (alist nil))
		     ((null specializers)
		      alist)
		   (let ((arg (car ll))
			 (specializer (car specializers)))
		     (if (member specializer abbreviate :test #'equal)
		       (push (list arg '!) alist)
		       (unless (and (not (eql only-these :all))
				    (not (member specializer only-these :test #'equal)))
			 (push (list arg
				     #-allegro
				     (if (listp specializer)
				       specializer
				       (class-name (class-of specializer)))
				     #+allegro
				     (typecase specializer
				       (aclmop:eql-specializer `(eql ',(aclmop:eql-specializer-object specializer)))
				       (otherwise (class-name specializer)))
				     )
			       alist)))))))
	  (declare (dynamic-extent #'method-argument-specializer-type-alist))
	  (print-lambda-list
	    (generic-function-lambda-list
	      (method-generic-function method))
	    stream
	    :brief brief
	    :types (method-argument-specializer-type-alist method
							   :abbreviate specializers-for-abbreviation
							   :only-these only-these-types)))
	(let ((qualifiers (method-qualifiers method)))
	  ;; Check for primary method of the method combination type
	  (format stream "~{ ~S~}" qualifiers))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *readtable* *dclos-readtable*))

(defun document-class-initargs (class &key direct (match #'true)
				           (stream *standard-output*))
  ;; This is where we record information about initialization keywords
  ;; as we find them.  They are stored in a heap so that they are
  ;; magically sorted for us and can be looked up quickly.  For each
  ;; keyword argument, we record the keyword, the slot it is an initarg
  ;; for, if any, and any default value we might find.  We also record
  ;; any methods for which it is an &KEY argument.
  #+Allegro (unless (clos:class-finalized-p class)
	      (clos:finalize-inheritance class))
  (let ((initialization-data (make-heap :test #'string-greaterp)))
    ;; Too bad there isn't a local version of DEFSTRUCT.
    (macrolet ((init-class (init)   `(nth 0 ,init))
	       (init-slots (init)   `(nth 1 ,init))	;list of affected slots
	       (init-default (init) `(nth 2 ,init))
	       (init-has-default (init) `(nth 3 ,init))
	       (init-methods (init) `(nth 4 ,init))
	       (make-init-info (&key class slot (default nil has-default) (methods nil))
		 `(list ,class ,slot ,default ,has-default ,methods)))
      (labels ((find-init (init-keyword)
		 ;; Given an init keyword, find the corresponding entry in
		 ;; INITIALIZATION-DATA, creating one if necessary
		 (multiple-value-bind (info key)
		     (find-heap-key init-keyword initialization-data :errorp nil)
		   (declare (ignore key))
		   (unless info
		     (setq info (make-init-info))
		     (add-item-to-heap init-keyword info initialization-data))
		   info))
	       (update-init-default (keyword default)
		 ;; Set the default value for the keyword to default, if
		 ;; it does not already have one
		 (let ((init (find-init keyword)))
		   (unless (init-has-default init)
		     (setf (init-default init) default)
		     (setf (init-has-default init) t))))
	       (update-init-slots (Keyword slot)
		 ;; Set the slot for keyword if it does not already have one.
		 ;; can an initarg be used to fill more than one slot?
		 (let ((init (find-init keyword)))
		   (pushnew (slot-definition-name slot)
			    (init-slots init))))
	       (process-class-inits (class)
		 ;; Given a class, extract information from its default-initargs and its
		 ;; direct slots.
		 (dolist (di (class-direct-default-initargs class))
		   (when (funcall match (car di))
		     (update-init-default (first di) (second di))))
		 #+++ignore	;this would get too much stuff
		 (dolist (init (class-direct-init-keywords class))
		   (when (funcall match init)
		     (find-init init)))
		 (dolist (slot (class-direct-slots class))
		   (dolist (slot-initarg (slot-definition-initargs slot))
		     (when (funcall match slot-initarg)
		       (update-init-slots slot-initarg slot))))))
	(declare (dynamic-extent #'find-init #'update-init-default #'update-init-slots 
				 #'process-class-inits))
	;; Collect the initializations
	(if direct
	  (process-class-inits class)
	  (progn
	    (dolist (init (class-make-instance-keywords class))
	      (when (funcall match init)
		(find-init init)))
	    (dolist (c (class-precedence-list class))
	      (process-class-inits c))))
	;; Print them out in alphabetical order
	(let ((info nil))
	  (loop (multiple-value-bind (init-info init-keyword more-p)
		    (delete-top-of-heap initialization-data)
		  (unless more-p (return))
		  (push (cons init-keyword init-info) info)))
	  (fresh-line stream)
	  (formatting-table (stream :x-spacing '(4 :character))
	    (formatting-row (stream)
	      (with-text-face (stream :italic)
		(formatting-cell (stream)
		  (write-string "Initarg" stream))
		(formatting-cell (stream)
		  (write-string "Slot name" stream))
		(formatting-cell (stream)
		  (write-string "Initform" stream))))
	    (dolist (this-info info)
	      (let* ((init-keyword (car this-info))
		     (init-info (cdr this-info))
		     (slots (init-slots init-info))
		     (init-default (init-default init-info))
		     (has-default (init-has-default init-info)))
		(formatting-row (stream)
		  (formatting-cell (stream)
		    (prin1 init-keyword stream))
		  (formatting-cell (stream)
		    (format-textual-list 
		      slots #'(lambda (slot stream) (prin1 slot stream))
		      :separator " " :stream stream))
 		  (formatting-cell (stream)
		    (if has-default
		      ;;--- Fix this
		      (progn ;abbreviating-output (stream :height 1 :show-abbreviation t)
			(prin1 init-default stream))
		      (write-string "" stream))))))))))))

(defun document-generic-function (generic-function &key (stream *standard-output*))
  (let ((arg-precedence (generic-function-argument-precedence-order generic-function))
	(lambda-list (generic-function-lambda-list generic-function)))
    (flet ((print-arglist (arglist stream)
	     (print-lambda-list arglist stream :brief t)))
      (declare (dynamic-extent #'print-arglist))
      (fresh-line stream)
      (format stream "~%Generic function ~/presentation/ "
	(list generic-function 'generic-function))
      (print-arglist lambda-list stream)
      (unless (do ((args lambda-list (cdr args))
		   (prec arg-precedence (cdr args)))
		  ((null prec) t)
		(unless (eql (car args) (car prec))
		  (return nil)))
	(fresh-line stream)
	(write-string "has argument precedence " stream)
	(print-arglist arg-precedence stream))
      (let ((combination #+Genera (clos-internals::method-combination-definition-name
				   (clos:generic-function-method-combination generic-function))
			 #+Lispworks (clos::method-combination-name
				      (clos:generic-function-method-combination generic-function))
			 #+Allegro (excl::method-combination-type
				    (clos:generic-function-method-combination generic-function))
			 #+CCL (ccl::method-combination-name
				(ccl::generic-function-method-combination generic-function))))
	(fresh-line stream)
	(format stream "Its methods are combined using ~S method combination." combination))
      (let ((source
	     #+Genera (si:get-source-file-name 
		       (clos:generic-function-name generic-function) 'defun)
	     #+Lispworks nil
	     #+CCL (loop for x in (ccl:get-source-files (ccl:function-name generic-function))
		         unless (listp x)
		           do (return x))
	     #+Allegro (excl:source-file generic-function)))
	(when source
	  (fresh-line stream)
	  (format stream "There is an explicit ~A in file ~A." 'defgeneric source)))))) 


;;; The CLOS class browser

(define-command-table class-browser-filters)

(add-menu-item-to-command-table 'class-browser-filters "Direct Methods"
				:command 'com-direct-methods
				:errorp nil)

(add-menu-item-to-command-table 'class-browser-filters "All Methods"
				:command 'com-all-methods
				:errorp nil)

(add-menu-item-to-command-table 'class-browser-filters "method-divider" 
				:divider  NIL
				:errorp nil)

(add-menu-item-to-command-table 'class-browser-filters "Direct Slots"
				:command 'com-direct-slots
				:errorp nil)

(add-menu-item-to-command-table 'class-browser-filters "All Slots"
				:command 'com-all-slots
				:errorp nil)

(define-command (com-direct-methods :command-table class-browser-filters) ()
  #-MCL
  (let ((command-table (find-command-table 'class-browser-filters)))
    (setf (class-browser-show-direct-methods *application-frame*) T
	  (command-enabled "All Methods" *application-frame* command-table) T
	  (command-enabled "Direct Methods" *application-frame* command-table) NIL))
  #+MCL
  (setf (class-browser-show-direct-methods *application-frame*) T
	(command-enabled "All Methods" *application-frame*) T
	(command-enabled "Direct Methods" *application-frame*) NIL))

(define-command (com-all-methods :command-table class-browser-filters) ()
  #-MCL
  (let ((command-table (find-command-table 'class-browser-filters)))
    (setf (class-browser-show-direct-methods *application-frame*) NIL
	  (command-enabled "Direct Methods" *application-frame* command-table) T
	  (command-enabled "All Methods" *application-frame* command-table) NIL))
  #+MCL
  (setf (class-browser-show-direct-methods *application-frame*) NIL
	(command-enabled "Direct Methods" *application-frame*) T
	(command-enabled "All Methods" *application-frame*) NIL))

(define-command (com-direct-slots :command-table class-browser-filters) ()
  #-MCL
  (let ((command-table (find-command-table 'class-browser-filters)))
    (setf (class-browser-show-direct-slots *application-frame*) T
	  (command-enabled "All Slots" *application-frame* command-table) T
	  (command-enabled "Direct Slots" *application-frame* command-table) NIL))
  #+MCL
  (setf (class-browser-show-direct-slots *application-frame*) T
	(command-enabled "All Slots" *application-frame*) T
	(command-enabled "Direct Slots" *application-frame*) NIL))

(define-command (com-all-slots :command-table class-browser-filters) ()
  #-MCL
  (let ((command-table (find-command-table 'class-browser-filters)))
    (setf (class-browser-show-direct-slots *application-frame*) NIL
	  (command-enabled "Direct Slots" *application-frame* command-table) T
	  (command-enabled "All Slots" *application-frame* command-table) NIL))
  #+MCL
  (setf (class-browser-show-direct-slots *application-frame*) NIL
	(command-enabled "Direct Slots" *application-frame*) T
	(command-enabled "All Slots" *application-frame*) NIL))

#|
(add-menu-item-to-command-table
  'class-browser-filters "methods" :radio-box
  '(:items (("Direct methods" :value t) ("All methods" :value nil))
    :callback (lambda (frame val)
                (setf (class-browser-show-direct-methods frame) (second val)))))
(add-menu-item-to-command-table
  'class-browser-filters "slots" :radio-box
  '(:items (("Direct slots" :value t) ("All slots" :value nil))
    :callback (lambda (frame val)
                (setf (class-browser-show-direct-slots frame) (second val)))))
|#

(defvar *class-browser-default-depth* 2
   "The default depth for tree graph displays.") 

;;--- What other stuff do we need in the menu bar?
(define-application-frame class-browser (browser-frame)
  ((class :initarg :class :initform nil
	  :accessor class-browser-class)
   (direct-methods :initform t
		   :accessor class-browser-show-direct-methods)
   (direct-slots   :initform t
		   :accessor class-browser-show-direct-slots))
  (:default-initargs :graph-type :graphical
   :browser-type :class
   :browser-subtype :subclasses	;or :superclasses...
   :browser-options nil
   :presentation-type 'class
   :node-maker #'make-class-call-node
   :root-node-maker #'make-class-browser-root
   :browser-depth *class-browser-default-depth*)
  (:command-table (class-browser :inherit-from (browser-activity
						editing
						inspection
						clos
						browser-frame
						selected-objects
						accept-values-pane)
				 :menu (("Activity" :menu browser-activity)
					("Selections" :menu selected-objects)
					("Filters" :menu class-browser-filters))))
  (:top-level (class-browser-top-level))
  (:pointer-documentation t)
  (:panes
    (class :accept-values
	   :height :compute
	   :display-function
	   '(accept-values-pane-displayer
	     :displayer read-class-browser-class)
	   :text-style '(:fix :roman :large)
	   :scroll-bars nil)
    (direction (make-pane 'option-pane
			  :label "Direction"
			  :items '(("Subclasses" . :subclasses)
				   ("Superclasses" . :superclasses))
			  :name-key #'car :value-key #'cdr
			  :value :subclasses
			  :value-changed-callback 'class-browser-direction-changed))
    (graph :application
	   :background +white+
	   :display-function 'display-graph-pane
	   :display-after-commands nil
	   :incremental-redisplay t
	   :scroll-bars :both
	   :end-of-page-action :allow
	   :end-of-line-action :allow)
    (slots :application
	   :background +white+
	   :display-after-commands nil
	   :scroll-bars :both
	   :end-of-page-action :allow 
	   :end-of-line-action :allow)
    (methods :application
	     :background +white+
	     :display-after-commands nil
	     :scroll-bars :both
	     :end-of-page-action :allow 
	     :end-of-line-action :allow))
  (:layouts
    (main
      (vertically () 
	(horizontally ()
	  (:fill #+Genera (outlining () (spacing () class))
		 #-Genera class)
	  (1/5 direction))
	(:fill graph)
	(1/4 (horizontally ()
	       (1/2 slots)
	       (1/2 methods)))))))

(defmethod class-browser-top-level ((frame class-browser))
  (enable-frame frame)
  (configure-for-browser-type frame (browser-subtype frame))
  (default-frame-top-level frame))

(defmethod browser-display-pane ((frame class-browser))
  (get-frame-pane frame 'graph))

(defmethod frame-redisplay :after ((browser class-browser))
  (redisplay-frame-pane browser 'slots)
  (redisplay-frame-pane browser 'methods))

(defmethod read-class-browser-class ((frame class-browser) stream)
  (with-slots (class) frame
    (when (null class)
      (setq class (or (let ((element (yank-from-history 
				       (presentation-type-history 'class))))
			(and element 
			     (presentation-history-element-object element)))
		      (find-class 't))))
    (multiple-value-bind (new-class type changed-p)
	(accept 'class
		:default class :prompt nil
		:stream stream)
      (declare (ignore type))
      (when changed-p
	(setq class new-class)
	(generate-browser-display frame class (browser-display-pane frame))))))

(defun class-browser-direction-changed (pane value)
  (let ((frame (pane-frame pane)))
    (setf (browser-subtype frame) value)
    (configure-for-browser-type frame (browser-subtype frame))))

(defmethod note-browser-root-changed ((browser class-browser) objects)
  (setf (class-browser-class browser) objects)
  (redisplay-frame-pane browser 'class :force-p t))


;;; CLOS browser call nodes, etc.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass class-call-node (call-node) ()))

(declaim (inline make-class-call-node))
(defun make-class-call-node (object)
  (make-instance 'class-call-node :object object))

(defmethod node-object-name ((node class-call-node))
  (class-name (node-object node)))

(defmethod display-node ((node class-call-node) stream)
  (with-output-as-presentation (stream node 'class-call-node)
    (present (node-object node) 'class :stream stream)))

(defun make-class-browser-root (object)
  (typecase object
    (class
      (make-class-call-node object))
    (symbol
      (let ((class (find-class object nil)))
	(when class
	  (make-class-call-node class))))))


(defmethod node-generate-inferior-objects
    ((node class-call-node) (type (eql ':superclasses)))
  (class-direct-superclasses (node-object node)))

(defmethod node-any-inferior-objects-p
    ((node class-call-node) (type (eql ':superclasses)))
  (not (null (class-direct-superclasses (node-object node)))))

(defmethod node-generate-inferior-objects
    ((node class-call-node) (type (eql ':subclasses)))
  (class-direct-subclasses (node-object node)))

(defmethod node-any-inferior-objects-p
    ((node class-call-node) (type (eql ':subclasses)))
  (not (null (class-direct-subclasses (node-object node)))))

(defmethod node-generate-superior-objects
    ((node class-call-node) (type (eql ':superclasses)))
  (node-generate-inferior-objects node :subclasses))

(defmethod node-any-superior-objects-p
    ((node class-call-node) (type (eql ':superclasses)))
  (node-any-inferior-objects-p node :subclasses))

(defmethod node-generate-superior-objects
    ((node class-call-node) (type (eql ':subclasses)))
  (node-generate-inferior-objects node :superclasses))

(defmethod node-any-superior-objects-p
    ((node class-call-node) (type (eql ':subclasses)))
  (node-any-inferior-objects-p node :superclasses))


(defclass clos-slot-call-subnode (call-subnode) ())

(declaim (inline make-clos-slot-call-subnode))
(defun make-clos-slot-call-subnode (class-node slots)
  (let ((subnode (make-instance 'clos-slot-call-subnode :object slots)))
    (setf (node-superiors subnode) (list class-node))
    subnode))

(defmethod node-object-name ((node clos-slot-call-subnode))
  (format nil "Slots of ~S" (node-object-name (first (node-superiors node)))))

(defmethod display-node ((node clos-slot-call-subnode) stream)
  (let ((slots (node-object node)))
    (with-output-as-presentation (stream node 'clos-slot-call-subnode
				  :single-box :highlighting)
      (surrounding-output-with-border (stream :shape :rectangle)
	(formatting-table (stream)
	  (dolist (slot slots)
            (updating-output (stream :unique-id slot)
	      (formatting-row (stream)
	        (formatting-cell (stream)
		  (present slot 'symbol :stream stream))))))))))


(defclass clos-method-call-subnode (call-subnode) ())

(declaim (inline make-clos-method-call-subnode))
(defun make-clos-method-call-subnode (class-node method-list)
  (let ((subnode (make-instance 'clos-slot-call-subnode :object method-list)))
    (setf (node-superiors subnode) (list class-node))
    subnode))

(defmethod node-object-name ((node clos-method-call-subnode))
  (format nil "Methods of ~S" (node-object-name (first (node-superiors node)))))

(defmethod display-node ((node clos-method-call-subnode) stream)
  (let ((methods (node-object node)))
    (with-output-as-presentation (stream node 'clos-method-call-subnode
				  :single-box :highlighting)
      (surrounding-output-with-border (stream :shape :rectangle)
	(formatting-table (stream)
	  (dolist (item methods)
            (updating-output (stream :unique-id item)
	      (formatting-row (stream)
	        (formatting-cell (stream)
		  (destructuring-bind (gf method) item
		    (declare (ignore method))
		    (present gf 'generic-function :stream stream)))))))))))


;;; The class browser commands

(defmethod configure-for-browser-type ((browser class-browser) subtype)
  (ecase subtype
    (:subclasses
     (setf (browser-grapher-args browser)
           '(:arc-drawer draw-arrow-arc)))
    (:superclasses
     (setf (browser-grapher-args browser)
	   '(:arc-drawer draw-arrow-arc :arc-drawing-options (:from-head t :to-head nil)))))
  (setf (browser-subtype browser) subtype)
  ;; Now regenerate the display
  (generate-browser-display
    browser (class-browser-class browser) (browser-display-pane browser)))


(define-class-browser-command (com-add-slots-subnode :name "Add Slots Subnode")
    ((class-node 'class-call-node
		 :prompt "class node to show slots for" :gesture :subnode-1))
  (with-application-frame (browser)
    (let* ((class (node-object class-node))
	   (slots (sort (mapcar #'slot-definition-name
				(#-MCL clos:class-direct-slots #+MCL aisl-clos:class-direct-slots class))
		        #'string-lessp)))
      (when (and slots
	         (not (subnode-object-present-in-node class-node slots :test #'equal)))
        (let ((subnode (make-clos-slot-call-subnode class-node slots)))
	  (push subnode (node-inferiors class-node))
	  (tick-node subnode)
          (redisplay-frame-pane browser (browser-display-pane browser)))))))

(define-class-browser-command (com-add-methods-subnode :name "Add Methods Subnode")
    ((class-node 'class-call-node
		 :prompt "class node to show methods for" :gesture :subnode-2))
  (with-application-frame (browser)
    (let* ((class (node-object class-node))
	   (methods (specializer-direct-methods class))
	   (method-list
	    (loop for method in methods
		  collect (list (method-generic-function method) method))))
      (setq method-list (sort method-list #'string-lessp
			      :key #'(lambda (item)
				       (let ((name (generic-function-name (first item))))
				         (if (listp name) (second name) name)))))
      (when (and method-list
                 )
        (let ((subnode (make-clos-method-call-subnode class-node method-list)))
	  (push subnode (node-inferiors class-node))
	  (tick-node subnode)
          (redisplay-frame-pane browser (browser-display-pane browser)))))))

(define-class-browser-command (com-show-class-details :name "Show Class Details")
    ((class 'class :gesture :describe))
  (with-application-frame (browser)
    (display-slots-pane browser class)
    (display-methods-pane browser class)))

(defmethod display-slots-pane ((browser class-browser) class)
  #+Allegro (unless (clos:class-finalized-p class)
	      (clos:finalize-inheritance class))
  (let* ((stream (get-frame-pane browser 'slots))
         (slots nil))
    (if (class-browser-show-direct-slots browser)
      (dolist (slot (#-MCL clos:class-direct-slots #+MCL class-direct-slots class))
	(pushnew slot slots :key #'slot-definition-name))
      (dolist (class (class-precedence-list class))
	(dolist (slot (#-MCL clos:class-direct-slots #+MCL class-direct-slots class))
	  (pushnew slot slots :key #'slot-definition-name))))
    (window-clear stream)
    (when slots
      (setq slots (sort slots #'string-lessp 
		        :key  #'slot-definition-name))
      (silica:inhibit-updating-scroll-bars #+Allegro (stream)
        (dolist (slot slots)
	  (document-slot slot
                         #+CCL class
		         :highlight-name t :verbose t
		         :stream stream))))))

(defmethod display-methods-pane ((browser class-browser) class)
   #+Allegro (unless (clos:class-finalized-p class)
	      (clos:finalize-inheritance class))
  (let ((stream (get-frame-pane browser 'methods))
        (methods nil)
        (interesting-types nil))
    (loop for cl in (class-precedence-list class)
          do (push cl interesting-types)
	  do (dolist (method (specializer-direct-methods cl))
	       (pushnew method methods))
          until (and (class-browser-show-direct-methods browser)
                     (eql cl class)))
    (window-clear stream)
    (when methods
      (setq methods (sort methods #'generic-function-sort-name-predicate
			  :key #'method-generic-function))
      (silica:inhibit-updating-scroll-bars #+Allegro (stream)
        (dolist (method methods)
	  (document-method method
			   :only-these-types interesting-types
			   :stream stream))))))


;;; The CLOS generic function browser

;;--- What other stuff do we need in the menu bar?
(define-application-frame generic-function-browser (selected-object-mixin)
  ((generic-function :initarg :generic-function :initform nil
		     :accessor generic-function-browser-function))
  (:command-table (generic-function-browser :inherit-from (activity
							   editing
							   inspection
							   clos
							   browser-frame
							   selected-objects
							   accept-values-pane)
					    :menu (("Activity" :menu activity)
						   ("Selections" :menu selected-objects))))
  (:top-level (generic-function-browser-top-level))
  (:pointer-documentation t)
  (:panes
    (generic-function :accept-values
		      :height :compute
		      :display-function
		        '(accept-values-pane-displayer
			   :displayer read-generic-function-browser-function)
		      :text-style '(:fix :roman :large)
		      :scroll-bars nil)
    (description :application
		 :background +white+
		 :display-after-commands nil
		 :scroll-bars :both
		 :end-of-page-action :allow
		 :end-of-line-action :allow)
    (methods :application
	     :background +white+
	     :display-after-commands nil
	     :scroll-bars :both
	     :end-of-page-action :allow 
	     :end-of-line-action :allow)
    (code :application
	  :background +white+
	  :display-after-commands nil
	  :scroll-bars :both
	  :end-of-page-action :allow 
	  :end-of-line-action :allow))
  (:layouts
    (main
      (vertically () 
	#+Genera (outlining () (spacing () generic-function))
	#-Genera generic-function
	(1/5 description)
	(:fill methods)
	(1/4 code)))))

(defmethod generic-function-browser-top-level ((frame generic-function-browser))
  (enable-frame frame)
  (generate-generic-function-browser-display frame)
  (default-frame-top-level frame))

(defmethod read-generic-function-browser-function ((frame generic-function-browser) stream)
  (with-slots (generic-function) frame
    (when (null generic-function)
      (setq generic-function
	    (or (let ((element (yank-from-history 
				 (presentation-type-history 'generic-function))))
		  (and element 
		       (presentation-history-element-object element)))
		#'class-name)))
    (multiple-value-bind (new-gf type changed-p)
	(accept 'generic-function
		:default generic-function :prompt nil
		:stream stream)
      (declare (ignore type))
      (when changed-p
	(setq generic-function new-gf)
	(generate-generic-function-browser-display frame)))))

(defmethod generate-generic-function-browser-display ((frame generic-function-browser))
  ;; Bind *ORIGINAL-STREAM* to NIL so that this can get called from
  ;; inside of accept-values panes.  Yetch!
  (let ((*original-stream* nil)
	(generic-function (generic-function-browser-function frame)))
    (let ((stream (get-frame-pane frame 'description)))
      (window-clear stream)
      (silica:inhibit-updating-scroll-bars #+Allegro (stream)
	(document-generic-function generic-function
	  :stream stream)))
    (let ((stream (get-frame-pane frame 'methods)))
      (window-clear stream)
      (silica:inhibit-updating-scroll-bars #+Allegro (stream)
	(let ((methods-list (sort-generic-function-methods
			      generic-function
			      (copy-list (generic-function-methods generic-function))
			      :sort :alphabetical)))
	  (dolist (method methods-list)
	    (with-output-as-presentation (stream method 'method)
	      (document-method method
		:only-these-types :all
		:show-generic-function-name nil
		:stream stream))))))))

(define-generic-function-browser-command (com-show-method-code :name "Show Method Code")
    ((method 'method :gesture :describe))
  (let* ((stream (get-frame-pane *application-frame* 'code))
	 (generic-function (method-generic-function method))
	 (specializers 
           (mapcar #'(lambda (s)
                       (if (typep s 'class)
			 (list :class s)
			 (list :object s)))
		   (method-specializers method)))
	 (applicable-methods
	   (find-applicable-methods generic-function specializers))
	 (effective-method
	   (and applicable-methods
	        #+Genera (clos-internals::compute-effective-method-1
			   generic-function applicable-methods
			   #'(lambda (type position)
			       (declare (ignore type))
			       (nth position specializers)))
	        #-Genera (compute-effective-method
			   generic-function
			   (generic-function-method-combination generic-function)
			   applicable-methods))))
    (window-clear stream)
    (when effective-method
      (let ((*print-pretty* t))
	(format stream "~S" effective-method)))))
