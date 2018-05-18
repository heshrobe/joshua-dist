;;; -*- Mode: Commom-lisp; Package: CC; readtable: joshua; syntax: joshua -*-

(in-package :cc)

(eval-when (:compile-toplevel :load-toplevel :execute) (enable-joshua))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Overview
;;;
;;; This is a congruence closure package, similar to 
;;; the one used in McAllester's RUP and probably even
;;; more similar to the one used in my (Shrobe) PhD reasoning
;;; system.
;;;
;;; Like in my thesis (and more recent congruence closure systems
;;; see paper in josh-dist/joshua/cc-book.pdf) Predications are flattened
;;; to term free form by substituting a skolem constant for terms nested in 
;;; in predications and then asserting that the term and the constant are congruent
;;;
;;; Internally to this package we use a variant of Union-Find to manage internal data
;;; structures that quickly compute equivalent classes.
;;;
;;; There will be 3 types of constants in this system:
;;;
;;; 1) Symbols: Symbols in the external predication and are by definition
;;; not congruent to one another: I.e. attempting to assert x == y signals a contradiction
;;;
;;; 2) Surrogates: These represent nested non-atomic terms such as (f x).  
;;; There are generated internally to the cc package and are asserted to be congruent
;;; to the term they replace.  These can be deduced to be congruent to one another and to constants 
;;;
;;; 3) Anonymous individuals: These are used by the external reasoning system for use in either
;;; existential-elimination: [exists (x) [P x]] => [P g0001] where g0001 is a gensym of this type
;;; or universal-introduction proving [p g0001] => [for-all (x) [p x]]
;;;
;;; When these guys are asserted to be congruent, there is a "better name" ordering
;;; Symbols > Surrogates > Anonymous
;;;
;;; The external interface includes predications of the form:
;;;
;;; [Congruent x y] where x and y are one of the 3 types above
;;;  Telling this merges the equivalence classes of x and y
;;;  Asking this checks whether the equivalence class of x and y are the same
;;;  All of this is supposed to be merged with the LTMS so that these assertions are retractable.
;;;
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Equivalence-Classes are indexed in a hash-table with the key being the external symbol
(defvar *equivalence-class-hash-table* (make-hash-table :test #'eql))

;;; Terms are indexed in a hash-table using the list expression as the key
(defvar *term-dn* (make-hash-table :test #'equal))

(defun init () 
  (clrhash *equivalence-class-hash-table*) 
  (clrhash *term-dn*) nil)

(defclass equivalence-class ()
  (;; the externally visible symbol that represents this class
   (representative :accessor representative :initarg :representative)  
   ;; number of members of the equivalence class
   (size :initform 1 :accessor size)
   ;; if non-nil the class I'm merged into
   ;; When classes are merged this is forwarded to the representative
   ;; of the merged equivalence class
   (parent :initform nil :accessor parent :initarg :parent)
   ;; The 
   (parent-justification :initform nil :accessor parent-justification)
   ;; all members of this equivalence class
   ;; If I'm merged with another class, this isn't updated
   ;; That's so that retraction can work better, I think
   (members :initarg :members :accessor members)
   ;; I guess I could just have one slot for both and have the code
   ;; typecase how to handle them
   ;; predications that I appear in 
   (predications :Initarg :predications :accessor predications :initform nil)
   ;; compound terms (e.g. (f x)) that I appear in
   (terms :initarg :terms :initform nil :accessor terms)
   ;; A hash table of all CONGURENT predications linking this to another equivalence class
   ;; indexed by the target of the CONGRUENT predications
   (parent-assertions :initform (make-hash-table :test #'eql) :accessor parent-assertions)
   )
  )

;;; This doesn't find the canonical version, just gives you the direct
;;; correlate of the symbol.  To get the equivalence class modulo
;;; congruence closure call find (which calls this).
(defun intern-symbol (symbol &optional (class 'constant))
  (or (gethash symbol *equivalence-class-hash-table*)
      (make-instance class :representative symbol)))

;;; Index these guys as they are created
(defmethod initialize-instance :after ((c equivalence-class) &key representative &allow-other-keys)
  (setf (gethash representative *equivalence-class-hash-table*) c)
  (setf (members c) (list c)))

;;; These guys print as their external form
(defmethod print-object ((c equivalence-class) stream)
  (with-slots (representative) c
    (princ representative stream)))

;;; These are the 3 sub-classes that are actually instantiated
;; These are unique symbols that are never congruent to one another
(defclass constant (equivalence-class) ())

(defmethod external-form ((c constant))
  (representative c))

;; This is skolem constance used by the reasoning system
(defclass anonymous-individual (equivalence-class) ())

(defmethod external-form ((a anonymous-individual))
  (representative a))

(defclass term-surrogate (equivalence-class)
  (;; what term am I a surrogate for
   (surrogate-for :accessor surrogate-for :initarg :surrogate-for)
   ))

(defmethod external-form ((ts term-surrogate))
  (expression (surrogate-for ts)))

(defmethod print-object ((ts term-surrogate) stream)
  (princ (surrogate-for ts) stream))


;;; A syntax extension to introduce anonymous individuals
(defmacro with-anonymous-individual ((variable string) &body body)
  `(let ((,variable (intern-symbol (cl:intern (gensym ,string)) 'anonymous-individual)))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Interning non-atomic terms
;;;
;;; When a predication is created all non-atomic terms are replaced by 
;;; a symbol (gensym'd) that is the representative of an equivalence class
;;; These terms are interned so that equal terms are eql
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass term ()
  (;; The expression of the term
   (expression :initarg :expression :accessor expression)
   ;; The class of the symbol that stands for this term
   (representative :accessor representative :initarg :representative)
   (depth :initform 1 :accessor depth)
   )
  )

(defmethod depth ((exp equivalence-class)) 1)

(defmethod depth ((ts term-surrogate)) 
  (depth (surrogate-for ts)))

;;; prints out in terms of its canonical form
(defmethod print-object ((term term) stream)
  (with-slots (expression) term
    (princ expression stream)))

;;; Intern-term is the function that's supposed to be used for creating these guys
;;; (not make-instance directly).
;;; Itern-term flattens the expression, interning all non-atomic sub-terms
;;; But not replacing things by their merged sets

;;; Call this with a canonicalized non-atomic term (i.e. a list)
(defun intern-term (expression)
  (let ((flattened-expression (cons (first expression)
				    (loop for item in (rest expression)
					collect (typecase item
						  ((or symbol list) (intern item))
						  (otherwise item))))))
    (multiple-value-bind (term new-p) (dn-put-term flattened-expression)
      (let* ((representative (representative term)))
	(when new-p
	  (setf (depth term)
	    (1+
	     (loop for item in (rest flattened-expression)
		 maximize (depth item))))
	  ;; Note: this part is being done for effect
	  ;; It returns the unmodified term!!!
	  ;; Check whether sub-structure includes items
	  ;; that are already have a better name
	  ;; and if so induce the implied congruence of me
	  ;; and to cause predication rewriting of any use of me
	  ;; TO DO: Below isn't right
	  (loop for item in (rest (expression term))
	      for canonical-item = (find item)
	      unless (eql item canonical-item)
		     ;; Parent-justification should be a CONGRUENT predication
		     ;; linking the item to its target (i.e. canonical-item)
	      do (rewrite-term term item canonical-item (parent-justification item)))
	  )
	(values term representative new-p)))))

(defun dn-put-term (list)
  (let ((answer nil)
	(new nil))
    (loop for tokens on list
	for token = (first tokens)
	for ht = *term-dn* then next-ht
	for next-ht = (gethash token ht)
	do (if (rest tokens)
	       ;; more to go
	       (unless next-ht
		 ;; create new branch
		 (setq next-ht (make-hash-table :test #'eql))
		 (setf (gethash token ht) next-ht))
	     ;; wer're at the end
	     (cond
	      ((null next-ht)
	       ;; need to create answer
	       (setq new t
		     answer (make-instance 'term :expression list))
	       (setf (gethash token ht) answer))
	      ;; we found answer and it's not new
	      (t (setq answer next-ht)))))
    (values answer new)))

;;; We need a lookup method that can handle variables which means recursive continuation
;;; passing

;;; After a term is created you need to link it to equivalence class of every symbol inside in
;;; Note that this external interface is "intern-term" which interns all nested terms, so by
;;; this point the expression is flat.
(defmethod initialize-instance :after ((term term) &key expression representative &allow-other-keys)
  (loop for item in (rest expression) do (push term (terms item)))
  (setf (representative term) (or representative
				  (make-instance 'term-surrogate
				    :surrogate-for term
				    :representative (gensym (string (first expression)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Intern
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod intern ((s symbol))
  (intern-symbol s))

(defmethod intern ((e equivalence-class)) e)

(defmethod intern ((term list))
  (multiple-value-bind (term equivalence-class) (intern-term term)
    (declare (ignore term))
    equivalence-class))

(defmethod intern ((term term)) term)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Equate is the external interface to the Union part of union-find
;;; It merges two equivalence classes
;;; It is polymorphic so that you can call it with symbols or
;;; equivalence classes, but I'm not going to bother with
;;; mixing up the two, i.e. either call with symbols and lists
;;; or with equivalence classes and terms
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (defmethod equate ((s1 symbol) (s2 symbol) justification)
;   (union s1 s2 justification))

; (defmethod equate ((expression list) (s symbol) justification)
;   (equate (intern-term expression) (intern-symbol s) justification))

; (defmethod equate ((s symbol) (expression list) justification)
;   (equate (intern-term expression) (intern-symbol s) justification))

; (defmethod equate ((term term) (constant equivalence-class) justification)
;   (union constant (representative term) justification))

; (defmethod equate ((constant equivalence-class) (term term) justification)
;   (union constant (representative term) justification))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Union
;;; this is the UNION part of union-find
;;; It agressively advances the parent pointer of all members of the equivalence class
;;; as the two classes are merged.
;;;
;;; Union is not an external interface.  It's only called when 
;;; from the act-on-truth-value-change method for congruent assertions.
;;; 
;;; To Do: These union methods are just conveniences for developer and 
;;; should probably go away
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric union (class-1 class-2 triggering-congruence))

; (defmethod union ((c1 symbol) (c2 symbol) justification)
;   (union (intern-symbol c1 'constant)
; 	 (intern-symbol c2 'constant)
; 	 justification))

;;; Union is supposed to set the parent of the smaller to the bigger
;;; If both sets are the same size, c1 wins
;;;
;;; This updates the parent pointer of each member of the set
;;; at the time of the merger.
;;; However it doesn't remover members from the victim set or reset its
;;; count to 0.  This is to allow a relatively simpler version of the
;;; retraction algorithm in which sets are unmerged.

;;; A condition to signal when attempting to merge two constants
(define-condition invalid-union (error)
  ((constant1 :Initform nil :initarg :constant1 :accessor constant1)
   (constant2 :Initform nil :initarg :constant2 :accessor constant2)))

(defun preferred-target (c1 c2)
  (let ((size-1 (size c1))
	(size-2 (size c2)))
    (if (or 
	 ;; Constants dominate other types
	 (and (typep c2 'constant)
	      (or (typep c1 'anonymous-individual)
		  (typep c1 'term-surrogate)))
	 ;; Term surrogates dominate Anonymous-individuals
	 (and (typep c2 'term-surrogate)
	      (typep c1 'anonymous-individual))
	 ;; same type
	 (and (eql (type-of c1) (type-of c2))
	      (or 
	       ;; less deep expressions are better (maybe only if equal size)
	       (< (depth c2) (depth c1))
	       ;; finally size counts, merge smaller into bigger
	       (< size-1 size-2))))
	c2
      c1)))

;;; Union is the core of congruence closure
;;; It is only invoked by a CONGRUENT assertion coming in.
;;; That assertion is the third argument
(defmethod union ((original-c2 equivalence-class) (original-c1 equivalence-class) triggering-congruence)
  ;; make sure we have the canoncial versions
  (let* ((c1 (find original-c1))
	 (c2 (find original-c2))
	 ;; getthese before anything changes
	 (parent-just-1 (parent-justification original-c1))
	 (parent-just-2 (parent-justification original-c2))
	 (size-1 (size c1))
	 (size-2 (size c2)))
    (cond
     ((eql c1 c2) c1)
     ((and (typep c1 'constant) (typep c2 'constant))
      (error 'invalid-union
	     :constant1 c1
	     :constant2 c2))
     (t
      ;; So we're actually going to do a merge of equivalence-classes
      ;; have to determine who merges into whom
      ;; C2 is going to be the victim that is merged into C1
      ;; There are two reasons for chosing the victim:
      ;; 1) Constant > term-Surrogate > Anonymous-individual
      ;; 2) They are of the same type, then the victim is the smaller set
      ;; In the code below C2 is the victim, so swap the C! & C2 if that's not the case
      (when (eql c2 (preferred-target c1 c2))
	(rotatef original-c1 original-c2)
	(rotatef c1 c2)
	(rotatef size-1 size-2))
      ;; Update the size of c1 to the sum of the two sets' sizes
      (incf (size c1) size-2)
      ;; below we advance the parent pointers of all other members of the c2 class
      ;; but before we make any other assertions we advance the parent pointer of c2 itself
      ;; Which will stop us from doing anything if we re-deduce that c1 and c2 are congruent
      (setf (parent c2) c1)
      ;;
      ;; Now onto asserting deduced congruences
      ;; Note: There are four cases {c1 = original-c1}x{c2 = orignal-c2}
      ;; We're giong to make all members of the c2 class conguent to c1
      ;; We're not going to bother making them congruent to original-c1
      ;; even when that's different from c1.  We'll defer worrying about 
      ;; that to when the congruence between original-c1 and c1 is retracted.
      ;; Of course if original-c1 isn't c1 we have to make sure that 
      ;; c2 is made congruent to original-c1 (even if c2 isn't original-c2)
      ;; 
      ;; The first assertion we're going to make is that c2 is congruent to c1
      ;; But only if original-c1 isn't c1 or original-c2 isn't c-2
      ;; The justification is the passed in congruence, plus the membership assertions
      ;; for original-c1 and original-c2 if they are different from c1 and c2 respectively
      (let* ((deduced-congruence-support (list triggering-congruence)))
	(when parent-just-1 (push parent-just-1 deduced-congruence-support))
	(when parent-just-2 (push parent-just-2 deduced-congruence-support))
	(let (;; this is why c2 is congruent to c1
	      (deduced-congruence (if (and (eql c1 original-c1) (eql c2 original-c2))
				      ;; if both c1 and c2 are in the original assertion
				      ;; then the triggering assertion is this guy
				      triggering-congruence
				    (tell `[congruent ,c2 ,c1]
					  :justification `(congruence-closure ,deduced-congruence-support)))))
	  (setf (parent-justification c2) deduced-congruence)
	  (push c2 (members c1))
	  (loop for member in (members c2)
	      for member-parent-just = (parent-justification member)
	      unless (eql member c2)
	      do (setf (parent member) c1)
		 (push member (members c1))
		 (setf (parent-justification member)
		   (if (and (eql member original-c2) (eql c1 original-c1))
		       ;; If this member was the guy in the original assertion
		       ;; and that assertion links to c1 (i.e. c1 = original-c1)
		       ;; then we've already made this assertion
		       triggering-congruence
		     ;; otherwise we have to construct the congruence and its justification
		     (tell `[Congruent ,member ,c1]
			   :justification `(Congruence-closure 
					    (,deduced-congruence
					     ;; why this guy is a member of this class
					     ,member-parent-just
					     ))))))
	  ;; Now we have to check if c2 had any non-congruence assertions
	  ;; and if so map them up to c1
	  (loop for assertion being the hash-values of (parent-assertions c2) using (hash-key target)
	      when (eql (predication-truth-value assertion) *false*)
	      do (tell `[not [congruent ,original-c1 ,target]]
		       :justification `(congruence-closure (,triggering-congruence) (,assertion))))
	  ;; Now rewrite all terms and predications that mention c2
	  (loop for term in (terms c2)
	      do (rewrite-term term c2 c1 deduced-congruence))
	  (loop for predication in (predications c2)
	      do ;; the first of these is necessary to not lose intermediate rewrites
		(rewrite-predication predication original-c2 original-c1 deduced-congruence) 
		;; (rewrite-predication predication c2 c1 deduced-congruence)
		))
	(values c1 c2)
	)))))

;;; This is called at notice-truth-value time
;;; and cleans up book keeping so that this guy 
;;; doesn't point to the old class he was in but
;;; to the representative most of his general 
;;; class.
;;; Children's membership will be taken care of
;;; when their CONGRUENT assertion goes out
;;; 
;;; To Do: We should also try to keep the non-congruence assertions
;;; consistent with the Union find structure
;;; I.e. If a set member has an explicit non-congruence assertion to another set
;;; Then after we find the represenative of his most general set
;;; we should propagate the non-congruence to the represenatives of both sets

(defun ununion (victim old-parent)
  ;; Remember that victim is a member of victim's equiv class
  ;; so this loop gets him too
  (when (member victim (members old-parent))
     (setf (members old-parent)
       (delete victim (members old-parent)))
     (decf (size old-parent))
     (setf (parent victim) nil
	   (parent-justification victim) nil)
     ;; Now that the union-find structure has been split
     ;; Find the victim's most general remaining class representative
     ;; and update his pointers to point there
     (loop with best-parent and best-parent-justification
	 for parent being the hash-keys of (parent-assertions victim) using (hash-value parent-assertion)
	 when (and (eql (predication-truth-value parent-assertion) *true*)
		   (or (null best-parent)
		       (eql (parent best-parent) parent)))
	 do (setq best-parent parent
		  best-parent-justification parent-assertion)
	 finally (when best-parent
		   (setf (parent victim) best-parent
			 (parent-justification victim) best-parent-justification))))
  )


;;; TO DO: Shouldn't this return the rewritten term??
;;; Rewrite term is called from intern-term
;;; Which will examine substructure of the new-term and get other
;;; congruent terms rewritten (so he really only has to call me
;;; when he finds first sub-term with a better name?)
;;; as well as from Union
;;; Justification is the congruent predication
;;; saying that old is congruent to new
(defun rewrite-term (term old new justification)
  (if (eql old new)
      term
    (let* ((old-surrogate (representative term))
	   (new-expression (subst new old (expression term)))
	   (new-term (intern-term new-expression)))
      (if (eql term new-term)
	  term
	(let ((new-surrogate (representative new-term)))
	  (tell `[congruent ,old-surrogate ,new-surrogate]
		:justification `(congruence-closure (,justification)))
	  new-term)))))


;;; Note on unmerging:
;;; Since you know everybody who is under the newly freed child
;;; you just need to update their parent pointers and remove 
;;; them from any set between you and the parent.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Find 
;;; this is the find part of Union-Find
;;; (this will require extension to return the tms justification)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod find ((s symbol))
  (let ((answer (intern-symbol s)))
    (find answer)))

;;; any non-symbol interns to itself
(defmethod find ((a t)) a)

;; Set the direct parents of all the guys traversed
;; to the ultimate parent

;; In this version, Union is aggressive and updates the parent pointer
;; of all members of the set at merge time, so find is trivial.
(defmethod find ((c equivalence-class))
  (let ((parent (parent c)))
    (if parent
	(values parent (parent-justification c))
      (values c nil))))

(defmethod find ((term list))
  (multiple-value-bind (interned-term representative) (intern-term term)
    (declare (ignore interned-term))
    (find representative)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Integration with Joshua
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; To Do: Should the better version of predications be treated
;;; just like terms, i.e. use union-find etc?

(define-predicate-model cc-mixin
    ((better-versions :Initform nil :accessor better-versions))
  ;; This means that assertions involving congruence closure
  ;; are always ground assertions i.e. contain no logic-variables
  ;; attempts to assert one with a variable will trap
  (no-variables-in-data-mixin))

(define-predicate-method (clear cc-mixin) (&optional clear-database undefrules)
  (declare (ignore undefrules))
  (when clear-database
    (init)))

;;; Note that this just flattens the predication and replaces symbols
;;; by their immediate (not merged) equivalence class.
(defmethod intern-predication ((pred cc-mixin))
  (let* ((statement (predication-statement pred))
	 (flattened-statement (cons (first statement)
				     (loop for item in (rest statement)
					 if (symbolp item)
					 collect (intern-symbol item)
					 else if (atom item)
					 collect item
					 else collect (representative (intern-term item))))))
    (make-predication flattened-statement)))

(define-predicate-method (insert cc-mixin :around) ()
  (multiple-value-bind (database-predication new-p) (call-next-method (intern-predication self))
    (when new-p
      (loop for item in (rest (predication-statement database-predication))
	  when (typep item 'equivalence-class)
	  do (push database-predication (predications item))))
    (values database-predication new-p)))

;;; This wouldn't even need to exist except that the query is typically 
;;; in terms of external representations and needs to be converted
;;; into equivalence class representation.  This is complicated
;;; by variables nested in terms which we need to walk.
(define-predicate-method (ask-data cc-mixin) (truth-value outer-continuation)
  (labels ((walk-statement (statement-so-far remaining-tokens)
	     ;; (format t "~%Statement ~a ~a" statement-so-far remaining-tokens )
	     (if (null remaining-tokens)
		 ;; so this should really call fetch
		 ;; and then build the right justification
		 (let ((modified-statement (make-predication (cons (first (predication-statement self))
								   (reverse statement-so-far)))))
		   (call-next-method modified-statement truth-value outer-continuation))
	       (destructuring-bind (first . rest) remaining-tokens
		 (typecase first
		   (equivalence-class
		    (walk-statement (cons first statement-so-far) rest))
		   (symbol
		    (let ((equivalence-class (find first)))
		      (walk-statement (cons equivalence-class statement-so-far) rest)))
		   (unbound-logic-variable
		    (walk-statement (cons first statement-so-far) rest))
		   (ji::joshua-logic-variable
		    (let ((value (joshua-logic-variable-value first)))
		      (walk-statement statement-so-far (cons value rest))))
		   (list
		    (walk-term first 
			       #'(lambda (next-token variable-bound?)
				   (declare (ignore variable-bound?))
				   (walk-statement (cons next-token statement-so-far) rest)))))))))
    (walk-statement nil (rest (predication-statement self)))))

(defun walk-term (term continuation &optional variable-bound?)
    (labels ((walk-the-rest (dn-node remaining-term continuation variable-bound?)
	       ;; (format t "~%Term ~a ~a ~a" dn-node remaining-term continuation)
	       (cond
		((null remaining-term)
		 (let ((the-guy (representative dn-node)))
		   (when the-guy
		     (funcall continuation the-guy variable-bound?))))
		((unbound-logic-variable-p remaining-term)
		 (error "foo"))
		(t
		 (destructuring-bind (first . rest) remaining-term
		   (typecase first
		     (symbol
		      (let* ((equiv-class (find first))
			     (next-dn-node (gethash equiv-class dn-node)))
			(when next-dn-node
			  (walk-the-rest next-dn-node rest continuation variable-bound?))))
		     (equivalence-class
		      (let ((next-dn-node (gethash first dn-node)))
			(when next-dn-node
			  (walk-the-rest next-dn-node rest continuation variable-bound?))))
		     (unbound-logic-variable
		      (loop for next-dn-node being the hash-values of dn-node using (hash-key key)
			  do (with-unification
				 (unify first key)
			       (walk-the-rest next-dn-node rest continuation t))))
		     (ji::joshua-logic-variable
		      (let* ((variable-value (joshua-logic-variable-value first))
			     (next-dn (gethash variable-value dn-node)))
			(walk-the-rest next-dn rest continuation variable-bound?)))
		     (list
		      (walk-term first
				 #'(lambda (surrogate variable-bound?)
				     (let ((next-dn-node (gethash surrogate dn-node)))
				       (when next-dn-node
					 (walk-the-rest next-dn-node rest continuation variable-bound?))))
				 variable-bound?))))))))
      (typecase term
	(symbol
	 (funcall continuation (intern term) variable-bound?))
	(equivalence-class
	 (funcall continuation term variable-bound?))
       (list
	(destructuring-bind (head . terms) term
	  (let ((first-node (gethash head *term-dn*)))
	    (when first-node
	      (walk-the-rest first-node terms continuation variable-bound?))))))))


;;; When a predication comes in for the first time find its congruence closure
;;; and justify that as depending on original statement plus all the congruence
;;; statements mapping items to their equivalence sets
;;; (note that at this point the predication is in canonical flat form)
(define-predicate-method (act-on-truth-value-change cc-mixin :after) (old-truth-value &optional old-state)
  (when (and (eql old-truth-value *unknown*)
	     (zerop (predication-bits-ive-been-in-before old-state)))
    (loop for item in (rest (predication-statement self))
	  if (typep item 'equivalence-class)
	do (loop for parent-justification being the hash-values of (parent-assertions item)
	       when (eql (predication-truth-value parent-justification) *true*)
	       do (with-statement-destructured (child parent) parent-justification
		    ;; swap parent and child if they're in the wrong order
		    ;; To Do: This has the effect of rewriting down
		    ;; I.e. creating an assertion for every child of the equiv-clas
		    ;; which seems like the wrong thing.
		    (if  (eql child item)
			(rewrite-predication self child parent parent-justification)
		      ;;(rewrite-predication self parent child parent-justification)
		      ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Congruent Predication
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-predicate-model congruence-model
    ((predication-mappings :initform nil :accessor predication-mappings))
  (no-variables-in-data-mixin ltms:ltms-predicate-model)
  )

;;; The idea is that each pred contains a pointer to its better versions
;;; and this pointer is coupled with the congruence assertion responsible for the rewriting
;;; This is also back-pointered for retraction purpurposes (in the congruence predication)
;;; Since the mapping is valid only as long as the congruence assertion (the better predication might
;;; have a justification that doesn't involve the congruence, but it's the better predication only if 
;;; the congruence is true), the entry is removed when the congruence goes out.
;;; ToDo: Setting and unsetting the stimuate-list in the Rete Network when things
;;; have a better name and when they finally don't.
(defmethod rewrite-predication ((pred cc-mixin) old-term new-term (support congruence-model))
  (let* ((new-statement (subst new-term old-term (predication-statement pred)))
	 (new-predication (tell (make-predication new-statement) :justification `(congruence-closure (,pred ,support)))))
    ;; the old version should get "retracted" from the rete-network, actually this means
    ;; something more like suspended.
    (rete-network-retract-predication  pred)
    ;; And further more marked as having a better version so that if a new forward rule
    ;; came in it wouldn't actually trigger off
    (push (list support new-predication) (better-versions pred))
    (push (list pred new-predication) (predication-mappings support))))

;;; A note: suppose you have a nested term (f (g x)) 
;;; that's referenced in a predication (as in test3a)
;;; and (g x) = y and (f y) = z
;;; so you have a series of predications:
;;; [test (f (g x))] -> [test (f y)] -> [test z]
;;; And then you unjustify (g x) = y
;;; So the 2nd pred gets removed as a better-version of the 1st
;;; But the 3rd doesn't get removed as a better name of the 2nd
;;; That's OK because the 2nd is Out, but if it were in
;;; the 3rd would be its better-version

(defmethod undo-predication-rewriting ((congruence congruence-model))
  (loop for (old new) in (predication-mappings congruence)
      for entry = (list congruence new)
      do (setf (better-versions old)
	   (delete entry (better-versions old)
		   :test #'equal))
      when (null (better-versions old))
      do (stimulate old (predication-truth-value old))))

;;; Insert indexes these things symmetrically, i.e.
;;; the assertion is put into both the parent and the child
;;; (should we use two hash tables one in each direction?)
(define-predicate-method (insert congruence-model) ()
  (with-statement-destructured (child parent) self
    (if (eql child parent) 
	self
      (let ((child (intern child))
	    (parent (intern parent)))
	(when (eql child (preferred-target parent child))
	  (rotatef child parent))
	;; symmetrically index the predication
	(let ((answer (gethash parent (parent-assertions child)))
	      (new-p nil))
	  (unless answer
	    (setq new-p t
		  answer (make-predication (list (type-of self) child parent)))
	    (setf (gethash parent (parent-assertions child)) answer)
	    (setf (gethash child (parent-assertions parent)) answer))
	  (values answer new-p))))))

(define-predicate-method (uninsert congruence-model) ()
  (with-statement-destructured (child parent) self
    (unless (eql child parent)
      (let ((child (intern child))
	    (parent (intern parent)))
	(remhash parent (parent-assertions child))
	(remhash child (parent-assertions parent))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Ask-data Congruent
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-predicate-method (ask-data congruence-model) (truth-value continuation)
  (with-statement-destructured (child parent) self
    (cond
     ;; Both unbound, just get everything that's stored explicitly
     ((and (unbound-logic-variable-p child) (unbound-logic-variable-p parent))
      (map-over-all-congruences
       truth-value
       #'(lambda (parent-assertion)
	   (with-unification
			(unify self parent-assertion)
		      (with-stack-list (backward-support self truth-value parent-assertion)
			(funcall continuation backward-support))))))
     ;; one variable, but not both unknown
     ((or (unbound-logic-variable-p child) (unbound-logic-variable-p parent))
      ;; put the unknown one into canonical position
      (when (unbound-logic-variable-p parent) (rotatef child parent))
      (if (eql truth-value *false*)
	  (ask-data-one-variable-false self child (intern parent) continuation)
	(ask-data-one-variable-true self child parent continuation)))
     (t ;; both are specific, need to use walk-term on both
      ;; To Do: Check that this handles true vs false appropriately
      ;; it might be that the use of walk-term here makes this symmetric?
      (let ((already-seen nil))
	(flet ((parent-continuation (parent parent-variable-bound)
		 (declare (ignore parent-variable-bound))
		 (flet ((child-continuation (child child-variable-bound?)
			  (declare (ignore child-variable-bound?))
			  (let ((entry (list parent child)))
			    (when (not (cl:find entry already-seen :test #'equal))
			      (push entry already-seen)
			      (push (reverse entry) already-seen)
			      (if (eql parent child)
				  (with-stack-list (bs self *true* '(rule congruence-closure))
				    (funcall continuation bs))
				(let ((parent-assertion (gethash (intern parent) (parent-assertions (intern child)))))
				  (cond
				   ;; see if it's there explicitly
				   (parent-assertion
				    (when (eql (predication-truth-value parent-assertion) truth-value))
				    ;; The explicit case works correctlly for both truth-values
				    (with-stack-list (backward-support self truth-value parent-assertion)
				      (funcall continuation backward-support)))
				   ;; Not explicitly asserted
				   ((eql truth-value *true*)
				    ;; If not, when we're looking for TRUE assertions see if parent and child are in the same class
				    (with-stack-list (rule-form 'rule 'congruence-closure)
				      (multiple-value-bind (child-set child-set-assertion) (find child)
					(multiple-value-bind (parent-set parent-set-assertion) (find parent)
					  (when (eql child-set parent-set)
					    ;; query had no variables so no with-unification
					    (with-stack-list (piece1 child-set-assertion *true* child-set-assertion)
					      (with-stack-list (piece2 parent-set-assertion *true* parent-set-assertion)
						(with-stack-list (backward-support self
										   truth-value
										   rule-form
										   piece1
										   piece2)
						  (funcall continuation backward-support)))))))))
				   ;; There's no explicit assertion and we're looking for FALSE assertions
				   ;; In this case their two classes are asserted to be non congruent.
				   (t (ask-data-no-variable-false self child parent continuation)))))))))
		   (walk-term child #'child-continuation))))
	  (walk-term parent #'parent-continuation)))))))

(defun map-over-all-congruences (truth-value continuation)
  (loop for child being the hash-values of *equivalence-class-hash-table*
      do (loop for parent-assertion being the hash-values of (parent-assertions child)
	     do (with-statement-destructured (statement-child statement-parent) parent-assertion
		  (declare (ignore statement-parent))
		  (when (and (eql (predication-truth-value parent-assertion) truth-value)
			     (eql statement-child child))
		    (funcall continuation parent-assertion))))))

(defmethod ask-data-one-variable-true ((the-congruence congruence-model) child parent continuation)
  ;; The parent is known so map him looking for backward pointers
  (let ((already-seen nil))
    (walk-term parent
	       #'(lambda (parent variable-bound?)
		   (declare (ignore variable-bound?))
		   (multiple-value-bind (class class-justification) (find parent)
		     (when class-justification
		       (with-unification
			   (unify child class)
			 (with-stack-list (backward-support the-congruence *true* class-justification)
			   (funcall continuation backward-support))))
		     (loop for member in (members class)
			 for entry = (list parent member)
			 for member-just = (parent-justification member)
			 when (and (not (eql member parent))
				   member-just
				   (not (cl:find entry already-seen :test #'equal))
				   (eql (predication-truth-value member-just) *true*))
			 do (push entry already-seen)
			    (push (reverse entry) already-seen)
			    (with-unification
				(unify child member)
			      (if class-justification
				  ;; This can get called to prefetch for a forward rule being inserted
				  ;; And it's supposed to return an actual database predication
				  ;; That's why I'm doing the tell here to manifest the predication
				  (let ((new-pred (tell (copy-object-if-necessary the-congruence)
							:Justification `(congruence-closure (,class-justification ,member-just)))))
				    (with-stack-list (backward-support the-congruence *true* new-pred)
				      (funcall continuation backward-support)))
				(with-stack-list (piece2 member-just *true* member-just)
				  (with-stack-list (backward-support the-congruence *true* piece2)
				    (funcall continuation backward-support)))))))))))

;;; this needs to use walk-term because the non-vqriable term could be a list with embedded variables
(defmethod ask-data-no-variable-false ((query congruence-model) class-1 class-2 continuation)
  ;; The case where the assertion between these two specific guys
  ;; is explicitly present in the database was handled above
  ;; So we have to find the representatives of the two equivalence classes
  ;; and see if they're not congruent.
  ;; Then there are 4 cases:
  ;; 1) Neither class-1 nor class-2 are the representative of their equivalence class
  ;; 2 and 3) one is and one isn't
  ;; 4) Both are.  But in that case they would have been explicitly asserted to be not congruent
  ;; and we would have found that above and not gotten here.
  ;; So there are 3 cases to handle.  We can collapse 2 and 3 by swapping
  ;; variables, so only 2 cases.
  (multiple-value-bind (real-class-1 class-1-assertion) (find class-1)
    (multiple-value-bind (real-class-2 class-2-assertion) (find class-2)
      (cond
       ((and class-1-assertion class-2-assertion)
	;; Here neither class-1 nor class-2 was the canonical rep of their equivalence classes
	(let ((non-congruence-assertion (gethash real-class-2 (parent-assertions real-class-1))))
	  (when (and non-congruence-assertion
		     (eql (predication-truth-value non-congruence-assertion) *false*))
	    (with-stack-list (rule-form 'rule 'congruence-closure)
	      ;; query had no variables so no with-unification
	      (with-stack-list (piece1 class-1-assertion *true* class-1-assertion)
		(with-stack-list (piece2 class-2-assertion *true* class-2-assertion)
		  (with-stack-list (piece3 non-congruence-assertion *false* non-congruence-assertion)
		    (with-stack-list (backward-support query
						       *false*
						       rule-form
						       piece3
						       piece1
						       piece2)
		      (funcall continuation backward-support)))))))))
       (t 
	;; Here one is the canonical rep of its equivalence class and one isn't
	;; So we'll canonicalize so that class-2 is just a normal instance of real-class-2
	;; and class-1 is the canonical representative of real-class-1
	(when (not (eql class-1 real-class-1))
	  (rotatef real-class-1 real-class-2)
	  (rotatef class-1-assertion class-2-assertion))
	(let ((non-congruence-assertion (gethash real-class-2 (parent-assertions real-class-1))))
	  (when (and non-congruence-assertion
		     (eql (predication-truth-value non-congruence-assertion) *false*))
	    (with-stack-list (rule-form 'rule 'congruence-closure)
	      ;; query had no variables so no with-unification
	      (with-stack-list (piece2 class-2-assertion *true* class-2-assertion)
		(with-stack-list (piece3 non-congruence-assertion *false* non-congruence-assertion)
		  (with-stack-list (backward-support query
						     *false*
						     rule-form
						     piece3
						     piece2)
		    (funcall continuation backward-support)))))
       )))))))

;;; The underlying invariant is that if an element A of one class is asserted to not be congruent
;;; to some element B of some other class, than the representative of A's class is also asserted to be not
;;; congruent to the representative of B's class.

(defmethod ask-data-one-variable-false ((pred congruence-model) child parent continuation)
  ;; here parent is the one which isn't a variable
  ;; Don't have to look for any explicitly stated ones, because
  ;; the parent will also have the same thing.
  ;; first do any specifically asserted non-congruences
  ; (loop for child-assertion being the hash-values of (parent-assertions parent) using (hash-key this-child)
  ;     when (eql (predication-truth-value child-assertion) *false*)
  ;     do (with-unification
  ; 	     (unify this-child child)
  ; 	   (with-stack-list (backward-support pred *false* child-assertion)
  ; 	     (funcall continuation backward-support))))
  ;; next check whether there are any implied due to the class
  ;; being asserted not congruent to another class
  (multiple-value-bind (his-parent parent-assertion) (find parent)
    ;; check that he was not the class representative in which case
    ;; we've already done what we can
    (when (not (eql his-parent parent))
      ;; This finds all classes we're not congruent to and calls the 
      ;; continuation once for each class.  To Do: But it could call it once for each member of each such class
      ;; which would be consistent with what we do for the 2 variable case.
      (loop for child-assertion being the hash-values of (parent-assertions his-parent) using (hash-key this-child)
	  when (eql (predication-truth-value child-assertion) *false*)
	  do (with-unification
		 (unify this-child child)
	       (with-stack-list (rule-form 'rule 'congruence-closure)
		 (with-stack-list (piece1 parent-assertion *true* parent-assertion)
		   (with-stack-list (piece2 child-assertion *false* child-assertion)
		     (with-stack-list (backward-support pred *false* rule-form piece1 piece2)
		       (funcall continuation backward-support))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Truth Value Change Congruent
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-predicate-method (notice-truth-value-change congruence-model :after) (old-truth-value)
  ;; clean up after a retraction
  (when (and (eql (predication-truth-value self) *unknown*)
	     (eql old-truth-value *true*))
    ;; Only true assertions cause structural change to the union-find data structure
    ;; false assertions just get indexed.  So if a false assertion goes out, everything
    ;; that needs to happen will be handled by the TMS.
    (with-statement-destructured (child parent) self
      (unUnion child parent)
      (undo-predication-rewriting self)
      )))

(define-predicate-method (act-on-truth-value-change congruence-model :after) (old-truth-value &optional old-state)
  (declare (ignore old-state))
  (cond
   ((and (eql (predication-truth-value self) *true*)
	 (eql old-truth-value *unknown*)
	 ;; See note below
	 ;; (zerop (predication-bits-ive-been-in-before old-state))
	 )
    ;; we only call Union if this is the first time this guy becomes true
    ;; any subsequent time, the TMS will take care of things.
    ;; To Do: Is this true?  What about the internal 
    ;; pointers in the union-find structure, won't they need updating?
    ;; Also suppose A joints Set B, then leaves.  Then C joins A and A joins B
    ;; again.  We'll miss C being part of B.
    (with-statement-destructured (victim parent) self
      (union victim parent self))
    )
   ;; Handling Negative Congruence
   ;; The invariant is that if a and b are not congruent
   ;; Then the class of a is asserted to be not congruent to the class of b
   ;; Note that the non-congruence between the original two guys
   ;; is already asserted.
   ;; To Do: Suppose both classes turn out to be the same
   ;; that's a contradiction.  Handle in either of two ways:
   ;; 1) Right here
   ;; 2) Before a Notice-truth-value-change if two elements are
   ;; eql then signal contradiction.  This will work because
   ;; if two guys are in the same class, then both their parents
   ;; will be the same thing.
   ((and (eql (predication-truth-value self) *false*)
	 (eql old-truth-value *unknown*))
    (with-statement-destructured (class-1 class-2) self
      (multiple-value-bind (real-class-1 class-1-justification) (find class-1)
	(multiple-value-bind (real-class-2 class-2-justification) (find class-2)
	  (cond
	   ;; Both of these are the class representatives
	    ;; nothing else to assert
	   ((and (eql class-1 real-class-1)
		 (eql class-2 real-class-2)))
	   ;; Here we have one class representative and one class member
	   ;; Assert non-congruence between the two classes
	   ((eql class-1 real-class-1)
	    (tell `[not [congrunent ,real-class-1 ,real-class-2]]
		  :justification `(congruence-closure (,class-2-justification)
						      (,self))))
	   ;; Here we have the other class representative and a class member from the first class
	   ;; Assert non-congruence between the two classes
	   ((eql class-2 real-class-2)
	    (tell `[not [congruent ,real-class-2 ,real-class-1]]
		  :justification `(congruence-closure (,class-1-justification)
						      (,self))))
	   ;; Here both are just members of their classes
	   ;; Assert the non-congruence between the classes
	   ;; and assert two others between the instances and the other class representative
	   (t (tell `[not [congruent ,real-class-1 ,real-class-2]]
		    :justification `(congruence-closure (,class-1-justification ,class-2-justification)
							(,self)))
	      (tell `[not [congruent ,class-1 ,real-class-2]]
		    :justification `(congruence-closure (,class-2-justification) (,self)))
	      (tell `[not [congruent ,class-2 ,real-class-1]]
		    :justification `(congruence-closure (,class-1-justification) (,self)))
	      ))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Forward Rule Integration
;;;
;;;
;;; The idea
;;; Matching [test a (f c) b] conceptually turns into
;;; Matching [and [test a ?f1 b]
;;;               [congruent ?f1 (f c)]]
;;; But something magic has to happen because the (f c) in the second form
;;; the magic is that the matcher code interns (f c) at match time
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-predicate-method (map-over-forward-rule-triggers congruence-model :around) (continuation)
  (with-statement-destructured (child parent) self
    (let ((fixed-statement (make-predication (list (predication-predicate self)
						   (if (unbound-logic-variable-p child)
						       child
						     (external-form child))
						   (if (unbound-logic-variable-p parent)
						       parent
						     (external-form parent))))))
      (call-next-method fixed-statement continuation))))

(define-predicate-method (map-over-forward-rule-triggers cc-mixin :around) (continuation)
  (let ((fixed-statement nil))
    (loop for thing in (rest (predication-statement self))
	if (unbound-logic-variable-p thing)
	do (push thing fixed-statement)
	else do (push (external-form thing) fixed-statement))
    (let ((replacement-statement (cons (first (predication-statement self)) (nreverse fixed-statement))))
      (call-next-method (make-predication replacement-statement) continuation))))

;;; Strategy: Map terms in assertions to logic-variables, coupled with a congruent
;;;   asssertion.  Running example: [test a (f c) b]:
;;; [test a (f c) b]=> [test a ?x b] & [congruent (f c) ?x]
;;; 
;;; 1) Exact match, the guys asserts what we're looking for: [test a (f c) b]
;;;    In this case, because of the variable introduction, need to check for 
;;;    the variable being (intern (f c))
;;;    alternatively, we can have the expander turn the whole thing into an or
;;;    of [test a (f c) b] and the rewrite above
;;; 2) He asserts something else, but we're congruent to that
;;;    asserted:  [test a y b] & [congruent (f c) y]
;;;    So we have to match for that as illustrated abob
;;; 3) He asserts something else, but that's congruent to something
;;;      that we're congruent to.
;;;    asserted: [test a x b] & [congruent x y] & [congruent (f c) y]
;;;    Term rewriting will change [test a x b] to [test a y b]
;;;    and then case 2 wins.
;;;
;;;  Keep in mind there might be more than one embedded term, so the approach has to handle that

(define-predicate-method (expand-forward-rule-trigger cc-mixin) (var-name truth-value context bound-variables)
  (declare (ignore context bound-variables))
  (let ((statement (predication-maker-statement self))
	(terms-variable-alist nil)
	(rewritten-statement nil))
    (loop for piece in (rest statement)
	if (or (logic-variable-maker-p piece) (atom piece))
	do (push piece rewritten-statement)
	else do (let ((new-logic-variable `(logic-variable-maker ,(gensym (concatenate 'string "?" (string (first piece)))))))
		  (push (list new-logic-variable piece) terms-variable-alist)
		  (push new-logic-variable rewritten-statement)))
    (setq rewritten-statement (cons (first statement) (nreverse rewritten-statement)))
    `(:and
       (:match (predication-maker ',rewritten-statement) ,var-name ,truth-value)
       ,@(loop for (variable term) in terms-variable-alist
	     collect `(:or 
		       (:procedure (eql (intern ',term) ,variable) ,var-name nil ,self)
		       (:match (predication-maker '(congruent ,term ,variable)) nil ,truth-value))
		       ))))

;;; Like regular semi matcher except that non logic-variables need to be interned
(define-predicate-method (write-forward-rule-semi-matcher cc-mixin) (statement-to-match environment)
  (declare (ignore environment))
  (let ((statement (rest (predication-maker-statement self))))
    `(let ((his-statement (rest (predication-statement ,statement-to-match))))
       (and
	,@(loop with variables-seen-so-far = nil
	     for thing in statement
	     collect (cond ((logic-variable-maker-p thing)
			    (cond ((member (logic-variable-maker-name thing) variables-seen-so-far)
				   `(eql ,(logic-variable-maker-name thing) (pop his-statement)))
				  (t (push (logic-variable-maker-name thing) variables-seen-so-far)
				     `(progn (setq ,(logic-variable-maker-name thing) (pop his-statement))
					     t))))
			   (t `(Progn 
				(eql (intern ',thing) (pop his-statement))))))
	))))

(define-predicate-method (write-forward-rule-semi-matcher congruence-model) (statement-to-match environment)
  (declare (ignore environment))
  (with-predication-maker-destructured (child parent) self
    (cond 
     ((and (logic-variable-maker-p child) (logic-variable-maker-p parent))
      `(with-statement-destructured (his-child his-parent) ,statement-to-match
	 (setq ,(logic-variable-maker-name child) his-child
	       ,(logic-variable-maker-name parent) his-parent)
	 ;; id he was stupid enough to write [congruent ?a ?a] which will never be asserted
	 ,(if (eql child parent)
	      `(eql his-child his-parent)
	    ;; otherwise this always succeeds
	    t)))
     ;; [congruent ?a x]
     ((logic-variable-maker-p child)
      `(with-statement-destructured (his-child his-parent) ,statement-to-match
	 (when (eql his-parent (intern ',parent))
	   (setq ,(logic-variable-maker-name child) his-child)
	   t)))
     ;; [congruent x ?a]
     ((logic-variable-maker-p parent)
      `(with-statement-destructured (his-child his-parent) ,statement-to-match
	 (when (eql his-child (intern ',child))
	   (setq ,(logic-variable-maker-name parent) his-parent)
	   t)))
     ;; [congruent x y]
     (t `(with-statement-destructured (his-child his-parent) ,statement-to-match
	   (and (eql his-child (intern ,child))
		(eql his-parent (intern ,parent))))))))

(define-predicate congruent (a b)
  ;; no-variables-in-data-mixin means you can't assert a congruent
  ;; predication with variables in it.
  (congruence-model)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Backward Rule Integration
;;;
;;; For cc-mixin predications
;;; Backward rules can rely on the fact that they can query for the "best name"
;;; version of the predication
;;;
;;; consider:
;;; (defrule chain-cc-mixin-test (:backward)
;;;   :then [cc-test a (f y) ?x]
;;;   :if    [and [cc-test a (g y) ?b]
;;; 	          [cc-test ?b (g y) ?x]]
;;;   )
;;; The RHS is equivalent to:
;;;  (ask `[cc-test a ,(find '(g y)) ?b]
;;        #'(lambda (just)
;;           (ask `[cc-test ?b ,(find '(g y)) ?x]
;;             #'(lambda (just)
;;                 - call continuation -
;;;       
;;;  
;;;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Rules can't get indexed in terms of equivalence classes, because the database
;;; can be cleared while the rules remain and then they'd be indexed in terms of 
;;; the equivalence classes that no longer exist.  (This is also true for forward-rules)
;;;
;;; So they are indexed in terms of the external form, meaning the symbol names
;;; for constants and logic-variables for embedded terms.  (locate-backward-rule-trigger)
;;; Could possibly be more accurate in indexing but that could lead to hair.
;;;
;;; Finding rules given a query has to map the equivalence classes in the query back to their
;;; external form.  This will guarantee that we find all relevant (and maybe some not relevant) rules.
;;; But the query passed in has to pass in equivalence classes.  Terms with embedded variables have to
;;; come in as they are.
;;; 
;;; We can open code the matcher (or write a more general unifier than the provided one)
;;; If the next token in the goal pattern is a variable unify it to the 
;;; next thing in the rule pattern. Otherwise:
;;; Variables get unified with whatever is the next thing
;;; Constants have to be congruent to the thing they match
;;; Lists without variables get interned and the treated the same as constants
;;; Lists with embedded variables get unified with a passed in list
;;; Lists with embedded variables when matched against equivalence class
;;; have to walk themselves as a term and see if there is 
;;; a term like it that's congruent to the passed in equivalence class.
;;; And actually we know the exact walk that would need to be done, so we could open code that too.
;;; One glitch: there could be more than one match for the embedded term, e.g.
;;; (f a) and (f b) are both congruent to z and the match we need to do is z vs (f ?x)
;;; so there are 2 winning matches and the head matcher would need to call the body twice
;;; that involves a mod to the rule compiler.

;;; What to do about terms with variables in the query and in the rule?
;;; That only affects the matcher because the term with embedded variables
;;; will match the variable substituted for the term in the rule-pattern
;;; But the matcher in the rule is going to have to handle all of that.
;;; Another view, the query can be walked recursively similarly to ask-data (including variables) 
;;; to find the equivalence class of each nested term, but what about
;;; (ask [test a (f ?x) b] ...
;;; (ask [and [test a ?f-x b] & [exists (?x) [congruent ?f-x (f ?x)]]
;;; so for existing f terms can walk all of them, unify ?x, then find their most
;;; general form and pose the query on that.
;;; For example, suppose (f z) is congruent to x
;;; and there's a rule
;;; (defrule case1 (:backward)
;;;   :then [test a x ?b]
;;;   :if [test ?b x a])
;;; BUT:
;;; (defrule demo (:backward)
;;;   :then [test a (f ?y) b]
;;;   :if [test a (g ?y) b])
;;; In this case walking the query might not find anything concrete, but there are
;;; potentially multiple answers, namely all (f ?y) where [test a (g ?y) b] holds.
;;; so have to pose the raw query as well for those that have embedded variables


#|

Given the way backward trigger expands (in the updated Joshua) these aren't necessary

(define-predicate-method (locate-backward-rule-trigger cc-mixin :around) (truth-value continuation context rule-name)
  (let ((fixed-statement nil))
    (loop for thing in (rest (predication-statement self))
	do (typecase thing
	     (unbound-logic-variable (push thing fixed-statement))
	     (list (push (make-unbound-logic-variable (gensym (string (first thing)))) fixed-statement))
	     (otherwise (push thing fixed-statement))))
    (let ((replacement-statement (cons (first (predication-statement self)) (nreverse fixed-statement))))
      (call-next-method (make-predication replacement-statement) truth-value continuation context rule-name))))

(define-predicate-method (map-over-backward-rule-triggers cc-mixin :around) (continuation)
  (let ((fixed-statement nil))
    (loop for thing in (rest (predication-statement self))
	do (typecase thing
	     (unbound-logic-variable (push thing fixed-statement))
	     (list (push thing fixed-statement))
	     (otherwise (push (external-form thing) fixed-statement))))
    (let ((replacement-statement (cons (first (predication-statement self)) (nreverse fixed-statement))))
      (call-next-method (make-predication replacement-statement) continuation))))

|#

(define-predicate-method (expand-backward-rule-trigger cc-mixin) (truth-value if-part)
  (declare (ignore truth-value))
  (let ((forms-discovered nil)
	(new-trigger nil))
    (destructuring-bind (predicate . rest) (predication-maker-statement self)
      (push predicate new-trigger)
      (loop for thing in rest
	  do (typecase thing
	       (logic-variable-maker (push thing new-trigger))
	       (list (let* ((name (cl:intern (gensym (concatenate 'string "?" (string (first thing))))))
			    (new-logic-variable `(logic-variable-maker ,name)))
		       (push new-logic-variable new-trigger)
		       (push `(predication-maker '(or 
						   (predication-maker '(unify ,new-logic-variable ,thing))
						   (predication-maker '(congruent ,new-logic-variable ,thing))))
			     forms-discovered)))
	       (otherwise (push thing new-trigger)))))
    (values `(predication-maker ',(nreverse new-trigger))
	    `(predication-maker '(and ,@(nreverse forms-discovered)
				  ,if-part)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Some tests
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

(define-predicate test (a b c) (cc-mixin ltms:ltms-predicate-model))
(define-predicate test2 (a b) (cc-mixin ltms:ltms-predicate-model))

(defun test1 ()
  (clear)
  (let ((a (intern-symbol 'a 'anonymous-individual))
	(b (intern-symbol 'b 'anonymous-individual))
	(c (intern-symbol 'c 'anonymous-individual))
	(d (intern-symbol 'd 'anonymous-individual)))
  (tell `[congruent ,a ,b])
  (tell `[congruent ,c ,d])
  (tell `[congruent ,a ,d])
  ))


;;; These two tests make sure that you don't get
;;; different results when the last two congruent forms are reversed
;;; Before a fix was made you don't get the intermediate result of
;;; [test a (f y) b] in test 2a
(defun test2a ()
  (clear)
  (tell [test a (f (g x)) b])
  (tell [congruent (f y) z])
  (tell [congruent (g x) y])
  )

(defun test2b ()
  (clear)
  (tell [test a (f (g x)) b])
  (tell [congruent (g x) y])
  (tell [congruent (f y) z])
  ) 

(defun test2c ()
      (clear)
      (tell [test a (f y) (g x)])
      (tell [congruent (g x) w])
      (tell [congruent (f y) z])
      )

(defun test3a ()
  (clear)
  (tell [congruent (f y) z])
  (tell [congruent (g x) y])
  (tell [test a (f (g x)) b]))

(defun test3b ()
  (clear) 
  (tell [congruent (f y) z]) 
  (tell [congruent (g x) w])
  (tell [test a (f y)  (g x)]))




(defun test1-negative ()
  (clear)
  (let ((a (intern-symbol 'a 'anonymous-individual))
	(b (intern-symbol 'b 'anonymous-individual))
	(c (intern-symbol 'c 'anonymous-individual))
	(d (intern-symbol 'd 'anonymous-individual)))
  (tell `[congruent ,a ,b])
  (tell `[congruent ,c ,d])
  (tell `[not [congruent ,a ,d]])
  ))

(defun test2-negative ()
  (clear)
  (let ((a (intern-symbol 'a 'anonymous-individual))
	(b (intern-symbol 'b 'anonymous-individual))
	(c (intern-symbol 'c 'anonymous-individual))
	(d (intern-symbol 'd 'anonymous-individual)))
  (tell `[not [congruent ,a ,c]])
  (tell `[congruent ,a ,b])
  (tell `[congruent ,c ,d])
  ))

(defun test3-negative ()
  (clear)
  (let ((a (intern-symbol 'a 'anonymous-individual))
	(b (intern-symbol 'b 'anonymous-individual))
	(c (intern-symbol 'c 'anonymous-individual))
	(d (intern-symbol 'd 'anonymous-individual)))
  (tell `[congruent ,a ,b])
  (tell `[congruent ,c ,d])
  (tell `[not [congruent ,a ,c]])
  ))

(defun test4-negative () 
  (clear) 
  (let ((a (intern-symbol 'a 'anonymous-individual))
	(b (intern-symbol 'b 'anonymous-individual))
	(c (intern-symbol 'c 'anonymous-individual))
	(d (intern-symbol 'd 'anonymous-individual)))
    (tell `[congruent ,a ,b])
    (tell `[not [congruent ,a ,c]])
    (tell `[congruent ,c ,d])
    ))

(defun test5-negative ()
  (clear)
  (let ((a (intern-symbol 'a 'anonymous-individual))
	(b (intern-symbol 'b 'anonymous-individual))
	(c (intern-symbol 'c 'anonymous-individual))
	(d (intern-symbol 'd 'anonymous-individual)))
    (tell `[congruent (f ,a) ,b])
    (tell `[not [congruent (f ,a) (g ,c)]])
    (tell `[congruent ,c ,d])
    ))



(defrule test4 (:forward)
  :if [test a (f (g x)) b]
  :then [test2 a b]
  )

(defrule test4 (:forward)
  :if [test a ?x b]
  :then [test2 a ?x]
  )

(defrule tb (:backward)
  :then [test a ?b c]
  :if [test a (f ?b) b])



(defrule foo (:backward)
  :then [test a (f ?y) b]
  :if [test2 ?y (f a)]
  )

(defun test1-backward ()
  (clear)
  (tell [test2 x (f a)])
  (tell [test2 a (f a)])
  (ask [test a (f ?p) b] 'print-query-results)
  )

(defun test2-backward ()
  (clear)
  (tell [test2 x (f a)])
  (tell [test2 a (f a)])
  (tell [congruent z (f a)])
  (ask [test a z b] #'print-query-results))

|#

;;; To Do: How about rules for congruent assertions?



