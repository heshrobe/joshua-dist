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
;;; Created 4/02/86 18:53:43 by sgr running on GROUSE at SCRC.

;;;
;;; Constructing and identifying logic variable environments.  A slot in a logic-variable environment
;;; contains one of 3 things:
;;; * a logic variable that points to itself (unbound).
;;; * a logic variable that points somewhere else (typically another logic variable).
;;; * a value that is not a logic variable (this slot is bound to that value).
;;;

(in-package :ji)

(defmacro lookup-in-environment (logic-variable-environment index)
  ;; How to look up a variable in an environment.
  `(aref ,logic-variable-environment ,index))

;;; Shouldn't this name be a bit less general sounding?   -Weav
(defun make-logic-variable-unbound (environment index)
  ;; stores an unbound logic variable at index in environment
  (let ((val (lookup-in-environment environment index)))
    (if (typep val 'joshua-logic-variable)
	(joshua-logic-variable-makunbound val)
	(setf (lookup-in-environment environment index)
	      (make-unbound-logic-variable nil)))))


;;; Matcher run-time function...
(defun copy-unified-value (value)
  ;; ensure that structures get copied.
  (declare (special *new-variables*))
  (labels ((copy-value (value)
	     #-sbcl (declare (values new-value new-value-p))
	     ;; we have to explicitly tell whether we changed anything, since
	     ;; unified variables will be eq
	     (typecase value
	       (cons
		 (multiple-value-bind (new-car new-car-p)
		     (copy-value (car value))
		   (multiple-value-bind (new-cdr new-cdr-p)
		       (copy-value (cdr value))
		     (if (or new-car-p new-cdr-p)
			 (values (cons new-car new-cdr) t)
			 (values value nil)))))
	       (unbound-logic-variable
		 (cond ((member value *new-variables* :test 'eq)
			;; it's in the new environment already
			(values value t))
		       (t
			;; put a new variable in the wayward variable list
			(let ((new-variable (make-unbound-logic-variable (logic-variable-name value))))
			  (push new-variable *new-variables*)
			  (%unify-variable value new-variable)
			  (values new-variable t)))))
	       (predication
		 (multiple-value-bind (new-pred new-p)
		     (copy-value (predication-statement value))
		   (if new-p
		       (values (make-predication new-pred) t)
		       (values value nil))))
	       (otherwise (values value nil)))))
    (copy-value value)))


;;;
;;; Tools for analyzing variable environments.
;;;

;;; For now, maps are just alists.  That will probably do.
(defun build-map (vars &optional output-variable)
  ;; construct a map that says the vars occur in this order
  (let* ((number-of-vars (length vars))
	 (basic-map (loop for i from 0
			  for var in vars
			  collecting (cons var i))))
    (when output-variable
      (push (cons output-variable number-of-vars) basic-map))
    basic-map))

(defun index-from-map (variable map)
  ;; given a variable name and a map, return the index 
  (cdr (assoc variable map)))

(defun generate-merged-environment-map (left-map right-map)
  ;; input is maps of left and right environments; output is map of merged environment
  ;; Example: (generate-merged-environment-map '((=x . 0) (=y . 1)) '((=y . 1) (=z . 0)))
  ;;   returns ((=X . 0) (=Y . 1) (=Z . 2))
  (loop with answer = (copy-alist left-map) ;initial assignments from left map
	with answer-max-index = (1- (length answer)) ;0-based
	for (right-var . nil) in right-map
	unless (assoc right-var answer)
	  ;; this var from the right map hasn't been seen yet, so cons it on
	  do (setq answer (acons right-var (incf answer-max-index) answer))
	finally
	  ;; the sorting is unecessary, but makes it nicer to look at while debugging
	  (return (sort answer #'< :key #'cdr))))


;;;
;;; Compiling forward rule match and merge functions at each node.  We
;;; compile both a semi-unification matcher and a full unification
;;; matcher.  At run time, a flag gets checked to decide which to use.
;;; This results in a significant speed-up in forward chaining, where
;;; variables in the database are rare.  These are cached in appropriate
;;; hash tables.  There are methods which control how these functions
;;; are generated (such as positions-matcher-can-skip); if any of these
;;; is redefined the hash tables are cleared.
;;; 


;;;  In Genera, this is the hash table's hash function.
;;;  Elsewhere, this is a key-generating function for an EQUAL hash table.
;;;  See get-matcher-or-merger-from-cache.
(defun matcher-hash (predication)
  ;; hash function for mapping from predications to their full & semi matchers
  (let ((variable-alist nil)
	(counter 0))
    (labels ((find-variable-stand-in (var)
	       ;; find the thing we're gonna use instead of the variable
	       (let ((cell (assoc var variable-alist)))
		 (unless cell
		   ;; new variable, not previously seen
		   ;; Note the use of ji::*variable* to make [foo =x] and [foo 1] have different hashes
		   (setq variable-alist (acons var `(*variable* ,(incf counter)) variable-alist)
			 cell (car variable-alist)))
		 (cdr cell)))
	     (rebuild (piece)
	       ;; copy a predication, replacing logic variables by integers
	       ;; (Note that this will give the same results for [foo =x] and [foo 1].)
	       (cond ((logic-variable-maker-p piece)
		      ;; replace this variable by a token
		      (find-variable-stand-in (logic-variable-maker-name piece)))
		     ((predication-maker-p piece)
		      ;; predications changed to their statements.  Note that the ji::*predication*
		      ;; gets added at the front to prevent embedded lists and embedded predications
		      ;; from getting the same hash.
		      (rebuild (cons '*predication* (predication-maker-statement piece))))
		     ((atom piece)
		      ;; most atoms just get passed through
		      piece)
		     (t
		       ;; lists copied and hacked recursively
		       (cons (rebuild (car piece))
			     (rebuild (cdr piece)))))))
      ;; the body
      (rebuild predication))))


(defun semi-matcher-hash (predication)
  ;; hash function for mapping from predications to their full & semi matchers
  (let ((variable-alist nil)
	(skip-positions (positions-forward-rule-matcher-can-skip predication))
	(counter 0))
    (labels ((find-variable-stand-in (var)
	       ;; find the thing we're gonna use instead of the variable
	       (let ((cell (assoc var variable-alist)))
		 (unless cell
		   ;; new variable, not previously seen
		   ;; Note the use of ji::*variable* to make [foo ?x] and [foo 1] have different hashes
		   (setq variable-alist (acons var `(*variable* ,(incf counter)) variable-alist)
			 cell (car variable-alist)))
		 (cdr cell)))
	     (rebuild (piece)
	       ;; copy a predication, replacing logic variables by integers
	       ;; (Note that this will give the same results for [foo ?x] and [foo 1].)
	       (cond ((member piece skip-positions)
		      (cons `skip (rebuild (cdr piece))))
		     ((logic-variable-maker-p piece)
		      ;; replace this variable by a token
		      (find-variable-stand-in (logic-variable-maker-name piece)))
		     ((predication-maker-p piece)
		      ;; predications changed to their statements.  Note that the ji::*predication*
		      ;; gets added at the front to prevent embedded lists and embedded predications
		      ;; from getting the same hash.
		      (rebuild (cons '*predication* (predication-maker-statement piece))))
		     ((atom piece)
		      ;; most atoms just get passed through
		      piece)
		     (t
		       ;; lists copied and hacked recursively
		       (cons (rebuild (car piece))
			     (rebuild (cdr piece)))))))
      ;; the body
      (rebuild predication))))

(defun create-matcher-cache () (make-hash-table :test #'equal))

(defvar *matcher-cache* (create-matcher-cache)
  "Table for remembering full & semi matchers from the predications that generate them.")

(defun create-semi-matcher-cache () (make-hash-table :test #'equal))

(defvar *semi-matcher-cache* (create-semi-matcher-cache)
  "Table for remembering full & semi matchers from the predications that generate them.")

(defun create-merger-cache () (make-hash-table :test #'equal))

(defvar *merger-cache* (create-merger-cache)
  "Hash table of Rete merger compiled function objects.  Keyed on the merge-id.")

(defvar *semi-merger-cache* (create-merger-cache))

(define-joshua-meter *matcher-cache-hits*   0)
(define-joshua-meter *matcher-cache-misses* 0)
(define-joshua-meter *merger-cache-hits*    0)
(define-joshua-meter *merger-cache-misses*  0)

(defun clear-match-&-merge-caches ()
  ;; hammer for getting out of wedged rule compilers
  ;; (presumably needed only by rule compiler developers)
  (clrhash *matcher-cache*)
  (clrhash *semi-matcher-cache*)
  (clrhash *merger-cache*)
  (clrhash *semi-merger-cache*) 
  (setq *matcher-cache-hits* 0
	*matcher-cache-misses* 0
	*merger-cache-hits* 0
	*merger-cache-misses* 0)
  t)

(defvar *original-compile-file*)
(defvar *compiling-joshua-file* nil)

(defun unadvise-compile-file ()
  (when (boundp '*original-compile-file*)
    (setf (symbol-function 'compile-file) *original-compile-file*)))

(defun advise-compile-file ()
  (unadvise-compile-file)
  (setq *original-compile-file* (symbol-function 'compile-file))
  (setf (symbol-function 'compile-file)
	#'(lambda (input-pathname &key output-file &allow-other-keys) 
	    (let ((*file-matcher-cache* nil)
		  (*file-semi-matcher-cache* nil)
		  (*file-merger-cache* nil)
		  (*file-semi-merger-cache* nil)
		  (*compiling-joshua-file* t))
	      (declare (special *file-matcher-cache* *file-semi-matcher-cache*
				*file-merger-cache* *file-semi-merger-cache*))
	      (if output-file
		  (funcall *original-compile-file* input-pathname :output-file output-file)
		  (funcall *original-compile-file* input-pathname))))))

#-(or genera mcl allegro sbcl) (advise-compile-file)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Notes on working with SBCL (and ASDF):
;;; SBCL doesn't have a public version of advise, defwrapper ...
;;; However sb-int:encapsulate is exported.  So it could be used.
;;; However, even more ASDF doesn't wind up calling compile-file
;;; for :joshua-file modules.  But you can advise compile-op for that
;;; with an :around method (and I'm already doing so to set the readtable).
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This might be a gentler way to play the game anyhow.
;;; except that in SBCL (and quite possibly other lisps)
;;; using SLIME compiling out of the buffer works by creating a temp
;;; file and the calling compile-file and load on that, so you actually
;;; need to advise compile file.
(defun compile-joshua-file (input-pathname &rest stuff &key &allow-other-keys)
  (let ((*file-matcher-cache* nil)
	(*file-semi-matcher-cache* nil)
	(*file-merger-cache* nil)
	(*file-semi-merger-cache* nil)
	(*compiling-joshua-file* t))
    (declare (special *file-matcher-cache* *file-semi-matcher-cache*
		      *file-merger-cache* *file-semi-merger-cache*))
    (apply #'compile-file input-pathname stuff)))

#+sbcl
(sb-ext:without-package-locks
 (advise-compile-file))

#+mcl
(ccl:advise compile-file 
            (let ((*file-matcher-cache* nil)
	          (*file-semi-matcher-cache* nil)
	          (*file-merger-cache* nil)
	          (*file-semi-merger-cache* nil)
	          (*compiling-joshua-file* t))
              (declare (special *file-matcher-cache* *file-semi-matcher-cache*
		                *file-merger-cache* *file-semi-merger-cache*))
              (:do-it))
            :when :around 
            :name 'set-up-joshua-cache )

#+allegro
(excl:advise compile-file :around set-up-joshua-cache nil
  (let ((*file-matcher-cache* nil)
	(*file-semi-matcher-cache* nil)
	(*file-merger-cache* nil)
	(*file-semi-merger-cache* nil)
	(*compiling-joshua-file* t))
    (declare (special *file-matcher-cache* *file-semi-matcher-cache*
		      *file-merger-cache* *file-semi-merger-cache*))
    :do-it))

#+genera
(zl:::scl::advise zl:::compiler:compile-file :around set-up-joshua-cache nil
  (let ((*file-matcher-cache* nil)
	(*file-semi-matcher-cache* nil)
	(*file-merger-cache* nil)
	(*file-semi-merger-cache* nil)
	(*compiling-joshua-file* t))
    (declare (special *file-matcher-cache* *file-semi-matcher-cache*
		      *file-merger-cache* *file-semi-merger-cache*))
    :do-it))
  

(defun get-matcher-or-merger-from-cache (key type-of-cache creator-if-not-found)
  ;; get a matcher or merger from the appropriate cache, creating it if not found
  #-sbcl (declare (values value found-in-cache-p found-key)
		  (dynamic-extent creator-if-not-found))
  (let ((compiling-to-core (not *compiling-joshua-file*)))
    (flet ((meter-match-merge-cache (type-of-cache hit-or-miss)
	     (if (eql hit-or-miss 'hit)
		 (case type-of-cache
		   ((matcher semi-matcher) (incf *matcher-cache-hits*))
		   ((merger semi-merger) (incf *merger-cache-hits*)))
		 (case type-of-cache
		   ((matcher semi-matcher) (incf *matcher-cache-misses*))
		   ((merger semi-merger) (incf *merger-cache-misses*)))))
	   (cache-name (type-of-cache)
	     (case type-of-cache
	       (matcher '*matcher-cache*)
	       (semi-matcher '*semi-matcher-cache*)
	       (merger '*merger-cache*)
	       (semi-merger '*semi-merger-cache*)))
	   (get-right-cache (type-of-cache)
	     (declare (special *file-matcher-cache* *file-semi-matcher-cache*
			       *file-merger-cache* *file-semi-merger-cache*))
	     (if compiling-to-core
		 ;; Compiling from an editor buffer or some such
		 ;; Just get the right in-core global cache
		 (case type-of-cache
		   (matcher *matcher-cache*)
		   (semi-matcher *semi-matcher-cache*)
		   (merger *merger-cache*)
		   (semi-merger *semi-merger-cache*))
		 ;; we're compiling to a file
		 ;; we want to see if this file already has a definition of the function.
		 (case type-of-cache
		   (matcher
		     (when (null *file-matcher-cache*)
		       ;; the file doesn't already have a matcher cache
		       ;; so create it
		       (setq *file-matcher-cache* (create-matcher-cache)))
		     *file-matcher-cache*)
		   (semi-matcher
		     (when (null *file-semi-matcher-cache*)
		       ;; the file doesn't already have a semi-matcher cache
		       ;; so create it
		       (setq *file-semi-matcher-cache* (create-semi-matcher-cache)))
		     *file-semi-matcher-cache*)
		   (merger
		     ;; the file doesn't already have a merger cache
		     ;; so create it
		     (when (null *file-merger-cache*)
		       (setq *file-merger-cache* (create-merger-cache)))
		     *file-merger-cache*)
		   (semi-merger
		     (when (null *file-semi-merger-cache*)
		       (setq *file-semi-merger-cache* (create-merger-cache)))
		     *file-semi-merger-cache*))))
	   (real-key (key)
	     (case type-of-cache
	       (matcher (matcher-hash key))
	       (semi-matcher (semi-matcher-hash key))
	       ((merger semi-merger) key))))
      (let ((cache (get-right-cache type-of-cache))
	    (real-key (real-key key)))
	(multiple-value-bind (value found-p) (gethash real-key cache)
	  (cond (found-p (meter-match-merge-cache type-of-cache 'hit))
		(t (multiple-value-bind (name code) (funcall creator-if-not-found)
		     (cond
		       ((not compiling-to-core)
			(when code
			  (collect-forward-rule-subsidiary-function `(defun ,name ,@(cdr code))))
			(collect-forward-rule-subsidiary-function
			  `(setf (gethash ',real-key ,(cache-name type-of-cache))
				 ,(when code `',name))))
		       (t (when code (compile name code))))
		     (when code
		       (setq value name)
		       (setf (gethash real-key cache) value)
		       (meter-match-merge-cache type-of-cache 'miss)))))
	  (values value found-p))))))

(defun write-forward-rule-matchers (rule-name predication environment support-variable)
  ;; construct full unification matcher, semi-unification matcher, and variable map
  #-sbcl (declare (values full-matcher semi-matcher output-map))
  (let* ((named-variables (logic-variable-makers-in-thing predication))
	 (output-map (build-map named-variables support-variable)))
    (when support-variable (push support-variable named-variables))
    ;; all of variables must be allocated a slot in output-map
    (when (set-difference named-variables output-map :test #'(lambda (x y) (eq x (car y))))
      (error "There are some variables in ~S that are not in the output map ~S."
	     predication output-map))
    (let (full-function semi-function)
      (setq full-function
	    (get-matcher-or-merger-from-cache
	      predication
	      'matcher
	      #'(lambda ()
		  ;; get the matchers from the cache if you can, but do
		  ;; this to make new ones if necessary
		  (values
		    (gentemp (format nil "RULE-~s-FULL-MATCHER-" rule-name))
		    (write-full-matcher predication output-map named-variables support-variable environment)))))
      (setq semi-function
	    (get-matcher-or-merger-from-cache
	      predication
	      'semi-matcher
	      #'(lambda ()
		  ;; get the matchers from the cache if you can, but do
		  ;; this to make new ones if necessary
		  (values
		    (gentemp (format nil "RULE-~s-SEMI-MATCHER-" rule-name))
		    (write-semi-matcher predication output-map named-variables support-variable environment)))))
      ;; return full matcher, semi matcher, and output-map
      (values full-function semi-function output-map))))

;;; Assuming the variables are just symbols.  Same for support variable.
(defun write-full-matcher (predication-maker output-map variables support-variable environment)
  ;; write a full unification matcher for pattern.  This is just a
  ;; closed call to unify until we write an open-coded one.
  (let ((generated-code (write-forward-rule-full-matcher predication-maker 'predication-to-match environment))
	(known-lvs (macroexpand '(known-lvs) environment)))
    (when generated-code
      `(lambda (predication-to-match)
	 (macrolet ((known-lvs () ',(union known-lvs variables)))
	   (with-unbound-logic-variables ,variables
	     (with-unification	    
	       ;; first do the unification
	       ,generated-code
	       ,@(when support-variable
		   `((%unify-variable ,support-variable predication-to-match)))
	       ;; now construct the environment
	       (let ((environment (make-array ,(length output-map)))
		     (*new-variables* nil))
		 (declare (special *new-variables*))
		 ;; put the value of each variable in the pattern into the environment
		 ;; this copies structures if necessary
		 ,@(loop for (variable . position) in output-map
			 if (member variable variables)
			   ;; we actually bound this variable, so put the value in the slot
			   collect (if (eql variable support-variable)
				       `(setf (lookup-in-environment environment ,position) ,variable)
				       `(setf (lookup-in-environment environment ,position)
					      (copy-unified-value ,variable)))
			 else
			   ;; this variable is someone else's problem (a disjunct, probably).
			   ;; Make it unbound.
			   collect `(make-logic-variable-unbound environment ,position))
		 ;; and return the environment
		 (values environment t)))))))))

(define-predicate-method (write-forward-rule-full-matcher default-rule-compilation-model) (predication-variable environment)
  (declare (ignore environment))
  `(unify-predication ,self ,predication-variable))

(defun write-semi-matcher (predication-maker output-map variables support-variable environment)
  (let ((new-variable-introduced (loop for (variable) in output-map
				       thereis (not (member variable variables)))))
      `(lambda (predication-to-match)
	 (progn predication-to-match)
	 (let ,variables
	   ;; create some local variables as vicars for the logic variables, then test
	   (and 
		,(write-forward-rule-semi-matcher predication-maker 'predication-to-match environment)
		,@(when support-variable
		   `((setq ,support-variable predication-to-match)))
		;; don't cons the environment until we've succeeded
		(let* ((environment (make-array ,(length output-map))))
		  ;; bash the values of the vicars into the environment
		  ,@(loop for (variable . position) in output-map
			  if (member variable variables)
			    ;; we actually bound this variable, so put the value in the slot
			    collect `(setf (lookup-in-environment environment ,position) ,variable)
			  else
			    ;; this variable is someone else's problem (a disjunct,
			    ;; probably).  Make it unbound.
			    collect `(make-logic-variable-unbound environment ,position))
		  ;; and return the environment
		  (values environment ,new-variable-introduced)))))))

(define-predicate-method (write-forward-rule-semi-matcher default-rule-compilation-model) (pred-variable environment)
  (progn environment)
  (let ((variables-seen nil))
    (labels ((compile-predication-match (predication-maker object-reference)
	       (let ((positions-to-skip (positions-forward-rule-matcher-can-skip predication-maker))
		     (statement (predication-maker-statement predication-maker)))
		 ;; compiles a match for a predication-maker
		 (if (member statement positions-to-skip)
		     ;; supposed to skip what's in predicate position; go hack the arguments.
		     (compile-match (cdr statement) `(cdr ,object-reference) positions-to-skip)
		     ;; have to hack both predicate and args, presumably.
		     `(let ((or ,object-reference))
			(and ,(compile-match (car statement) `(car or) positions-to-skip)
			     ,(compile-match (cdr statement) `(cdr or) positions-to-skip))))))
	     (compile-match (pattern object-reference positions-to-skip)
	       ;; The workhorse of this thing.  dispatch on the type.
	       (typecase pattern
		 (logic-variable-maker
		   ;; trying to match a logic variable -- these never get skipped?
		   (let ((name (logic-variable-maker-name pattern)))
		     (cond ((member name variables-seen)
			    ;; not first occurrence -- unify
			    `(unify-constants-p ,object-reference ,name))
			   (t
			    ;; first occurrence -- just save it
			    (push name variables-seen)
			    `(progn
			       (setq ,name ,object-reference)
			       t)))))
		 (predication-maker
		   ;; make sure target is also a predication and call a matcher
		   (if (member pattern positions-to-skip)
		       t
		       `(let ((or ,object-reference))
			  (and
			    (typep or 'predication)
			    ,(compile-predication-match pattern `(predication-statement or))))
		       ))
		 (cons
		   ;; dispatch on car & cdr (to deal properly with dotted lists)
		   (if (member pattern positions-to-skip)
		       (compile-match (cdr pattern) `(cdr ,object-reference) positions-to-skip)
		       `(let ((or ,object-reference))
			  (and (consp or)
			       ,(compile-match (car pattern) `(car or) positions-to-skip)
			       ,(compile-match (cdr pattern) `(cdr or) positions-to-skip)))))
		 (null
		   ;; found a nil
		   (if (member pattern positions-to-skip)
		       t
		       `(null ,object-reference)))
		 (string
		   ;; found a string
		   (if (member pattern positions-to-skip)
		       t
		       `(string= ,object-reference ',pattern)))
		 (t
		   ;; anything else (usually symbol, number, or something like that)
		   (if (member pattern positions-to-skip)
		       t
		       `(eql ,object-reference ',pattern))))))
      (compile-predication-match self `(predication-statement ,pred-variable)))))


(defun write-environment-mergers (rule-name left-map right-map semi-unification-only)
  ;; given left and write maps, return function to merge and map of its output
  ;; Test case:
  ;;  (write-environment-merger '((?a . 0) (?b . 1) (?c . 2) (?d . 3))
  ;;                            '((?c . 0) (?d . 1) (?e . 2)))
  #-sbcl (declare (values full-merger semi-merger output-map merge-id))
  (labels
    ((validate-output-map (left-map right-map output-map)
       ;; every variable in either left or right must be in output
       (when (set-difference left-map output-map :test #'(lambda (x y) (car x) (car y)))
	 (error "There is a variable in the left map, ~S, that's not in the output map ~S."
		left-map output-map))
       (when (set-difference right-map output-map :test #'(lambda (x y) (car x) (car y)))
	 (error "There is a variable in the right map, ~S, that's not in the output map ~S."
		right-map output-map))
       ;; no variable can have more than 1 slot
       ;; no 2 distinct variables can get the same slot
       ))
    ;; the body
    (let ((output-map (generate-merged-environment-map left-map right-map)))
      (validate-output-map left-map right-map output-map)
      (multiple-value-bind (shared-vars unshared-left-vars unshared-right-vars)
	  (analyze-environment-sharing left-map right-map)
        (declare (ignore unshared-left-vars))
	(let ((merge-id (generate-merge-id left-map right-map output-map)))
	  (let (full semi)
	    (unless semi-unification-only
	      (setq full
		    (get-matcher-or-merger-from-cache
		      merge-id
		      'merger
		      #'(lambda ()
			  ;; first look in the cache of mergers to see if we can
			  ;; slime out of compiling new ones, but do this if necessary
			  (values
			    (gentemp (format nil "RULE-~s-FULL-MERGER-" rule-name))
			    (write-full-merger left-map right-map shared-vars
					       unshared-right-vars output-map))))))
	    (setq semi
		  (get-matcher-or-merger-from-cache
		    merge-id
		    'semi-merger
		    #'(lambda ()
			;; first look in the cache of mergers to see if we can
			;; slime out of compiling new ones, but do this if necessary
			(values
			  (gentemp (format nil "RULE-~s-SEMI-MERGER-" rule-name))
			  (write-semi-merger left-map right-map shared-vars
					     unshared-right-vars output-map)))))
	    ;; return full-merger, semi-merger, output-map, and merge-id
	    (values full semi output-map merge-id)))))))

(defun write-full-merger (left-map right-map shared-vars unshared-right-vars output-map)
  ;; write an environment merger, assuming full unification is required
  (let ((output-size (length output-map)))
    `(lambda (left-env right-env)
       (progn left-env right-env)		;so rules with no variables don't get warnings
       (let ((*new-variables* nil))
	 (declare (special *new-variables*))
	 (with-unification			;set up trail mechanism
	   ;; first unify all the elements of left and right envs.  This results in pointers
	   ;; from unbound variables in right to unbound variables in left.
	   ,@(loop for shared-var in shared-vars 
		   collecting `(unify (lookup-in-environment
					left-env ,(index-from-map shared-var left-map))
				      (lookup-in-environment
					right-env ,(index-from-map shared-var right-map))))
	   ;; no point in consing new env until here, since Part 1 doesn't refer to it.
	   (let ((output-env (make-array ,output-size)))
	     ;; second relocate variables with homes in the left environment to the new one.
	     ;; this generates huge amounts of code!
	     ;; copy all the data from slots in right env to slots in output env
	     ,@(loop for (left-var . left-index) in left-map
		     for output-index = (index-from-map left-var output-map)
		     collecting `(setf
				   (lookup-in-environment output-env ,output-index)
				   ;; this deals with structures as well as atomic data
				   (copy-unified-value
				     (lookup-in-environment left-env ,left-index))))
	     ;; copy all the data from slots in the left env to slots in output env
	     ,@(loop for unshared-right-var in unshared-right-vars
		     for right-index = (index-from-map unshared-right-var right-map)
		     for output-index = (index-from-map unshared-right-var output-map)
		     collecting `(setf
				   (lookup-in-environment output-env ,output-index)
				   (copy-unified-value
				     (lookup-in-environment right-env ,right-index))))
	     ;; find slots that aren't used, and fill them with unbound logic variables (This
	     ;; happens, for example, with disjunctive triggers that bind different variables.)
	     ,@(loop for (var . index) in output-map
		     unless (or (find var left-map  :key #'car)
				(find var right-map :key #'car))
		       ;; this variable is not in either the left or the right map, so make it
		       ;; unbound.
		       collect `(make-logic-variable-unbound output-env ,index))
	     ;; return the output environment
	     (values output-env t)))))))

(defun write-semi-merger (left-map right-map shared-vars unshared-right-vars output-map)
  ;; given left and write maps, return function to merge and map of its output
  ;; Test case:
  ;;  (write-environment-merger '((?a . 0) (?b . 1) (?c . 2) (?d . 3))
  ;;                            '((?c . 0) (?d . 1) (?e . 2)))
  (let ((output-size (length output-map)))
    ;; now generate the function, returning it and the output map
    (let ((new-variable-introduced
	    (loop for (var) in output-map
		  thereis (not
			    (or (find var left-map :key #'car)
				(find var right-map :key #'car))
			    ))))
      `(lambda (left-env right-env)
	 (progn left-env right-env)		;so rules with no variables don't get warnings
	 (when (and
		 ,@(loop for shared-var in shared-vars 
			 collecting
			   `(equal
			      (lookup-in-environment left-env
						     ,(index-from-map shared-var left-map))
			      (lookup-in-environment right-env
						     ,(index-from-map shared-var right-map)))))
	   ;; first unify all the elements of left and right envs.
	   (let* ((output-env (make-array ,output-size)))
	     ;; copy all the data from slots in right env to slots in output env
	     ,@(loop for (left-var . left-index) in left-map
		     for output-index = (index-from-map left-var output-map)
		     collecting `(setf (lookup-in-environment output-env ,output-index)
				       ;; this deals with structures as well as atomic data
				       (lookup-in-environment left-env ,left-index)))
	     ;; copy all the data from slots in the left env to slots in output env
	     ,@(loop for unshared-right-var in unshared-right-vars
		     for right-index = (index-from-map unshared-right-var right-map)
		     for output-index = (index-from-map unshared-right-var output-map)
		     collecting `(setf (lookup-in-environment output-env ,output-index)
				       (lookup-in-environment right-env ,right-index)))
	     ;; find slots that aren't used, and fill them with unbound logic variables (This
	     ;; happens, for example, with disjunctive triggers that bind different variables.)
	     ,@(loop for (var . index) in output-map
		     unless (or (find var left-map :key #'car)
				(find var right-map :key #'car))
		       ;; this variable is not in either the left or the right map, so make it
		       ;; unbound.
		       collect `(make-logic-variable-unbound output-env ,index))
	     ;; return the output environment
	     (values output-env ,new-variable-introduced)))))))

(defun analyze-environment-sharing (left-map right-map)
  ;; returns list of variables left and right have in common and a list of variables in left
  ;; which are not shared, and list of variables in right which are not shared.
  ;; Example: (analyze-environment-sharing '((?x . 0) (?y . 1)) '((?y . 1) (?z . 0)))
  ;;   returns (?Y) and (?Z).
  #-sbcl (declare (values shared unshared-left unshared-right))
  (values
    (mapcar #'car
	    (intersection right-map left-map :key #'car))
    (mapcar #'car
	    (set-difference left-map right-map :key #'car))
    (mapcar #'car
	    (set-difference right-map left-map :key #'car))))

(defun generate-merge-id (left-map right-map output-map)
  ;; A description of how slots in the input environments map to slots in the output
  ;; environment.  It's a pair of lists, mapping left env slots to output slots and right env
  ;; slots to output slots.
  (cons (loop for (left-var . left-index) in left-map
	      collecting (cons left-index (index-from-map left-var output-map)))
	(loop for (right-var . right-index) in right-map
	      collecting (cons right-index (index-from-map right-var output-map)))))

;;; Stuff to write code for PROCEDUREs, i.e. lisp code in trigger position of forward rules
;;;
;;; We're given the lisp code the map and a list a variables that occur in
;;; the preceeding triggers.
;;;
;;; The procedure function is passed an environment a rete node and some
;;; other stuff needed for rete networking.  The code is supposed to
;;; execute "in the match environment" represented by the passed in
;;; environment.  The procedure can either just return T or call succeed.
;;; Calling succeed should cause a new environment to get built and
;;; passed to the next node in the rete network.  The environment passed
;;; on contains all the lvs that came in, plus any new ones that are
;;; referenced in this code.
;;;
;;; Around the lisp code we wrap stuff that unpacks the lvs from the
;;; environment that gets passed in.  We're going to need to build an
;;; environment which contains all these lvs anyhow, so we unpack them
;;; all.

;;; We create an flet'd succeed function which builds a new environment
;;; and calls the right routine to drop that into the next node of the
;;; rete network.  Logic variables in this code are mapped to lisp
;;; variables of the same name (done by building a set of
;;; variable-analyses).  We also go to some pain to determine if the
;;; environment that will be passed on is logic-variable-free.

;;; This now uses calls-succeed-p to generate better code when it can.

(defun write-forward-procedure-code (function-name map environment analysis)
  (declare (special *forward-rule-trigger-compiler-environment*))
  (let* ((variables-seen-so-far (loop for (variable) in map collect variable))
	 (variables-referenced (procedure-pattern-analysis-variables-referenced analysis))
	 (old-variables-referenced (intersection variables-seen-so-far variables-referenced))
	 (code (procedure-pattern-analysis-expression analysis))
	 (body-calls-succeed (calls-succeed-p code environment))
	 (support-variable (pattern-analysis-name analysis))
	 (new-variables (procedure-pattern-analysis-new-variables analysis))
	 (known-lvs (union new-variables variables-seen-so-far))
	 (downstream-variables (pattern-analysis-variables-used analysis))
	 (new-variables-that-somebody-uses (intersection new-variables downstream-variables))
	 (old-lvs-plus-used-new-lvs (union variables-seen-so-far new-variables-that-somebody-uses))
	 (semi-unification-only (pattern-analysis-pure-semi-unification? analysis))
	 (new-map (or (when (null new-variables-that-somebody-uses) map)
		      (append map
			      (loop for var in new-variables-that-somebody-uses
				    for i from (length map)
				  collect (cons var i))))))
    (let* ((succeed-code (if semi-unification-only
			     `(let* ((.new-env. (make-array ,(length new-map))))
				(progn old-state-has-lvs)
				;; stuff the values of the logic-variables into the new environment.
				;; Should this be using copy-unified-value?
				(replace .new-env. .env.)
				,@(loop for variable in new-variables-that-somebody-uses
					for position = (cdr (assoc variable new-map))
					collect `(setf (aref .new-env. ,position) (logic-variable-maker ,variable)))
				(funcall .continuation. .new-env. ,support-variable nil)
				nil)
			     `(let* ((new-state-has-lvs (if old-state-has-lvs 
							    (or ,@(loop for variable in old-lvs-plus-used-new-lvs
									collect `(unbound-logic-variable-p ,variable)))
							    (or ,@(loop for variable in new-variables-that-somebody-uses
									collect `(unbound-logic-variable-p ,variable)))))
				     (.new-env. (make-array ,(length new-map))))
				;; stuff the values of the logic-variables into the new environment.
				;; Should this be using copy-unified-value?
				,@(loop for (variable . position) in new-map
					if (member variable variables-seen-so-far)
					  collect `(setf (aref .new-env. ,position) (logic-variable-maker ,variable))
					else collect `(setf (aref .new-env. ,position)
							    (if (unbound-logic-variable-p ,variable)
								(make-unbound-logic-variable ',variable)
							      (logic-variable-maker ,variable))))
				(funcall .continuation. .new-env. .support. new-state-has-lvs)
				nil)))
	   (variable-initialization-code
	     (if semi-unification-only
		 (loop for variable in old-variables-referenced
		       for position = (cdr (assoc variable map))
		       when (member variable variables-referenced)
			 collect `(setq ,variable (aref .env. ,position)))
		 (loop for (variable . position) in map
		       when (member variable variables-seen-so-far)
			 collect `(setf (joshua-logic-variable-value-cell ,variable)
                                        (aref .env. ,position)))))
	   (core-body `(with-unbound-logic-variables
			 ;; bind all the variables
			 ,(if semi-unification-only new-variables known-lvs)
			 (macrolet ((known-lvs () ',known-lvs))
			   ;; initialize the previously existing variables
			   ;; by pulling the value out of the environment.
			   ,@variable-initialization-code
			   ;; now run the code
			   (with-unification
			     (flet ((succeed (&optional .support.)
				      #+genera (declare (sys:downward-function))
				      ,@(if (null support-variable)
					    `((declare (ignorable .support.)))
					  `((unify ,support-variable .support.)))
				      ,succeed-code))
			       (declare (dynamic-extent #'succeed))
			       ,(if body-calls-succeed
				    code
				  `(when ,code (succeed nil)))))))))
      (let ((generated-code `(defun ,function-name (.env. old-state-has-lvs .continuation.)
			       #-sbcl (declare (dynamic-extent .continuation.))
			       ,@(when (null variable-initialization-code)
				   `((progn .env.)))
			       ,(if semi-unification-only
				    `(let ,old-variables-referenced
				       ,core-body)
				    core-body))))
	(collect-forward-rule-subsidiary-function generated-code))
      new-map)))




(defun write-forward-or-trigger-shuffler (function-name input-map analysis)
  (let* ((output-map (pattern-analysis-map analysis))
	 (new-variables (loop for entry in output-map
			      for (variable) = entry
			      unless (assoc variable input-map)
				collect variable))
	 (known-to-be-semi-unification (pattern-analysis-pure-semi-unification? analysis))
	 shuffling-code
	 binding-code
	 has-lv-expression 
	 (old-env-refd nil))
    (setq shuffling-code
	  (loop for (out-variable . output-position) in output-map
		for (nil . in-position) = (assoc out-variable input-map)
		if in-position
		  collect `(setf (aref .new-env. ,output-position) (aref .old-env. ,in-position))
		  and do (setq old-env-refd t)
		else collect `(setf (aref .new-env. ,output-position)
				    (make-unbound-logic-variable ',out-variable))))
    (setq binding-code
	  (cond (known-to-be-semi-unification
		 `(,@(when old-env-refd `((.old-env. (rete-internal-state-environment .rete-internal-state.))))
		   (.new-env. (make-array ,(length output-map)))))
		(new-variables
		 ;; Known to be full unification in this case.
		 `(,@(when old-env-refd `((.old-env. (rete-internal-state-environment .rete-internal-state.))))
		   (.new-env. (make-array ,(length output-map)))))
		(t `((has-lvs (not (zerop (rete-internal-state-has-logic-variables .rete-internal-state.))))
		     ,@(when old-env-refd `((.old-env. (rete-internal-state-environment .rete-internal-state.))))
		     (.new-env. (make-array ,(length output-map)))))))
    (setq has-lv-expression
	  (cond (known-to-be-semi-unification 0)
		(new-variables 1)
		(t `(if has-lvs 1 0))))
    (collect-forward-rule-subsidiary-function
      `(defun ,function-name (.node. .rete-internal-state.)
	 (let* ,binding-code
	   ,@shuffling-code
	   (let ((next-state (make-rete-internal-state
			       :environment .new-env.
			       :predications (rete-internal-state-predications .rete-internal-state.)
			       :my-node .node.
			       :has-logic-variables ,has-lv-expression)))
	     (push next-state (rete-internal-state-children .rete-internal-state.))
	     (rete-proceed-to-next-node next-state .node.)))))))
