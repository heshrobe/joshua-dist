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
;;; Created 12/19/85 13:17:50 by sgr running on GROUSE at SCRC.

;;;
;;; Simple discrimination net for Joshua.  This is for predications, not
;;; rule trigger patterns.  Predications have a ratio of update to access
;;; that can be near 1, hence this graph-interpreting algorithm.
;;; Contrast with the compiler approach for rule trigger patterns.
;;;
;;; Cribbed from AMORD and from "Artificial Intelligence Programming", by
;;; Charniak, Riesbeck, & McDermott.  Many sources talk about complicated 
;;; ideas that I think would just slow this down; I tried to go for simplicity.
;;;
;;; The 8-fold way of discrimination nets (CR&M, ch. 14):
;;;
;;; [1] Are variables allowed in the data patterns? Yes
;;; [2] Are variables allowed in the query patterns? Yes
;;; [3] Does one keep track of variable bindings during fetching? No.
;;; [4] Should one return a list or stream of possibilities? Pass closure down into dn fetcher.
;;; [5] Should one use CAR or CAR-CDR indexing? CAR.  (Except for tail variables.)
;;; [6] Should one uniquify subexpressions? Yes, simplifies variable X predication match.
;;; [7] Should one completely discriminate the data? Yes.
;;; [8] Should one use multiple indexing? No.
;;;
;;; Note that if we answer CAR to 5 and Yes to 6, we don't need
;;; *BEGIN-PREDICATION* markers, since they all go through the root node,
;;; where it's implicit.  However, we do need *END-PREDICATION* markers,
;;; so that [IS NOT] and [IS NOT EITHER] end in different places.
;;; (Otherwise, the node for EITHER would both contain [IS NOT] and be
;;; non-terminal.  We could hack that, but it hairs up the code.  Maybe
;;; later, since it does compress the network.  But it requires splitting
;;; the info-or-table slot in two.)
;;;
;;; Also note that since segment variables are only allowed in tail position,
;;; the seg-var-link field of a discrimination-net-node could just contain terminals.  For now,
;;; it contains a *segment-variable* node for clarity, which costs us an extra
;;; link traversal in the case of a tail variable.
;;;
;;; These discrimination nets are collections of nodes supporting 
;;; the following protocol:
;;;
;;; [1] (dn-tell dn predication) -- adds predication, creating nodes as necessary
;;; [2] (dn-ask  dn predication continuation) -- funcalls continuation on each node it finds
;;; [3] (dn-clear  dn)           -- flushes everything in the dn
;;; [4] (dn-graph  dn)           -- draws a pretty picture of the dn (for debugging the dn)
;;;
;;;

;;;
;;; Faster tables.  These are implemented as defstructs instead of flavors
;;; for speed (aref is faster than instance-ref, no method table lookup overhead, etc.).
;;;
;;; Entrypoints: make-dn-table, dn-table-clear, dn-table-get, (setf dn-table-get),
;;;              dn-table-size, dn-table-map, dn-table-keys, dn-table-values
;;;

(in-package "JI")

(defparameter *dn-table-crossover-size* 18.
  ;; this number was generated using the tools in sys:joshua;tests;time-dn-tables.
  "Below this number of elements, an alist is faster than a hash table.")

(defstruct (dn-table)
  "Fast, adaptive table."
  (alist-p t) ;plist would be faster, but this needs EQUAL (sigh)
  (current-size 0.)
  (data nil))

(eval-when (compile load eval)
  (proclaim '(inline is-dn-table-p))
  (defun is-dn-table-p (x)
    (dn-table-p x)))

;;;
;;; Functions defined on dn-tables.
;;;

(defun dn-table-clear (dn-table)
  "Makes dn-table empty."
  (setf (dn-table-alist-p dn-table)      t
	(dn-table-current-size dn-table) 0.
	(dn-table-data dn-table)         nil))


(eval-when (compile load eval)
  (proclaim '(inline assoc-equal))
  (defun assoc-equal (item alist)
    "Open-coding of (assoc item alist :test #'equal)."
    (loop for cell in alist when (equal item (car cell)) return cell)))


(eval-when (compile load eval)
  (proclaim '(inline dn-table-get))
  (defun dn-table-get (key dn-table)
    "Gets data associated with key in dn-table."
    (cond ((dn-table-alist-p dn-table) (cdr (assoc-equal key (dn-table-data dn-table))))
	  (t (gethash key (dn-table-data dn-table))))))

(defun dn-table-put (key dn-table value)
  "Put data into dn-table, indexed under key."
  (cond ((dn-table-alist-p dn-table)
	 ;; implemented as an alist
	 (let ((cell (assoc-equal key (dn-table-data dn-table))))
	   (cond (cell
		   ;; found a cell
		   (rplacd cell value))
		 (t
		   ;; no cell for key, so add it
		   (push (cons key value) (dn-table-data dn-table))
		   (incf (dn-table-current-size dn-table))
		   (when (> (dn-table-current-size dn-table)
			    *dn-table-crossover-size*)
		     ;; time to change into a hash table
		     (loop with table = (make-hash-table :test #'equal) ;give some of the sexy new args?
			   for (existing-key . existing-value) in (dn-table-data dn-table)
			   doing (setf (gethash existing-key table) existing-value)
			   finally (setf (dn-table-data dn-table) table
					 (dn-table-alist-p dn-table) nil)))))))
	(t
	  ;; implemented as a hash table
	  (setf (gethash key (dn-table-data dn-table)) value)))
  ;; return value
  value)

;;; How to store into a dn-table.
(defsetf dn-table-get dn-table-put)

(defun dn-table-size (dn-table)
  "How many elements there are in the table now."
  (if (dn-table-alist-p dn-table)
      (dn-table-current-size dn-table)
      (hash-table-count (dn-table-data dn-table))))

;;; There's no iteration path because of this complexity.
(defun dn-table-map (dn-table function)
  "Map a function over all the key-datum pairs."
  (declare (dynamic-extent function))
  (if (dn-table-alist-p dn-table)
      ;; loop over the alist
      (loop for cell in (dn-table-data dn-table)
	    doing (funcall function (car cell) (cdr cell)))
      ;; let the hash table do it
      (maphash function (dn-table-data dn-table))))

(defun dn-table-keys (dn-table)
  "Return a list of all the keys in the table."
  (if (dn-table-alist-p dn-table)
      (loop for (key) in (dn-table-data dn-table) collecting key)
      (let (val)
	(maphash #'(lambda (key elt) elt (push key val))
		 (dn-table-data dn-table))
	val)
      #|| (loop for datum being the hash-elements of (dn-table-data dn-table) with-key key
	    collecting key)||#
      ))

(defun dn-table-values (dn-table)
  "Return a list of all the values in the table."
  (if (dn-table-alist-p dn-table)
      (loop for (nil . datum) in (dn-table-data dn-table)
	    collecting datum)
      (let (val)
	(maphash #'(lambda (key elt) key (push elt val))
		 (dn-table-data dn-table))
	val)
      #||(loop for datum being the hash-elements of (dn-table-data dn-table)
	    collecting datum)||#
      ))


(eval-when (compile load eval)
  (proclaim '(inline discrimination-net-node-terminal-p))
  (defun discrimination-net-node-terminal-p (node)
    "Whether or not this node is terminal."
    (eq (discrimination-net-node-token node) '*end-predication*)))

(defun discrimination-net-node-children (node)
  "Return list of children of this node."
  ;; called by the grapher
  (let ((kidz nil)
	(table (discrimination-net-node-info-or-table node)))
    ;; union of things in table, var-link, and seg-var-link
    (when (is-dn-table-p table)
      (setq kidz (dn-table-values table)))
    (when (discrimination-net-node-var-link node)
      (push (discrimination-net-node-var-link node) kidz))
    (when (discrimination-net-node-seg-var-link node)
      (push (discrimination-net-node-seg-var-link node) kidz))
    kidz))

;;;;;;;;;; Tell protocol

(defun dn-tell-token (token from-node)
  ;; Walks token to next node of dn.  Token is neither predication, variable, nor segment variable.
  (cond ((null (discrimination-net-node-info-or-table from-node))
	 ;; this node has never sprouted a token, so give it a table
	 (setf (discrimination-net-node-info-or-table from-node) (make-dn-table))
	 (let ((new-node (make-discrimination-net-node :token token)))
	   ;; tell the old node about the new one and return the new one
	   (setf (dn-table-get token (discrimination-net-node-info-or-table from-node)) new-node)
	   new-node))
	;; see if this node knows about this token
	((dn-table-get token (discrimination-net-node-info-or-table from-node)))
	(t
	  ;; doesn't know about this token, so teach it
	  (let ((new-node (make-discrimination-net-node :token token)))
	    ;; tell the old node about the new one, and return the new one
	    (setf (dn-table-get token (discrimination-net-node-info-or-table from-node)) new-node)
	    new-node))))

(eval-when (compile eval load)
  (proclaim '(inline dn-tell-variable))
  (defun dn-tell-variable (from-node)
    (cond ((discrimination-net-node-var-link from-node))
	  (t (let ((new-node (make-discrimination-net-node :token '*variable*)))
	       (setf (discrimination-net-node-var-link from-node) new-node)
	       new-node)))))

(eval-when (compile eval load)
  (proclaim '(inline dn-tell-segment-variable))
  (defun dn-tell-segment-variable (from-node)
    (cond ((discrimination-net-node-seg-var-link from-node))
	  (t (let ((new-node (make-discrimination-net-node :token '*segment-variable*)))
	       (setf (discrimination-net-node-seg-var-link from-node) new-node)
	       new-node)))))

(eval-when (compile eval load)
  (proclaim '(inline dn-tell-embedded-list))
       (defun dn-tell-embedded-list (node)
         (dn-tell-token '*embedded-list* node)))

(defun dn-tell-predication (predication root-node)
  ;; Walks an predication through dn.  Embedded subpredications get re-discriminated from root.
  (loop	for node = root-node
		 then (typecase token
			(predication
			  ;; this token is a subpredication, so walk it from the root
			  ;; and use the resulting node as a token
			  (dn-tell-token (dn-tell-predication token root-node) node))
			(unbound-logic-variable
			  ;; non-tail variable
			  (dn-tell-variable node))
			(cons
			  ;; embedded list
			  (dn-tell-embedded-list node))
			(otherwise
			  ;; just an ordinary token, go talk to the table
			  (dn-tell-token token node)))
	for rest-tokens = (predication-statement predication) then (cdr rest-tokens)
	while (consp rest-tokens)
	for token = (car rest-tokens)
	finally
	  (when rest-tokens
	    ;; tail variable
	    (setq node (dn-tell-segment-variable node)))
	  ;; now walk the closing *END-PREDICATION* and return that node
	  (return (dn-tell-token '*END-PREDICATION* node))))

(defun discrimination-net-insert (root-node predication)
  "Adds a predication to the discrimination net.  Returns canonicalized predication."
  ;; Note that insert can effectively ignore variables in the db!
  ;; find the terminal node corresponding to this predication
  (loop with terminal-node = (dn-tell-predication predication root-node)
	;; open-code (find predication (discrimination-net-node-info-or-table terminal-node) :test #'variant)
	for this-one in (discrimination-net-node-info-or-table terminal-node)
	when (variant predication this-one)
	  ;; already there, so return the canonical version
	  return (values this-one nil)
	finally
	  ;; not already there, so stick it in
	  (let ((database-predication (copy-object-if-necessary predication)))
	    (push database-predication (discrimination-net-node-info-or-table terminal-node))
	    ;; and return this version, which is now the canonical one
	    (return (values database-predication t)))))

(defun discrimination-net-uninsert (root-node database-predication)
  "Removes a database predication from the discrimination net."
  (let ((terminal-node (dn-tell-predication database-predication root-node)))
    (setf (discrimination-net-node-info-or-table terminal-node)
	  (delete database-predication (discrimination-net-node-info-or-table terminal-node)))
    (values)))


;;;;;;;;;; Ask protocol

(defun dn-ask-token-variables (root-node current-node predication generator-state collector-function)
  ;; select children along variable links matching this non-variable, non-segment-variable token
  (declare #+genera (dbg:invisible-frame joshua-internals)
	   (dynamic-extent collector-function))
  (when (discrimination-net-node-var-link current-node)
    ;; has a var-link, so recurse on it
    (dn-ask-next root-node (discrimination-net-node-var-link current-node) predication generator-state
		 collector-function))
  (when (discrimination-net-node-seg-var-link current-node)
    ;; has a seg-var link, so accept its fringe
    (dn-ask-segment-variable (discrimination-net-node-seg-var-link current-node) collector-function)))

(defun dn-ask-token (root-node current-node predication generator-state collector-function
			       token do-variables-p)
  ;; select children matching this non-variable, non-segment-variable token
  (declare #+genera (dbg:invisible-frame joshua-internals)
	   (dynamic-extent collector-function))
  (when (is-dn-table-p (discrimination-net-node-info-or-table current-node))
    ;; this node has a table, so look up the token in it
    (let ((next-node (dn-table-get token (discrimination-net-node-info-or-table current-node))))
      (when next-node
	;; found one, so recurse on it
	(dn-ask-next root-node next-node predication generator-state collector-function))))
  (when do-variables-p
    ;; dn-ask-next binds this to NIL when doing embedded subpredications, so that
    ;; it ends up following variables exactly once.
    (dn-ask-token-variables root-node current-node predication generator-state collector-function)))

(defun dn-ask-*END-PREDICATION* (current-node collector-function)
  ;; just like dn-ask-token, except it asks the constant token *END-PREDICATION*,
  ;; ignores regular variables in the data, and only returns terminal nodes.
  (declare #+genera (dbg:invisible-frame joshua-internals)
	   (dynamic-extent collector-function))
  (when (is-dn-table-p (discrimination-net-node-info-or-table current-node))
    ;; current node has a table, so look for *END-PREDICATION*'s in it
    (let ((next-node (dn-table-get '*END-PREDICATION* (discrimination-net-node-info-or-table current-node))))
      (when next-node
	;; found one, so collect it
	(funcall collector-function next-node))))
  (when (discrimination-net-node-seg-var-link current-node)
    ;; has a seg-var link, so accept its fringe
    (dn-ask-segment-variable (discrimination-net-node-seg-var-link current-node) collector-function)))

(defun dn-ask-variable (root-node current-node predication generator-state collector-function)
  ;; a variable in the query pattern means to skip a level.  Since we canonicalize
  ;; sub-predications, this just means to recurse on the kids of current-node
  (declare #+genera (dbg:invisible-frame joshua-internals)
	   (dynamic-extent collector-function))
  (when (is-dn-table-p (discrimination-net-node-info-or-table current-node))
    ;; recurse on all nodes in the table
    (dn-table-map (discrimination-net-node-info-or-table current-node)
		  #'(lambda (token child-node)
		      (declare (ignore token))
		      (dn-ask-next root-node child-node predication generator-state
				   collector-function))))
  (when (discrimination-net-node-var-link current-node)
    ;; recurse on the var-link if it's present
    (dn-ask-next root-node (discrimination-net-node-var-link current-node) predication generator-state
		 collector-function))
  (when (discrimination-net-node-seg-var-link current-node)
    ;; has a seg-var link, so accept its fringe
    (dn-ask-segment-variable (discrimination-net-node-seg-var-link current-node) collector-function)))

;;; This should be called dn-ask-tail-variable.
(defun dn-ask-segment-variable (current-node collector-function)
  ;; a tail segment variable (in either query or data) 
  ;; means collect this node's fringe
  (declare #+genera (dbg:invisible-frame joshua-internals)
	   (dynamic-extent collector-function))
  (when (is-dn-table-p (discrimination-net-node-info-or-table current-node))
    ;; this node has a table, so look at each entry
    (dn-table-map (discrimination-net-node-info-or-table current-node)
		  #'(lambda (token kid)
		      (declare (ignore token))
		      (if (discrimination-net-node-terminal-p kid)
			  ;; terminal node, so collect it
			  (funcall collector-function kid)
			  ;; non-terminal node, so recurse on it
			  (dn-ask-segment-variable kid collector-function)))))
  (when (discrimination-net-node-var-link current-node)
    ;; this node has a data variable, so recurse on it
    (dn-ask-segment-variable (discrimination-net-node-var-link current-node) collector-function))
  (when (discrimination-net-node-seg-var-link current-node)
    ;; has a seg-var link, so accept its fringe
    (dn-ask-segment-variable (discrimination-net-node-seg-var-link current-node) collector-function)))

(eval-when (compile eval load)
  (proclaim '(inline dn-ask-predication))
  (defun dn-ask-predication (root-node predication collector-function)
    (declare (dynamic-extent collector-function))
    (dn-ask-next root-node
		 root-node
		 predication
		 (predication-statement predication)
		 collector-function)))

(eval-when (compile eval load)
  (proclaim '(inline dn-ask-embedded-list))
  (defun dn-ask-embedded-list (root-node current-node predication generator-state
			       collector-function)
    (declare (dynamic-extent collector-function))
    (dn-ask-token root-node
		  current-node
		  predication
		  generator-state
		  collector-function
		  '*embedded-list*
		  t)))

(defun dn-ask-next (root-node current-node predication generator-state collector-function)
  ;; dispatch on type of next token to walk it
  (declare #+genera (dbg:invisible-frame joshua-internals)
	   (dynamic-extent collector-function))
  (cond ((null generator-state)
	 ;; no more tokens go get, call collector-function on terminal kids
	 (dn-ask-*END-PREDICATION* current-node collector-function))
	((atom generator-state)
	 ;; tail segment variable
	 (dn-ask-segment-variable current-node collector-function))
	(t
	 ;; still some left
	 (let ((next-token (joshua-logic-variable-value (pop generator-state))))
	   ;; dispatch on the type of the next token
	   (typecase next-token
	     (unbound-logic-variable
	       ;; regular variable
	       (dn-ask-variable root-node current-node predication generator-state collector-function))
	     (cons
	       ;; embedded list
	       (dn-ask-embedded-list root-node current-node predication generator-state
				     collector-function))
	     (predication
	       ;; embedded sub-predication, so tokenize it (note this could succeed many times...)
	       (let ((nodes nil))
		 ;; look for matches, putting them in a recursive collector
		 (flet ((continuation (x)
			  #+(or cloe genera) (declare (sys:downward-function))
			  (push x nodes)))
		   (declare (dynamic-extent #'continuation))
		   (dn-ask-predication root-node next-token #'continuation))
		     
		 ;; now ask the question again, using the nodes in recursive-collector as tokenized
		 ;; versions
		 (loop for node in nodes do
		   ;; use it to further discriminate, but DON'T DO VARIABLES -- otherwise,
		   ;; we'd do the variables multiple times, once for each element of the
		   ;; recursive stack collector.  That's a fix for the following BUG:
		   ;; the embedded sub-predication might not be in the dn, whereas the pattern
		   ;; we're matching against might still accept it by having a variable there.
		   ;; Example: suppose
		   ;;      [inform =speaker =auditor =info] is in the dn, and we want to match 
		   ;;      [inform wendy    sgr      [is wendy in-england]] against it.
		   ;; This fails, but shouldn't.
		   (dn-ask-token root-node current-node predication generator-state
				 collector-function node nil)))
	       ;; NOW do the variables, just once.
	       (dn-ask-token-variables root-node current-node predication generator-state
				       collector-function))
	     (otherwise
	       ;; random token
	       (dn-ask-token root-node current-node predication generator-state collector-function
			     next-token t)))))))

(defun discrimination-net-fetch (root-node predication continuation)
  (declare (dynamic-extent continuation))
  ;; in order to save stack depth, collect nodes from all over the net,
  ;; then call the continuation
  (let ((nodes nil))
    (flet ((continuation (node)
	     #+(or cloe genera) (declare (sys:downward-function))
	     (push node nodes)))
      (declare (dynamic-extent #'continuation))
      (dn-ask-predication root-node predication #'continuation))
    (loop for node in nodes do
      (loop for predication in (discrimination-net-node-info-or-table node)
	    doing (funcall continuation predication)))))

;;;;;;;;;; Clear protocol

(defun discrimination-net-Clear (root-node)
  " the dn."
  ;; all nodes below the root are just cut off, to hang twisting in the breeze
  ;; using a resource would fix that, at the expense of some speed
  ;; But since this is done relatively seldom, punt the resource.
  (setf (discrimination-net-node-info-or-table root-node) nil
	(discrimination-net-node-var-link root-node)      nil
	(discrimination-net-node-seg-var-link root-node)  nil))
