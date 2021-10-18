;;; -*- Mode: JOSHUA; Package: JOSHUA-INTERNALS; Syntax: Joshua-ansi; Base: 10; Lowercase: T; -*-
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
;;; Created 5/04/89 11:26:45 by JGA running on JUBJUB at SCRC.

;;; Editor support stuff for Joshua.  It's necessary so that buffers can
;;; be hacked with the right readtable, so that [] and {} match, blink,
;;; and can be c-m-F'd over.  Also a zwei:Joshua-mode-hook, for those
;;; who want to set it to electric-style-lock, or something similar.
;;;
;;; Unlike earlier versions, this doesn't attempt any ambitious package-defaulting,
;;; automatic code-fonting, etc.  We assume people will use Electric Style-Lock mode
;;; as a default minor mode if that's what they want.  (Should document how to do that.)
;;; Also, we provide a command m-X Create Initial Joshua Attribute List that
;;; puts in reasonable defaults.
;;;

(defun make-defining-form-irrelevant (name)
  ;; put into places Zmacs promises never to look
  (pushnew name zwei:*irrelevant-functions* )
  (pushnew name zwei:*irrelevant-defining-forms*)
  nil)

;;;
;;; Blink switches for the editor.
;;;

(defparameter *blink-predications-in-joshua-mode* t
  "Whether or not [] blinks in Joshua mode.")

(defparameter *blink-variables-in-joshua-mode* nil
  "Whether or not foo blinks in Joshua mode.")

(defparameter *blink-vertical-bars-in-joshua-mode* nil
  "Whether or not |foo| blinks in joshua mode.")

(defparameter *blink-strings-in-joshua-mode* nil
  "Whether or not \"foo\" blinks in Joshua mode.")

;;;
;;; Defaults for the initial attribute list.
;;;

(defparameter *default-package-for-joshua-mode* (find-package "JU")
  "Default package attribute to be put into initial attribute lists of Joshua buffers.")

(defparameter *default-base-for-joshua-mode* 10.
  "Default base attribute to be put into initial attribute lists of Joshua buffers.")

(defparameter *default-vsp-for-joshua-mode* nil	;should probably default to NIL
  "An integer to specify a vsp, nil to turn off the feature.")

;;;
;;; Syntax tables used in the editor mode.
;;;

(defparameter *joshua-mode-list-syntax-table* nil	;initialized below
  "List syntax table for Joshua mode in the editor.")

(defparameter *joshua-mode-word-syntax-table* nil	;initialized below
  "word syntax table for Joshua mode in the editor.")

(defparameter *joshua-mode-atom-word-syntax-table* nil	;initialized below
  "Atom word syntax table for Joshua mode in the editor.")

(defun initialize-joshua-syntax-tables ()
  (setq *joshua-mode-list-syntax-table*      (zwei:copy-syntax-table zwei:*cl-list-syntax-table*)
	*joshua-mode-word-syntax-table*      (zwei:copy-syntax-table zwei:*word-syntax-table*)
	*joshua-mode-atom-word-syntax-table* (zwei:copy-syntax-table zwei:*atom-word-syntax-table*))
  ;; list syntax table
  (zwei:set-char-syntax zwei:list-alphabetic *joshua-mode-list-syntax-table* *joshua-variable-char*)
  ;; (zwei:set-char-syntax zwei:list-alphabetic *joshua-mode-list-syntax-table* *joshua-alternative-variable-char*)
  (zwei:set-char-syntax zwei:list-open       *joshua-mode-list-syntax-table* *joshua-open-predication-char*)
  (zwei:set-char-syntax zwei:list-close      *joshua-mode-list-syntax-table* *joshua-close-predication-char*)
  ;; word syntax zwei:table
  (zwei:set-char-syntax zwei:word-alphabetic *joshua-mode-word-syntax-table* *joshua-variable-char*)
  ;; (zwei:set-char-syntax zwei:word-alphabetic *joshua-mode-word-syntax-table* *joshua-alternative-variable-char*)
  (zwei:set-char-syntax zwei:word-delimiter  *joshua-mode-word-syntax-table* *joshua-open-predication-char*)
  (zwei:set-char-syntax zwei:word-delimiter  *joshua-mode-word-syntax-table* *joshua-close-predication-char*)
  ;; atom word zwei:syntax table
  (zwei:set-char-syntax zwei:word-alphabetic *joshua-mode-atom-word-syntax-table* *joshua-variable-char*)
  ;; (zwei:set-char-syntax zwei:word-alphabetic *joshua-mode-atom-word-syntax-table* *joshua-alternative-variable-char*)
  (zwei:set-char-syntax zwei:word-delimiter  *joshua-mode-atom-word-syntax-table* *joshua-open-predication-char*)
  (zwei:set-char-syntax zwei:word-delimiter  *joshua-mode-atom-word-syntax-table* *joshua-close-predication-char*)
  ;; now tell the IE about this syntax
  (let ((cell (assoc :joshua si:*lisp-mode-to-appropriate-zwei-list-syntax-table*)))
    (if cell
	;; if it's already there, just update in place
	(rplacd cell '*joshua-mode-list-syntax-table*)
	;; otherwise create a new entry.
	(push (cons :joshua '*joshua-mode-list-syntax-table*)
	      si:*lisp-mode-to-appropriate-zwei-list-syntax-table*)))
  ;; given an opening delimiter and a mode, figure out what arglist finder to hand to ARGLIST.
  (let ((cell (find *joshua-open-predication-char* si:*delimiter-and-mode-to-arglist-finder-table*
		     :test #'(lambda (char entry)
			       (and (char-equal char (first entry))
				    (eql :joshua (second entry)))))))
    (if cell
	;; if it's already there, just update in place
	(rplaca (last cell) 'find-predicate-arglist)
	;; otherwise create a new entry.
	(push (list *joshua-open-predication-char* :joshua 'find-predicate-arglist)
	      si:*delimiter-and-mode-to-arglist-finder-table*))))

(initialize-joshua-syntax-tables)

;;;
;;; Definition of Joshua as a Zwei language mode.
;;;

(scl:defflavor joshua-mode () (zwei:lisp-mode))     ;lisp mode with teeth!

(scl:defmethod (:all-uppercase joshua-mode) ()
  "Prevent random case-changes."
  nil)

(scl:defmethod (:mode-line-name joshua-mode) ()     ;name printed on mode line & attribute line
  "Joshua")

(scl:defmethod (:get-default-attribute joshua-mode :syntax) ()
  (let ((use-list (package-use-list *package*)))
    (cond ((member (find-package "Joshua") use-list) :joshua)
	  ((or (member (find-package "COMMON-LISP") use-list)
	       (member (find-package "SYMBOLICS-COMMON-LISP") use-list))
	   :common-lisp))))

;;; written as a separate method so it gets called compiled (from :mode-forms below)
(scl:defmethod (flush-cached-line-parse-info joshua-mode) ()
  (loop with last-line = (zwei:bp-line (zwei:interval-last-bp zwei:*interval*))
	for line = (zwei:bp-line (zwei:interval-first-bp zwei:*interval*))
		 then (zwei:line-next-in-buffer line)
	do (remf (zwei:line-contents-plist line) 'zwei:lisp-parse-line)
	do (remf (zwei:line-contents-plist line) 'zwei:presentation-nodes)
	until (eq line last-line)))

;;; trouble note - when recompiling this guy, don't forget
;;; (send (zwei:mode-of-flavor 'joshua-mode) :clear-mode-forms-cache)

(scl:defmethod (:mode-forms joshua-mode) ()
  "Evaluate these forms when the mode is entered."
  ;; there's a wrapper in zwei:mode with :append combination, so these get added to lisp mode.
  '(;; list syntax -- like *cl-list-syntax-table*
    (zwei:set-syntax-table-indirection zwei:*mode-list-syntax-table*
				       *joshua-mode-list-syntax-table*)
    ;; word syntax in mode's word table -- like *word-syntax-table*
    (zwei:set-syntax-table-indirection zwei:*mode-word-syntax-table*
				       *joshua-mode-word-syntax-table*)
    ;; word syntax in atom-word table -- like *atom-word-syntax-table*
    (zwei:set-syntax-table-indirection zwei:*atom-word-syntax-table*
				       *joshua-mode-atom-word-syntax-table*)
    ;; also *pname-word-syntax-table*?
    ;; - note the progn is necessary for zwei:evaluate-forming-undo-list
    (progn (flush-cached-line-parse-info (zwei:mode-of-flavor 'joshua-mode)))))

;;; Center for the Abuse of Blinkers
;;; Minor buglet: ,@(foo) blinks the open-paren, not the comma.
(scl:defwhopper (:matching-char-to-blink joshua-mode) (bp start-limit-bp end-limit-bp)
  "Whop the method in lisp-syntax-mixin so that it hacks matching delimiters, strings & variables."
  ;; don't blink anything if you're at the beginning of the interval
  ;; this fixes a long-standing bug where (forward-char bp -1) sometimes returned nil!
  (unless (zwei:bp-= bp (zwei:interval-first-bp zwei:*interval*))
    ;; this is sort of like using a cannon against a flea.
    (cond ((and (= (zwei:list-syntax (zwei:bp-char-before bp)) zwei:list-double-quote)
		(scl:neq zwei:*interval* (zwei:window-interval zwei:*mini-buffer-window*))
		(multiple-value-bind (in-string slashified in-comment)
		    ;; get syntactic context of the string-former BEFORE bp
		    (zwei:lisp-bp-syntactic-context (zwei:forward-char bp -1.))
		  ;; test for real strings or for vertical-bars
		  (or (and in-string (not slashified) (not in-comment)	;real strings
			   *blink-strings-in-joshua-mode*)
		      (and (not in-string) (not slashified) (not in-comment)	;vertical-bars
			   (char-equal #\| (zwei:bp-char (zwei:forward-char bp -1.)))
			   *blink-vertical-bars-in-joshua-mode*))))
	   ;; if this is a double-quote (" or |)
	   ;; and this isn't the mini-buffer window
	   ;; and either
	   ;; it's in a string, not slashified, not commented, and string blinker on
	   ;; or
	   ;; it's in vertical-bars, not in a string, not slashified,
	   ;;    not commented, and vertical-bar blinker is on
	   (zwei:forward-sexp bp -1 nil 0 start-limit-bp))

	  ;; Clean these 2 bozos up, sgr!  There are really 3 cases each for variables and cells:
	  ;; foo -- cursor in an identifier;
	  ;;   
	  ;; foo -- cursor after an identifier;  \
	  ;;                                      | In both these cases, the cursor is on a delimiter
	  ;;  -- cursor after an anonymous        | after the thing we want to blink.
	  ;;                                     /
	  ;;
	  ((and *blink-variables-in-joshua-mode*
		(scl:neq zwei:*interval* (zwei:window-interval zwei:*mini-buffer-window*))
		(or (in-identifier-with-prefix-p bp *joshua-variable-char*)	;actually in ident
		    (in-identifier-with-prefix-p bp *joshua-variable-char*)
		    ;; check if after the prefix or after the identifier
		    (and (= (zwei:atom-word-syntax (zwei:bp-char bp)) zwei:word-delimiter)
			 (or (and (char-equal (zwei:bp-char-before bp) *joshua-variable-char*)
				  (zwei:forward-char bp -1.))
			     (let ((previous (zwei:forward-char bp -1.)))
			       (and previous
				    (in-identifier-with-prefix-p
				      previous *joshua-variable-char*)))))
#|                  (and (= (zwei:atom-word-syntax (zwei:bp-char bp)) zwei:word-delimiter)
			 (or (and (char-equal (zwei:bp-char-before bp) *joshua-alternative-variable-char*)
				  (zwei:forward-char bp -1.))
			     (let ((previous (zwei:forward-char bp -1.)))
			       (and previous
				    (in-identifier-with-prefix-p
				      previous *joshua-alternative-variable-char*)))))
|#
		    )))

	  (t
	    ;; it's something else.  let the system handle it.
	    (let ((bp-value (scl:continue-whopper bp start-limit-bp end-limit-bp)))
	      ;; bp-value is bp AT, not after the corresponding opening delimiter.
	      (cond ((null bp-value)
		     ;; system doesn't know either, so do nothing.
		     nil)
		    ((or (matching-joshua-delimiter-p (zwei:bp-char-before bp) (zwei:bp-char bp-value))
			 (matching-joshua-delimiter-p (zwei:bp-char bp-value) (zwei:bp-char bp)))
		     ;; delimiters match, so just check the blink switches
		     (if (blink-this-joshua-char-p (zwei:bp-char bp-value))
			 ;; blink switch is on
			 bp-value
			 ;; blink switch is off
			 nil))
		    ;; the delimiters do not match.
		    ((not (zwei:bp-= bp (scl:send zwei:*interval* :get 'matching-char-to-blink-error-bp)))
		     ;; Furthermore, this is not the same as the last such
		     ;; error.  So note this occurence...
		     ;; copy bp to keep it from tracking (point) -- thanks to sotko!
		     (scl:send zwei:*interval* :putprop (zwei:copy-bp bp) 'matching-char-to-blink-error-bp)
		     ;; ... and barf
		     (zwei:barf "~A does not match ~A"
				(zwei:bp-char bp-value)
				(zwei:bp-char-before bp)))))))))

;;; once upon a time there was a clause in the above to blink comments
;;; we now feel that comment blinking is not the purview of Joshua mode
;;; but that if someone wanted it they could make their own minor mode
;;; which whopped the above method even more.
;;; here it is preserved for posterity
;;;       ((and *blink-comments-in-joshua-mode*
;;;	     (neq zwei:*interval* (zwei:window-interval zwei:*mini-buffer-window*))
;;;	     (multiple-value-bind (ignore ignore in-comment)
;;;		 ;; get syntactic context of char AT bp
;;;		 (zwei:lisp-bp-syntactic-context bp)
;;;	       in-comment))
;;;	;; if comment blink switch is on and
;;;	;; if we're not in the minubuffer window, and
;;;	;; char AT bp is in a comment, then blink leftmost ; in this line.
;;;	(zwei:forward-char (zwei:beg-line bp) (zwei:find-comment-start (zwei:bp-line bp))))

(defun in-identifier-with-prefix-p (bp prefix-char &aux atom-begin)
  "Returns bp of prefix-char if in an identifier begining with prefix-char; otherwise nil"
  (and (= (zwei:atom-word-syntax (zwei:bp-char bp))        zwei:word-alphabetic)
       (= (zwei:atom-word-syntax (zwei:bp-char-before bp)) zwei:word-alphabetic)
       ;; if current bp is INSIDE an atom, go to front of word (NIL if at front of buffer)
       (setq atom-begin (zwei:forward-atom bp -1.))
       ;; does it begin with the right prefix?
       (char-equal (zwei:bp-char atom-begin) prefix-char)
       ;; if it begins with right char, then it's a variable
       atom-begin))

(defun matching-joshua-delimiter-p (closer opener)
  "non-NIL if closer & opener are matching delimiters in Joshua."
  ;; maybe this should be stashed in a readtable somehow? I don't understand readtables, anyway.
  ;; this shows the need in CL for something like select, that evals the keys
  (or						; any one of the following situations will suffice
    ;; matching predications
    (and (char-equal opener *joshua-open-predication-char*)
	 (char-equal closer *joshua-close-predication-char*))
    ;; matching parens (or any of the cruft we allow before parens)
    (and (member opener '(#\( #\' #\` #\, #\#) :test #'char-equal)
	 (char-equal closer #\)))
    ;; quote, backquote and comma can open any list-like structure
    (and (member opener '(#\' #\` #\,) :test #'char-equal)	;# wouldn't make sense here
	 (member closer *joshua-list-close-chars* :test #'char-equal))))

(defun blink-this-joshua-char-p (opener)
  "Whether or not blinking is enabled for this delimiter."
  (or (member opener '(#\( #\' #\` #\, #\# #\)) :test #'char-equal)	;these always blink (could make switches...)
      (symbol-value
	;; look up the opener, see if its blink switch is on.  Don't blink
	;; if it's off or not found.
	(cdr (assoc opener
		    '((*joshua-open-predication-char*     . *blink-predications-in-joshua-mode*)
		      (*joshua-variable-char*             . *blink-variables-in-joshua-mode*)
		      ;; (*joshua-alternative-variable-char* . *blink-variables-in-joshua-mode*)
		      (#\|                                . *blink-vertical-bars-in-joshua-mode*)
		      (#\"                                . *blink-strings-in-joshua-mode*)
		      (#\;                                . *blink-comments-in-joshua-mode*)
		      ;; KHS's reverse blinkers
		      (*joshua-close-predication-char* . *blink-predications-in-joshua-mode*))
		    :test #'(lambda (x y) (char-equal x (eval y))))))))

(defun (:property :joshua fs:syntax-attribute-handler) ()
  "Called when this file is hacked by the system, e.g., loading, suspend, compiling, etc."
  ;; returns a list of variables and a list of values to bind them to
  ;; cribbed from (:property :common-lisp fs:syntax-attribute-handler)
  (values (list 'fs:readtable '*readtable*)	;unclear which of these to use...
	  (list *joshua-readtable* *joshua-readtable*)))

(defun define-joshua-syntax ()
  "Define this syntax, flushing whatever randomness might already be there"
  (setq zwei:*lisp-syntax-alist* (delete-if #'(lambda (x) (string-equal (car x) "Joshua"))
					    zwei:*lisp-syntax-alist*))
  ;; I think what's going on here is that the third element is a minor mode that goes
  ;; into effect with this syntax.  Joshua is a major mode, so it can define its own blinkage.
  (push '("Joshua" :joshua nil *joshua-mode-list-syntax-table*)
	zwei:*lisp-syntax-alist*))

(define-joshua-syntax)

;;; This should be last, as it does a compile-flavor-methods of joshua-mode.
(zwei:defmode com-Joshua-mode joshua-mode "Editor mode for dealing with Joshua programs." :joshua)

1
0;;;
;;; Globally-available commands.  (Joshua-mode-only commands should be put
;;; in the :mode-forms method...)
;;;

(zwei:defcom com-create-initial-joshua-attribute-list "create an initial attribute list for Joshua buffers" ()
  (let ((start-bp (zwei:copy-bp (zwei:interval-first-bp zwei:*interval*))))
    ;; start-bp is now the bp of the top of the file
    (zwei:insert-moving start-bp
			;; insert the first line of the file
			(format nil
				"~&;;; -*- Mode: Joshua; Package: ~A; Syntax: Joshua~@[; Vsp: ~D~] -*-~%"
				*default-package-for-joshua-mode*
				*default-vsp-for-joshua-mode*))
    (zwei:insert-moving start-bp
			;; timestamp it
			(format nil ";;; Created ~A by ~A running on ~A at ~A.~2%"
				(time:print-universal-time (get-universal-time) nil)
				global:user-id
				(machine-instance)
				(short-site-name)))
    ;; reparse the attribute list so that this will take effect
    (zwei:com-reparse-attribute-list)
    ;; make the comments we just put in in style 0
    (zwei:change-style-interval (zwei:copy-bp (zwei:interval-first-bp zwei:*interval*)) start-bp t
				(si:parse-character-style '(nil nil nil))))
  ;; do redisplay
  zwei:dis-text)

;;; Install this in the standard command table, so people can enter Joshua mode from anywhere.
(zwei:set-comtab zwei:*zmacs-comtab*
		 nil
		 (zwei:make-command-alist '(com-create-initial-joshua-attribute-list com-joshua-mode)))
