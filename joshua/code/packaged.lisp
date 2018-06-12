;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: cl-user -*-
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
;;;> Wheels, Dynamic Windows (R), Showcase, SmartStore (R), Semanticue (R), Frame-Up (R),
;;;> Firewall (R), MACSYMA (R), COMMON LISP MACSYMA, CL-MACSYMA (R), LISP MACHINE
;;;> MACSYMA (R), MACSYMA Newsletter (R), Document Examiner (R), S-DYNAMICS (R),
;;;> S-GEOMETRY (R), S-PAINT (R), S-RENDER (R), "Your Next Step in Computing" (R), Ivory,
;;;> Symbolics C, Symbolics Pascal, Symbolics Prolog, Symbolics Fortran, CLOE, Joshua,
;;;> Concordia, and Statice are trademarks of Symbolics, Inc.
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

#-Genera
(in-package #+MCL "COMMON-LISP-USER"
            #+(or allegro sbcl) :common-lisp-user
	    #+LUCID "USER"
	    #+CLOE "CLOE")
;;;
;;; Package definitions for Joshua. This stuff is not in the defsystem
;;; file so that it's patchable.  E.g., we might want to export a new
;;; symbol in a patch.  (Also due to make-system brain-death in always loading
;;; the latest version of the defsystem file.)
;;;
;;; The symbols fall into 3 classes:
;;;
;;; * Undocumented internal symbols.
;;;   Users shouldn't ever have to refer to these.
;;;   If they do, either they're doing something wrong or  we forgot to export something.
;;;   These are internal symbols of JI.

;;; * Potentially-dangerous low-level things:
;;;   We'd rather users not mess with these, but it's ok if they know what they're doing.
;;;   These are external symbols exported from JI.

;;; * Documented user-available things in Joshua.
;;;   These are in the JOSHUA package.

;;; Here's how that is implemented.  There are 3 packages: Joshua-Internals, Joshua, 
;;; and Joshua-User.  Implementation is done in Joshua-Internals which uses Joshua.
;;; So symbols in category 3 are simply defined in Joshua and put into the export-list of Joshua.
;;; User programs should be written in packages which use Joshua and either CL or SCL.
;;; Joshua-User is an example of such a package.  
;;; Such packages inherit all the category 3 symbols, making them accessible without prefix.
;;; Category 2 symbols are put in the export list of JI.  This makes them accessible as JI:foobar.

;;; Implementors note:  If you want to "promote" a symbol from category 1 (protected internals)
;;; to category 2 (an accessible internal).
;;; then you add it to the :EXPORT list
;;; of JI (but don't patch the defpackage).   
;;; Then put (export '(JI::foo)) in the patch.  Note that you must not do this in your
;;; running  world before you make the patch, or the patch's bin file
;;; will get ji:foo as opposed to ji::foo  dumped to it.  You could
;;; unexport before making the patch, I suppose. Or make the patch say
;;; (export (list (intern "FOO" 'ji))).

;;;   If you want to promote it to category 2 (user accessible) add it to the :EXPORT
;;;   list of JOSHUA (but don't patch the defpackage).  Then add
;;;   (globalize 'ji::foo "JOSHUA") to the patch.  Be sure the first arg
;;;   to globalize is the symbol, not a string, so eq-ness will be
;;;   preserved.  Note that you must not do this in your running world
;;;   before you make the patch, or the patch's bin file will get
;;;   joshua:foo as opposed to ji::foo dumped to it.
;;;   (globalize (intern "FOO" 'ji) "JOSHUA") might be safer.

(defpackage joshua
  ;; user-visible Joshua stuff goes here
  ;; code shouldn't be written in this package
  ;; "Be my guest"
  #+mcl (:shadow "CLEAR" "CCL")
  #+lucid (:shadow using-resource clear-resource)
  #+allegro (:import-from excl with-stack-list with-stack-list*)
  #+(or genera cloe-developer) (:import-from sys with-stack-list with-stack-list* stack-let)
  (:export
   "JOSHUA-MODULE" "SEPARATE-DESTINATION-JOSHUA-MODULE"
    "ENABLE-JOSHUA" "DISABLE-JOSHUA"
    "SELF"
    "DEF-DEFINING-FORM" "UNDEF-DEFINING-FORM"
    ;;
    "OF"
    "LOGIC-VARIABLE-NAME" "JOSHUA-LOGIC-VARIABLE-VALUE"
    "WITH-UNBOUND-LOGIC-VARIABLES" "BINDING-LVS"
    "UNBOUND-LOGIC-VARIABLE"			;the deftype
    "UNBOUND-LOGIC-VARIABLE-P"		;The Test
    "NOT-BEEN-IN-BEFORE-P" "BEEN-IN-BEFORE-P"
    ;; predicate stuff
    "PREDICATION" "PREDICATIONP" "MAKE-PREDICATION"
    "DEFINE-PREDICATE-MODEL" "UNDEFINE-PREDICATE-MODEL" "DEFINE-PREDICATE" "UNDEFINE-PREDICATE"
    "DEFAULT-PREDICATE-MODEL" "DEFAULT-PROTOCOL-IMPLEMENTATION-MODEL"
    "DEFAULT-ASK-MODEL" "DEFAULT-TELL-MODEL"
    "DEFAULT-RULE-COMPILATION-MODEL"
    "WITH-STATEMENT-DESTRUCTURED" "PREDICATION-PREDICATE" "PREDICATION-STATEMENT"
    "PREDICATION-MAKER" "PREDICATION-MAKER-STATEMENT" "PREDICATION-MAKER-PREDICATE"
    "WITH-PREDICATION-MAKER-DESTRUCTURED" "PREDICATION-MAKER-P"
    "LOGIC-VARIABLE-MAKER" "LOGIC-VARIABLE-MAKER-NAME" "LOGIC-VARIABLE-MAKER-P"
    "KNOWN" "PROVABLE"
    ;; how to define elements of the protocol
    "UNDEFINE-PREDICATE-METHOD" "DEFINE-PREDICATE-METHOD" 
    ;; the joshua protocol
    "SAY" 
    "ASK" "TELL" "UNTELL"
    "ASK-DATA" "ASK-RULES" "ASK-QUESTIONS"
    "CLEAR" "INSERT" "FETCH" "UNINSERT"
    "JUSTIFY" "NOTICE-TRUTH-VALUE-CHANGE" "ACT-ON-TRUTH-VALUE-CHANGE" "UNJUSTIFY" 
    "SUPPORT" "CONSEQUENCES" "FIND-INDEPENDENT-SUPPORT"
    "CURRENT-JUSTIFICATION" "ALL-JUSTIFICATIONS"
    "BUILD-JUSTIFICATION-FROM-BACKWARD-SUPPORT"
    "NONTRIVIAL-TMS-P"
    "PREFETCH-FORWARD-RULE-MATCHES"
    "MAP-OVER-FORWARD-RULE-TRIGGERS" "MAP-OVER-BACKWARD-RULE-TRIGGERS" "MAP-OVER-BACKWARD-QUESTION-TRIGGERS"
    "ADD-FORWARD-RULE-TRIGGER" "ADD-BACKWARD-RULE-TRIGGER" "ADD-BACKWARD-QUESTION-TRIGGER" 
    "DELETE-FORWARD-RULE-TRIGGER" "DELETE-BACKWARD-RULE-TRIGGER" "DELETE-BACKWARD-QUESTION-TRIGGER"
    "LOCATE-FORWARD-RULE-TRIGGER" "LOCATE-BACKWARD-RULE-TRIGGER" "LOCATE-BACKWARD-QUESTION-TRIGGER"
    "WRITE-FORWARD-RULE-FULL-MATCHER" "WRITE-FORWARD-RULE-SEMI-MATCHER"
    "COMPILE-FORWARD-RULE-ACTION" "EXPAND-FORWARD-RULE-TRIGGER"
    "WRITE-BACKWARD-RULE-MATCHER" "EXPAND-BACKWARD-RULE-TRIGGER" "EXPAND-BACKWARD-RULE-ACTION"
    "POSITIONS-FORWARD-RULE-MATCHER-CAN-SKIP"
    ;; iterative syntax for the above mapcar-like versions
    "DO-QUERIES*" "DO-QUERIES-NAMED*" 
    ;; ask without the justification and the lambda form
    "ASK*"
    ;; discrimination net stuff
    "DISCRIMINATION-NET-FETCH" "DISCRIMINATION-NET-INSERT" "DISCRIMINATION-NET-CLEAR"
    "DISCRIMINATION-NET-UNINSERT"
    "MAKE-DISCRIMINATION-NET-NODE"
    ;; predicate models
    "DISCRIMINATION-NET-DATA-MIXIN"
    "ASK-DATA-ONLY-MIXIN" "ASK-RULES-ONLY-MIXIN" "ASK-QUESTIONS-ONLY-MIXIN"
    "ASK-DATA-AND-RULES-ONLY-MIXIN" "ASK-RULES-AND-QUESTIONS-ONLY-MIXIN"
    "ASK-DATA-AND-QUESTIONS-ONLY-MIXIN"
    "NO-VARIABLES-IN-DATA-MIXIN"
    "TELL-ERROR-MODEL" "ASK-ERROR-MODEL" "ERROR-MODEL" "FALSE-QUERY-ERROR-MODEL" "UNFICATION-MODEL"
    "BASIC-TMS-MIXIN"
    "LOCATION-VALUE-MODEL" "LOCATE-PREDICATION-VALUE"
    ;; keywords to ask & tell, so define-*-method can grok user code
    "JUSTIFICATION" "DO-BACKWARD-RULES" "QUERY-CONTEXT"
    ;; unification stuff
    "WITH-UNIFICATION" "UNIFY" "VARIANT"
    ;; rules
    "DEFRULE" "UNDEFRULE"
    "*UNKNOWN*" "*TRUE*" "*FALSE*" "*CONTRADICTORY*" "NEGATE-TRUTH-VALUE"
    "PREDICATION-TRUTH-VALUE" "DIFFERENT-OBJECTS" "SUCCEED"
    ;; questions
    "DEFQUESTION" "UNDEFQUESTION"
    ;; tms interface
    "TRIGGERS" "TMS-CONTRADICTION" "TMS-CONTRADICTION-HARD-CONTRADICTION-FLAVOR"
    "TMS-HARD-CONTRADICTION" 
    "TMS-CONTRADICTION-JUSTIFICATION" "TMS-CONTRADICTION-CONTRADICTORY-PREDICATION"
    "TMS-CONTRADICTION-PREMISES" "TMS-CONTRADICTION-NON-PREMISES" "TMS-CONTRADICTION-SUPPORT"
    "TMS-BITS"
    "DESTRUCTURE-JUSTIFICATION" "REMOVE-JUSTIFICATION"
    "EXPLAIN"
    "SUPPORT-WITH-NAME" "ASSUMPTION-SUPPORT" "PREMISE-SUPPORT"
    ;; graphing things
    "GRAPH-RETE-NETWORK" "GRAPH-DISCRIMINATION-NET" "GRAPH-TMS-SUPPORT" "GRAPH-ASSUMPTION-SUPPORT"
    ;; ask continuation functions
    "PRINT-QUERY" "SAY-QUERY" "PRINT-QUERY-RESULTS" "GRAPH-QUERY-RESULTS"
    "ASK-QUERY" "ASK-QUERY-TRUTH-VALUE" "ASK-DATABASE-PREDICATION" "ASK-DERIVATION"
    "MAP-OVER-DATABASE-PREDICATIONS"
    "RULE" "QUESTION"
    "DATABASE-PREDICATION"			;PRESENTATION TYPE
    "TRUTH-VALUE"
    "COPY-OBJECT-IF-NECESSARY"
    ;; for the object-modelling feature, the predicate-model mixins
    "SLOT-VALUE-MIXIN" "TYPE-OF-MIXIN" "PART-OF-MIXIN" "EQUATED-MIXIN"
    ;; ... the predicates
    "VALUE-OF" "OBJECT-TYPE-OF" "PART-OF" "NAMED-PART-OF" "EQUATED" "PARENT-OF"
    ;; ... and useful functions
    "ADD-ACTION" "REMOVE-ACTION"
    "MAP-OVER-OBJECT-HIERARCHY"
    "ALL-SLOT-NAMES" "MAP-OVER-SLOTS-OF-OBJECT" "MAP-OVER-SLOTS-IN-OBJECT-HIERARCHY"
    "SUPERPART"
    "FOLLOW-PATH" "PATH-NAME" "ROLE-NAME"
    "MAKE-OBJECT" "DEFINE-OBJECT-TYPE" "KILL"
    "SLOT-CURRENT-PREDICATION" "SLOT-CURRENT-VALUE" "SLOT-MY-OBJECT"
    "NOTICE-SLOT-CHANGE" "SLOT-IS-EMPTY-P"
    ;; if you want to use :after methods on intialize-instance of an object type
    ;; you have to know is you're running this for the prototype-builder or for a real
    ;; object builder.  This switch is the one.
    *building-prototype*
    ;; the atomic-and features
    "ATOMIC-AND-MODEL" "ATOMIC-AND" "WITH-ATOMIC-ACTION"
    ;; slot presentations and commands
    "SLOT-PRESENTATION" "COM-CHANGE-SLOT-VALUE" "COM-SET-SLOT-VALUE"
    "OBJECT-PRESENTATION" "COM-DESCRIBE-JOSHUA-OBJECT"
    "MAKE-HEAP" "HEAP-EMPTY-P" "HEAP-INSERT" "HEAP-TOP" "HEAP-REMOVE"
    "HEAP-ELEMENTS" "HEAP-ACTIVE-SIZE"
    "DELETE-BY-ITEM" "DELETE-BY-KEY" "FIND-BY-ITEM" "FIND-BY-KEY"
    "WITH-AUTOMATIC-UNJUSTIFICATION"
     WITH-STACK-LIST
     WITH-STACK-LIST*
     STACK-LET SHOW-JOSHUA-DATABASE
     "RULE-CERTAINTY-FACTOR"
     "CF-MIXIN" "CF-PREDICATE-MODEL"
     "CERTAINTY-FACTOR"

    )
  )

#+allegro (eval-when (:compile-toplevel :load-toplevel :execute) (require :cltl1))

(defpackage joshua-internals
  ;; Joshua developers write code here
  (:nicknames :ji)
  #+lucid (:shadowing-import-from "JOSHUA" "USING-RESOURCE" "CLEAR-RESOURCE")
  #+mcl (:shadowing-import-from "JOSHUA" "CLEAR")
  #+mcl (:shadowing-import-from "CCL" "ARGLIST")
  #+mcl (:import-from ccl class-class-slots class-instance-slots compiler-let slot-definition-name
                      class-direct-subclasses
                      class-precedence-list function-name generic-function-methods method-specializers)
  #+genera (:import-from "SYS" "FUNCTION-NAME")
  #+genera (:import-from "LISP" "DEFINE-SETF-METHOD" "GET-SETF-METHOD" "COMPILER-LET" )
  #+genera (:import-from clos class-slots slot-definition-name class-precedence-list 
			 generic-function-methods method-specializers class-direct-subclasses)
  #+genera (:import-from scl arglist)
  #+(or genera cloe-developer) (:import-from scl record-source-file-name)
  #+cloe-runtime (:import-from si with-stack-list with-stack-list* stack-let)
  #+allegro (:import-from cltl1 get-setf-method define-setf-method)
  #+allegro (:import-from clos slot-definition-name class-precedence-list)
  #+allegro (:import-from mop method-specializers generic-function-methods class-direct-subclasses)
  #+allegro (:import-from excl compiler-let funwrap fwrap arglist def-fwrapper call-next-fwrapper)
  (:use :joshua
         #+genera "CL"
         #+cloe "CLOE"
	 #+mcl "COMMON-LISP"
	 #+lucid "LISP" #+lucid "LUCID-COMMON-LISP" #+lucid "CLOS"
	 #+allegro :common-lisp)
  ;; "Well, if you insist"
  (:export
    "*BLINK-PREDICATIONS-IN-JOSHUA-MODE*"   "*BLINK-SETS-IN-JOSHUA-MODE*"
    "*BLINK-VARIABLES-IN-JOSHUA-MODE*"      "*BLINK-CELL-REFERENCES-IN-JOSHUA-MODE*"
    "*BLINK-VERTICAL-BARS-IN-JOSHUA-MODE*"  "*BLINK-STRINGS-IN-JOSHUA-MODE*"
    "*BLINK-COMMENTS-IN-JOSHUA-MODE*"
    "*SUPPORT*" "*RUNNING-RULE*"
    ;; backquoting machinery
    "BACKQUOTE" "*BACKQUOTE-COMMA-FLAG*" "*BACKQUOTE-COMMA-ATSIGN-FLAG*" "*BACKQUOTE-COMMA-DOT-FLAG*"
    ;; defaults to go into the attr list made by m-x create initial joshua attribute list
    "*DEFAULT-PACKAGE-FOR-JOSHUA-MODE*"     "*DEFAULT-LOWERCASE-FOR-JOSHUA-MODE*"
    "*DEFAULT-BASE-FOR-JOSHUA-MODE*"        
    ;; readtable management
    ;; maybe define a mode on top of this?
    "JOSHUA-MODE"
    "WITH-JOSHUA-READTABLE"
    "*DATA-DISCRIMINATION-NET*"
    "*FORWARD-TRIGGER-DISCRIMINATION-NET*"
    "*BACKWARD-TRIGGER-DISCRIMINATION-NET*"
    "*QUESTION-DISCRIMINATION-NET*"
    "VARIABLE-PREDICATE-MODEL" "VARIABLE-PREDICATE"
    ;; rules
    "MAKE-UNBOUND-LOGIC-VARIABLE"
    "CLEAR-RULE"
    "PREDICATE-SYNONYMS"
    "PREDICATE-IS-SYNONYM-FOR" "UNDEFINE-PREDICATE-SYNONYM" "DEFINE-PREDICATE-SYNONYM" 
    "PREDICATE-MAX-ARGS" "PREDICATE-MIN-ARGS" "PREDICATE-REST-ARG" "PREDICATE-ARGLIST" 
    ;; iv's of a contradiction flavor; used by tms writers
    "STATEMENT" "BITS" "PREDICATION-BITS"
    "PREMISES" "JUSTIFICATION" "CONTRADICTORY-PREDICATION" "NON-PREMISES"
    "PREDICATION-BITS-HAS-BEEN-IN-DATABASE"
    "PREDICATION-BITS-TRUTH-VALUE" "PREDICATION-BITS-HAS-LOGIC-VARIABLES"
    "PREDICATION-BITS-IVE-BEEN-IN-BEFORE" "PREDICATION-BITS-TMS-BITS"
    "PREDICATION-BITS-IVE-BEEN-UNTOLD"
    "PREDICATION-GENERATION-MARK" "GENERATION-MARK" "*GENERATION-COUNTER*"
    "ASK-INTERNAL" "TELL-INTERNAL"
    "PRINT-DATABASE-PREDICATION-WITHOUT-TRUTH-VALUE"
    ;; meters
    "*MATCH-COUNT*" "*MERGE-COUNT*" "*FORWARD-FIRE-COUNT*" "*BACKWARD-FIRE-COUNT*"
    "*SUCCESSFUL-MATCH-COUNT*" "*SUCCESSFUL-MERGE-COUNT*" "CLEAR-METERS"
    "ASK-QUESTION"
    ;; condition names for modelling failures
    "MODEL-CANT-HANDLE-QUERY" "MODEL-CANT-HANDLE-QUERY-QUERY" "MODEL-CANT-HANDLE-QUERY-MODEL"
    "MODEL-CAN-ONLY-HANDLE-POSITIVE-QUERIES"
    "MODEL-CAN-ONLY-HANDLE-POSITIVE-QUERIES-QUERY"
    "MODEL-CAN-ONLY-HANDLE-POSITIVE-QUERIES-MODEL"
    "SETF-CAREFULLY"
    "UPDATE-AFTER-COPYING"			;METHOD TO UPDATE A PREDICATION AFTER COPY-OBJECT-IF-NECESSARY
    "SORT-BY-TRUTH-VALUE"
    "RULE-CERTAINTY-FACTOR"
    "CF-MIXIN" "CF-PREDICATE-MODEL"
    "CERTAINTY-FACTOR"
    ;; Copy protocol
    "INIT-PLIST"
    "RETE-NETWORK-RETRACT-PREDICATION" "STIMULATE"
    "BETTER-VERSIONS"
    ))

(defpackage joshua-user
  ;; A typical Joshua application package.
  ;; Applications should imitate this to get their own packages.
  (:nicknames :ju)
  ;; (:relative-names-for-me (joshua user)) ;make user a synonym for ju relative to joshua 
  #+mcl (:shadowing-import-from "JOSHUA" "CLEAR")
  #+lucid (:shadowing-import-from "JOSHUA" "USING-RESOURCE" "CLEAR-RESOURCE")
  (:use :joshua
         #+genera "CL"
         #+cloe "CLOE"
	 #+mcl "COMMON-LISP"
	 #+allegro :common-lisp
	 #+lucid "LISP" #+lucid "LUCID-COMMON-LISP" #+lucid "CLOS"))

(defpackage ltms
  ;; package used by the LTMS; other TMS writers make their own.
  #+mcl (:shadowing-import-from "JOSHUA" "CLEAR")
  #+lucid (:shadowing-import-from "JOSHUA" "USING-RESOURCE" "CLEAR-RESOURCE")
  (:use :joshua :ji
         #+genera "CL"
         #+cloe "CLOE"
	 #+mcl "COMMON-LISP"
	 #+allegro :common-lisp
	 #+lucid "LISP" #+lucid "LUCID-COMMON-LISP" #+lucid "CLOS")
  ;; This is for the object-modelling Stuff
  (:shadow "VALUE-OF" "OBJECT-TYPE-OF" "PART-OF" "NAMED-PART-OF" "EQUATED")
  (:export
   "ASSUME" "ASSUMPTION-FOR"
   "LTMS-MIXIN" "LTMS-PREDICATE-MODEL" "ONE-OF" "CONTRADICTION"
   "LTMS-CONTRADICTION" "LTMS-HARD-CONTRACTION"
   "BACKTRACK" "NOGOOD"
   "CONSEQUENCES"
   ;; this is for the object-modelling stuff
   "VALUE-OF" "OBJECT-TYPE-OF"  "PART-OF" "NAMED-PART-OF" "CONNECTED-TO" "EQUATED"))

(defpackage congruence-closure
  (:nicknames "CC")
  (:use :joshua :joshua-internals :common-lisp)
  ;; This is for the object-modelling Stuff
  (:shadow common-lisp:find common-lisp:union common-lisp:intern)
  (:export 
   "MERGE" "CONGRUENT" "FIND" "EQUATE" "UNION"
   "INIT" "INTERN-TERM" "INTERN" "INTERN-SYMBOL"
   "ANONYMOUS-INDIVIDUAL" "TERM-SURROGATE" "CONSTANT"
   ))

(defpackage jlt
  (:use #+cloe "CLOE"
	#+genera "CL"
	#+mcl "COMMON-LISP"
	#+allegro :common-lisp
	#+lucid "LISP" #+lucid "LUCID-COMMON-LISP" #+lucid "CLOS")
  #+(or mcl lucid allegro) (:import-from joshua WITH-STACK-LIST WITH-STACK-LIST* STACK-LET)
  #+mcl (:import-from CCL compiler-let)
  #+lucid (:Import-from LOOP LOOP-FINISH)
  #+(or genera cloe-developer) (:import-from SYS WITH-STACK-LIST WITH-STACK-LIST* STACK-LET)
  #+genera (:import-from lisp compiler-let)
  #+allegro (:import-from excl compiler-let)
  ;; In the Genera world, we use Genera's lt templates as well as our
  ;; own.  This makes the symbols in templates be the same in both LT
  ;; and JLT, except for a couple which are also Symbolics Common Lisp
  ;; functions different from the Ansi CL functions (e.g. function if).
  (:export
   "MAPFORMS" "COPYFORMS" "MAPFORMS-1" "COPYFORMS-1"
   "DEFPROPFUN"
   "USED-VARIABLES"
   "EFFECT" "SMASH" "PROP" "TEST" "SYMEVAL"
   "START" "TAG" "BODY" "COMPLETION"
   "BODY-START" "BODY-END" 
   "INTERNAL-FUNCTION-START" "INTERNAL-FUNCTION-END" "INTERNAL-FUNCTION-DEFINITION"
    *MAPFORMS-BOUND-VARIABLES* *MAPFORMS-BLOCK-NAMES* *MAPFORMS-GO-TAGS* *MAPFORMS-LEVEL*
    *MAPFORMS-NON-FORM-KINDS* *MAPFORMS-LOCATOR-START* *MAPFORMS-LOCATOR-END*
    *MAPFORMS-BLOCK-ALIST* *MAPFORMS-FUNCTION* *MAPFORMS-STATE*
    *MAPFORMS-LEXICAL-FUNCTION-ENVIRONMENT* 
    env-variables env-functions env-blocks env-tagbodies env-declarations env-evacuation env-parent
    *COPYFORMS-FLAG*
    *MAPFORMS-APPLY-FUNCTION* *MAPFORMS-ITERATION-HOOK* *MAPFORMS-EXPAND-SUBSTS*
    *MAPFORMS-PARALLEL-BINDS* *COPYFORMS-EXPAND-ALL-MACROS*
    
    )
  )
