;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: Ideal;  Base: 10 -*-

(in-package :ideal)



;;;;********************************************************
;;;;  Copyright (c) 1989, 1992 Rockwell International -- All rights reserved.
;;;;  Rockwell International Science Center Palo Alto Lab
;;;;********************************************************



;;;;;;;;;;;;;;;;;;;;;;;; Sampath ;;;;;;;;;;;;;;;;;;;;


(export '( *DISPLAY-PRECISION*
	  *DEFAULT-DIAG-PATH-NAME*
	  *IDEAL-DEBUG*
	  *DIAGRAM-FORMAT-CHANGE-DATE*
	  *DIAGRAM*))

;--------- Some implementation specific compiler declarations---------

; In Mac Allegro CL V 1.3, mapc and mapl should not be compiled in line since
; there is a broken compiler transformation.

#+:CCL-1.3
(proclaim '(notinline mapc))
#+:CCL-1.3
(proclaim '(notinline mapl))

; All the other defvars are not needed outside the IDEAL package.
;-------------------------------------------------------------------------------------

; All the defvars are here coz super thick Allegro CL does not realize
; that when u put a defvar at the head of a file, the variable may be
; used in a file. It compiles the code and then loads it, with the
; result that when compiling code further down the file it breaks coz it
; does not yet know about the defvar.  So I am putting the defvars such
; that they are loaded before the files that use them are compiled.
;------------------------------------------------------------------------------------

; Default path name with which pathnames are merged by load-diagram and
; save-diagram.

; For the Allegro version there must be a definition for the logical
; pathname DIAGRAMS in the Allegro init file or some other appropriate
; place.

(defparameter *default-diag-path-name*
	#+LISPM "ideal:diagrams;.diag"
	#-lispm (cl-user::translate-logical-pathname "ideal:diagrams;")
;;	#+:CORAL "DIAGRAMS;.diag"
;;	#+(not (or :CORAL :LISPM)) (make-pathname :type "diag")
	)

; The macro store-ideal-struct-info automatically pushes new types on this variable.

(defvar *ideal-structure-types* nil "A list of structure type names whose used in IDEAL")

; This variable is used by copy-diagram and during file-io.  It's actual
; binding is irrelvant as it is always locally bound before use.

(defvar *non-circular-copy* nil)

; When this is bound to a non-nil value many functions print a trace of what they
; are doing.

(defvar *ideal-debug* nil)

(defvar *diagram* nil "If a diagram is an optional argument to the fn, this is the default")

; This variable is there basically for style. It is the lowest possible
; label-id-number, i.e all label-id-number are > or =
; *lowest-label-id-number*.  If it is greater than zero then probability
; arrays are oversized and therefore inefficient.  (see
; make-probability-array).  This was made a variable so that the lowest
; label-id-number is explicit instead of being hardcoded in all over the
; place.

(defvar *lowest-label-id-number* 0 "The lowest possible label id number")

; This variable is put in for style. I could have hardcoded the node lowest index to
; anything coz only relative values of this index matter.

; This variable is used in a very low level and important function :-
; get-key-for-conditioning-case. The *node-lowest-index* used to mean
; the lowest index that could be assigned to a node. Semantics changed
; now. The variable is assigned -1 to keep compatibility with old
; diagrams (created before this change). Could be anything actually.

(defvar *node-lowest-index* -1 "All nodes are assigned indexes > than this variable")

; Precision of displayed probabilities, used in display functions.

(defvar *display-precision* 10)

; When this variable is bound to nil, structures with print functions
; which are defined by the macro defidealprintfn are printed in default
; lisp readable format.  See the file store-ideal-struct-info.lisp for
; details

(defvar *default-ideal-structure-printing* nil)

; This symbol is written into files as a file type identifier. Later on
; if some implementation detail of diagram files changes then we will be
; able to change this var. That will allow us to distinguish between the
; two types of diagram formats.

(defvar *current-diagram-format* :VERSION-2 )

; Recursive-array-copy has to be behave differently when it is operating
; on a array in an old-format-diagram. This happens when it is called
; under the fn convert-diagram-format. *old-format-diagram* is just a
; flag to recursive-array-copy. Passing an argument down will
; unneccesarily make the rest of the code contain this hack.

(defvar *old-format-diagram* nil)

; Used by get initialized-diagram in consistency-checking.lisp

(defvar *algorithm-types*
	'(:LW :CLUSTERING :CONDITIONING :POLY-TREE :SIMULATION :JENSEN)	
  "The various possible IDEAL algorithms")


; Used by fn order-for-clustering in join-tree.lisp

(defvar *jensen-search* nil "If this variable is non-nil then the triangulation scheme
                             used is the jensen scheme")


; List of structure types that cannot be copied recursively.
; When copying a diagram, when one of these is encountered the
; copier silently just returns nil instead of attempting to
; recursively copy the object. The idea, for example, is
; that when some algorithm specific data structures are
; left on the diagram, copy-diagram does not break, and does *not*
; make copies of these structures onto the new diagram.
; For example, internal-pt structures in poly-tree-infer, which
; are stored in the actual-bel slots of nodes.

(defvar *non-copiable-structure-types* '(INTERNAL-PT COND-DS))


