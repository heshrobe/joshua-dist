;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: Cl-user -*-

#-genera
(in-package #+MCL "COMMON-LISP-USER"
	    #+allegro "COMMON-LISP-USER"
	    #+LUCID "USER")

(defvar *cloe-clos-package* (find-package "CLOS"))

(defvar *CLOE-PACKAGE* (find-package "CLOE"))

#+cloe-developer
(cloe-si::make-package-available-internal "CLIM")

#+cloe
(defun customize-cloe-world ()
  ;; Get the CLOS symbols into CLOE as exported symbols.
  (when (and *cloe-clos-package* *cloe-package*)
    (do-external-symbols
      (s (find-package "CLOS"))
      (shadowing-import s *CLOE-PACKAGE*)
      (export s *CLOE-PACKAGE*)))
  ;; In the Cloe developer, SETF is not the CLOS setf.  This doesn't matter
  ;; except in defining setf methods explicitly, which I do in LTMS.  To avoid
  ;; needing to do (defmethod (clos:setf foo) ...) I'm doing this.
  ;; I doubt this is needed in the Real Cloe.
  #+cloe-developer
  (let ((setf-symbol (find-symbol "SETF" (find-package "clos"))))
    (shadowing-import setf-symbol *CLOE-PACKAGE*)
    (export setf-symbol *CLOE-PACKAGE*))
  )

#+cloe
(eval-when (load eval) 
  (customize-cloe-world))

#| 

Issues discovered going to MCL:

With-stack-list and with-stack-list* are not available in this lisp to my
knowledge.  Use dynamic-extent declaration. The dummy definition needed to be exported
from the joshua package and then imported into the jlt package.

Also added with-stack-list.

Compiler-let was in the ccl package.  This needed to be imported into
the ji and jlt packages.

All the packages needed to Use Common-Lisp.  This was only conditionalized for 
cloe before hand.

CLOS is part of the common lisp package and so the stuff at the top of this file
is irrelevant.

Once-only was not available.  This was used by with-make-predication-form-parsed. 
Also used in the rule compiler.  I replaced by equivalent code.
I conditionalized this to not used once-only if not cloe or genera.

(declare (values ...)) doesn't exist in MCL. (Proclaim '(declaration values))
seems to fix this.

(in-package ) now takes either an unquoted symbol or a string as an argument
I'm changing it to strings everywhere.

In predicat: Class-slots and slot-definition-name seems to be in ccl.
I imported both these into JI.

(declare (sys:downward-function)) needs to go to dynamic-extent from predimpl on

(declare ) loses in defgeneric.  This was macroexpanded into by define-protocol function.
I fixed it to expand only if there's something there.

clear is part of the ccl kernal.  Needed to shadow it.

NEQ isn't common lisp, I've replaced those.

Home for wayward variables should be nuked.  A better version of the whole idea
is in the code I hacked for the joshua paper.  Bring home a version.


|#