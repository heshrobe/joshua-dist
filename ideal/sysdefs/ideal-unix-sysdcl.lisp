;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: COMMON-LISP-USER -*-

(in-package :cl-user)

;;;;********************************************************
;;;;  Copyright (c) 1989, 1992 Rockwell International -- All rights reserved.
;;;;  Rockwell International Science Center Palo Alto Lab
;;;;********************************************************

#|
File:      ideal-unix-sysdcl.lisp
Description:
  System file for IDEAL - unix systems

Notes:
|#

(unless (find-package :ideal)
  (make-package :ideal :use '(:common-lisp) :nicknames '())
  )

(defparameter *ideal-src-directory*
  (translate-logical-pathname "ideal:code;"))
(defparameter *ideal-bin-directory*
  (translate-logical-pathname "ideal:code;"))

#+mcl
(clim-defsys:defsystem ideal
  (:default-pathname *ideal-src-directory*
      :default-binary-pathname *ideal-bin-directory*
      :needed-systems ()
      :load-before-compile ())
  ("ideal-package-name" :language :lisp-unix)
  ("global-vars" :language :lisp-unix)
  ("store-ideal-struct-info" :language :lisp-unix)
  ("struct" :language :lisp-unix)
  ("wrapped-access-functions" :language :lisp-unix)
  ("low-level-array-ops" :language :lisp-unix)
  ("probability-access-fns" :language :lisp-unix)
  ("utils-macros" :language :lisp-unix)
  ("utils" :language :lisp-unix)
  ("display-and-query-fns" :language :lisp-unix)
  ("ordering" :language :lisp-unix)
  ("consistency-checking" :language :lisp-unix)
  ("noisy-or-nodes" :language :lisp-unix)
  ("copy-diagram-macros" :language :lisp-unix)
  ("copy-diagram" :language :lisp-unix)
  ("file-io" :language :lisp-unix)
  ("edit-functions-macros" :language :lisp-unix)
  ("primitives" :language :lisp-unix)
  ("id-creation" :language :lisp-unix)
  ("edit-functions" :language :lisp-unix)
  ("algorithms" :language :lisp-unix)
  ("message-processing" :language :lisp-unix)
  ("evidence+dummy-nodes" :language :lisp-unix)
  ("poly-tree-utils" :language :lisp-unix)
  ("clique-struct" :language :lisp-unix)
  ("clique-macros" :language :lisp-unix)
  ("clique-node-manipulation" :language :lisp-unix)
  ("join-tree" :language :lisp-unix)
  ("poly-tree" :language :lisp-unix)
  ("clustering-algorithm" :language :lisp-unix)
  ("clustering-est" :language :lisp-unix)
  ("conditioning-utils" :language :lisp-unix)
  ("conditioning" :language :lisp-unix)
  ("lw-utils" :language :lisp-unix)
  ("lw-infer" :language :lisp-unix)
  ("pearl-simulation-utils" :language :lisp-unix)
  ("pearl-simulation" :language :lisp-unix)
  ("jensen-join-tree" :language :lisp-unix)
  ("jensen-search" :language :lisp-unix)
  ("jensen-infer" :language :lisp-unix)
  ("jensen-est" :language :lisp-unix)
  ("peot-simulation-macros" :language :lisp-unix)
  ("peot-simulation" :language :lisp-unix)
  )

#+allegro
(defsystem ideal
  (:default-pathname "ideal:code;"
     :default-module-class separate-destination-module
     )
  (:serial
   "ideal-package-name"
  "global-vars"
  "store-ideal-struct-info"
  "struct"
  "wrapped-access-functions"
  "low-level-array-ops"
  "probability-access-fns"
  "utils-macros"
  "utils"
  "display-and-query-fns"
  "ordering"
  "consistency-checking"
  "noisy-or-nodes"
  "copy-diagram-macros"
  "copy-diagram"
  "file-io"
  "edit-functions-macros"
  "primitives"
  "id-creation"
  "edit-functions"
  "algorithms"
  "message-processing"
  "evidence+dummy-nodes"
  "poly-tree-utils"
  "clique-struct"
  "clique-macros"
  "clique-node-manipulation"
  "join-tree"
  "poly-tree"
  "clustering-algorithm"
  "clustering-est"
  "conditioning-utils"
  "conditioning"
  "lw-utils"
  "lw-infer"
  "pearl-simulation-utils"
  "pearl-simulation"
  "jensen-join-tree"
  "jensen-search"
  "jensen-infer"
  "jensen-est"
  "peot-simulation-macros"
  "peot-simulation"
  ))

#+asdf
(asdf:defsystem ideal
  :name "Ideal"
  :maintainer "Howie Shrobe"
  :description "Ideal Baysian Inference System"
  :pathname "../code"
  :serial t
  :components
  (
   (:file "ideal-package-name")
   (:file "global-vars")
   (:file "store-ideal-struct-info")
   (:file "struct")
   (:file "wrapped-access-functions")
   (:file "low-level-array-ops")
   (:file "probability-access-fns")
   (:file "utils-macros")
   (:file "utils")
   (:file "display-and-query-fns")
   (:file "ordering")
   (:file "consistency-checking")
   (:file "noisy-or-nodes")
   (:file "copy-diagram-macros")
   (:file "copy-diagram")
   (:file "file-io")
   (:file "edit-functions-macros")
   (:file "primitives")
   (:file "id-creation")
   (:file "edit-functions")
   (:file "algorithms")
   (:file "message-processing")
   (:file "evidence+dummy-nodes")
   (:file "poly-tree-utils")
   (:file "clique-struct")
   (:file "clique-macros")
   (:file "clique-node-manipulation")
   (:file "join-tree")
   (:file "poly-tree")
   (:file "clustering-algorithm")
   (:file "clustering-est")
   (:file "conditioning-utils")
   (:file "conditioning")
   (:file "lw-utils")
   (:file "lw-infer")
   (:file "pearl-simulation-utils")
   (:file "pearl-simulation")
   (:file "jensen-join-tree")
   (:file "jensen-search")
   (:file "jensen-infer")
   (:file "jensen-est")
   (:file "peot-simulation-macros")
   (:file "peot-simulation")
  ))

#+(and asdf (not Allegro) (not mcl))
(defun compile-ideal-system (&rest keys)
  (apply #'asdf:compile-system :ideal keys)
  )

#+(and asdf (not Allegro) (not mcl))
(defun load-ideal-system (&rest load-system-args)
  (apply #'asdf:load-system :ideal load-system-args))

#+(or (not asdf) Allegro mcl)
(defun compile-ideal-system (&rest keys)
  (apply #+mcl #'clim-defsys:compile-system
	 #+allegro #'compile-system
	 'ideal keys)
  )

#+(or (not asdf) Allegro mcl)
(defun load-ideal-system (&rest load-system-args)
  (apply #+mcl #'clim-defsys:load-system
	 #+allegro #'load-system
         #+ (and +asdf -Allegro) #'asdf:load-system
	 'ideal load-system-args)
  )
