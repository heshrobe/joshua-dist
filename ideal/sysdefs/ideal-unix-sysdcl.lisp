;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: COMMON-LISP-USER -*-

(in-package :cl-user)

;;;;********************************************************
;;;;  Copyright (c) 1989, 1992 Rockwell International -- All rights reserved.
;;;;  Rockwell International Science Center Palo Alto Lab
;;;;********************************************************


;;; My call tree analysis says that these are the only
;;; files that AWDRAT needs but I only did this for
;;; called funtions, not variables
; "algorithms.lisp"
; "consistency-checking.lisp"
; "display-and-query-fns.lisp"
; "edit-functions.lisp"
; "evidence+dummy-nodes .lisp"
; "jensen-infer.lisp"
; "jensen-join-tree.lisp"
; "jensen-search.lisp"
; "join-tree.lisp"
; "low-level-array-op s.lisp"
; "message-processing.lisp"
; "noisy-or-nodes.lisp"
; "primitives.lisp"
; "primitives.lisp"
; "probability-access-fns.lisp"
; "struct.lisp"
; "utils-macros.lisp"
; "utils.lisp"
; "wrapped-access-functions.lisp"


(unless (find-package :ideal)
  (make-package :ideal :use '(:common-lisp) :nicknames '())
  )

(defparameter *ideal-src-directory*
  (translate-logical-pathname "ideal:code;"))
(defparameter *ideal-bin-directory*
  (translate-logical-pathname "ideal:code;"))


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
  ;; "ordering"
  ;; "consistency-checking"
  "noisy-or-nodes"
  ;; "copy-diagram-macros"
  ;; "copy-diagram"
  ;; "file-io"
  "edit-functions-macros"
  "primitives"
  ;; "id-creation"
  "edit-functions"
  "algorithms"
  "message-processing"
  "evidence+dummy-nodes"
  ;; "poly-tree-utils"
  ;; "clique-struct"
  ;; "clique-macros"
  ;; "clique-node-manipulation"
  "join-tree"
  ;; "poly-tree"
  ;; "clustering-algorithm"
  ;; "clustering-est"
  ;; "conditioning-utils"
  "conditioning"
  ;; "lw-utils"
  ;; "lw-infer"
  ;; "pearl-simulation-utils"
  ;; "pearl-simulation"
  "jensen-join-tree"
  "jensen-search"
  "jensen-infer"
  "jensen-est"
  ;; "peot-simulation-macros"
  ;; "peot-simulation"
  ))

