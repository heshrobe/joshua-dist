;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: Ideal;  Base: 10 -*-

(in-package :ideal)

;;;;********************************************************
;;;;  Copyright (c) 1989, 1992 Rockwell International -- All rights reserved.
;;;;  Rockwell International Science Center Palo Alto Lab
;;;;********************************************************


;;;;;;;;;;;;;;;;;;;;;;;; Sampath ;;;;;;;;;;;;;;;;;;;;


(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(CLIQUE-NODE
            CLIQUE-NODE-NAME
            CLIQUE-NODE-PRINT-NAME
            CLIQUE-NODE-UNUSED-SLOT
            CLIQUE-NODE-COMPONENT-NODES
            CLIQUE-NODE-SEPERATOR-NODES
            CLIQUE-NODE-RESIDUAL-NODES)))

;--------------------------------------------------------
; Clique node structures

;******************************* CLIQUE NODES: **************************

;********************Structure, Access, Creation and all

(defstruct (clique-node (:print-function print-clique-node)(:include node))
  component-nodes
  residual-nodes
  seperator-nodes
  number-of-residual-states
  lambda-activator-nodes
  pi-activated-p)


(store-ideal-struct-info (clique-node (:print-function print-clique-node)(:include node))
  component-nodes
  residual-nodes
  seperator-nodes
  number-of-residual-states
  lambda-activator-nodes
  pi-activated-p)
  
(defidealprintfn clique-node (print-clique-node (c-node st)
						(format st "#<CLIQUE ~A>"
							(clique-node-name c-node))))
