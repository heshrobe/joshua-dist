;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: COMMON-LISP-USER -*-
(in-package :cl-user)

;;;;********************************************************
;;;;  Copyright (c) 1989, 1992 Rockwell International -- All rights reserved.
;;;;  Rockwell International Science Center Palo Alto Lab
;;;;********************************************************

#|
File:      sysdcl.lisp
Description:
  System declaration file for IDEAL system - actually loads appropriate implementation
    specific system file from sysdefs directory.

Notes:
  This file must be in the top-level IDEAL directory
|#

#+genera
(load "sysdefs;ideal-genera-sysdcl.lisp")

#+(or unix mcl allegro)
(load "ideal:sysdefs;ideal-unix-sysdcl.lisp" #+mcl :external-format  #+mcl :unix)

#-(or unix mcl genera allegro)
(error "Sysdef file not found")
