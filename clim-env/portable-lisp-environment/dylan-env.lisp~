;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-ENV; Base: 10; Lowercase: Yes -*-

"Copyright (c) 1994 Harlequin, Inc.  All rights reserved."

(in-package :clim-env)


;;; Dylan environment top level

(add-menu-item-to-command-table 'tools-menu 'dylan-divider :divider nil)

(define-command (com-start-dylan-listener :command-table tools-menu
					  :name t :menu "Dylan Listener") ()
  (let ((width  #+Genera +fill+ #-Genera 750)
        (height #+Genera +fill+ #-Genera 550))
    (make-clim-environment-application *application-frame* 'dylan-listener
      :width width :height height)))

(define-command (com-start-library-browser :command-table tools-menu
					   :name t :menu "Library Browser") ()
  (make-clim-environment-application *application-frame* 'library-browser
    :width 700 :height 650))

(define-command (com-start-module-browser :command-table tools-menu
					  :name t :menu "Module Browser") ()
  (make-clim-environment-application *application-frame* 'module-browser
    :width 700 :height 650))

