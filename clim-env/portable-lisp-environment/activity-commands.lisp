;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-ENV; Base: 10; Lowercase: Yes -*-

;;; Copyright (c) 1994-2000, Scott McKay.
;;; Copyright (c) 2001-2003, Scott McKay and Howard Shrobe.
;;; All rights reserved.  No warranty is expressed or implied.
;;; See COPYRIGHT for full copyright and terms of use.

(in-package :clim-env)

;;; Commands local to a single activity

(define-command (com-redisplay :command-table activity :menu "Redisplay") ()
  (frame-redisplay *application-frame*))

(defmethod frame-redisplay ((frame standard-application-frame))
  (redisplay-frame-panes frame))

(define-command (com-inspect :command-table activity :menu "Inspect") ()
  (let* ((sheet (frame-top-level-sheet *application-frame*)))
    #+(and Lispworks CLUE) (tools:win-inspect sheet)
    #-(and Lispworks CLUE) (com-invoke-inspector sheet)))

#+(and Lispworks CLUE)
(define-command (com-resources :command-table activity :menu "Resources") ()
  (let* ((sheet (frame-top-level-sheet *application-frame*))
         (mirror (sheet-mirror sheet)))
    (tools:browse-resources (clue:contact-parent (capi:representation mirror)))))

(add-menu-item-to-command-table 'activity "quit-divider" :divider nil
				:errorp nil)

(define-command (com-quit :command-table activity :menu "Quit") ()
  (with-application-frame (frame)
    (let ((navigator (find-navigator :port (port frame) :framem (frame-manager frame)
                                     :errorp nil)))
      (when navigator
	(setf (navigator-frames navigator)
	      (delete frame (navigator-frames navigator))))
      (frame-exit frame))))

