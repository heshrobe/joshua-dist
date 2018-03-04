;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2005 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2002-2007 Franz Inc, Oakland, CA - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: presentations.lisp,v 2.7 2007/04/17 21:45:50 layer Exp $

(in-package :clim-internals)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

;;; This comes from the CLIM files presentations

;;; The problem addressed shows up as follows:
;;; 1) If there are two CLIM applications on the screen then when the mouse moves
;;;    over a presentation on one of them, the documentation pane on the other updates 
;;;    as if the presentation belonged to it
;;; 2) If the presenting the documentation requires it to invoke a present method for the 
;;;    the presentation under the mouse, then that method gets the wrong arguments including
;;;    the frame of the other application that it doesns't belong to.  The can cause an error
;;; 3) Not so visible, both applications' top-level loops are doing work for mouse motion that
;;;    only one of them should be doing.  There are no interlocks between these and synchronization
;;;    issues are lurking.

;;; The cause of the problem is in this little bitty guy which finds the "appropriate window"
;;;  for responses during wait loops (e.g. with-input-context ...
;;; The original version doesn't check that the window under the moudr (the first two lines of the let
;;; is related to the wait-loop.  The fix is to check whether that guy's frame is the current *application-frame*
;;; i.e. the one for this process.
;;; This seems to work, but there are some comments in the code about why you should compare to *application-frame*
;;; rather than the frame of pane that's waiting, due to confusions caused by AVV panes, but I think this is OK anyhow.
;;; It does seem to fix the problem.
;;; Actually, there's an even better test: window-cohorts-p which tests if they share an event queue.  Which as the comment
;;; at its definition says is the best test we have whether they're related.

(defun find-appropriate-window (stream)
  ;;--- How do we hack multiple pointers?
  (when (extended-input-stream-p stream)
    (let* ((pointer (stream-primary-pointer stream))
           (window (pointer-sheet pointer)))
      ;; It ain't no good if it doesn't have a history.
      (when (and window
		 ;; this is the fix maybe isn't necessary with 
		 ;; the other fix below
		 (window-cohorts-p (if (encapsulating-stream-p stream) 
				       (encapsulating-stream-stream stream)
				       stream)
				   window)  
		 (port window)
		 (output-recording-stream-p window))
	window))))

;;; The same check in here when the modifier state changes is an even more
;;; fundamental way to fix this
(defun invoke-with-input-context (type override body-continuation context-continuation)
  (declare (dynamic-extent body-continuation context-continuation)
	   (special *input-wait-handler*
		    *input-wait-test*
		    *pointer-button-press-handler*))
  ;; At one time, this used to canonicalize the presentation type by
  ;; calling WITH-PRESENTATION-TYPE-DECODED and then consing just the
  ;; type name and parameters.  That turns out not to be necessary any
  ;; more since everyplace else in CLIM is careful to decode the type.
  ;; Furthermore, it is necessary to include the type options in the
  ;; input context in case the options are needed to correctly present
  ;; an object using the context type as the presentation type.
  (with-stack-list (this-tag '#:context-tag type) ;guaranteed to be unique
    (with-stack-list (this-context type this-tag)
      (let ((pwindow nil)
            (px 0)
            (py 0)
            (old-state 0))
        (flet ((pointer-motion-pending (stream)
                 (let ((moved-p nil))
                   (multiple-value-setq (moved-p pwindow px py)
                     (pointer-state-changed (stream-primary-pointer stream)
                                            pwindow px py))
                   (or 
		    (and moved-p
			 pwindow
			 (window-cohorts-p stream pwindow))
		    ;; This is more like the pointer moving than anything else.
		    (and
		     (/= old-state (setf old-state (window-modifier-state stream)))
		     ;; this is the fix
                     (window-cohorts-p (if (encapsulating-stream-p stream) 
					   (encapsulating-stream-stream stream)
					 stream)
				       pwindow)
		     )))))
          #-allegro (declare (dynamic-extent #'pointer-motion-pending))
          (let ((*input-wait-handler* #'highlight-presentation-of-context-type)
                (*input-wait-test* #'pointer-motion-pending)
                (*pointer-button-press-handler* #'input-context-button-press-handler))
	    ;; Push the new context.  The most specific context is at the
	    ;; beginning of *INPUT-CONTEXT, the least specific is at the end.
            (with-stack-list* (*input-context* this-context (unless override *input-context*))
              (block with-input-context
                (multiple-value-bind (object ptype event options)
                    (catch this-tag
                      (return-from invoke-with-input-context
                        (funcall body-continuation)))
                  (funcall context-continuation object ptype event options))))))))))