;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-ENV; Base: 10; Lowercase: Yes -*-

;;; Copyright (c) 1994-2000, Scott McKay.
;;; Copyright (c) 2001-2003, Scott McKay and Howard Shrobe.
;;; All rights reserved.  No warranty is expressed or implied.
;;; See COPYRIGHT for full copyright and terms of use.

(in-package :clim-env)

;;; Various macros that are needed early

(defmacro condition-restart-loop ((conditions description . args) &body body)
  #---ignore (declare (ignore conditions))
  (let ((tag (clim-utils:gensymbol 'restart)))
    `(tagbody ,tag
       (restart-case
	   (progn ,@body)
	 (nil ()
	   #|| :test (lambda (condition)
		       (some #'(lambda (x) (typep condition x)) ',conditions)) ||#
	   :report (lambda (stream)
		     (format stream ,description ,@args))))
       (go ,tag))))

(defmacro with-output-recording-reset ((stream) &body body)
  `(flet ((with-output-recording-reset-body (,stream) ,@body))
     (declare (dynamic-extent #'with-output-recording-reset-body))
     (invoke-with-output-recording-reset
       ,stream #'with-output-recording-reset-body)))

;; Attempt to reset the state of CLIM's output recording
(defun invoke-with-output-recording-reset (stream continuation)
  (let ((*original-stream* nil))
    (multiple-value-bind (width height)
	(bounding-rectangle-size (stream-output-history stream))
      (declare (ignore width))
      ;; Just set the stream's cursorpos to the end of the stream
      (let* ((cx 0)
	     (cy height)
	     (current-output-position
	       (clim-internals::stream-output-history-position stream))
	     (medium (sheet-medium stream)))
	(clim-utils:letf-globally 
	    (((stream-current-output-record stream) nil)
	     ((point-x current-output-position) 0)
	     ((point-y current-output-position) 0)
	     ((clim-internals::stream-redisplaying-p stream) nil)
	     ((medium-ink medium) (medium-foreground medium))
	     ((medium-transformation medium) +identity-transformation+)
	     ((silica:medium-+Y-upward-p medium) nil))
	  (stream-set-cursor-position stream cx cy)
	  (with-output-recording-options (stream :draw t :record t)
	    (funcall continuation stream)))))))

