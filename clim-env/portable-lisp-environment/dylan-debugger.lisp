;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-ENV; Base: 10; Lowercase: Yes -*-

"Copyright (c) 1994 Harlequin, Inc.  All rights reserved."

(in-package :clim-env)


;;; The Dylan debugger

(define-application-frame dylan-debugger (debugger)
    ()
  (:command-definer t)
  (:command-table (dylan-debugger
                    :inherit-from (debugger)
                    :inherit-menu :keystrokes))
  (:menu-bar nil)
  (:top-level (debugger-top-level)))

(define-application-frame dylan-debugger-frame (dylan-debugger)
    ((current-frame-item :accessor debugger-current-frame-item :initform nil))
  (:command-definer nil)
  (:command-table (dylan-debugger-frame
                    :inherit-from (activity
				   debugger-stack-frames 
				   debugger-restarts
				   selected-objects
				   dylan-debugger)
		    :inherit-menu :keystrokes
		    :menu (("Activity" :menu activity)
			   ("Frames" :menu debugger-stack-frames)
			   ("Restarts" :menu debugger-restarts)
			   ("Selections" :menu selected-objects))))
  (:top-level (debugger-top-level))
  (:pointer-documentation t)
  (:panes
    (proceeds :application
	      :output-record (make-instance 'text-scroll-output-history)
	      :display-after-commands nil
	      :end-of-line-action :allow
	      :end-of-page-action :allow)
    (backtrace :application
	       :output-record (make-instance 'text-scroll-output-history)
	       :display-after-commands nil
	       :end-of-line-action :allow
	       :end-of-page-action :allow)
    (interactor :interactor)
    (code :application
	  :display-after-commands nil
	  :end-of-line-action :allow
	  :end-of-page-action :allow)
    (locals :application
	    :display-after-commands nil
	    :end-of-line-action :allow
	    :end-of-page-action :allow))
  (:layouts
    (main (horizontally ()
	    (1/2 (vertically ()
		   (1/5 proceeds)
		   (2/5 backtrace)
		   (2/5 interactor)))
	    (1/2 (vertically ()
		   (1/2 code)
		   (1/2 locals)))))))


(add-command-to-command-table 'com-describe-dylan-object 'dylan-debugger)
(add-presentation-translator-to-command-table
  'dylan-debugger (find-presentation-translator 'describe-dylan-object 'dylan-listener))
(add-presentation-translator-to-command-table
  'dylan-debugger (find-presentation-translator 'describe-lisp-object-in-dylan 'dylan-listener))


(defmethod debugger-top-level-1 ((frame dylan-debugger))
  (let* ((interactor (frame-standard-input frame))
	 (command-table (frame-command-table frame)))
    (let* ((*standard-output*
	     (or (frame-standard-output frame) *standard-output*))
	   (*standard-input* 
	     (or (frame-standard-input frame) *standard-output*))
	   (*query-io* 
	     (or (frame-query-io frame) *standard-input*))
	   (*error-output* 
	     (or (frame-error-output frame) *standard-output*))
	   (*pointer-documentation-output*
	     (frame-pointer-documentation-output frame))
	   #+Genera (sys:trace-conditions nil)
	   (*break-on-signals* nil)
	   (*package* *package*)
	   (*** nil) (** nil) (* nil)
	   (/// nil) (// nil) (/ nil)
	   (+++ nil) (++ nil) (+ nil)
	   (- nil))
      (terpri interactor)
      (with-command-table-keystrokes (keystrokes command-table)
	(condition-restart-loop (#+Genera (sys:error sys:abort)
				 #-Genera (error)
				 "Restart this debugger")
	  (debugger-command-reader
	    frame *standard-input* command-table
	    :keystrokes keystrokes))))))

(defmethod additional-prompt-string ((frame dylan-debugger) stack-frame)
  (if (dylan-frame-p stack-frame)
    (format nil "Dylan ~D" *debugger-level*)
    (format nil "Lisp ~D" *debugger-level*)))


(defun dylan-eval-in-frame-environment (expression stack-frame)
  (multiple-value-list
    (dbg::dbg-eval
      (dylan::translate-dylan-expr
        expression
        (generic-env::lisp->xlator-env
          (dbg::call-frame-environment-for-eval stack-frame)))
      stack-frame)))

(defun dylan-function-p (function)
  ;; In the emulator, Dylan modules use a package-name convention that
  ;; we can use to determine whether a function is a Dylan function
  (let ((function-name (function-name function)))
    (when (listp function-name)
      (setq function-name (second function-name)))
    (and (symbolp function-name)
         (string-equal (package-name (symbol-package function-name))
                       "DYLAN+DYLAN" :end1 11))))

(defun dylan-frame-p (stack-frame)
  (dylan-function-p (stack-frame-function stack-frame)))

;;--- The next four should be methods on a "language" object
(defmethod stack-frame-expression-type (stack-frame (frame dylan-debugger))
  (if (dylan-frame-p stack-frame)
    'dylan-expression
    (call-next-method)))

(defmethod stack-frame-form-type (stack-frame (frame dylan-debugger))
  (if (dylan-frame-p stack-frame)
    'dylan-form
    (call-next-method)))

(defmethod stack-frame-command-or-form-type (stack-frame (frame dylan-debugger))
  (if (dylan-frame-p stack-frame)
    'command-or-dylan-form
    (call-next-method)))

(defmethod stack-frame-eval-function (stack-frame (frame dylan-debugger))
  (if (dylan-frame-p stack-frame)
    #'dylan-eval-in-frame-environment
    (call-next-method)))
