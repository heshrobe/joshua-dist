;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-ENV; Base: 10; Lowercase: Yes -*-

"Copyright (c) 1994 Harlequin, Inc.  All rights reserved."

(in-package :clim-env)


;;; The Dylan Listener

(define-command-table listener-libraries :inherit-from (libraries))
(add-menu-item-to-command-table
  'listener-libraries "Compile" :function
  (make-command-from-selected-item
    com-compile-library dylan-library *application-frame*))
(add-menu-item-to-command-table
  'listener-libraries "Load" :function
  (make-command-from-selected-item
    com-load-library dylan-library *application-frame*))


(define-application-frame dylan-listener (basic-listener)
    ()
  (:command-table (dylan-listener :inherit-from (activity
						 listener-files 
						 listener-libraries
						 listener-restarts
						 listener-history
                                                 basic-listener
						 selected-objects
						 global)
				  :menu (("Activity" :menu activity)
					 ("File" :menu listener-files)
					 ("Libraries" :menu listener-libraries)
					 ("Restarts" :menu listener-restarts)
					 ("History" :menu listener-history)
					 ("Selections" :menu selected-objects))))
  (:command-definer t)
  (:top-level (dylan-listener-top-level))
  (:pointer-documentation t)
  (:panes
    (interactor :interactor 
		:background +white+
		:scroll-bars :both))
  (:layouts (default
	      (vertically ()
		interactor))))

(defmethod clim-internals::frame-document-highlighted-presentation-1
           ((frame dylan-listener) presentation input-context window x y stream)
  (format stream "{~A} " (dylan::module-name (dylan::current-module)))
  (call-next-method))


(defparameter *dylan-listener-prompt* *prompt-arrow*)

(defmethod dylan-listener-top-level ((frame dylan-listener))
  (enable-frame frame)
  (let* ((*listener-frame* frame)
	 (window (frame-standard-input frame))
	 (command-table (frame-command-table frame))
	 (presentation-type `(command-or-dylan-form :command-table ,command-table)))
    (with-input-focus (window)
      (let* ((*listener-io* window)
	     (*listener-depth* (1+ *listener-depth*))
	     (*standard-input* *listener-io*)
	     (*standard-output* *listener-io*)
	     #-Genera (*pointer-documentation-output*
	                (frame-pointer-documentation-output frame))
	     #+(or Minima Allegro) (*error-output* *listener-io*)
	     (*query-io* *listener-io*)
	     #+Minima (*debug-io* *listener-io*)
	     (*package* (find-package :dylan))
	     (*** nil) (** nil) (* nil)
	     (/// nil) (// nil) (/ nil)
	     (+++ nil) (++ nil) (+ nil)
	     (- nil))
	(terpri *listener-io*)
	(with-command-table-keystrokes (keystrokes command-table)
	  (condition-restart-loop (#+Genera (sys:error sys:abort)
				   #-Genera (error)
				   "Restart CLIM Dylan listener")
            (setf (command-menu-enabled 'listener-restarts frame) nil)
	    (listener-command-reader
	      frame *standard-input* command-table presentation-type
	      :keystrokes keystrokes
	      :listener-depth *listener-depth*
	      :prompt *dylan-listener-prompt*
              :evaluator #'dylan-evaluator
              :expression-type 'dylan-expression
              :debugger-hook #'dylan-debugger-hook)))))))

(defun dylan-evaluator (form)
  (labels ((find-$ (form)
             (cond ((symbolp form)
                    (let ((i (position (symbol-name form) '("$" "$$" "$$$")
                                       :test #'string-equal)))
                      (and i (values (symbol-value (nth i '(* ** ***))) t))))
                   ((and (listp form)
                         (null (cddr form))
                         (eq (first form) 'dylan::begin))
                    (find-$ (second form))))))
    (multiple-value-bind (value value-p) (find-$ form)
      (if value-p
	value
	(dylan::dylan-eval form)))))

(defun dylan-debugger-hook (condition hook)
  (declare (ignore hook))
  (let* ((*application-frame* *listener-frame*)
	 #+Minima (*debug-io* (frame-query-io *application-frame*))
	 (*error-output* (frame-standard-output *application-frame*))
	 (stream *listener-io*))
    (stream-close-text-output-record stream)
    (clim-utils:letf-globally
        (((stream-current-output-record stream) (stream-output-history stream))
	 ((stream-recording-p stream) t)
	 ((stream-drawing-p stream) t)
	 ((clim-internals::stream-current-redisplay-record stream) nil))
      (setf (command-menu-enabled 'listener-restarts *application-frame*) t)
      (enter-debugger condition *listener-io*
                      :debugger-class 'dylan-debugger
                      :display-debugger-class 'dylan-debugger-frame))))


(define-dylan-listener-command (com-describe-dylan-object :name t)
    ((object 't))
  (describe object))

(define-presentation-to-command-translator describe-dylan-object
    (dylan-expression com-describe-dylan-object dylan-listener
     :documentation "Describe this Dylan object"
     :gesture :describe)
    (object)
  (list object))

(define-presentation-to-command-translator describe-lisp-object-in-dylan
    (expression com-describe-dylan-object dylan-listener
     :documentation "Describe this Lisp object"
     :gesture :describe)
    (object)
  (list object))


(define-presentation-to-command-translator inspect-lisp-object-in-dylan
    (expression com-invoke-inspector dylan-listener
     :documentation "Inspect this Lisp object"
     :gesture :inspect-object)
    (object)
  (list object))

(define-presentation-to-command-translator inspect-dylan-object
    (dylan-expression com-invoke-inspector dylan-listener
     :documentation "Inspect this Dylan object"
     :gesture :inspect-object)
    (object)
  (list object))


(define-dylan-listener-command (com-set-module :name t)
    ((module 'dylan-module
             :prompt "module name"))
  (setf (dylan::current-module) module)
  (with-frame-standard-output (stream)
    (fresh-line stream)
    (format stream "Module set to ~A." (dylan::module-name module))))


(define-dylan-listener-command (com-expand-macro :name t)
    ((form 'dylan-form)
     &key
     (all 'boolean
          :prompt "expand all levels"
          :default nil :mentioned-default t
          :documentation "Fully macroexpand the form"))
  (with-frame-standard-output (stream)
    (let ((expansion (if all
		       (dylan::maybe-expand* form)
		       (dylan::maybe-expand form))))
      (format stream "~S" expansion))))

(define-dylan-listener-command (com-translate-form :name t)
    ((form 'dylan-form))
  (with-frame-standard-output (stream)
    (let ((expander (dylan::get-dylan-translator (first form) '())))
      (present (dylan::translate-dylan-expr form '()) 'form
               :stream stream))))

(define-dylan-listener-command (com-where-is :name t)
    ((name 'symbol))
  (with-frame-standard-output (stream)
    (multiple-value-bind (home real-name)
        (dylan::canonicalize-module-variable (dylan::current-module) name)
      (format stream "The binding ~S resolves to ~S in the module ~A"
              name real-name (dylan::module-name home)))))

(define-dylan-listener-command (com-show-undefined-symbols :name t)
    ((module 'dylan-module
	     :default (dylan::current-module)))
  (with-frame-standard-output (stream)
    (let ((undefined
	   (loop for sym being each symbol in (dylan::module-package module)
	         when (and (and (not (fboundp sym)) (not (boundp sym)))
			   (not (member (symbol-name sym) '("$" "$$" "$$$")
				        :test #'string=)))
	           collect sym)))
      (filling-output (stream :fill-width '(72 :character))
        (format stream "The following are undefined in the module ~A: "
                (dylan::module-name module))
        (format-textual-list undefined #'princ
                             :stream stream :conjunction "and")
	(write-string "." stream)))))

;;--- Trace/Untrace
;;--- Break/Unbreak
;;--- Undefine

