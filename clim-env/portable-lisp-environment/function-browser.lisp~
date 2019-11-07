;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-ENV; Base: 10; Lowercase: Yes -*-

;;; Copyright (c) 1994-2000, Scott McKay.
;;; Copyright (c) 2001-2003, Scott McKay and Howard Shrobe.
;;; All rights reserved.  No warranty is expressed or implied.
;;; See COPYRIGHT for full copyright and terms of use.

(in-package :clim-env)

;;; Function browser

(define-command-table function-browser-function :inherit-from (editing tracing))

(add-menu-item-to-command-table
  'function-browser-function "Edit" 
  :function (make-command-from-selected-item
	      com-edit-definition expression *application-frame*)
  :errorp nil)

(add-menu-item-to-command-table
  'function-browser-function "Trace" 
  :function (make-command-from-selected-item
	      com-trace-function (sequence function-spec) *application-frame*)
  :errorp nil)

(add-menu-item-to-command-table
  'function-browser-function "Untrace" 
  :function (make-command-from-selected-item
	      com-untrace-function (sequence function-spec) *application-frame*)
  :errorp nil)

(define-application-frame function-browser (browser-frame)
    ((function :initarg :function :initform nil
	       :accessor function-browser-function))
  (:default-initargs :graph-type :graphical
		     :browser-type :function
		     :browser-subtype :callers		;or :callees...
		     :browser-options nil
		     :presentation-type 'function-spec
		     :node-maker #'make-function-call-node
		     :root-node-maker #'make-function-browser-root)
  (:command-table (function-browser :inherit-from (browser-activity
						   editing
						   inspection
						   tracing
						   browser-frame
						   selected-objects
						   accept-values-pane)
				    :menu (("Activity" :menu browser-activity)
					   ("Function" :menu function-browser-function)
					   ("Selections" :menu selected-objects))))
  (:top-level (function-browser-top-level))
  (:pointer-documentation t)
  (:panes
    (function :accept-values
	      :height :compute
	      :display-function
	        '(accept-values-pane-displayer
		   :displayer read-function-browser-function)
	      :text-style '(:fix :roman :large)
	      :scroll-bars nil)
    (direction (make-pane 'option-pane
		 :label "Direction"
		 :items '(("Callers" . :callers)
			  ("Callees" . :callees))
		 :name-key #'car :value-key #'cdr
		 :value :callers
		 :value-changed-callback 'function-browser-direction-changed))
    (graph :application
	   :background +white+
	   :display-function 'display-graph-pane
	   :display-after-commands nil
	   :incremental-redisplay t
	   :scroll-bars :both
	   :end-of-page-action :allow
	   :end-of-line-action :allow))
  (:layouts
    (main
      (vertically () 
	(horizontally ()
	  (:fill #+Genera (outlining () (spacing () function))
		 #-Genera function)
	  (1/5 direction))
	(:fill graph)))))

(defmethod function-browser-top-level ((frame function-browser))
  (enable-frame frame)
  (configure-for-browser-type frame (browser-subtype frame))
  (default-frame-top-level frame))

(defmethod browser-display-pane ((frame function-browser))
  (get-frame-pane frame 'graph))

(defmethod read-function-browser-function ((frame function-browser) stream)
  (with-slots (function) frame
    (when (null function)
      (setq function (let ((element (yank-from-history 
				      (presentation-type-history 'function-spec))))
		       (and element
			    (presentation-history-element-object element)))))
    (multiple-value-bind (new-function type changed-p)
	(accept 'function-spec
		:default function :prompt nil
		:stream stream)
      (declare (ignore type))
      (when changed-p
	(setq function new-function)
	(generate-browser-display frame function (browser-display-pane frame))))))

(defun function-browser-direction-changed (pane value)
  (let ((frame (pane-frame pane)))
    (setf (browser-subtype frame) value)
    (configure-for-browser-type frame (browser-subtype frame))))

(defmethod note-browser-root-changed ((browser function-browser) objects)
  (setf (function-browser-function browser) objects)
  (redisplay-frame-pane browser 'function :force-p t))


;;; Function browser nodes, etc.

(defclass function-call-node (call-node) ())
  
(declaim (inline make-function-call-node))
(defun make-function-call-node (object)
  (make-instance 'function-call-node :object object))

(defmethod node-object-name ((node function-call-node))
  (let ((function (node-object node)))
    #+Genera (if (functionp function) (sys:function-name function) function)
    #+Allegro (if (functionp function) (function-name function) function)
    #+Lispworks (if (functionp function) (sys::function-name function) function)
    #+CCL (if (functionp function) (ccl::function-name function) function)
    #-(or Genera Lispworks) function))

(defmethod display-node ((node function-call-node) stream)
  (labels ((draw (stream)
	     (with-output-as-presentation (stream node 'function-call-node)
	       (present (node-object-name node) 'function-spec :stream stream))))
    (declare (dynamic-extent #'draw))
    (if (node-recurses node)
      (surrounding-output-with-border (stream :shape :oval)
	(draw stream))
      (draw stream))))

(defun make-function-browser-root (object)
  (typecase object
    (function 
     (make-function-call-node object))
    (symbol
     (when (fboundp object)
       (make-function-call-node (fdefinition object))))))


(defmethod node-generate-inferior-objects
    ((node function-call-node) (type (eql ':callees)))
  (calls-who (node-object node)))

(defun calls-who (function)
  #+Genera
  (typecase function
    (flavor:generic-function
     ;; Return all the real methods for this generic function
     (loop for method in (sort (flavor:generic-function-methods
				 (sys:generic-function-name function))
			       #'string-lessp
			       :key #'flavor:method-flavor)
	   as function = (unless (and (listp method)
				      (eql (first method) 'flavor:read-instance-variable))
			   (si:valid-function-definition method))
	   when function
	     collect function))
    (clos:generic-function
     ;; Return all the real methods for this generic function
     (loop for method in (clos-internals::sort-generic-function-methods
			   function
			   (copy-list (clos:generic-function-methods function))
			   :sort :heuristic)
	   as function = (clos:method-function method)
	   when function
	     collect function))
    (compiled-function
     (let ((callees nil))
       (si:map-over-compiled-function-callees
	 function
	 #'(lambda (caller callee how)
	     (declare (ignore caller))
	     (case how
	       ((:function :generic-function)
		(pushnew (sys:fdefinition callee) callees))
	       (:variable
		 (pushnew callee callees))))
	 :external-references t)
       (nreverse callees))))
  #+Lispworks
  (let ((name (if (functionp function) (sys::function-name function) function)))
    (compiler::calls-who name))
  #+Allegro
  (let* ((name    (if (functionp function) (function-name function) function))
	 (callees (xref:get-relation :direct-calls name xref::.xref-wildcard.
				     :in-files nil
				     :in-functions nil)))
    (loop for callee in callees
	  when (not (and (listp callee)
			 (eq (car callee) :top-level-form)))
	    collect callee)))  


(defmethod node-generate-inferior-objects
    ((node function-call-node) (type (eql ':callers)))
  (who-calls (node-object node)))

(defun who-calls (function)
  #+Genera
  (let ((name (if (functionp function) (sys:function-name function) function)))
    (typecase name
      (clos:method
       (list (clos:method-generic-function name)))
      (t
       (if (and (listp name)
		(eql (car name) 'flavor:method))
	 (list (flavor:method-generic name))
	 (let ((callers nil))
	   (si:map-over-callers
	    (sys:function-name function)
	    #'(lambda (caller how)
		(case how
		  ((:function :generic-function :macro)
		   (pushnew (sys:fdefinition caller) callers))
		  ((:variable :constant)
		   (pushnew caller callers))))
	    :called-how '(:function :generic-function :macro :variable :constant))
	   (nreverse callers))))))
  #+MCL
  (let ((name (if (functionp function) (ccl::function-name function) function)))
    (ccl::callers name))
  #+Lispworks
  (let ((name (if (functionp function) (sys::function-name function) function)))
    (compiler::who-calls name))
  #+Allegro
  (let* ((name    (if (functionp function) (function-name function) function))
	 (callers (xref:get-relation :direct-calls xref::.xref-wildcard. name
				     :in-files nil
				     :in-functions nil)))
    (loop for caller in callers
	  when (not (and (listp caller)
			 (eq (car caller) :top-level-form)))
	    collect caller)))
		 


(defmethod node-generate-superior-objects ((node function-call-node) (type (eql ':callees)))
  (node-generate-inferior-objects node :callers))

(defmethod node-generate-superior-objects ((node function-call-node) (type (eql ':callers)))
  (node-generate-inferior-objects node :callees))



;;; The function browser commands

(defmethod configure-for-browser-type ((browser function-browser) subtype)
  (ecase subtype
    (:callees
     (setf (browser-grapher-args browser)
           '(:arc-drawer draw-arrow-arc)))
    (:callers
     (setf (browser-grapher-args browser)
           '(:arc-drawer draw-arrow-arc :arc-drawing-options (:from-head t :to-head nil)))))
  (setf (browser-subtype browser) subtype)
  ;; Now regenerate the display
  (generate-browser-display
   browser (function-browser-function browser) (browser-display-pane browser)))


(define-presentation-to-command-translator edit-function-spec
    (function-spec com-edit-definition editing
     :gesture :edit)
    (object)
  (list object :type 'defun))

