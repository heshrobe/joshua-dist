;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-ENV; Base: 10; Lowercase: Yes -*-

;;; Copyright (c) 1994-2000, Scott McKay.
;;; Copyright (c) 2001-2003, Scott McKay and Howard Shrobe.
;;; All rights reserved.  No warranty is expressed or implied.
;;; See COPYRIGHT for full copyright and terms of use.

(in-package :clim-env)

;;; Package browser

(define-application-frame package-browser (browser-frame)
    ((package :initarg :package :initform nil
	      :accessor package-browser-package))
  (:default-initargs :graph-type :graphical
		     :browser-type :package
		     :browser-subtype :uses		;or :USED-BY...
		     :browser-options nil
		     :presentation-type 'package
		     :node-maker #'make-package-call-node
		     :root-node-maker #'make-package-browser-root
                     :browser-depth 2)
  (:command-table (package-browser :inherit-from (browser-activity
						  editing
						  inspection
						  browser-frame
						  selected-objects
						  accept-values-pane)
				   :menu (("Activity" :menu browser-activity)
				          ("Selections" :menu selected-objects))))
  (:top-level (package-browser-top-level))
  (:pointer-documentation t)
  (:panes
    (package :accept-values
	     :height :compute
	     :display-function
	       '(accept-values-pane-displayer
		  :displayer read-package-browser-package)
	     :text-style '(:fix :roman :large)
	     :scroll-bars nil)
    (direction (make-pane 'option-pane
		 :label "Direction"
		 :items '(("Uses" . :uses)
			  ("Used by" . :used-by))
		 :name-key #'car :value-key #'cdr
		 :value :uses
		 :value-changed-callback 'package-browser-direction-changed))
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
	  (:fill #+Genera (outlining () (spacing () package))
		 #-Genera package)
	  (1/5 direction))
	(:fill graph)))))

(defmethod package-browser-top-level ((frame package-browser))
  (enable-frame frame)
  (configure-for-browser-type frame (browser-subtype frame))
  (default-frame-top-level frame))

(defmethod browser-display-pane ((frame package-browser))
  (get-frame-pane frame 'graph))

(defmethod read-package-browser-package ((frame package-browser) stream)
  (with-slots (package) frame
    (when (null package)
      (setq package (or (let ((element (yank-from-history 
					 (presentation-type-history 'package))))
			  (and element 
			       (presentation-history-element-object element)))
			(find-package 'common-lisp))))
    (multiple-value-bind (new-package type changed-p)
	(accept 'package
		:default package :prompt nil
		:stream stream)
      (declare (ignore type))
      (when changed-p
	(setq package new-package)
	(generate-browser-display frame package (browser-display-pane frame))))))

(defun package-browser-direction-changed (pane value)
  (let ((frame (pane-frame pane)))
    (setf (browser-subtype frame) value)
    (configure-for-browser-type frame (browser-subtype frame))))

(defmethod note-browser-root-changed ((browser package-browser) objects)
  (setf (package-browser-package browser) (first objects))
  (redisplay-frame-pane browser 'package :force-p t))


;;; Package browser nodes, etc.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass package-call-node (call-node) ()))
  
(declaim (inline make-package-call-node))
(defun make-package-call-node (object)
  (make-instance 'package-call-node :object object))

(defmethod node-object-name ((node package-call-node))
  (package-name (node-object node)))

(defmethod display-node ((node package-call-node) stream)
  (with-output-as-presentation (stream node 'package-call-node)
    (present (node-object node) 'package :stream stream)))

(defun make-package-browser-root (object)
  (typecase object
    (clim-lisp:package
      (make-package-call-node object))
    ((or string symbol)
     (let ((package (find-package object)))
       (and package
	    (make-package-call-node package))))))

(defmethod node-generate-inferior-objects ((node package-call-node) (type (eql ':uses)))
  (package-use-list (node-object node)))

(defmethod node-any-inferior-objects-p ((node package-call-node) (type (eql ':uses)))
  (not (null (package-use-list (node-object node)))))

(defmethod node-generate-inferior-objects ((node package-call-node) (type (eql ':used-by)))
  (package-used-by-list (node-object node)))

(defmethod node-any-inferior-objects-p ((node package-call-node) (type (eql ':used-by)))
  (not (null (package-used-by-list (node-object node)))))


(defmethod node-generate-superior-objects ((node package-call-node) (type (eql ':uses)))
  (node-generate-inferior-objects node :used-by))

(defmethod node-any-superior-objects-p ((node package-call-node) (type (eql ':uses)))
  (node-any-inferior-objects-p node :used-by))

(defmethod node-generate-superior-objects ((node package-call-node) (type (eql ':used-by)))
  (node-generate-inferior-objects node :uses))

(defmethod node-any-superior-objects-p ((node package-call-node) (type (eql ':used-by)))
  (node-any-inferior-objects-p node :uses))


(defclass package-symbols-subnode (call-subnode) ())

(declaim (inline make-package-symbols-subnode))
(defun make-package-symbols-subnode (package-node symbols)
  (let ((subnode (make-instance 'package-symbols-subnode :object symbols)))
    (setf (node-superiors subnode) (list package-node))
    subnode))

(defmethod node-object-name ((node package-symbols-subnode))
  (format nil "Instance variables of ~S" (node-object-name (first (node-superiors node)))))

(defmethod display-node ((node package-symbols-subnode) stream)
  (let ((symbols (node-object node)))
    (with-output-as-presentation (stream node 'package-symbols-subnode
				  :single-box :highlighting)
      (surrounding-output-with-border (stream :shape :rectangle)
	(formatting-table (stream :multiple-columns t)
	  (dolist (symbol symbols)
            (updating-output (stream :unique-id symbol)
	      (formatting-row (stream)
	        (formatting-cell (stream)
		  (present symbol 'symbol :stream stream))))))))))


;;; The package browser commands

(defmethod configure-for-browser-type ((browser package-browser) subtype)
  (ecase subtype
    (:uses
      (setf (browser-grapher-args browser)
	    '(:arc-drawer draw-arrow-arc :arc-drawing-options (:from-head t :to-head nil))))
    (:used-by
      (setf (browser-grapher-args browser)
	    '(:arc-drawer draw-arrow-arc))))
  (setf (browser-subtype browser) subtype)
  ;; Now regenerate the display
  (generate-browser-display
    browser (package-browser-package browser) (browser-display-pane browser)))


(define-package-browser-command com-show-exported-symbols
    ((package-node 'package-call-node
		   :prompt "package node to show exported symbols of" :gesture :subnode-1))
  (with-application-frame (browser)
    (let ((package (node-object package-node))
	  symbols)
      (do-external-symbols (symbol package)
        (when (eql (symbol-package symbol) package)
	  (push symbol symbols)))
      (setq symbols (sort symbols #'string-lessp :key #'symbol-name))
      (when (and symbols
	         (not (subnode-object-present-in-node package-node symbols :test #'equal)))
        (let ((subnode (make-package-symbols-subnode package-node symbols)))
	  (push subnode (node-inferiors package-node))
	  (tick-node subnode)
          (redisplay-frame-pane browser (browser-display-pane browser)))))))


(define-command (com-show-package-symbols :name t :command-table inspection)
    ((package 'package :prompt "package")
     &key
     (types '(subset :variable :function :class :all)
	    :default '(:all)
	    :prompt "symbol types"
	    :documentation "Kinds of symbols to search for")
     (internal 'boolean
               :default nil :mentioned-default t
               :documentation "Show internal symbols as well")
     (imported-symbols 'boolean
		       :default nil
		       :documentation "Show imported symbols too"))
  (with-frame-standard-output (stream)
    (fresh-line stream)
    (let ((symbols (let ((symbols nil))
                     (flet ((collect (symbol)
                              (when (or imported-symbols
                                        (eql (symbol-package symbol) package))
                                (if (member :all types)
                                  (push symbol symbols)
				  (cond ((and (member :variable types)
					      (boundp symbol))
					 (push symbol symbols))
					((and (member :function types)
					      (fboundp symbol))
					 (push symbol symbols))
					((and (member :class types)
					      (find-class symbol nil))
					 (push symbol symbols)))))))
		       (declare (dynamic-extent #'collect))
                       (if internal
                         (do-symbols (symbol package)
			   (collect symbol))
			 (do-external-symbols (symbol package)
			   (collect symbol))))
		     (sort symbols #'string-lessp))))
      (formatting-table (stream)
        (dolist (symbol symbols)
          (formatting-row (stream)
            (let ((fboundp (fboundp symbol))
                  (boundp (boundp symbol))
                  (class (find-class symbol nil)))
              (formatting-cell (stream)
                (if class
		  (present class 'class :stream stream)
		  (present symbol (cond (fboundp 'function-spec)
					(boundp 'form)
					(t 'expression))
			   :stream stream)))
	      (formatting-cell (stream)
                (when fboundp
                  (if (typep (symbol-function symbol) 'generic-function)
		    (write-string "Generic  " stream)
		    (write-string "Function " stream)))
                (when boundp
                  (write-string "Variable " stream))
                (when class
                  (write-string "Class " stream))))))))))
