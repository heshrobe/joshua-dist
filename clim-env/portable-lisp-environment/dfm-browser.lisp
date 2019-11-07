;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-ENV; Base: 10; Lowercase: Yes -*-

"Copyright (c) 1994 Harlequin, Inc.  All rights reserved."

(in-package :clim-env)


(define-application-frame dfm-browser (browser-frame)
    ((root :initarg :root :initform nil		;a <temporary> or a <computation>
	   :accessor dfm-browser-root))
  (:default-initargs :graph-type :graphical
		     :browser-type :dfm
		     :browser-subtype nil
		     :browser-options nil
		     :presentation-type 'expression
		     :node-maker #'make-dfm-node
		     :root-node-maker #'make-dfm-root)
  (:command-table (dfm-browser :inherit-from (browser-activity
					      inspection
					      browser-frame
					      selected-objects
					      accept-values-pane)
			       :menu (("Activity" :menu browser-activity)
				      ("Selections" :menu selected-objects))))
  (:top-level (dfm-browser-top-level))
  (:pointer-documentation t)
  (:panes
    (graph :application
	   :display-function 'display-graph-pane
	   :display-after-commands nil
	   :incremental-redisplay t
	   :scroll-bars :both
	   :end-of-page-action :allow
	   :end-of-line-action :allow))
  (:layouts
    (main
      (vertically () 
	graph))))

(defmethod dfm-browser-top-level ((frame dfm-browser))
  (enable-frame frame)
  (configure-for-browser-type frame (browser-subtype frame))
  (default-frame-top-level frame))

(defmethod browser-display-pane ((frame dfm-browser))
  (get-frame-pane frame 'graph))

(defmethod note-browser-root-changed ((browser dfm-browser) objects)
  (setf (dfm-browser-root browser) objects))


;;; DFM browser nodes, etc.

(defclass dfm-node (call-node) ())
  
(proclaim '(inline make-dfm-node))
(defun make-dfm-node (object)
  (make-instance 'dfm-node :object object))

(defun make-dfm-root (object)
  (make-dfm-node object))

(defmethod node-object-name ((node dfm-node))
  (let ((object (node-object node)))
    (with-output-to-string (s)
      (print-object object s))))

(defmethod display-node ((node dfm-node) stream)
  (labels ((draw (stream)
	     (with-output-as-presentation (stream node 'dfm-node)
	       (princ (node-object-name node) stream))))
    (declare (dynamic-extent #'draw))
    (let ((object (node-object node)))
      (cond ((typep object dylan+dylan/dfm::<temporary>)
	     (surrounding-output-with-border (stream :shape :oval)
	       (draw stream)))
	    ((typep object dylan+dylan/dfm::<computation>)
	     (draw stream))))))

(defmethod node-arc-drawer ((node dfm-node))
  (if (typep (node-object node) dylan+dylan/dfm::<temporary>)
      (values #'draw-arrow-arc '(:line-dashes #(2 2)))
      (values #'draw-arrow-arc nil)))


;; The idea here is to preserve EQness while we convert from the internal
;; Dylan representation to a representation we can use in Lisp...
(defvar *listify-eql-table* (make-hash-table))
(defun listify (x)
  (or (gethash x *listify-eql-table*)
      (let ((val (cond ((null x) nil)
                       ((listp x) x)
                       ((vectorp x) (coerce x 'list))
                       ((typep x dylan+dylan/internal::<stretchy-vector>)
                        (coerce (slot-value x 'dylan+dylan/internal::stretchy-vector-data)
                                'list))
                       ((typep x 'dylan::dylan-boolean) nil)	;--- kludge!
                       (t (list x)))))
        (setf (gethash x *listify-eql-table*) val)
        val)))

(defun empty? (x)
  (cond ((null x) t)
	((listp x) nil)
	((vectorp x) (zerop (length x)))
	((typep x dylan+dylan/internal::<stretchy-vector>)
	 (zerop (length (slot-value x 'dylan+dylan/internal::stretchy-vector-data))))
        ((typep x 'dylan::dylan-boolean) t)			;--- kludge!
	(t nil)))

(defmethod node-generate-inferior-objects ((node dfm-node) type)
  (declare (ignore type))
  (let ((object (node-object node)))
    (cond ((typep object dylan+dylan/dfm::<temporary>)
	   (append (listify (slot-value object 'dylan+dylan/dfm::users))
                   (listify (slot-value (slot-value object 'dylan+dylan/dfm::generator)
                                        'dylan+dylan/dfm::next-computations))))
	  ((typep object dylan+dylan/dfm::<computation>)
	   (append (listify (slot-value object 'dylan+dylan/dfm::next-computations))
                   (listify (slot-value object 'dylan+dylan/dfm::temporary))))
          (t (error "Whaddya mean???")))))

(defmethod node-any-inferior-objects-p ((node dfm-node) type)
  (declare (ignore type))
  (let ((object (node-object node)))
    (cond ((typep object dylan+dylan/dfm::<temporary>)
	   (or (not (empty? (slot-value object 'dylan+dylan/dfm::users)))
	       (not (empty? (slot-value (slot-value object 'dylan+dylan/dfm::generator)
					'dylan+dylan/dfm::next-computations)))))
	  ((typep object dylan+dylan/dfm::<computation>)
	   (or (not (empty? (slot-value object 'dylan+dylan/dfm::next-computations)))
	       (not (empty? (slot-value object 'dylan+dylan/dfm::temporary))))))))

(defmethod node-generate-superior-objects ((node dfm-node) type)
  (declare (ignore type))
  (let ((object (node-object node)))
    (cond ((typep object dylan+dylan/dfm::<temporary>)
	   (listify (slot-value object 'dylan+dylan/dfm::generator)))
	  ((typep object dylan+dylan/dfm::<computation>)
           (listify (slot-value object 'dylan+dylan/dfm::previous-computations)))
          (t (error "Whaddya mean???")))))

(defmethod node-any-superior-objects-p ((node dfm-node) type)
  (declare (ignore type))
  (let ((object (node-object node)))
    (cond ((typep object dylan+dylan/dfm::<temporary>)
	   (not (empty? (slot-value object 'dylan+dylan/dfm::generator))))
	  ((typep object dylan+dylan/dfm::<computation>) ;
           (not (empty? (slot-value object 'dylan+dylan/dfm::previous-computations)))))))


(defmethod configure-for-browser-type ((browser dfm-browser) subtype)
  (ecase subtype
    (nil
      (setf (browser-grapher-args browser)
	    '(:orientation :vertical :arc-drawer draw-arrow-arc))))
  (setf (browser-subtype browser) subtype)
  ;; Now regenerate the display
  (generate-browser-display
    browser (dfm-browser-root browser) (browser-display-pane browser)))


(define-presentation-type dfm-object ())

(define-presentation-method presentation-typep (object (type dfm-object))
  (or (typep object dylan+dylan/dfm::<temporary>)
      (typep object dylan+dylan/dfm::<computation>)))

(define-command (com-invoke-dfm-browser :command-table inspection :name t)
    ((object '(or expression dylan-expression)))
  (make-clim-environment-application 
    (find-navigator :framem (frame-manager *application-frame*)) 'dfm-browser
    :root object
    :width 700 :height 650))

(define-presentation-to-command-translator dfm-browser
    ((or expression dylan-expression) com-invoke-dfm-browser inspection
     :tester ((object)
	      (or (typep object dylan+dylan/dfm::<temporary>)
                  (typep object dylan+dylan/dfm::<computation>)
                  (typep object dylan+dylan/dfm::<&top-level-lambda>))))
    (object)
  (if (typep object dylan+dylan/dfm::<&top-level-lambda>)
      (list (slot-value object 'dylan+dylan/dfm::body))
      (list object)))
