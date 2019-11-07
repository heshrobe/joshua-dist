;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-ENV; Base: 10; Lowercase: Yes -*-

;;; Copyright (c) 1994-2000, Scott McKay.
;;; Copyright (c) 2001-2003, Scott McKay and Howard Shrobe.
;;; All rights reserved.  No warranty is expressed or implied.
;;; See COPYRIGHT for full copyright and terms of use.

(in-package :clim-env)

;;; Utilities for the Browser

(defparameter *browser-print-length* 2)
(defparameter *browser-print-level* 2)

(defparameter *display-node-character-style* 
	      (parse-text-style '(:sans-serif :roman :very-small)))

(defmacro with-browser-io-environment (&body body)
  `(invoke-with-browser-io-environment #'(lambda () ,@body)))

(defun invoke-with-browser-io-environment (continuation)
  (declare (dynamic-extent continuation))
  ;; Heavily truncate the printed result
  (let ((*print-base* 10)
	(*print-radix* nil)	  
	(*print-readably* nil)
	(*print-pretty* nil)
	(*print-length* *browser-print-length*)
	(*print-level* *browser-print-level*)
	(*print-circle* nil)
	(*print-array* nil)
	(*print-escape* t)
	#+Genera (scl:*print-string-length* nil)
	#+Genera (scl:*print-structure-contents* nil))
    (funcall continuation)))

(define-gesture-name :show-graph :pointer-button (:left :shift))
(define-gesture-name :subnode-1 :pointer-button (:left :control :meta))
(define-gesture-name :subnode-2 :pointer-button (:middle :control :meta))


;;; Basic call nodes

;; These are the generic functions that are intended to be specialized
(defgeneric node-object-name (call-node))
(defgeneric display-node (call-node stream))
(defgeneric node-arc-drawer (call-node)
  #+Genera (declare (values arc-drawer arc-drawing-options)))

;; This protocol assumes that there is a symmetric inferior/superior
;; relationship, but what the heck...
(defgeneric node-generate-inferior-objects (call-node subtype))
(defgeneric node-any-inferior-objects-p (call-node subtype))
(defgeneric node-generate-superior-objects (call-node subtype))
(defgeneric node-any-superior-objects-p (call-node subtype))


(defclass basic-call-node ()
    ((object :reader node-object :initarg :object)
     (inferiors :accessor node-inferiors :initform nil)
     (superiors :accessor node-superiors :initform nil)
     ;; RECURSES is T iff this node eventually calls itself
     (recurses  :accessor node-recurses  :initform nil)
     (tick :accessor node-tick :initform 0)))
  
(defmethod node-object-name ((node basic-call-node))
  (node-object node))

(defmethod print-object ((node basic-call-node) stream)
  (with-browser-io-environment
    (if *print-escape*
      (print-unreadable-object (node stream :type t :identity t)
	(write (node-object-name node) :stream stream :escape nil))
      (write (node-object-name node) :stream stream :escape nil))))

;; Propagate ticks up the graph to get proper redisplay.
(defmethod tick-node ((node basic-call-node))
  (labels ((tick (node)
	     (incf (node-tick node))
	     ;; No need to check for recursion since it is the responsibility
	     ;; of the graph generator to do that.
	     (dolist (superior (node-superiors node))
	       (tick superior))))
    (declare (dynamic-extent #'tick))
    (tick node))
  (incf (browser-global-tick *application-frame*)))

(defparameter *node-selection-highlighting-ink* +yellow+)
(defmethod display-node :around ((node basic-call-node) stream)
  (updating-output (stream :unique-id node
			   :cache-value (node-tick node))
    (with-text-style (stream *display-node-character-style*)
      ;; We always add a border to ensure that the background of these nodes
      ;; is an opaque background.  Since the grapher tries to put the edges
      ;; behind the nodes, this makes the display a bit easier to read.
      (surrounding-output-with-border 
          (stream :shape :selection-rectangle
		  :ink (if (object-selected-p *application-frame* node)
			 *node-selection-highlighting-ink*
			 +background-ink+))
	(call-next-method node stream)))))

(defmethod display-node ((node basic-call-node) stream)
  (with-output-as-presentation (stream node (presentation-type-of node))
    (write (node-object node) :stream stream :escape nil)))

;; Let the "to" node contribute to the drawing of the arc to it.
(defmethod node-arc-drawer ((node basic-call-node))
  (values #'draw-arrow-arc nil))

(defmethod node-generate-inferior-objects ((node basic-call-node) subtype)
  (declare (ignore subtype))
  nil)

(defmethod node-generate-superior-objects ((node basic-call-node) subtype)
  (declare (ignore subtype))
  nil)

;; Answers the question "will NODE-GENERATE-INFERIOR-OBJECTS return anything?",
;; except that it's allowed to be much faster.  If this returns T, that means
;; NODE-GENERATE-INFERIOR-OBJECTS might return something; if this returns NIL,
;; then NODE-GENERATE-INFERIOR-OBJECTS will definitely not return anything.
(defmethod node-any-inferior-objects-p ((node basic-call-node) subtype)
  (declare (ignore subtype))
  t)

(defmethod node-any-superior-objects-p ((node basic-call-node) subtype)
  (declare (ignore subtype))
  t)

(defun node-eql (n1 n2)
  (eql (node-object n1) (node-object n2)))


;; Targets for some translators
(defclass call-node-target ()
    ((call-node :reader target-call-node :initarg :call-node)))

(defclass call-node-inferiors-target (call-node-target) ())

(defclass call-node-superiors-target (call-node-target) ())


;; This is the most basic instantiable sort of call node.
;; Commands and translators are written on this, not on BASIC-CALL-NODE.
(defclass call-node (basic-call-node) ())

;; Add targets to the display of the nodes
(defparameter *node-target-color* (make-gray-color 3/4))
(defmethod display-node :around ((node call-node) stream)
  (updating-output (stream :unique-id node
			   :cache-value (node-tick node))
    (let ((dy (- (text-style-height *display-node-character-style* (sheet-medium stream)) 4))
	  (inferior-target (make-instance 'call-node-inferiors-target :call-node node))
	  (superior-target (make-instance 'call-node-superiors-target :call-node node)))
      (multiple-value-bind (cx cy) (stream-cursor-position stream)
	(incf cy)
	(with-output-as-presentation (stream inferior-target 'call-node-superiors-target
				      :single-box :position)
	  (draw-triangle* stream cx (+ cy (floor dy 2)) (+ cx dy) cy (+ cx dy) (+ cy dy)
			  :filled t :ink *node-target-color*))
	(stream-increment-cursor-position stream (+ dy 2) nil))
      (multiple-value-prog1
	  (call-next-method node stream)
	(multiple-value-bind (cx cy) (stream-cursor-position stream)
	  (incf cy)
	  (incf cx 2)
	  (with-output-as-presentation (stream superior-target 'call-node-inferiors-target
					:single-box :position)
	    (draw-triangle* stream cx cy cx (+ cy dy)  (+ cx dy) (+ cy (floor dy 2))
			    :filled t :ink *node-target-color*)))))))


;; CALL-SUBNODEs are a stripped-down version of CALL-NODEs, which mostly
;; means that most commands and translators don't operate on subnodes.
(defclass call-subnode (basic-call-node) ())

(defmethod node-any-inferior-objects-p ((node call-subnode) type)
  (declare (ignore type))
  nil)

(defmethod node-arc-drawer ((node call-subnode))
  (values #'draw-arrow-arc '(:line-dashes #(2 2))))


;; Ellipsis nodes
(defclass ellipsis-call-node
	  (basic-call-node)
  ((replaced-node :reader ellipsis-node-replaced-node :initarg :replaced-node)))

(declaim (inline make-ellipsis-call-node))
(defun make-ellipsis-call-node (object replaced-node)
  (make-instance 'ellipsis-call-node :object object :replaced-node replaced-node))

(defmethod initialize-instance :after ((node ellipsis-call-node) &rest init-options)
  (declare (ignore init-options))
  ;; The inferiors of the ellipsis node are all of the combined inferiors
  ;; of the node we are replacing
  (let ((inferiors nil))
    (dolist (inferior (node-inferiors (ellipsis-node-replaced-node node)))
      (setq inferiors (nconc inferiors (copy-list (node-inferiors inferior)))))
    (setf (node-inferiors node) inferiors))
  node)

(defmethod display-node ((node ellipsis-call-node) stream)
  (with-output-as-presentation (stream node 'ellipsis-call-node)
    ;;--- Could do better choosing a label for the node...
    (format stream "~A" (node-object-name (ellipsis-node-replaced-node node)))
    (write-string "..." stream)))


;;; The basic browser frame class

(define-application-frame browser-frame (selected-object-mixin)
    ((graph-type :initarg :graph-type :initform :graphical
		 :reader browser-graph-type)
     (browser-type :initarg :browser-type
		   :reader browser-type)
     (browser-subtype :initarg :browser-subtype
		      :accessor browser-subtype)
     (browser-options :initarg :browser-options :initform nil
		      :reader browser-options)
     (browser-ptype :initarg :presentation-type
		    :reader browser-presentation-type)
     (node-maker :initarg :node-maker
		 :reader browser-node-maker)
     (root-node-maker :initarg :root-node-maker
		      :reader browser-root-node-maker)
     (grapher-args :initform nil :accessor browser-grapher-args)
     (root-nodes :initform nil :accessor browser-root-nodes)
     (all-nodes :initform nil :accessor browser-nodes)
     (graph-record :initform nil :accessor browser-graph-record)
     (global-tick :initform 0 :accessor browser-global-tick)
     ;;--- The next two should go into some sort of control panel in each browser
     (tree-depth :initarg :browser-depth :initform 1
		 :accessor browser-tree-depth)
     (merge-duplicate-nodes :initarg :browser-merge-duplicates :initform t
			    :reader browser-merge-duplicate-nodes))
  (:command-definer define-browser-command))


;; Selections take place on the call node, so each browser has to extract
;; the relevant object by hand
(defmethod object-selectable-p ((frame browser-frame) object)
  (typep object 'call-node))

(defmethod select-object :before ((frame browser-frame) (node call-node)
                                  &optional presentation)
  (declare (ignore presentation))
  (tick-node node))

(defmethod add-object-to-selection :before ((frame browser-frame) (node call-node)
                                            &optional presentation)
  (declare (ignore presentation))
  (tick-node node))

(defmethod deselect-object :before ((frame browser-frame) (node call-node)
                                    &optional presentation)
  (declare (ignore presentation))
  (tick-node node))

(defmethod deselect-all-objects :before ((frame browser-frame))
  (dolist (entry (frame-selected-objects frame))
    (tick-node (car entry))))

(defmethod highlight-selected-object 
	   ((frame browser-frame) object (presentation standard-presentation) state)
  ;; Highlighting gets done during redisplay for this
  (declare (ignore state object))
  )


(defmethod invoke-with-frame-standard-output ((frame browser-frame) continuation)
  (declare (dynamic-extent continuation))
  (with-pop-up-window (stream frame)
    (let ((*standard-output* stream))
      (funcall continuation stream))))

(defgeneric display-graph-pane-1 (browser type stream))

(defvar *graph-displayer-types* nil)
(defmacro define-graph-displayer (type (browser stream) &body body)
  `(progn
     (pushnew ',type *graph-displayer-types*)
     (defmethod display-graph-pane-1 ((,browser browser-frame) (type (eql ',type)) ,stream)
       ,@body)))

(defun draw-browser-graph (root-nodes node-printer inferior-producer
			   &key (stream *standard-output*)
				(orientation :horizontal)
				(center-nodes nil)
				(merge-duplicates nil)
				(arc-drawer #'draw-line-arc)
				(arc-drawing-options nil)
				(generation-separation 
				  *default-generation-separation*)
				(within-generation-separation
				  *default-within-generation-separation*)
				node-filter)
  (flet ((inferior-producer (node)
	   (let ((inferiors (funcall inferior-producer node)))
	     (if (null node-filter)
	       inferiors
	       (delete-if-not node-filter inferiors)))))
    (declare (dynamic-extent #'inferior-producer))
    (format-graph-from-roots root-nodes node-printer #'inferior-producer
			     :graph-type :dag	;we eliminate circularities ourselves
			     :stream stream
			     :orientation orientation
			     :center-nodes center-nodes
			     :merge-duplicates merge-duplicates
			     :arc-drawer arc-drawer
			     :arc-drawing-options arc-drawing-options
			     :generation-separation generation-separation
			     :within-generation-separation within-generation-separation)))

(defparameter *draw-rectilinear-arcs* nil)

(defun draw-line-arc (stream from-node to-node x1 y1 x2 y2 &rest drawing-options)
  (declare (dynamic-extent drawing-options))
  (declare (ignore from-node to-node))
  (if *draw-rectilinear-arcs*
    (let* ((x0 x1)
	   (x1 (+ x1 5)))
      (clim-utils:with-stack-list (pts x0 y1 x1 y1
				       x1 y1 x1 y2
				       x1 y2 x2 y2)
	(apply #'draw-lines* stream pts drawing-options)))
    (apply #'draw-line* stream x1 y1 x2 y2 drawing-options)))

(defun draw-arrow-arc (stream from-node to-node x1 y1 x2 y2 &rest drawing-options)
  (declare (dynamic-extent drawing-options))
  (declare (ignore from-node to-node))
  (if *draw-rectilinear-arcs*
    (let* ((x0 x1)
	   (x1 (+ x1 5)))
      (clim-utils:with-stack-list (pts x0 y1 x1 y1
				       x1 y1 x1 y2)
	(apply #'draw-lines* stream pts drawing-options))
      (apply #'draw-arrow* stream x1 y2 x2 y2 drawing-options))
    (apply #'draw-arrow* stream x1 y1 x2 y2 drawing-options)))

(define-graph-displayer :graphical (browser stream)
  (let ((root-nodes (browser-root-nodes browser))
	(merge-duplicates (browser-merge-duplicate-nodes browser))
	(browser-options (browser-options browser))
	(grapher-args (browser-grapher-args browser)))
    (updating-output (stream :unique-id root-nodes
                             :cache-value (browser-global-tick browser))
      (setf (browser-graph-record browser)
            (apply #'draw-browser-graph root-nodes #'display-node #'node-inferiors
					:stream stream
				        :merge-duplicates merge-duplicates
				        :node-filter (getf browser-options :display-filter)
				        grapher-args)))))

(define-graph-displayer :textual (browser stream)
  (let ((root-nodes (browser-root-nodes browser)) 
	(browser-options (browser-options browser)))
    (setf (browser-graph-record browser) nil)
    (let ((filter (getf browser-options :display-filter)))
      (labels ((display (node stream indentation)
		 (when (or (null filter)
			   (funcall filter node))
		   (fresh-line stream)
		   (dotimes (i indentation)
		     (write-char #\Space stream))
		   (display-node node stream)
		   (dolist (inferior (node-inferiors node))
		     (display inferior stream (+ indentation 2))))))
	(declare (dynamic-extent #'display))
	(updating-output (stream :unique-id root-nodes
                                 :cache-value (browser-global-tick browser))
	  (with-end-of-page-action (stream :allow)
	    (with-end-of-line-action (stream :allow)
	      (dolist (node root-nodes)
		(display node stream 0)))))))))

(defmethod display-graph-pane ((browser browser-frame) stream)
  (when (browser-root-nodes browser)
    (display-graph-pane-1 browser (browser-graph-type browser) stream)))

(defmethod generate-call-graph ((browser browser-frame) nodes
				&optional (depth (browser-tree-depth browser)))
  (let ((subtype (browser-subtype browser)))
    (when nodes
      (let ((generated nil))
	(labels 
	  ((collect-inferiors (node parent-node depth)
	     ;; Disallow direct recursion in a simple-minded way
	     (when (and (plusp depth)
			(not (eql node parent-node)))
	       (let ((places nil)
		     (inferior-objects
		       (filter-generated-objects
			 browser (node-generate-inferior-objects node subtype))))
		 (when inferior-objects
		   (setq generated t)		;we generated something
		   ;; Use this hairy DO stuff to detect dotted lists
		   (do ((place inferior-objects (cdr place)))
		       (())
		     (when (null place) (return nil))
		     (let* ((object (if (consp place) (car place) place))
			    (inferior-node
			      (intern-node-for-object browser object)))
		       (cond ((member place places)
			      ;; Beware of circular lists
			      (setf (node-recurses node) t)
			      (return nil))
			     (t
			      (push place places)))
		       (cond ((node-recurses-p browser node inferior-node)
			      (setf (node-recurses node) t))
			     (t
			      (unless (member node (node-superiors inferior-node))
				(setf (node-superiors inferior-node)
				      (nconc (node-superiors inferior-node) (list node))))
			      (unless (member inferior-node (node-inferiors node)
					      :test #'node-eql)
				(setf (node-inferiors node)
				      (nconc (node-inferiors node) (list inferior-node))))))
		       ;; Recursively collect inferiors for these nodes
		       (collect-inferiors inferior-node node (1- depth))
		       (unless (consp place) (return nil)))))))))
	  (declare (dynamic-extent #'collect-inferiors))
	  (dolist (node nodes)
	    (collect-inferiors node nil depth)))
	generated))))

;; Find or intern a new node
(defmethod intern-node-for-object ((browser browser-frame) object &key (test #'eql))
  (let ((all-nodes (browser-nodes browser))
	(node-maker (browser-node-maker browser)))
    (dolist (node all-nodes)
      (when (funcall test object (node-object node))
	(return-from intern-node-for-object node)))
    (let ((node (funcall node-maker object)))
      (setf (browser-nodes browser) (nconc all-nodes (list node)))
      node)))

;; Find an old node
(defmethod find-node-for-object ((browser browser-frame) object &key (test #'eql))
  (let ((all-nodes (browser-nodes browser)))
    (dolist (node all-nodes)
      (when (funcall test object (node-object node))
	(return-from find-node-for-object node)))
    nil))

(defmethod node-recurses-p ((browser browser-frame) node inferior)
  (let ((all-nodes (browser-nodes browser)))
    (or (eql node inferior)		;quick test often succeeds
	(let ((mark-table 
	       #+Genera (scl:make-hash-table :size (length all-nodes) :number-of-values 0)
	       #-Genera (make-hash-table :size (length all-nodes))))
	  (labels ((recurses (inferiors)
		     (when (member node inferiors)
		       (return-from node-recurses-p t))
		     (dolist (inferior inferiors)
		       (unless (gethash inferior mark-table)
			 (setf (gethash inferior mark-table) t)
			 (recurses (node-inferiors inferior))))))
	    (declare (dynamic-extent #'recurses))
	    (setf (gethash node mark-table) t)
	    (setf (gethash inferior mark-table) t)
	    (recurses (node-inferiors inferior))
	    nil)))))

;; Given a generator, create the call graph for a node.
(defmethod filter-generated-objects ((browser browser-frame) objects)
  (let ((predicate (getf (browser-options browser) :object-filter)))
    (if (null predicate)
      objects
      (delete-if-not predicate objects))))

(defmethod generate-upward-call-graph ((browser browser-frame) inferior-node 
                                       &optional (depth (browser-tree-depth browser)))
  (declare (ignore depth))
  (let* ((subtype (browser-subtype browser))
         (superior-objects
	  (filter-generated-objects
	    browser (node-generate-superior-objects inferior-node subtype))))
    (when superior-objects
      (dolist (object superior-objects)
	(let ((node (intern-node-for-object browser object)))
	  (cond ((node-recurses-p browser node inferior-node)
		 (setf (node-recurses node) t))
		(t
		 (unless (member node (node-superiors inferior-node))
		   (setf (node-superiors inferior-node)
			 (nconc (node-superiors inferior-node) (list node))))
		 (unless (member inferior-node (node-inferiors node)
				 :test #'node-eql)
		   (setf (node-inferiors node)
			 (nconc (node-inferiors node) (list inferior-node))))))))
      (recompute-root-nodes browser)
      t)))

(defmethod recompute-root-nodes ((browser browser-frame))
  (let ((all-nodes (browser-nodes browser))
	(root-nodes (browser-root-nodes browser)))
    (loop for node in all-nodes
	  when (null (node-superiors node))
	    collect node into new-roots
	  finally (unless (node-set-equal new-roots root-nodes)
		    (setf (browser-root-nodes browser) new-roots)))))

(defun node-set-equal (nodes1 nodes2)
  (let ((l1 (length nodes1))
	(l2 (length nodes2)))
    (and (= l1 l2)
	 (let ((mark-table
		#+Genera (scl:make-hash-table :size l1 :number-of-values 0)
		#-Genera (make-hash-table :size l1)))
	   (dolist (node nodes1)
	     (setf (gethash (node-object node) mark-table) t))
	   (every #'(lambda (node) (gethash (node-object node) mark-table))
		  nodes2)))))


;;; Browser commands

(define-browser-command com-show-graph
    ((objects (let ((ptype (browser-presentation-type *application-frame*)))
		`(or ,ptype (sequence ,ptype) call-node))
	      :default nil))
  (with-application-frame (browser)
    (generate-browser-display browser objects (browser-display-pane browser))
    (note-browser-root-changed browser objects)))

;; Expand the set of root nodes to all levels
(define-browser-command com-expand-all ()
  (with-application-frame (browser)
    (let ((objects (mapcar #'node-object (browser-root-nodes browser))))
      ;; Gee, I hope 10,000 is deep enough...
      (clim-utils:letf-globally (((browser-tree-depth browser) 10000))
        (generate-browser-display browser objects (browser-display-pane browser))
        (note-browser-root-changed browser objects)))))

(defmethod note-browser-root-changed ((browser browser-frame) objects)
  (declare (ignore objects))
  nil)

(defmethod generate-browser-display ((browser browser-frame) objects stream)
  ;; Bind *ORIGINAL-STREAM* to NIL so that this can get called from
  ;; inside of accept-values panes.  Yetch!
  (let ((*original-stream* nil))
    (let ((root-node-maker (browser-root-node-maker browser)))
      (if (typep objects 'call-node)
	(setf (browser-root-nodes browser)
	      (list (funcall root-node-maker (node-object objects))))
	(loop for object in (if (atom objects) (list objects) objects)
	      as new-node = (funcall root-node-maker object)
	      when new-node
	        collect new-node into new-root-nodes
	      finally (setf (browser-root-nodes browser) new-root-nodes))))
    ;; ALL-NODES and ROOT-NODES must not be EQ lists...
    (setf (browser-nodes browser) (copy-list (browser-root-nodes browser)))
    (window-clear stream)
    (generate-call-graph browser (browser-root-nodes browser))
    (redisplay-frame-pane browser stream :force-p t)))

(define-presentation-to-command-translator show-graph
    (call-node com-show-graph browser-frame
     :gesture :show-graph
     :tester ((object)
	      (presentation-typep
		(node-object object) (browser-presentation-type *application-frame*))))
    (object)
  ;; Show Graph expects a sequence of root objects...
  (list (list (node-object object))))


(define-command-table browser-activity
  :inherit-from (activity) :inherit-menu t)

(defgeneric browser-display-pane (browser))

;; Used by the Redisplay command in the Activity command table
(defmethod frame-redisplay ((browser browser-frame))
  (redisplay-frame-pane browser (browser-display-pane browser) :force-p t))

(define-command (com-decache :command-table browser-activity) ()
  (with-application-frame (browser)
    (setf (browser-nodes browser) nil
	  (browser-root-nodes browser) nil)
    (window-clear (browser-display-pane browser))))

(define-command (com-hardcopy-graph :name t :command-table browser-activity)
    (&key
     (file 'pathname
	   :default (make-pathname :name "GRAPH-HARDCOPY"
				   :type "PS"
				   :defaults (user-homedir-pathname)
				   #+ansi-90 :case #+ansi-90 :common)
	   :documentation "File in which to put the PostScript result")
     (orientation '(member :landscape :portrait)
		  :default :portrait
		  :documentation "Orientation to use on the paper")
     (scale-to-fit 'boolean
		   :default nil :mentioned-default t
		   :documentation "Scale the graph to fit on a single page"))
  (with-application-frame (browser)
    (flet ((hardcopy (fs)
	     (with-output-to-postscript-stream (stream fs 
						:orientation orientation
						:scale-to-fit scale-to-fit
						:multi-page (not scale-to-fit))
	       (terpri stream)
	       (terpri stream)
	       (display-graph-pane browser stream))))
      (declare (dynamic-extent #'hardcopy))
      (with-open-file (fs file :direction :output)
	(hardcopy fs)))))


(define-browser-command com-show-node-inferiors
    ((node 'call-node :prompt "node to show inferiors for"))
  (with-application-frame (browser)
    (when (generate-call-graph browser (list node) 1)
      (tick-node node)
      (redisplay-frame-pane browser (browser-display-pane browser)))))

(define-presentation-to-command-translator show-node-inferiors
   (call-node-inferiors-target com-show-node-inferiors browser-frame
    :gesture :select
    :tester ((object)
             (let ((node (target-call-node object)))
               (and (null (node-inferiors node))
	            (node-any-inferior-objects-p 
	              node (browser-subtype *application-frame*)))))
    :echo nil :maintain-history nil)
   (object)
  (let ((node (target-call-node object)))
    (list node)))

(define-browser-command com-hide-node-inferiors
    ((node 'call-node :prompt "node to hide inferiors of"))
  (with-application-frame (browser)
    (when (node-inferiors node)
      (setf (node-inferiors node) nil)
      (tick-node node)
      (redisplay-frame-pane browser (browser-display-pane browser)))))

(define-presentation-to-command-translator hide-node-inferiors
    (call-node-inferiors-target com-hide-node-inferiors browser-frame
     :gesture :select
     :tester ((object)
              (let ((node (target-call-node object)))
		(not (null (node-inferiors node)))))
     :echo nil :maintain-history nil)
    (object)
  (let ((node (target-call-node object)))
    (list node)))

(define-browser-command com-delete-node
    ((node 'call-node :prompt "node to delete"))
  (with-application-frame (browser)
    (when (node-superiors node)
      (dolist (superior (node-superiors node))
        (setf (node-inferiors superior) (delete node (node-inferiors superior))))
      (tick-node node)
      (redisplay-frame-pane browser (browser-display-pane browser)))))

(define-presentation-to-command-translator delete-node
    (call-node com-delete-node browser-frame
     :gesture :delete
     :tester ((object) (and (null (node-inferiors object))
			    (not (null (node-superiors object)))))
     :echo nil :maintain-history nil)
    (object)
  (list object))

(define-browser-command com-show-node-superiors
    ((node 'call-node :prompt "node to show superiors for"))
  (with-application-frame (browser)
    (when (generate-upward-call-graph browser node 1)
      (tick-node node)
      (redisplay-frame-pane browser (browser-display-pane browser)))))

(define-presentation-to-command-translator show-node-superiors
    (call-node-superiors-target com-show-node-superiors browser-frame
     :gesture :select
     :tester ((object)
              (let ((node (target-call-node object)))
                (node-any-superior-objects-p 
		  node (browser-subtype *application-frame*))))
     :echo nil :maintain-history nil)
    (object)
  (let ((node (target-call-node object)))
    (list node)))

;; Given a node, replace it by another one which contains the inferiors of
;; all of its inferiors (i.e., splice out the intermediates so that the
;; graph takes up less space).
(define-browser-command com-ellipsize-node
    ((node 'call-node :prompt "node to ellipsize"))
  (with-application-frame (browser)
    (let ((all-nodes (browser-nodes browser))
	  (ellipsis (make-ellipsis-call-node (node-object-name node) node)))
      ;; Dike out all references to the ellipsized node
      (dolist (n all-nodes)
        (nsubstitute ellipsis node (node-inferiors n)))
      (nsubstitute ellipsis node all-nodes)
      (incf (node-tick node))
      (tick-node ellipsis)
      (redisplay-frame-pane browser (browser-display-pane browser)))))

(define-presentation-to-command-translator ellipsize-node
    ((or call-node ellipsis-call-node) com-ellipsize-node browser-frame
     :gesture :delete
     :echo nil :maintain-history nil)
    (object)
  (list object))

(define-browser-command com-unellipsize-node
    ((ellipsis 'ellipsis-call-node 
	       :prompt "node to unellipsize"
	       :gesture :select))
  (with-application-frame (browser)
    (let ((all-nodes (browser-nodes browser))
	  (node (ellipsis-node-replaced-node ellipsis)))
      ;; Replace all references to the ellipsized node
      (dolist (n all-nodes)
        (nsubstitute node ellipsis (node-inferiors n)))
      (nsubstitute node ellipsis all-nodes)
      (tick-node node)
      (redisplay-frame-pane browser (browser-display-pane browser)))))


;;; Less general browser commands, mostly pertaining to subnodes

(define-browser-command com-remove-subnode
    ((subnode 'call-subnode 
	      :prompt "subnode to remove"
	      :gesture :delete))
  (with-application-frame (browser)
    (let ((node (first (node-superiors subnode))))
      (setf (node-inferiors node) (delete subnode (node-inferiors node)))
      (setf (browser-nodes browser) (delete subnode (browser-nodes browser)))
      (tick-node subnode)
      (redisplay-frame-pane browser (browser-display-pane browser)))))

(defun subnode-object-present-in-node (node object &key (test #'eql))
  (dolist (subnode (node-inferiors node))
    (when (and (typep subnode 'call-subnode)
	       (funcall test object (node-object subnode)))
      (return-from subnode-object-present-in-node t)))
  nil)


;;; Graph overviews

;; Generates a bunch of simple edges that connect each node to its
;; children at their centers, and calls FUNCTION on the four coordinates
;; that represent the endpoints of the edge.
(defmethod map-over-graph-edges
    (function (graph clim-internals::basic-graph-output-record))
  (declare (dynamic-extent function))
  (let ((stream (clim-internals::output-record-stream graph)))
    (multiple-value-bind (xoff yoff)
	(convert-from-relative-to-absolute-coordinates
	  stream (output-record-parent graph))
      (dolist (root-node (graph-root-nodes graph))
	(labels ((node-position (node)
		   (bounding-rectangle-center* node))
		 (map-edges (parent)
		   (dolist (child (graph-node-children parent))
		     (when (graph-node-children child)
		       (map-edges child))
		     (multiple-value-bind (px py) (node-position parent)
		       (multiple-value-bind (cx cy) (node-position child)
			 (translate-coordinates xoff yoff px py cx cy)
			 (funcall function px py cx cy))))))
	  (declare (dynamic-extent #'node-position #'map-edges))
	  (map-edges root-node))))))

;; Draws an overview of the graph, which simply consists of all of the
;; node-centered edges scaled to fit in the specified area.
(defmethod draw-graph-overview
    ((graph clim-internals::basic-graph-output-record) stream 
     &rest drawing-options
     &key width height show-viewport (filled nil) &allow-other-keys)
  (declare (dynamic-extent drawing-options))
  (when (or (null width) (null height))
    (multiple-value-setq (width height)
      (bounding-rectangle-size (window-viewport stream))))
  (multiple-value-bind (graph-width graph-height)
      (bounding-rectangle-size graph)
    (let ((x-scale (/ width graph-width))
	  (y-scale (/ height graph-height)))
      (with-room-for-graphics (stream :first-quadrant nil)
	(with-scaling (stream x-scale y-scale)
          (when show-viewport
	    (let ((gstream (clim-internals::output-record-stream graph)))
	      (multiple-value-bind (vwidth vheight)
		  (bounding-rectangle-size (window-viewport gstream))
		(multiple-value-bind (vx vy) (window-viewport-position gstream)
		  (clim-utils:with-keywords-removed
		      (drawing-options drawing-options 
		       '(:show-viewport :width :height :filled))
		    (apply #'draw-rectangle* stream vx vy (+ vx vwidth) (+ vy vheight)
			   :filled filled drawing-options))))))
	  (map-over-graph-edges
	    #'(lambda (x1 y1 x2 y2)
		(draw-line* stream x1 y1 x2 y2))
	    graph))))))

(defparameter *viewport-highlighting-ink* +yellow+)
(define-command (com-show-graph-overview :command-table browser-activity) ()
  (with-application-frame (browser)
    (let* ((graph-pane (browser-display-pane browser))
           (graph (browser-graph-record browser))
           (width (floor (window-inside-width graph-pane) 2))
           (height (floor (window-inside-height graph-pane) 2)))
      (with-pop-up-window (stream browser 
			   :scroll-bars nil :width width :height height)
        (draw-graph-overview graph stream 
                             :width width :height height
                             :show-viewport t
                             :filled t :ink *viewport-highlighting-ink*)))))


;; Fix up the Browser's Activity menu
(add-menu-item-to-command-table
  'browser-activity "Overview" 
  :command '(com-show-graph-overview)
  :errorp nil
  :after :start)

(add-menu-item-to-command-table
  'browser-activity "Decache" :command '(com-decache)
  :errorp nil
  :after "Overview")

(add-menu-item-to-command-table
  'browser-activity "Expand All" :command '(com-expand-all)
  :errorp nil
  :after "Decache")

(add-menu-item-to-command-table
  'browser-activity "Hardcopy" 
  :function #'(lambda (gesture arg)
		gesture arg
		(let ((pathname (select-file *application-frame*)))
		  (when pathname
		    `(com-hardcopy-graph :file ,pathname))))
  :errorp nil
  :after "Expand All")

(add-menu-item-to-command-table
  'browser-activity "browser-divider" 
  :divider nil
  :errorp nil
  :after "Hardcopy")

