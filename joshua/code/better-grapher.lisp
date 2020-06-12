;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics Inc.  All rights reserved.
;;; Portions copyright (c) 1992 Franz, Inc.  All rights reserved."

#|

;;; Here just for easy reference
(defclass standard-graph-node-output-record
          (standard-sequence-output-record graph-node-output-record)
    ((generation :accessor graph-node-generation :initform 0)
     ;; The output records corresponding to this node's parents and children
     (node-children :accessor graph-node-children :initform nil)
     (node-parents :accessor graph-node-parents :initform nil)
     (object :accessor graph-node-object :initarg :object))
    (:default-initargs :size 5))

|#


(defclass my-generation-descriptor ()
  ((generation :accessor my-generation-generation :initarg :generation)
   (breadth :accessor my-generation-breadth :initform 0)
   (depth :accessor my-generation-depth :initform 0)
   (start-depth :accessor my-generation-start-depth :initform nil)
   (size :accessor my-generation-size :initform 0)
   (breadth-so-far :accessor my-generation-breadth-so-far :initarg :breadth-so-far)
   (inner-breadth-separation :accessor my-generation-inner-breadth-separation :initform nil)
   (edge-breadth-separation :accessor my-generation-edge-breadth-separation :initform nil)
   (touched :accessor my-generation-touched :initform nil)
   (vector :accessor my-generation-vector :initform (make-array 5 :adjustable t :fill-pointer 0)))
  )

(defclass my-graph-output-record (directed-graph-output-record)
  ()
  )

(define-graph-type :my-graph my-graph-output-record)

;; The basic graph node output record class

(defclass my-graph-node-output-record
          (standard-graph-node-output-record)
  ((generation-position :accessor graph-node-generation-position :initform 0
			:documentation "Position in the generation bredth-wise")
   (center :accessor graph-node-center :initarg :center :initform 0
	   :documentation "The center of the node.  Updated without moving the record")
   (breadth :accessor graph-node-breadth :initarg :breadth :initform 0
	    :documentation "Includes one half of the within-generation-spacing")
   (cost :accessor graph-node-cost :initform 0
	 :documentation "Distance from center of this node to its optimal position")
   (in-queue :accessor graph-node-in-queue :initform nil
	     :documentation "Is this guy currently in the queue")
   ))

;;; A data structure to hold all the global state we need

(defclass graph-layout-context ()
  ((generation-descriptors :accessor generation-descriptors :initarg :generation-descriptors)
   ;; (generation-layers :accessor generation-layers :initarg :generation-layers)
   (max-breadth :accessor max-breadth :initarg :max-breadth)
   (broadest-generation :accessor broadest-generation)
   (within-generation-separation :accessor within-generation-separation :initarg :within-generation-separation)
   (generation-separation :accessor generation-separation :initarg :generation-separation)
   (Priority-queue :accessor priority-queue :initarg :priority-queue)
   ))


;;; The only reason that this is here is so that the children nodes are of the right class
;;; Seems like there should be a better way to do this than to replicate the code
;;; with such a small difference.
(defmethod generate-graph-nodes ((graph my-graph-output-record) stream
                                 root-objects object-printer inferior-producer
                                 &key duplicate-key duplicate-test
                                      offpage-connector-out-printer
                                      offpage-connector-in-printer)
  (declare (dynamic-extent object-printer inferior-producer))
  (with-slots (n-generations) graph
    (let* ((hash-table (slot-value graph 'hash-table))
           (graph-type (slot-value graph 'graph-type))
           (properties (slot-value graph 'properties))
           (cutoff-depth (getf properties :cutoff-depth))
           (maximize-generations (getf properties :maximize-generations))
           (root-nodes nil)
           (connectors-in nil))
      (labels ((inferior-mapper (function node)
                 (map nil function (funcall inferior-producer node)))
               (new-node-function (parent-object parent-record child-object)
                 (let ((child-record
                         (with-stream-cursor-position-saved (stream)
                           (with-new-output-record
                               (stream 'my-graph-node-output-record nil
                                :object child-object
                                :duplicate-key duplicate-key
                                :duplicate-test duplicate-test)
                             (funcall object-printer child-object stream)))))
                   ;; This guarantees that the next phase will have at least one
                   ;; node from which to start.  Otherwise the entire graph gets
                   ;; lost.  If the first node isn't really a root, it will be
                   ;; deleted from the set of roots when the cycle is
                   ;; detected.
                   (when (null root-nodes)
                     (push child-record root-nodes))
                   (old-node-function parent-object parent-record child-object child-record)))
               (old-node-function (parent-object parent-record child-object child-record)
                 (declare (ignore parent-object child-object))
                 (unless maximize-generations
                   (let ((old-generation (graph-node-generation child-record)))
                     ;; Set the generation of this node to 1 greater than the parent,
                     ;; and keep track of the highest generation encountered.
                     (maxf n-generations
                           (maxf (graph-node-generation child-record)
                                 (if parent-record
                                     (1+ (graph-node-generation parent-record))
                                     0)))
                     ;; If the child-record got its generation adjusted, then we must
                     ;; adjust the generation-number of already-processed children,
                     ;; and their children, etc.
                     (unless (eql (graph-node-generation child-record) old-generation)
                       (increment-generation child-record))))
                 ;; Preserve the ordering of the nodes.  Generation numbers are
                 ;; computed later in the case when MAXIMIZE-GENERATIONS is T.
                 (when parent-record
                   (unless (member parent-record (graph-node-parents child-record))
                     (setf (graph-node-parents child-record)
                           (nconc (graph-node-parents child-record)
                                  (list parent-record))))
                   (unless (member child-record (graph-node-children parent-record))
                     (setf (graph-node-children parent-record)
                           (nconc (graph-node-children parent-record)
                                  (list child-record)))))
                 child-record)
               (increment-generation (record)
                 (let ((new-generation (1+ (graph-node-generation record))))
                   (dolist (child (graph-node-children record))
                     ;; Remember which generation the child belonged to.
                     (let ((old-generation (graph-node-generation child)))
                       (maxf n-generations
                             (maxf (graph-node-generation child) new-generation))
                       ;; If it has changed, fix up the next generation recursively.
                       (unless (eql (graph-node-generation child) old-generation)
                         (increment-generation child)))))))
        (declare (dynamic-extent #'inferior-mapper #'increment-generation
                                 #'new-node-function #'old-node-function))
        (traverse-graph root-objects #'inferior-mapper
                        hash-table duplicate-key
                        #'new-node-function #'old-node-function
                        cutoff-depth))
      (map-node-table #'(lambda (key node)
                          (declare (ignore key))
                          (when (and (typep node 'graph-node-output-record)
                                     (null (graph-node-parents node)))
                            (pushnew node root-nodes)))
                      hash-table)
      (when (and (member graph-type '(:directed-graph :digraph))
                 offpage-connector-in-printer offpage-connector-out-printer)
        (labels
          ((break-cycles (node &optional path)
             (do ((c (graph-node-children node) (cdr c)))
                 ((null c))
               (let ((child (car c)))
                 (if (member child path)
                     (let* ((id (connector-id
                                  (or (find child connectors-in
                                            :key #'(lambda (x)
                                                     (car (graph-node-children x))))
                                      (let* ((id (1+ (length connectors-in)))
                                             (rec (with-new-output-record
                                                      (stream 'graph-node-connector-in-output-record nil
                                                              :object
                                                              (make-instance 'grapher-fake-object
                                                                             :data `(:in-connector ,node ,child))
                                                       :connector-id id)
                                                    (funcall offpage-connector-in-printer
                                                             stream id))))
                                        (push rec connectors-in)
                                        (setf (graph-node-children rec) (list child))
                                        ;;--- What about ordering?
                                        (push rec (graph-node-parents child))
                                        (setf root-nodes (delete child root-nodes))
                                        rec)))))
                       (setf (car c)
                             (with-new-output-record
                                 (stream 'graph-node-connector-out-output-record nil
                                         :object (make-instance 'grapher-fake-object
                                                  :data `(:out-connector ,node ,child))
                                  :connector-id id)
                               (funcall offpage-connector-out-printer stream id))))
                     (break-cycles child (cons node path)))))))
          (declare (dynamic-extent #'break-cycles))
          (map nil #'break-cycles (copy-list root-nodes))))
      (setf (slot-value graph 'root-nodes)
            (nconc (nreverse connectors-in) (nreverse root-nodes)))
      (when maximize-generations
        (compute-graph-effective-generations graph)
        (add-graph-filler-output-records graph stream))))
  graph)

(defmethod layout-graph-nodes ((graph my-graph-output-record) stream
                               arc-drawer arc-drawing-options)
  ;; Maximize generations is an Allegro special that I've diked
  ;; out and it's the only reason that these are provided
  (declare (ignore arc-drawer arc-drawing-options stream))
  (with-slots (root-nodes hash-table n-generations) graph
    (when root-nodes
      (let* ((properties (slot-value graph 'properties))
             (orientation (getf properties :orientation))
             (center-nodes (getf properties :center-nodes))
             (generation-separation (getf properties :generation-separation))
             (within-generation-separation (getf properties :within-generation-separation))
             (start-x (coordinate 0))
             (start-y (coordinate 0)))
        (flet ((inferior-mapper (function node)
                 (map nil function (graph-node-children node)))
               (yx-output-record-set-position (record y x)
                 (output-record-set-position record x y)))
          (declare (dynamic-extent #'inferior-mapper #'yx-output-record-set-position))
	  ;; The spec only includes :horizontal and :vertical
	  ;; The Allegro code involves permissable extensions :up :down :left :right
	  ;; But I'm not supporting those (yet).
          (multiple-value-bind (breadthfun depthfun set-positionfun start-breadth start-depth)
              (ecase orientation
                (:vertical
                 (values #'bounding-rectangle-width #'bounding-rectangle-height
                         #'output-record-set-position start-x start-y))
                (:horizontal
                 (values #'bounding-rectangle-height #'bounding-rectangle-width
                         #'yx-output-record-set-position start-y start-x)))
	    (let* ((layout-context (make-layout-context n-generations start-breadth within-generation-separation generation-separation)))
	      ;; map-over the object hierarchy putting obects into their slots in the layout context
	      ;; and computing all the relevant derived data
	      (initialize-layout-context layout-context graph root-nodes #'inferior-mapper hash-table breadthfun depthfun start-depth)
	      ;; Now that we have a reasonable initial placement, we're going to optimze the placement
	      ;; using hill-climbing.  The metric for each node is the horizontal distance from its center
	      ;; to the center of its children.  We use a priority-queue (a logarithmic heap) driven by that
	      ;; metric to pick the node that has the greatest potential for movement.
	      ;; For that guy we either move him left or right so that he's directly above the center of his children
	      ;; unless his neighbor in that direction is currently placed to block that move.  If so we only go as far
	      ;; as the right of that guy plus the within-generation-spacing.  Then we requeue the guy to this guy's right
	      ;; to see allow it another shot at moving closer to where it want to be.  We also requeue the parent of the guy
	      ;; who moved.  We keep track of whether a node is already in the queue to avoid the expense of trying to remove an
	      ;; node that isn't in the queue.  Before enqueuing we recalculate its metric.
	      (improve-placements layout-context)
	      ;; Everything is now in its best position, so update the graph-nodes bounding-boxes
	      ;; might want to do this by mapping over the generation descriptors rather than traversing again
	      (loop for generation-descriptor across (generation-descriptors layout-context)
		  for vector = (my-generation-vector generation-descriptor)
		  do (loop for node across vector
			 for depth = (funcall depthfun node)
			 do (funcall set-positionfun
				     node
				     (- (graph-node-center node) (round (graph-node-breadth node) 2))
				     (if center-nodes
                                        (+ (my-generation-start-depth generation-descriptor)
                                           (floor (- (my-generation-depth generation-descriptor) depth) 2))
				       (my-generation-start-depth generation-descriptor))))))
	      ))))))


;;; This simply sets up the data structures that we will be using
;;; Including the generation descriptors and and the vector for each generation descriptor
;;; (could be merged into the generation-descriptor)
(defun make-layout-context (n-generations start-breadth within-generation-separation generation-separation)
  (let* ((generation-descriptors (make-array (1+ n-generations)))
	 ;; (generation-layers (make-array n-generations))
	 (layout-context (make-instance 'graph-layout-context
			   :within-generation-separation within-generation-separation
			   :generation-separation generation-separation
			   :generation-descriptors generation-descriptors
			   ;; :generation-layers generation-layers
			   )))
    (loop for generation upto n-generations
	do (setf (aref generation-descriptors generation) (make-instance 'my-generation-descriptor
							   :generation generation
							   :breadth-so-far start-breadth)
		 ;; (aref generation-layers generation) (make-array 1 :adjustable t :fill-pointer 0))
		 ))
    layout-context
    ))

;;; This passes over the output-record hierarchy putting information
;;; into the layout context data structures
(defun initialize-layout-context (layout-context graph root-nodes inferior-mapper hash-table breadthfun depthfun start-depth)
  (macrolet ((traverse (new-node-function &optional (old-node-function '#'false))
	       `(traverse-graph root-nodes inferior-mapper
				hash-table #'identity
				,new-node-function ,old-node-function))
	     ;; "Breadth" is the width in vertical orientation, otherwise
	     ;; it's the height.  "Depth" is vice-versa.
	     (breadth (node) `(funcall breadthfun ,node))
	     (depth (node) `(funcall depthfun ,node)))
    ;; Determine the breadth and depth of each generation
    (with-slots (generation-descriptors within-generation-separation) layout-context
      ;; map the hierarchy, putting each guy into its layer
      ;; and updating related data structures
      (flet ((collect-node-size (p ph child-node)
	       (declare (ignore p ph))
	       (let* ((generation-number (graph-node-generation child-node))
		      (descr (aref generation-descriptors generation-number))
		      (layer (my-generation-vector descr))
		      (generation-position (fill-pointer layer))
		      (breadth (breadth child-node)))
		 (vector-push-extend child-node layer)
		 (setf (graph-node-generation child-node) generation-number
		       (graph-node-generation-position child-node) generation-position
		       (graph-node-breadth child-node) breadth)
		 (incf (my-generation-size descr))
		 (incf (my-generation-breadth descr) breadth)
		 (maxf (my-generation-depth descr) (depth child-node))
		 )))
	(declare (dynamic-extent #'collect-node-size))
	(traverse #'collect-node-size)
	(compute-max-breadth-and-spacing layout-context start-depth)
	(do-initial-placement graph layout-context)))))

;;; When this is called every node is in its layer
;;; we know the half-breadth of each node
;;; we know the cumulative breadth of each layer
(defun compute-max-breadth-and-spacing (layout-context start-depth)
  (with-slots (generation-descriptors within-generation-separation generation-separation) layout-context
    (let ((max-gen-breadth 0)
	  (broadest-gen-descr nil))
    ;; Determine max-breadth and starting-depth
      (loop with depth-so-far = start-depth
	  for descr across generation-descriptors
	  for gen-breadth = (my-generation-breadth descr)
	  when (> gen-breadth max-gen-breadth)
	  do (setf max-gen-breadth gen-breadth broadest-gen-descr descr)
	  do (setf (my-generation-start-depth descr) depth-so-far)
	     (incf depth-so-far (+ generation-separation (my-generation-depth descr))))
    ;; Determine breadth-spacing
      (incf max-gen-breadth (* within-generation-separation (my-generation-size broadest-gen-descr)))
      (setf (max-breadth layout-context) max-gen-breadth
	    (broadest-generation layout-context) (my-generation-generation broadest-gen-descr))
      (loop for descr across generation-descriptors
	  for excess = (floor (- max-gen-breadth (my-generation-breadth descr))
			      (max (my-generation-size descr) 1))
	  do (setf (my-generation-inner-breadth-separation descr) excess)
	     (setf (my-generation-edge-breadth-separation descr) (floor excess 2))))))

(defmethod do-initial-placement ((graph my-graph-output-record) (layout-context graph-layout-context))
  ;; At this point:
  ;; we have everybody placed in generations
  ;; we know the breadth of each generation
  ;; we know the maximum breadth and which generation has that breadth
  ;; for each other generation we know the excess room available to that generatoin
  ;; Next we're going to "place" each node (really just set its center)
  ;; So that all the nodes in that generation are spaced apart by the intra-generation spacing
  ;; for that layer. With half of the intra-generation-spacing being used at each edge
  (loop for generation-descriptor across (generation-descriptors layout-context)
      for layer = (my-generation-vector generation-descriptor)
      for generation-inner-spacing = (my-generation-inner-breadth-separation generation-descriptor)
      do (loop for breadth-so-far = (my-generation-edge-breadth-separation generation-descriptor) then next-breadth-so-far
	     for node across layer
	     for his-breadth = (graph-node-breadth node)
	     for his-center = (+ breadth-so-far (round his-breadth 2))
	     for next-breadth-so-far = (+ breadth-so-far his-breadth generation-inner-spacing)
	     do (setf (graph-node-center node) his-center)
		)))

;;; here we use edges based on center (placement-left or right)
;;; rather than the bounding-box edges
;;; because I'm not actually moving the node until after the
;;; optimization pass but just updating center
;;; Also "left" and "right" are fibs if we're using horizontal orientation
;;; its actually top and bottom.  These are min and max in the breadth direction.

(defmethod left-neighbor ((node my-graph-node-output-record) (layout-context graph-layout-context))
  (with-slots (generation-descriptors) layout-context
    (with-slots (generation-position generation) node
      (let ((generation-vector (my-generation-vector (aref generation-descriptors generation)))
	    (neighbor-position (1- generation-position)))
	(when (>= neighbor-position 0)
	  (aref generation-vector neighbor-position))))))

(defmethod right-neighbor ((node my-graph-node-output-record) (layout-context graph-layout-context))
  (with-slots (generation-descriptors) layout-context
    (with-slots (generation-position generation) node
      (let ((generation-vector (my-generation-vector (aref generation-descriptors generation)))
	    (neighbor-position (1+ generation-position)))
	(when (< neighbor-position (length generation-vector))
	  (aref generation-vector neighbor-position))))))

(defmethod placement-left ((node my-graph-node-output-record))
  (- (graph-node-center node) (round (graph-node-breadth node) 2)))

(defmethod placement-right ((node my-graph-node-output-record))
  (+ (graph-node-center node) (round (graph-node-breadth node) 2)))

;;; These are used to calculate a score for enqueuing
;;; And then on dequeueing we recompute it since the children
;;; might have moved between the enqueueing and dequeueing
;;; The optimal placement is for my center to be directly
;;; above the center of the space spanned by my children
(defmethod optimal-placement ((node my-graph-node-output-record))
  (loop for child in (graph-node-children node)
	minimize (placement-left child) into left
	maximize (placement-right child) into right
      finally (return (round (+ left right) 2))))

;;; Use for enqueing in which we don't care about the sign of
;;; score
(defmethod placement-quality ((node my-graph-node-output-record))
  (abs (desired-move node)))

(defmethod desired-move ((node my-graph-node-output-record))
  (- (optimal-placement node) (graph-node-center node)))

;;; Used in analyzing the possible move.  You want to move to the
;;; optimal placement but you can't move to cover the neighbor in that direction
(defmethod room-to-left ((node my-graph-node-output-record) (layout-context graph-layout-context))
  (with-slots (within-generation-separation) layout-context
    (let ((left-neighbor (left-neighbor node layout-context))
	  (my-left-edge (placement-left node)))
      (if left-neighbor
	  (let ((his-right-edge (+ (placement-right left-neighbor) within-generation-separation)))
	    (max 0 (- my-left-edge his-right-edge)))
	my-left-edge))))

(defmethod room-to-right ((node my-graph-node-output-record) (layout-context graph-layout-context))
  (with-slots (within-generation-separation max-breadth) layout-context
    (let ((right-neighbor (right-neighbor node layout-context))
	  (my-right-edge (placement-right node)))
      (if right-neighbor
	  (let ((his-left-edge (- (placement-left right-neighbor) within-generation-separation)))
	    (max 0 (- his-left-edge my-right-edge)))
	(- max-breadth my-right-edge)))))

(defmethod move-to-left ((node my-graph-node-output-record) (layout-context graph-layout-context) desired-amount)
  (with-slots (priority-queue) layout-context
    (let* ((room-to-left (room-to-left node layout-context))
	   (possible-amount (min desired-amount room-to-left)))
      (when (> possible-amount 0)
	(setf (graph-node-center node)
	  (- (graph-node-center node) possible-amount))
	(let ((right-neighbor (right-neighbor node layout-context)))
	  (when right-neighbor
	    (enqueue right-neighbor priority-queue)))
	(loop for parent in (graph-node-parents node)
	    do (enqueue parent priority-queue))))))

(defmethod move-to-right ((node my-graph-node-output-record) (layout-context graph-layout-context) desired-amount)
  (with-slots (priority-queue) layout-context
    (let* ((room-to-right (room-to-right node layout-context))
	   (possible-amount (min desired-amount room-to-right)))
      (when (> possible-amount 0)
	(setf (graph-node-center node)
	  (+ (graph-node-center node) possible-amount))
	(let ((left-neighbor (left-neighbor node layout-context)))
	  (when left-neighbor
	    (enqueue left-neighbor priority-queue)))
	(loop for parent in (graph-node-parents node)
	    do (enqueue parent priority-queue))))))







;;; The idea here is to bump the guy over either as far as desired or until
;;; he bumps into his neighbor (or the edges of the design).  If he's blocked by his neighbor
;;; there are 2 possibilities:
;;; 1) The neighbor can move more in the desired direction and it will improve his position
;;;    In this case the neighbor is still in the queue and will move and then notify us
;;;    that we can do better
;;; 2) The neighbor can't move more in that direction either because it's blocked or
;;;      because it will make his position worse.  If the second then the net gain from
;;;      moving both the neighbor and me is zero because every good step that I move is a bad step for him.
;;; The requirement is that if I move left then I have to queue up the guy to my right (and similarly for the opposite direction)
;;;   I could be more clever and check whether the guy to my right abutted me before the move and only queue him up then
;;;     also if he's in the queue already I don't have to put him back in
;;; The second requirement is that if I move then I have to notify my parent, allowing him to recalculate his placement-quality
;;;  and adjust his queue placement based on that.

(defmethod enqueue ((node my-graph-node-output-record) heap)
  (let ((placement-quality (placement-quality node))
	(was-in-queue (graph-node-in-queue node)))
    ;; if he was in the enqueue then we're either going
    ;; to replace the old element with a new one
    ;; or it's now at optimal placement so either way
    ;; get it out
    (when was-in-queue
      (ji::delete-by-item heap node #'eql)
      (setf (graph-node-in-queue node) nil))
    ;; if placement quality is 0, then no need to enqueue
    (when (> placement-quality 0)
      (ji::heap-insert heap node placement-quality)
      (setf (graph-node-cost node) placement-quality
	    (graph-node-in-queue node) t))
    (values node placement-quality was-in-queue (graph-node-in-queue node))))

(defun dequeue (heap)
  (multiple-value-bind (node key was-there) (ji::heap-remove heap)
    (when node (setf (graph-node-in-queue node) nil))
    (values node key was-there)))

(defmethod initialize-search ((layout-context graph-layout-context))
  (with-slots (generation-descriptors) layout-context
    (let ((heap (ji::make-heap :test #'>)))
      (setf (priority-queue layout-context) heap)
      ;; enqueue all the nodes but don't enqueue the guys in the broadest generation
      (loop for descriptor across generation-descriptors
	  for layer = (my-generation-vector descriptor)
	  for generation-number = (my-generation-generation descriptor)
	  unless (or (eql generation-number (broadest-generation layout-context))
		     (eql generation-number (1- (length generation-descriptors))))
	  do (loop for node across layer
		 do ;; clear stale state
		   (setf (graph-node-in-queue node) nil)
		   (enqueue node heap)))
      heap)))

(defun improve-placements (layout-context)
  (let ((heap (initialize-search layout-context)))
    (with-slots (max-breadth) layout-context
      (loop for node = (dequeue heap)
	  until (null node)
	  do (let* ((desired-move (desired-move node)))
		 ;; Note: move-to-left and move-to-right enqueue
		 ;; the neighbors and parents affected
		 (when (minusp desired-move)
		   (move-to-left node layout-context (abs desired-move)))
		 (when (plusp desired-move)
		   (move-to-right node layout-context desired-move)))))))
