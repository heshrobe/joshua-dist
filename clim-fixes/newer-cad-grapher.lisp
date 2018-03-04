;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

(in-package :clim-internals)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   CAD like grapher for use in flow diagrams and the like
;;;
;;;     We're going to lay things out in aligned columns (rows) with channels for routing
;;;     between each column and row for vertical and horizontal runs.  So this is a bit like
;;;     table layout for the nodes, buy with e-cad like lines connecting things.
;;;     The flow is always horizontal or vertical, although I can imagine a future extension
;;;       in which both happen (say data goes left to right and control top to bottom).
;;;     See comments below for understanding the routine of lines.
;;;
;;;     The grapher is set up to go either horizontally or vertically 
;;;      (and either left to right or right to left or top to bottom or bottom to top)
;;;      This isn't yet respected in the edge oriented code!
;;;
;;;     Terminology                 Horizontal             Vertical
;;;      Generations                 columns                rows
;;;      Tiers                       rows                   column
;;;      Depth                       horizontal width       vertical height
;;;      Breadth                     vertical height        horizontal width
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defclass cad-graph-output-record (directed-graph-output-record)
  ((generation-map :initform nil :accessor generation-map)
   (tier-map :initform nil :accessor tier-map)))

(define-graph-type :cad cad-graph-output-record)

;;; Included just for reference purposes
;;;(defclass standard-graph-node-output-record 
;;;          (standard-sequence-output-record graph-node-output-record)
;;;    ((generation :accessor graph-node-generation :initform 0)
;;;     ;; The output records corresponding to this node's parents and children
;;;     (node-children :accessor graph-node-children :initform nil)
;;;     (node-parents :accessor graph-node-parents :initform nil)
;;;     (object :accessor graph-node-object :initarg :object))
;;;  (:default-initargs :size 5))


;;; This adds another slot to keep track of the tier of the node as well
;;; as the generation.
;;; It also adds a "fiducial mark" with an x and y position
;;; The idea is that generations and tiers are aligned by the fiducial marks.
;;; These are provided as return values by the drawing function.
;;; If the box drawing function returns 3 values with first value the keyword :fiducial
;;;  then the second and third values are the fiducial box width and height
;;; Otherwise, the box center is used.

;;; This makes alignment more complicated.
;;; To determine the width of a column you must find
;;; 1) the maximum distance from the left to the fiducial mark - the fiducial-offset
;;; 2) the maximum distnace from the fiducial mark to the right - the fiducial-residue
;;; The width of the column is the sum of these two
;;; and each box is aligned to that its fiducial mark is offset by (1) from the
;;; left of the column.
;;; Note that (1) is the value stored in the record as fiducial-x-offset and that
;;;           (2) is the difference between the box width and (1)
;;; of course replace column by row and x by y for the other direction

(defclass cad-graph-node-output-record (standard-graph-node-output-record)
  ;; Keep track of vertical position in the column (for horizontal graph)
  ((tier :initform 0 :accessor tier)
   (fiducial-x-offset :initform nil :accessor fiducial-x-offset) 
   (fiducial-y-offset :initform nil :accessor fiducial-y-offset))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;     The main change in generate-graph-nodes is that the option "maximize generations" is irrelevant
;;;       if invoked it would cause a reconvergent node that would have been in two generations
;;;       to have the lesser generation replaced with a "filler node" that when sizing the column
;;;       width would be as wide as the widest thing in its column (or row and height)
;;;     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod generate-graph-nodes ((graph cad-graph-output-record) stream
                                 root-objects object-printer inferior-producer
                                 &key duplicate-key duplicate-test)
  (declare (dynamic-extent object-printer inferior-producer))
  (with-slots (n-generations) graph
    (let* ((hash-table (slot-value graph 'hash-table))
           (properties (slot-value graph 'properties))
           (cutoff-depth (getf properties :cutoff-depth))
           (root-nodes nil)
           (connectors-in nil))
      (labels ((inferior-mapper (function node)
                 (map nil function (funcall inferior-producer node)))
               (new-node-function (parent-object parent-record child-object)
		 (let (fiducial? fiducial-x fiducial-y)
		   ;; this is really ugly coding, but I need to capture the return
		   ;; value of the drawing function to get the fiducial mark offsets
		   (let ((child-record (with-stream-cursor-position-saved (stream)
					 (with-new-output-record
					     (stream 'cad-graph-node-output-record nil
						     :object child-object
						     :duplicate-key duplicate-key
						     :duplicate-test duplicate-test)
					   (multiple-value-setq (fiducial? fiducial-x fiducial-y)
					     (funcall object-printer child-object stream))))))
		     (if (eql fiducial? :fiducial)
		       (setf (fiducial-x-offset child-record) fiducial-x
			     (fiducial-y-offset child-record) fiducial-y)
		       (with-bounding-rectangle* (left top right bottom) child-record
			 (setf (fiducial-x-offset child-record) (floor (- right left) 2)
			       (fiducial-y-offset child-record) (floor (- bottom top) 2)))
		       )
		     ;; This guarantees that the next phase will have at least one
		     ;; node from which to start.  Otherwise the entire graph gets
		     ;; lost.  If the first node isn't really a root, it will be
		     ;; deleted from the set of roots when the cycle is
		     ;; detected.
		     (when (null root-nodes)
		       (push child-record root-nodes))
		     (old-node-function parent-object parent-record child-object child-record))))
               (old-node-function (parent-object parent-record child-object child-record)
                 (declare (ignore parent-object child-object))
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
		     (increment-generation child-record)))
                 ;; Preserve the ordering of the nodes.  
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
      ;; We lost the order of the roots nodes now recover it
      (loop for node in root-nodes
	  for his-object = (graph-node-object node)
	  for his-tier = (position his-object root-objects)
	  do (setf (tier node) his-tier))
      (setq root-nodes (sort root-nodes #'< :key #'tier))
      ;; I've removed the cycle breaking code from here
      ;; If it's ever relevant it's back in the graph-formatting file of clim
      (setf (slot-value graph 'root-nodes) 
            (nconc (nreverse connectors-in) root-nodes))))
  graph)

;;; To be done later
;;; Note that at this point we have a complete mapping of parents <-> children
;;; But have done no geometry assignments. This makes this a good place to 
;;; undertake a shuffling phase to reduce line crossings!
;;; 
;;; We'd assign a tier number to each member of each generation (now it's done below)
;;; A crossing is when a pair of parent nodes in the same generation
;;; have a pair of children in the next generation, so that the order of the parents'
;;; tiers is the opposite of that of the children.
;;; You could swap the children if that reduced the total crossings between the adjacent
;;; generations.
;;; There are papers that describe algorithms for doing this (find references)
;;; Generally, the approach is a greedy algorithm with possible annealing


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Placing and aligning the nodes
;;;  As noted above the nodes are aligned by their fiducial marks
;;;  this requires calculating the maximum fiducial offset for each generation
;;;  as well as the maximum fiducial residue, i.e. the distance after the fiducal 
;;;  (which is the width minus the fiducial offset) and similarly for each tier.
;;;  This easiest way to do this is to first traverse the structure and collect in each generation
;;;  all of its nodes.  Then we can map over them within the generation and collect this information
;;;  for the generation and the calculate the depth of the generation as the sum of the two.
;;;  Alignment along the generation then involves placing the nodes so that each one's fiducial offset
;;;  is at the generation's fiducial offset.
;;;  The we have to scan across the generation and do the same sort of thing for each tier.
;;;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; I made my own data structure as a CLOS object
;;; but there are occassional problems of accessor name conflict
;;; with the generation descriptor defstruct in core CLIM
(defclass has-fiducial-offset ()
  ((maximum-fiducial-offset :initform 0 :accessor maximum-fiducial-offset)
   (maximum-fiducial-residue :initform 0 :accessor maximum-fiducial-residue)
   ))

(defclass has-routing-channels ()
  ((channel-before :initform nil :initarg :channel-before :accessor channel-before)
   (channel-after :initform nil :initarg :channel-after :accessor channel-after))
  )

(defclass generation-descriptor (has-fiducial-offset has-routing-channels)
  ((number :initform nil :initarg :generation-number :accessor generation-number)
   (depth :initform 0 :accessor depth)
   (start-depth :initform 0 :accessor start-depth)
   (size :initform 0 :accessor size)
   (members :initform nil :accessor members)
  ))

(defclass tier-descriptor (has-fiducial-offset has-routing-channels)
  ((tier-number :initform 0 :initarg :tier-number :accessor tier-number)
   (breadth :initform 0 :accessor tier-breadth)
   ;; where the tier begins
   (start-breadth :initform 0 :accessor start-breadth)
   ))

(defmethod layout-graph-nodes ((graph cad-graph-output-record) stream
			       arc-drawer arc-drawing-options)
  (declare (ignore arc-drawer arc-drawing-options stream))
  (with-slots (root-nodes hash-table n-generations) graph
    (when root-nodes
      (let* ((properties (slot-value graph 'properties))
	     (orientation (getf properties :orientation))
	     ;; in CAD graphing we always "center" i.e. align fiducials
	     ;; (center-nodes (getf properties :center-nodes))
	     (generation-separation (getf properties :generation-separation))
	     (within-generation-separation (getf properties :within-generation-separation))
	     (tier-vector nil)
	     (max-generation-size 0))
	(flet ((inferior-mapper (function node)
		 (map nil function (graph-node-children node)))
	       (yx-output-record-set-position (record y x)
		 (output-record-set-position record x y)))
	  (declare (dynamic-extent #'inferior-mapper #'yx-output-record-set-position))
	  (multiple-value-bind (breadthfun depthfun set-positionfun 
				fiducial-breadth-fun fiducial-depth-fun)
	      (ecase orientation
		((:vertical :down :up)
		 (values #'bounding-rectangle-width #'bounding-rectangle-height
			 #'output-record-set-position 
			 #'fiducial-x-offset #'fiducial-y-offset
			 ))
		((:horizontal :right :left)
		 (values #'bounding-rectangle-height #'bounding-rectangle-width
			 #'yx-output-record-set-position 
			 #'fiducial-y-offset #'fiducial-x-offset)))
	    (macrolet ((traverse (new-node-function &optional (old-node-function '#'false))
			 `(traverse-graph root-nodes #'inferior-mapper 
					  hash-table #'identity
					  ,new-node-function ,old-node-function))
		       ;; "Breadth" is the heigth in horizontal orientation, width in vertical orientation
		       ;; "Depth" is vice-versa.
		       (fiducial-depth (node) `(funcall fiducial-depth-fun ,node))
		       (fiducial-breadth (node) `(funcall fiducial-breadth-fun ,node))
		       (node-breadth (node) `(funcall breadthfun ,node))
		       (node-depth (node) `(funcall depthfun ,node)))
	      ;; At this point we know the number of generations so we set up
	      ;; their descriptors.  
	      (let ((generation-descriptors (make-array (1+ n-generations))))
		(loop for index upto n-generations
		    do (setf (aref generation-descriptors index)
			 (make-instance 'generation-descriptor :generation-number index)))
		(when (member orientation '(:up :left))
		  ;; note nreverse works on arrays
		  (setq generation-descriptors (nreverse generation-descriptors)))
		;; Determine the members of each generation
		;; Because of the alignment to fiducial marks, the depth and breadth
		;; of each generation is actually a holistic function that is calculated
		;; after this.
		(flet ((collect-node (p ph child-node)
			 (declare (ignore p ph))
			 (let ((generation (aref generation-descriptors (graph-node-generation child-node))))
			   ;; remember the index of this guy in the generation
			   (setf (tier child-node) (size generation))
			   (incf (size generation))
			   ;; determine the maximum number of rows (in horizontal mode)
			   (maxf max-generation-size (size generation))
			   (push child-node (members generation))
			   )))
		  (declare (dynamic-extent #'collect-node-size))
		  (traverse #'collect-node))
		;; this probably is irrelevant but it's nice to have them in increasing tier order
		(loop for generation across generation-descriptors
		    do (setf (members generation) (nreverse (members generation))))
		;; Set up a vector that gives the height breadth of each tier
		;; (i.e. the height of each row for horizontal layout)
		;; We need to collect the maximum fiducial offsets and the maximum fiducial residues
		;; also
		(setq tier-vector (make-array max-generation-size))
		;; (setq foobar (list tier-vector generation-descriptors))
		(loop for i below max-generation-size
		    do (setf (aref tier-vector i)
			 (make-instance 'tier-descriptor :tier-number i)))
		;; Now we can go over all the nodes generation by generation
		;; building up the maximum-fiducial-offset and -residue
		;; for each generation
		;; We can also do this for each tier, because the nodes
		;; have their tier number in them.
		(loop for previous-generation = nil then generation
		    for depth-so-far = 0 
		    then (+ depth-so-far generation-separation (depth previous-generation))
		    for generation across generation-descriptors
		    for i from 0
		    do (loop for node in (members generation)
					 ;; this is the fiducial offset in the depth of generation direction
			   for fiducial-depth = (fiducial-depth node)
			   for depth = (node-depth node)
			   for tier = (aref tier-vector (tier node))
				      ;; this is the fiducial offset in the breadth of tier direction
			   for fiducial-breadth = (fiducial-breadth node)
			   for breadth = (node-breadth node)
			   do (maxf (maximum-fiducial-offset generation) fiducial-depth)
			      (maxf (maximum-fiducial-residue generation) (- depth fiducial-depth))
			      (maxf (maximum-fiducial-offset tier) fiducial-breadth)
			      (maxf (maximum-fiducial-residue tier) (- breadth fiducial-breadth)))
		       ;; now we know the depth of this generation and it's fiducial mark position
		       (setf (depth generation) (+ (maximum-fiducial-offset generation)
						   (maximum-fiducial-residue generation))
			     (start-depth generation) depth-so-far)
		       )
		;; Now calculate the tier size and cummulative offset for each tier
		(loop for tier across tier-vector
		    for cummulative = 0 then (+ cummulative within-generation-separation current-breadth)
		    for tier-fiducial-offset = (maximum-fiducial-offset tier)
		    for tier-fiducial-residue = (maximum-fiducial-residue tier)
		    for current-breadth = (+ tier-fiducial-offset tier-fiducial-residue)
		    do (setf (tier-breadth tier) current-breadth
			     (start-breadth tier) cummulative))
		;; pass this information along to the edge routing phase
		;; there's some code there that rebuilds all this
		;; that can be simplified away
		(setf (generation-map graph) generation-descriptors
		      (tier-map graph) tier-vector)
		;; So at this point we know where everything ought to be
		;; we know the fiducial-offset of each generation, tier and node
		;; we know the cummulative offset of each generation and tier
		;; 
		;; So now we can place the nodes
		;; just iterate over the generations and then the nodes within the generation
		(loop for generation across generation-descriptors
		    for generation-offset = (start-depth generation)
		    for generation-fiducial-offset = (maximum-fiducial-offset generation)
		    do (loop for node in (members generation)
			   for node-breadth-fiducial-offset = (fiducial-breadth node)
			   for node-depth-fiducial-offset = (fiducial-depth node)
			   for tier = (aref tier-vector (tier node))
			   for tier-offset = (start-breadth tier)
			   for tier-fiducial-offset = (maximum-fiducial-offset tier)
			   do (funcall set-positionfun
				       node
				       (+ tier-offset (- tier-fiducial-offset node-breadth-fiducial-offset))
				       (+ generation-offset (- generation-fiducial-offset node-depth-fiducial-offset)))
							      ))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    The Routing Phase:
;;;
;;; Channels (in describing this consider that generations are colums and tiers are rows)
;;;  A channel is a gap between generations or tiers in which connections are routed
;;;  There is one between every pair of generations
;;;   and for convenience we're going to have one before the first generation that will
;;;   be degenerate, i.e. we'll never put a track in it.
;;;
;;;  There is one above the first tier, between every pair of tiers, and one below the last tier
;;;  so that we can route conveniently.
;;;
;;;  Intergenerational channels have two sub-areas in which we route connections
;;;   One area is for routing connections out of the prior generation (i.e. to the left)
;;;    and the second area is for routing connections into the next generation (i.e. to the right)
;;;
;;;   For tiers there are also two areas because we're connecting to tracks in it from sources
;;;       above and below it.
;;;   
;;;   We first figure out how many tracks each channel will need to have and therefore how big it is.
;;;   Then we bump over the generations and tiers to make room for the channels
;;;   Then finally we route the connections through the channels
;;;   
;;;   A connection between nodes in two adjacent generations will require a track in the channel
;;;    between the two generations.  That track will run vertically between the highest and lowest
;;;    target in the next generation.
;;;
;;;   A connection that reaches a generation beyond the next one will require a track in the "closest"
;;;     horizontal channel that reaches as far as the most distant target generation
;;;     It will also require a vertical track in the channel next to it that will span all targets in the 
;;;     next generation as well as the horizontal track.
;;;     The "closest" horizontal channel is the one below is the source is lower than the middle of the node
;;;     and otherwise the one above it
;;;     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass channel ()
  (;; previous is the generation or tier before this.  It can be null for tiers
   ;; because there is a channel before the first tier and after the last
   (previous :initform nil :initarg :previous :accessor previous)
   ;; next is the generation or tier before this.
   (next :initform nil :initarg :next :accessor next)
   ;; Bump here is the cummulative space for this channel and all previous ones
   (bump :initform 0 :accessor bump)
   (start :initform 0 :initarg :start :accessor start)
   (end :initform 0 :initarg :end :accessor end)
   (number-of-entry-tracks :initform 0 :accessor number-of-entry-tracks)
   (number-of-exit-tracks :initform 0 :accessor number-of-exit-tracks)
   ;; these are used during the actually routing of tracks to allocate the next
   ;; available trace on each side
   (next-before-track :initform 1 :accessor next-before-track)
   (next-after-track :initform 1 :accessor next-after-track)
   )
  )

(defparameter *track-spacing* 10)


;;; Data structures for organizing routes
(defclass has-entries ()
  ((entries :initform nil :accessor entries)))

(defclass has-generation ()
  ((generation :initarg :generation :accessor generation)
   ))

(defclass has-coordinates-and-node ()
  ((primary-coordinate :initarg :primary-coordinate :accessor primary-coordinate)
   (secondary-coordinate :initarg :secondary-coordinate :accessor secondary-coordinate)
   (node :initarg :node :accessor node)))

(defclass source-generation-entry (has-generation has-entries)
  ())

(defclass source-entry (has-generation has-coordinates-and-node has-entries)
  ((single-straight-shot :initform nil :accessor single-straight-shot)
   (needs-a-tier-track :initform nil :accessor needs-a-tier-track)
   ;; one of :before or :after assuming needs-tier-track is t
   (which-tier-channel-to-use :initform nil :accessor which-tier-channel-to-use)
   (has-non-local-destination :initform nil :accessor has-non-local-destination)
   ))

(defclass destination-generation-entry (has-generation has-entries)
  ())

(defclass destination-entry (has-coordinates-and-node)
  ((drawing-options :initarg :drawing-options :initform nil :accessor drawing-options)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Step 1: collect all routes that need to be done
;;; 
;;; This will return a tree like structure
;;; The first level of the tree is a list of entries one for each generation in which a route starts
;;;  this is sorted by generation number
;;;
;;; The second level (within each generation) is a list of route sources
;;;   one for each route that starts in this generation.  
;;;  This is sorted by position in the generation.
;;;
;;;  The third level (within each routing source) is a list of destination generations
;;;   one for each generation that this route has a destination in.
;;;  This is sorted by the generation.
;;;
;;;  Finally (for each destination generation of each route) a list of destinations
;;;   that the route terminates at in this generation.
;;;  This is sorted by position in the generation
;;;
;;;  Position within generation means the y-coordinate for :horizontal graphs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric assign-tracks (graph connection-map))
(defgeneric move-nodes-to-make-room-for-channels (graph))
(defgeneric route-edges (graph stream connection-map))

(defmethod layout-graph-edges ((graph cad-graph-output-record) stream
			       arc-drawer arc-drawing-options)
  (declare (ignore arc-drawing-options))
  (with-slots (root-nodes hash-table properties properties) graph
    ;; A table to gather up all routes from each starting point
    (let ((connection-map nil)
	  (orientation (or (getf properties :orientation) :horizontal)))
      (when root-nodes
	;; All this stuff is just the normal node traversal calling the arc drawer
	;; Except we don't actually draw at this stage
	;; we capture all the routes that need to be done
	;; in the format described above, then call route-edges to do the hard work
	;; of channel allocation and then the final drawing.
	(flet ((inferior-mapper (function node) (map nil function (graph-node-children node)))
	       (collect-edge (x1 y1 x2 y2 source destination options)
		 (let ((source-primary-coordinate x1)
		       (source-secondary-coordinate y1)
		       (destination-primary-coordinate x2)
		       (detination-secondary-coordinate y2))
		   (when (eql orientation :horizontal) 
		     (rotatef source-primary-coordinate source-secondary-coordinate)
		     (rotatef destination-primary-coordinate detination-secondary-coordinate))
		   (let ((source-generation (graph-node-generation source))
			 (destination-generation (graph-node-generation destination)))
		     (let ((source-generation-entry (find source-generation connection-map :test #'= :key #'generation)))
		       (unless source-generation-entry
			 (setq source-generation-entry (make-instance 'source-generation-entry :generation source-generation))
			 (push source-generation-entry connection-map))
		       ;; TODO: generalize for vertical graphs
		       (let ((source-entry (find source-primary-coordinate (entries source-generation-entry) :test #'= :key #'primary-coordinate)))
			 (unless source-entry
			   (setq source-entry (make-instance 'source-entry 
						:primary-coordinate source-primary-coordinate
						:secondary-coordinate source-secondary-coordinate
						:node source))
							   
			   (push source-entry (entries source-generation-entry)))
			 (let ((destination-generation-entry (find destination-generation (entries source-entry) :test #'= :key #'generation)))
			   (unless destination-generation-entry
			     (setq destination-generation-entry (make-instance 'destination-generation-entry
								  :generation destination-generation))
			     (push destination-generation-entry (entries source-entry)))
			   (let ((destination-entry
				  (find destination-primary-coordinate (entries destination-generation-entry) :test #'= :key #'primary-coordinate)))
			     (unless destination-entry
			       (setq destination-entry (make-instance 'destination-entry
							 :primary-coordinate destination-primary-coordinate
							 :secondary-coordinate detination-secondary-coordinate
							 :node destination
							 :drawing-options options))
			       (push destination-entry (entries destination-generation-entry)))))))))))
	  (declare (dynamic-extent #'inferior-mapper #'collect-edge))
          (multiple-value-bind (xoff yoff)
	      (convert-from-relative-to-absolute-coordinates stream (output-record-parent graph))
            (with-identity-transformation (stream)
              (with-output-recording-options (stream :draw nil :record t)
                (with-new-output-record (stream 'standard-sequence-output-record nil :parent graph)
		  ;; Note that draw-edge doesn't actually draw anything it does call the user's drawing
		  ;; function but it just calls our collection function after figuring out what it wants
		  ;; to connect
		  (flet ((draw-edge (parent ph child &optional ch)
			   (declare (ignore ph ch))
			   (when parent
			     (multiple-value-bind (parent-x parent-y) 		 
				 (with-bounding-rectangle* (left top right bottom) parent
				   (declare (ignore left bottom))
				   (values right top))
			       (multiple-value-bind (child-x child-y) 
				   (with-bounding-rectangle* (left top right bottom) child
				     (declare (ignore right bottom))
				     (values left top))
				 (translate-coordinates xoff yoff parent-x parent-y child-x child-y)
				 (Funcall arc-drawer #'(lambda (x1 y1 x2 y2 &optional options)
							 (collect-edge x1 y1 x2 y2 parent child options))
					  (graph-node-object parent) (graph-node-object child)
					  parent-x parent-y child-x child-y))))))
		    (declare (dynamic-extent #'draw-edge))
		    (traverse-graph root-nodes #'inferior-mapper hash-table #'identity #'draw-edge #'draw-edge))))))))
      ;; At this point we've got all the data we need, now we're going to sort it
      ;; sorting might actually be superfluous, but it's more orderly
      (setq connection-map (sort connection-map #'< :key #'generation))
      (loop for source-generation-entry in connection-map
	  for source-entries = (sort (entries source-generation-entry) #'< :key #'primary-coordinate)
	  do (setf (entries source-generation-entry) source-entries)
	     (loop for source-entry in source-entries
		 for destination-generation-entries = (sort (entries source-entry) #'< :key #'generation)
		 do (setf (entries source-entry) destination-generation-entries)
		    (loop for destination-generation-entry in destination-generation-entries
			for destination-entries = (sort (entries destination-generation-entry) #'< :key #'primary-coordinate)
			do (setf (entries destination-generation-entry) destination-entries))))
      ;; Now move onto repositioning the nodes and running the routes
      (when connection-map
	(assign-tracks graph connection-map)
	(move-nodes-to-make-room-for-channels graph)
	(route-edges graph stream connection-map))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Step 2: Assign routes to tracks
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod assign-tracks ((graph-record cad-graph-output-record) connection-map)
  (with-slots (root-nodes hash-table generation-map tier-map) graph-record
    ;; The node layout computed a map of generations and tiers already
    (with-slots (generation-map tier-mapoptions properties) graph-record
      ;; build and connect all the intergenerational and inter-tier channels
      (thread-channels generation-map tier-map)
      (let* ((orientation (or (getf properties :orientation) :horizontal))
	     (swap (member orientation '(:left :right :horizontal))))
	(labels ((has-only-1-straight-shot? (source-entry source-generation)
		   (let ((source-location (primary-coordinate source-entry))
			 (destination-generation-entries (entries source-entry)))
		     ;; only has 1 destionation
		     (setf (single-straight-shot source-entry)
		       (and (null (rest destination-generation-entries))
			    (let* ((first-destination-entry (first destination-generation-entries))
				   (destination-generation (generation first-destination-entry))
				   (destination-entries (entries first-destination-entry)))
			      ;; destination is in the next generation
			      (and (= destination-generation (1+ source-generation))
				   ;; it's the only one in that generation
				   (null (rest destination-entries))
				   (let* ((first-destination-entry (first destination-entries))
					  (destination-location (primary-coordinate first-destination-entry)))
				     ;; and it's a straight shot
				     (= source-location destination-location))))))))
		 (has-non-local-destination? (source-entry source-generation)
		   (loop with next-generation = (1+ source-generation)
		       for destination-generation-entry in (entries source-entry)
		       for destination-generation = (generation destination-generation-entry)
		       when (not (= destination-generation next-generation))
		       return (setf (has-non-local-destination source-entry) t)))
		 (needs-a-tier-track? (source-entry)
		   (setf (needs-a-tier-track source-entry) (has-non-local-destination source-entry)))
		 (add-a-generation-track (generation-number direction)
		   (let* ((generation (aref generation-map generation-number))
			  (previous-channel (channel-before generation))
			  (next-channel (channel-after generation)))
		     (case direction
		       (:before (incf (number-of-exit-tracks previous-channel)))
		       (:after (incf (number-of-entry-tracks next-channel))))))
		 (add-a-tier-track (source-entry)
		   (let* ((source (node source-entry))
			  (source-location (primary-coordinate source-entry))
			  (source-tier  (tier source))
			  (tier (aref tier-map source-tier))
			  (previous-channel (channel-before tier))
			  (next-channel (channel-after tier))
			  (destinations-before 0)
			  (destinations-after 0))
		     (loop for destination-generation-entry in (entries source-entry)
			 do (loop for destination-entry in (entries destination-generation-entry)
				for destination = (node destination-entry)
				for destination-tier = (tier destination)
				when (<= destination-tier source-tier)
				do (incf destinations-before)
				else do (incf destinations-after)))
		     (with-bounding-rectangle* (primary-1 secondary-1 primary-2 secondary-2) source
		       (when swap
			 (rotatef primary-1 secondary-1)
			 (rotatef primary-2 secondary-2))
		       (cond
			((> destinations-before destinations-after)
			 (setf (which-tier-channel-to-use source-entry) :before) (incf (number-of-exit-tracks previous-channel)))
			((> destinations-after destinations-before)
			 (setf (which-tier-channel-to-use source-entry) :after)
			 (incf (number-of-entry-tracks next-channel)))
			;; same number above and below
			;; If the start point is below the half-way point go before otherwise after
			((< source-location (+ primary-1 (floor (abs (- primary-2 primary-1)) 2)))
			 (setf (which-tier-channel-to-use source-entry) :before)
			 (incf (number-of-exit-tracks previous-channel)))
			(t 
			 (setf (which-tier-channel-to-use source-entry) :after)
			 (incf (number-of-entry-tracks next-channel))))))))
	  ;; build up all the information we'll need about integenerational spacing to accomodate tracks
	  ;; The rules: 
	  ;; A source with destinations in a generation other than the next (i.e. a non-local target) needs one tier-chanel track
	  ;; A source route needs one generation-channel track in the channel after the generation
	;;; (unless it only has one destination that's in the next generation and the destination is a straight shot
	  ;; Each destination generation other than the next generation requires one generation-channel track in the channel before
	;;;  the destination 
	  (loop for source-generation-entry in connection-map
	      for source-generation = (generation source-generation-entry)
	      do (loop for source-entry in (entries source-generation-entry)
		     do (has-only-1-straight-shot? source-entry source-generation)
			(has-non-local-destination? source-entry source-generation)
			(needs-a-tier-track? source-entry)
		     when (needs-a-tier-track source-entry)
		     do (add-a-tier-track source-entry)
			;; If it's a single straight shot then you don't need any tracks
			;; otherwise you need one in the source channel
			;; and one in each destination channel
		     when (not (single-straight-shot source-entry))
		     do (add-a-generation-track source-generation :after)
			(loop for destination-generation-entry in (entries source-entry)
			    for destination-generation = (generation destination-generation-entry)
			    unless (= destination-generation (1+ source-generation))
			    do (add-a-generation-track destination-generation :before)))))))))

;;; We're creating a channel before the first generation
;;; and after the last generation even though these will never be used
;;; (i.e. no tracks will be allocated in them and they'll always take up
;;; 0 space)
;;; This just makes things a lot more uniform
(defun thread-channels (generation-map tier-map)
  (loop for previous-channel = (make-instance 'channel) then next-channel
      for generation across generation-map
      for next-channel = (make-instance 'channel
			   :previous generation)
      do (setf (channel-before generation) previous-channel
	       (channel-after generation) next-channel
	       (next previous-channel) generation
	       (previous next-channel) generation
	       ))
  ;; There's one more tier-channel than there are tiers
  ;; so the channel above a tier has the same index as the tier
  ;; and the channel below the tier has an index one larger
  (loop for previous-channel = (make-instance 'channel) then next-channel
      for tier across tier-map
      for next-channel = (make-instance 'channel
			   :previous tier)
      do (setf (channel-before tier) previous-channel
	       (channel-after tier) next-channel
	       (next previous-channel) tier
	       (previous next-channel) tier
	       )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Step 3: Move nodes to make room for routing channels
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Note that this doesn't account for orientation at the bottom

(defmethod move-nodes-to-make-room-for-channels ((graph-record cad-graph-output-record))
  ;; the bump in a channel is how much we need to move the thing after it by
  (with-slots (generation-map tier-map properties) graph-record
    ;; first go over all the generation channels and add up the spacing each will need
    (setf (start (channel-before (aref generation-map 0))) 0)
    (loop with total-bump = 0
	with generation-separation = (getf properties :generation-separation)
	for i from 0
	for generation across generation-map
	for depth = (depth generation)
	for channel-before = (channel-before generation)
	for channel-after = (channel-after generation)
	for entry-tracks = (number-of-entry-tracks channel-before)
	for exit-tracks = (number-of-exit-tracks channel-before)
	for total-tracks = (+ entry-tracks exit-tracks)
	for room-for-tracks = (* *track-spacing* (+ 1 total-tracks))
	for next-generation-size = (+ depth generation-separation)
	do (incf total-bump room-for-tracks)
	   (setf (bump channel-before) total-bump)
	   (setf (end channel-before) (+ (start channel-before) room-for-tracks))
	   (setf (start channel-after) (+ next-generation-size (end channel-before))))
    ;; now do the tiers similarly
    ;; the first channel is above everything and starts at 0
    (setf (start (channel-before (aref tier-map 0))) 0)
    (loop with total-bump = 0
	with within-generation-separation = (getf properties :within-generation-separation)
	for tier across tier-map
	for breadth = (tier-breadth tier)
	for channel-before = (channel-before tier)
	for channel-after = (channel-after tier)
	for entry-tracks = (number-of-entry-tracks channel-before)
	for exit-tracks = (number-of-exit-tracks channel-before)
	for total-tracks = (+ entry-tracks exit-tracks)
	for room-for-tracks = (* *track-spacing* (+ 1 total-tracks))
	for next-tier-size = (+ breadth within-generation-separation)
	do (incf total-bump room-for-tracks)
	   (setf (bump channel-before) total-bump)
	   (setf (end channel-before) (+ (start channel-before) room-for-tracks))
	   (setf (start channel-after) (+ next-tier-size (end channel-before))))
    ;; now go over every node and kick it over by the appropriate ammount
    (let* ((orientation (getf properties :orientation))
	   (swap? (member orientation '(:left :right :horizontal))))
      (loop for generation across generation-map
	  for generation-bump = (bump (channel-before generation))
	  do (loop for node in (members generation)
		 for his-tier = (aref tier-map (tier node))
		 for tier-bump = (bump (channel-before his-tier))
		 do (with-bounding-rectangle* (left top right bottom) node
		      (declare (ignore right bottom))
		      (let ((x-bump tier-bump)
			    (y-bump generation-bump))
			(when swap?
			  (rotatef x-bump y-bump))
			(output-record-set-position node (+ left x-bump) (+ top y-bump)))))
	     ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Step 4: Actually draw the routes
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; New strategy, pass segments down until you encounter the destination
;;; Then draw all segments with the destination drawing options.

(defmethod route-edges ((graph-record cad-graph-output-record) stream connection-map)
  (with-slots (generation-map tier-map properties) graph-record
    (let* ((orientation (or (getf properties :orientation) :horizontal))
	   (swap (member orientation '(:left :right :horizontal))))
      (flet ((get-next-track (channel side)
	       (case side
		 (:entry 
		  ;; Channels are init'd at 1 so that takes care of leaving
		  ;; an empty track on each side
		  (let ((track-number (next-before-track channel)))
		    (incf (next-before-track channel))
		    (+ (start channel) (* track-number *track-spacing*))))
		 (:exit
		  (let ((track-number (next-after-track channel)))
		    (incf (next-after-track channel))
		    (- (end channel) (* track-number *track-spacing*))))))
	     (draw-edge (x1 y1 x2 y2 &optional options)
	       (when swap (rotatef x1 y1) (rotatef x2 y2))
	       (apply #'clim:draw-line* stream x1 y1 x2 y2 options)
	       )
	     (draw-dot (x1 y1)
	       (when swap (rotatef x1 y1))
	       (clim:draw-circle* stream x1 y1 5))
	     )
	;; traverse the connection map actually drawing lines
	(loop for source-generation-entry in connection-map
	    for source-generation-number = (generation source-generation-entry)
	    for next-generation-number = (1+ source-generation-number)
	    for source-generation = (aref generation-map source-generation-number)
	    for source-generation-bump = (bump (channel-before source-generation))
	    do (loop for source-entry in (entries source-generation-entry)
		   for destination-generation-entries = (entries source-entry)
		   for source = (node source-entry)
		   for source-tier-number = (tier source)
		   for source-tier = (aref tier-map source-tier-number)
		   for source-tier-bump = (bump (channel-before source-tier))
		   for just-straight-shot =  (single-straight-shot source-entry)
		   for source-primary-coordinate =  (+ source-tier-bump (primary-coordinate source-entry))
		   for source-secondary-coordinate = (+ source-generation-bump (secondary-coordinate source-entry))
		   do (cond 
		       (just-straight-shot
			;; then draw it
			(let* ((destination-generation-entry (first (entries source-entry)))
			       (destination-entry (first (entries destination-generation-entry)))
			       (destination-options (drawing-options destination-entry))
			       (destination (node destination-entry))
			       (destination-generation-number (generation destination-generation-entry))
			       (destination-generation (aref generation-map destination-generation-number))
			       (destination-generation-bump (bump (channel-before destination-generation)))
			       (destination-tier-number (tier destination))
			       (destination-tier (aref tier-map destination-tier-number))
			       (destination-tier-bump (bump (channel-before destination-tier)))
			       (destination-primary-component (+ destination-tier-bump (primary-coordinate destination-entry)))
			       (destination-secondary-component (+ destination-generation-bump (secondary-coordinate destination-entry))))
			  (draw-edge source-primary-coordinate source-secondary-coordinate
				     destination-primary-component destination-secondary-component
				     destination-options)))
		       (t ;; in all other cases we'll have a channel track 
			;; and possibly a tier track
			;; and then for each destination generation another channel track
			(let* ((source-track (get-next-track (channel-after source-generation) :entry))
			       (source-branch-points nil)
			       (tier-branch-points nil)
			       (need-a-tier-track (needs-a-tier-track source-entry))
			       (which-tier-channel-to-use (which-tier-channel-to-use source-entry))
			       (tier-channel (if (eql which-tier-channel-to-use :before) (channel-before source-tier)(channel-after source-tier))) 
			       (tier-track (when need-a-tier-track 
					     (if (eql which-tier-channel-to-use :before) 
						 (get-next-track tier-channel :exit)
					       (get-next-track tier-channel :entry)))))
			  ;; draw line to channel track from the source
			  ;; and keep track of it so we can draw dots on intermdiate
			  ;; channel tracks
			  ;;; Process all generations that this source terminates in
			  (loop 
			      for destination-generation-entry in destination-generation-entries
			      for destination-generation-number = (generation destination-generation-entry)
			      for destination-generation = (aref generation-map destination-generation-number)
			      for destination-generation-bump = (bump (channel-before destination-generation))
			      for destination-is-next-generation = (= destination-generation-number next-generation-number)
			      for destination-track = (if destination-is-next-generation 
							  source-track
							(get-next-track (channel-before destination-generation) :exit))
			      for destination-generation-branch-points = nil
			      do 
				(loop 
				    for destination-entry in (entries destination-generation-entry)
				    for destination = (node destination-entry)
				    for destination-options = (drawing-options destination-entry)
				    for destination-tier-number = (tier destination)
				    for destination-tier = (aref tier-map destination-tier-number)
				    for destination-tier-bump = (bump (channel-before destination-tier))
				    for destination-primary-coordinate = (+  destination-tier-bump (primary-coordinate destination-entry))
				    for destination-secondary-coordinate = (+ destination-generation-bump (secondary-coordinate destination-entry))
				    do
				      ;; Draw the line from the destination to the destination track
				      (draw-edge destination-primary-coordinate destination-track 
						 destination-primary-coordinate destination-secondary-coordinate destination-options)
				      ;; but if this is the same generation as the source, then this is to the source track
				      ;; In that case add it to the source branch points
				      ;; otherwise to the dedtination branch points
				      (if destination-is-next-generation
					  (pushnew destination-primary-coordinate source-branch-points :test #'=)
					(pushnew destination-primary-coordinate destination-generation-branch-points :test #'=))
				      ;; draw the line connecting the source to the channel track
				      ;; This gets redrawn for each destination so we can use different drawing options
				      ;; for each.
				      (draw-edge source-primary-coordinate source-secondary-coordinate 
						 source-primary-coordinate source-track destination-options)
				      ;; now add it to the source branch points (hence pushnew to only get it once)
				      (pushnew source-primary-coordinate source-branch-points :test #'=)
				      (cond
				       (destination-is-next-generation
					;; Then we just need to connect the source and destination segements
					;; by drawing a segment in the source channel track
					(draw-edge source-primary-coordinate source-track
						   destination-primary-coordinate source-track destination-options))
				       (t 
					;; here we need:
					;; 1) A segment in the source track from the source to the tier track
					;; 2) A segment in the tier track from the source track to the tier track
					;; 3) A segment in the destination track from the tier track to the destination
					(draw-edge source-primary-coordinate source-track tier-track source-track destination-options)
					(pushnew source-primary-coordinate tier-branch-points :test #'=)
					(draw-edge tier-track source-track tier-track destination-track destination-options)
					(pushnew destination-track tier-branch-points :test #'=)
					(draw-edge  tier-track destination-track destination-primary-coordinate destination-track 
						    destination-options)
					)))
				;; At this point we've drawn lines to all the destinations in this generation
				;; so we can figure out where to draw branch dots (if any) 
				;; unless this destination generation is the source generation
				;; then we delay until everything is draw
				(unless destination-is-next-generation
				  (let ((destination-branch-points (sort destination-generation-branch-points #'<)))
				    ;; the first and last points are not branch points
				    (loop for points on (rest destination-branch-points)
					for point = (first points)
					when (rest points) ;; skip the last
					do (draw-dot point destination-track)
					   ))))
			  ;; at this point we've draw all destination generations
			  ;; so we can figure out where to put dots (if any)
			  ;; on the tier track and where to put dots (if any)
			  ;; in the source track
			  (let ((source-branch-points (sort source-branch-points #'<)))
			    ;; the first and last points are not branch points
			    (loop for points on (rest source-branch-points)
				for point = (first points)
				when (rest points) ;; skip the last
				do (draw-dot point source-track)))
			  (let ((tier-branch-points (sort tier-branch-points #'<)))
			    ;; the first and last points are not branch points
			    (loop for points on (rest tier-branch-points)
				for point = (first points)
				when (rest points) ;; skip the last
				do (draw-dot tier-track point)))
			  )))))))))

