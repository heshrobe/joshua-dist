;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: graph-formatting.lisp,v 1.14 92/08/18 17:24:58 cer Exp $

(in-package :clim-internals)

(defmethod generate-graph-nodes ((graph directed-graph-output-record) stream
				 root-objects object-printer inferior-producer
				 &key duplicate-key duplicate-test)
  (declare (dynamic-extent object-printer inferior-producer))
  (with-slots (n-generations) graph
    (let* ((hash-table (slot-value graph 'hash-table))
	   (graph-type (slot-value graph 'graph-type))
	   (properties (slot-value graph 'properties))
	   (cutoff-depth (getf properties :cutoff-depth))
	   (root-nodes nil))
      (declare (ignore graph-type))
      (labels ((inferior-mapper (function node)
		 (map nil function (funcall inferior-producer node)))
	       (new-node-function (parent-object parent-record child-object)
		 (let ((child-record
			 (with-stream-cursor-position-saved (stream)
			   (with-new-output-record
			       (stream 'standard-graph-node-output-record nil
				:object child-object
				:duplicate-key duplicate-key
				:duplicate-test duplicate-test)
			     (funcall object-printer child-object stream)))))
		   ;; This guarantees that the next phase will have at least one
		   ;; node from which to start.  Otherwise the entire graph gets
		   ;; lost.  If the first node isn't really a root, it will be
		   ;; deleted from the set of roots when the cycle is detected.
		   (when (null root-nodes)
		     (push child-record root-nodes))
		   (old-node-function parent-object parent-record child-object child-record)))
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
		       (when (> (GRAPH-NODE-GENERATION CHILD) OLD-GENERATION)
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
			    (pushnew node root-nodes))
                          (LET ((PARENTS (GRAPH-NODE-PARENTS NODE))
                                (GENERATION (GRAPH-NODE-GENERATION NODE)))
                            (UNLESS
                              (LOOP FOR PARENT IN PARENTS
                                    ALWAYS (> GENERATION (GRAPH-NODE-GENERATION PARENT)))
                              (ERROR "SCREWED UP NODE ~A" NODE))))
		      hash-table)
      (setf (slot-value graph 'root-nodes) (nreverse root-nodes))))
  graph)