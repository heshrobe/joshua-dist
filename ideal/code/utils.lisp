;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: Ideal;  Base: 10 -*-


(in-package :ideal)

;;;;********************************************************
;;;;  Copyright (c) 1989, 1992 Rockwell International -- All rights reserved.
;;;;  Rockwell International Science Center Palo Alto Lab
;;;;********************************************************


;;;;;;;;;;;;;;;;;;;;;;;; Sampath ;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(DISTRIBUTION-TYPE
            SAFE-APPEND
            EXACTLY-ONE
            DESCENDANT-P
            CHANCE-NODE-P
            ANCESTOR-P
            VALUE-NODE-P
            DECISION-NODE-P
            ANCESTORS REMOVE-NODE DESCENDANTS
            CREATE-EMPTY-DISTRIBUTION
            INSTANTIATED-P
            GENERATE-DIAGRAM
            DISCRETE-DIST-NODE-P
            DETERMINISTIC-NODE-P PROBABILISTIC-NODE-P
            FIND-VALUE-NODE
            BARREN-NODE-P SMOOTH-PROBABILITIES
            INFORMATIONAL-PREDECESSORS
            CONVERT-AND-SMOOTH-DIAGRAM
            SINGLE-DIRECTED-PATH-P
            CREATE-RANDOM-DIAGRAM
            CREATE-RANDOM-EVIDENCE)))
			       
; ****************************** FOR ID-CREATION  ***************************************

; These functions were first defined to support the code in
; id-creation.lisp.

; To be used when creating nodes on the fly

(defun create-empty-node (&key distribution-type)
  (let ((n (make-node)))
    (ecase distribution-type
      (:DISCRETE (setf (node-distribution n) (make-discrete-dist))))
    (values n)))

; This function is used occasionally only for completeness. It is
; supposed to make it easier to incorporate continuous ditributions.

(defun distribution-type (node)
  (etypecase (node-distribution node)
    (DISCRETE-DIST :DISCRETE)))

; Gets a valid id number for a new node that is to be added to diagram

(defun generate-new-id-number (diagram)
  (1+ (cond
	((null diagram)  *node-lowest-index*)
	(t (apply #'max (mapcar #'node-id-number diagram))))))


; Removes all predecessor/successor links in a diagram. Used when
; re-setting links in a screwed up diagram.

(defun delete-all-links (&optional (diagram *diagram*))
  (mapc #'(lambda (node)(setf (node-successors node ) nil
			      (node-predecessors node)  nil)) diagram))

; Makes pred a predecessor of node if that is not so already and returns
; t. If the operation would cause a loop it does nothing and returns
; nil.

(defun add-link (node pred &key (error-mode nil))
  (cond
    ((descendant-p node pred)
     (if error-mode
         (error "Making ~A a predecessor of ~A causes a cycle." pred node)
	 (ideal-warning "Making ~A a predecessor of ~A causes a cycle. ~
                         It is not being done."
			pred node)))
    ((member pred (node-predecessors node))
     (if error-mode
	 (error "There already is a link from ~A to ~A" pred node)
	 (values t)))
    (t (ideal-debug-msg "~%---- Adding link from node ~A to ~A~%" pred node)
       (push pred (node-predecessors node))
       (push node (node-successors pred))
       (values t))))

; Returns t if node-1 is a descendant of or is eq to node-2.

(defun descendant-p (node-2 node-1)
  "Returns t if node-1 is a descendant of node-2 or is eq to node-2"
  (or (eq node-1 node-2)
      (some #'(lambda (succ)
		(descendant-p succ node-1)) (node-successors node-2))))

; Returns t if node-1 is an ancestor of or is eq to node-2

(defun ancestor-p (node-2 node-1)
  "Returns t if node-1 is an ancestor of node-2 or is eq to node-2"
  (or (eq node-2 node-1)
      (some #'(lambda (pred)
		(ancestor-p pred node-1))(node-predecessors node-2))))

; Deletes the arc from pred to node (the actual link, that is.)


(defun delete-link  (node pred &key (silent t))
  (when (not silent)
    (if (not (find pred (node-predecessors node)))
	(ideal-warning "Delete-link was handed ~A as pred and ~A as successor. No link ~
                   between the two. Proceeding with no action taken." pred node)))
  ; If these copy-lists are not there the old node-predecessors list
  ; would be destructively changed and would cause wierd things to
  ; happen in some of the functions is primitives.lisp (for eg.
  ; remove-discrete-dec-node). Better not trust the user to realize such
  ; subtleties.
  (setf (node-predecessors node)
	(delete pred (copy-list (node-predecessors node))))
  (setf (node-successors pred)
	(delete node (copy-list (node-successors  pred))))
  (values))


(defun ancestors (node &optional (ancestor-list nil))
  (dolist (pred (node-predecessors node) ancestor-list)
    (when (not (member pred ancestor-list))
      (setq ancestor-list (ancestors pred (cons pred ancestor-list))))))

(defun descendants (node &optional (descendant-list nil))
  (dolist (succ (node-successors node) descendant-list)
    (when (not (member succ descendant-list))
      (setq descendant-list (descendants succ (cons succ descendant-list))))))


; Implementation change: Apr '92.

; This function was always used to create and set a new distribution with
; a form like (setf (distribution-repn node)(create-empty-distribution
; node)). It used to return a distribution structure.

; It has been changed to be side-effecting now. It basically creates
; and sets the new distribution of the node and returns no useful values now.

(defun create-empty-distribution (node)
  (etypecase (node-distribution node)
    (DISCRETE-DIST
      (ecase (relation-type node)
	(:DET
	  (setf (distribution-repn node)
		(make-probability-array (node-predecessors node)
					:element-type 't :initial-element nil)))
	(:PROB
	  (when (noisy-or-node-p node)
	    (create-empty-noisy-or-distribution node))
	  (setf (distribution-repn node)
		(make-probability-array (cons node (node-predecessors node))))))))
  (values))

; These are used by primtives.lisp

(defun instantiated-p (node)
  (and (probabilistic-node-p node)(member (node-state node)(state-labels node))))

(defun discrete-dist-node-p (node)
  (and (node-p node)(discrete-dist-p (node-distribution node))))

(defun deterministic-node-p (node)
  (and (node-p node) (eq (relation-type node) :DET)))

(defun probabilistic-node-p (node)
  (and (node-p node)(eq (relation-type node) :PROB)))

(defun find-value-node (node-list)
  (find-if #'value-node-p node-list))

(defun barren-node-p (node)
  (and (not (value-node-p node)) (null (node-successors node))))

(defun find-better-decision (u1 d1 u2 d2)
  (if (or (null u1)(> u2 u1)) (values u2 d2) (values u1 d1)))

(defun informational-predecessors (node)
  (cond
    ((decision-node-p node)
     (cons node (mapcan #'informational-predecessors (node-predecessors node))))
    (t (list node))))

; Deletes links from node-predecessors of node to node.

(defun delete-links (node)
  (map nil #'(lambda (pred)
	       (setf (node-successors pred)(delete node (node-successors pred))))
       (node-predecessors node))
  (setf (node-predecessors node) nil)
  (values node))

(defun single-directed-path-p (pred succ)
  (block START
    (labels ((number-of-paths (pred succ &optional (numb-of-paths 0))
			      (cond
				((eq pred succ)
				 (cond
				   ((= numb-of-paths 1)(return-from START nil))
				   (t (1+ numb-of-paths))))
				(t
				 (dolist (node (node-successors pred) numb-of-paths)
				   (setq numb-of-paths
					 (number-of-paths node succ numb-of-paths)))))))
      (= (number-of-paths pred succ) 1))))


; Used when combining node predecessor lists etc in preference to
; append. This is to aviod structure sharing between the new list and
; the actual predecesor lists of the nodes (or whatever other lists you
; may use as input).

(defun safe-append (&rest lists)
  (mapcan #'copy-list lists))

; Returns t if the list consists of exactly one element.
(defun exactly-one (list)
  (and list (null (cdr list))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim '(inline safe-append exactly-one)))

; Returns non-nil if node precedes the value node and no other node.

(defun precedes-only-value-node (node)
  (and (= (length (node-successors node)) 1)
       (find-if #'value-node-p (node-successors node))))

; These are required so that confusing lambda fns in the code can be avoided

(defun chance-node-p (node)(eq (node-type node) :CHANCE))

(defun value-node-p (node)(eq (node-type node) :VALUE))

(defun decision-node-p (node)(eq (node-type node) :DECISION))

(defun binary-node-p (node)(= (number-of-states node) 2))

; IMPORTANT FN: A lot of wierd bugs have come up due to structure
; sharing in the node-predecessors lists. I assumed that it is
; guaranteed that a list ppp and (remove aaa ppp) share no structure,
; i.e there is simultaneous copying.  Not so. Therefore ....

(defun remove-node (node node-list)
  (delete node (copy-list node-list)))

; t iff names of the nodes are the same.

(defun same-name (node-1 node-2)
  (eql (node-name node-1)(node-name node-2)))

; Needed for edit functions

(defun centre-most-state-of (node)
  (when (state-labels node)
    (labels ((middle-state-number (node)
	       (+ *lowest-label-id-number* (floor (number-of-states node) 2))))
      (find (middle-state-number node) (state-labels node) :key #'label-id-number))))


; Generates a diagram consisting of all nodes that are connected to node
; or consisting of all the nodes connected to the nodes in node-list.

(defun generate-diagram (node-or-node-list)
  (let ((mark-symbol (gensym)))
    (cond
      ((node-p node-or-node-list)
       (mapc #'unmark-node (generate-diagram-1 node-or-node-list mark-symbol nil)))
      ((listp node-or-node-list)
       (let (untraversed-node generated-diagram)
	 (loop
	   (setq untraversed-node
		 (find-if-not #'(lambda (n)(marked-p n mark-symbol)) node-or-node-list))
	   (if (null untraversed-node)
	       (return (mapc #'unmark-node generated-diagram)))
	   (setq generated-diagram
		 (nconc (generate-diagram-1 untraversed-node mark-symbol nil)
			generated-diagram)))))
      (t (error "Input is neither a list nor a node")))))


(defun mark-node (node mark-symbol)(setf (node-activating-node node) mark-symbol))
(defun unmark-node (node)(setf (node-activating-node node) nil))
(defun marked-p (node mark-symbol)(eq (node-activating-node node) mark-symbol))

(defun generate-diagram-1 (node mark-symbol diagram)
  (when (not (marked-p node mark-symbol))
    (mark-node node mark-symbol)
    (push node diagram)
    (dolist (n (node-predecessors node))
      (setq diagram (generate-diagram-1 n mark-symbol diagram)))
    (dolist (n (node-successors node))
      (setq diagram (generate-diagram-1 n mark-symbol diagram))))
  (values diagram))

; These three functions change the probabilities in a non-strictly
; positive diagram slightly such that 1) it is still consistent 2) It is
; strictly positive. The function guarantees that the minimum possible
; probability in the diagram is the argument <factor> after the function
; has been run.

(defun smooth-probabilities (node &optional (factor 1e-6))
  (cond
    ((probabilistic-node-p node) (smooth-probabilities-1 node factor))
    (t (error "Node ~A is of not a probabilistic node" node))))

(defun smooth-probabilities-1 (node factor)
  (let ((scale-factor (- 1 (* (number-of-states node) factor))))
    (for-all-cond-cases (cond-case (node-predecessors node))
      (for-all-cond-cases (node-case (list node))
	(setf (prob-of node-case cond-case)
	      (+ (* (prob-of node-case cond-case) scale-factor) factor))))
    (values node)))

(defun convert-and-smooth-diagram (diagram &optional (factor 1e-6))
  (dolist (node diagram)
    (if (deterministic-node-p node) (convert-det-node-to-prob node))
    (if (probabilistic-node-p node)(smooth-probabilities node factor)))
  (values diagram))


; This is required for setting up the join tree in the clustering algorithms.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim '(inline prob-/)))

; Man, this fn is 100 times slower than regular division.

(defun prob-/ (a b)
  (cond
    ((zerop b)(cond
		((zerop a) 0)
		(t (error "Trying to divide ~A by ~A is meaningless" a b ))))
    (t (/ a b ))))



; Useful fn when debugging

(defun print-bels (bnet file-name &optional
		   (default-dir #+:LISPM "A:>srinivas>ideal-bugs>*.text" #-:LISPM ""))
  (with-open-file (st (merge-pathnames file-name default-dir) :direction :output)
    (diag-display-beliefs bnet st)))


; ----- Creating a random belief network -----------------------------------------

; Creates a random belief net that has a strictly positive distribution
; with <n> binary nodes. Random means that each node has a set of preds
; chosen at random and a prob dist set at random. The diagram that this
; fn returns is consistent-p.  <in-degree-limit> is the max number of
; predecessors a node may have.  If in-degree limit is <n> then then the
; problem of setting up the diagram has combinatorial complexity in the
; worst case and basically this fn takes for ever to run for n > 15.

(defun create-random-diagram (n &key (in-degree-limit 3) (max-number-of-states 4))
  (let ((diagram nil) new-node)
    (ideal-debug-msg "~%Creating Node:")
    (dotimes (k n)
      (ideal-debug-msg " ~A" k)
      (setq new-node (create-new-node k max-number-of-states))
      (add-links new-node (choose-random-subset diagram in-degree-limit))
      (set-node-distribution-randomly new-node)
      (push new-node diagram))
    (values diagram)))

(defun create-new-node (n max-number-of-states)			
  (let ((node (make-node :name  (intern (format nil "N-~A" n))
			 :id-number n		
			 :type-* :CHANCE)))
    (multiple-value-bind (label-list number-of-labels)
	(make-state-labels-for-node-number node max-number-of-states)
      (setf (node-distribution node)
	    (make-discrete-dist 
	      :relation-type-* :PROB	
	      :state-labels-* label-list
	      :number-of-states-* number-of-labels)))
    (values node)))

(defun make-state-labels-for-node-number (node max-number-of-states)
  (let ((number-of-labels (get-random-number-of-states max-number-of-states))
	labels)
    (dotimes (i number-of-labels)
      (push (make-label :name (intern (format nil "~A-~A" (node-name node) i))
			:id-number i
			:node node)
	    labels))
    (values (reverse labels) number-of-labels)))

(defun get-random-number-of-states (max-number-of-states)
  (cond
    ((not (and (integerp max-number-of-states)(> max-number-of-states 1)))
     (error "Max-number-of-states is ~A. Should be an integer greater than 1."
	    max-number-of-states))
    (t (+ 2 (random (- max-number-of-states 1))))))

;------------------------------------------------------------------------

(defun add-links (n pred-list)
  (dolist (p pred-list)(push n (node-successors p)))
  (setf (node-predecessors n) pred-list))

(defun choose-random-subset (list  &optional max-size)
  (let* ((list-length (length list))
	 (max-subset-size (if max-size (min max-size list-length) list-length))
	 (subset-size (random (+ max-subset-size 1)))
	 (random-subset nil)
	 random-element)
    (loop
      (if (zerop subset-size)(return random-subset))
      (setq random-element (nth (random list-length) list))
      (push random-element random-subset)
      (setq list (remove random-element list))
      (decf subset-size)
      (decf list-length))))

; -----Other utilities -----------------------------------------------------------

; Returns the number of arcs in the diagram.

(defun number-of-arcs (diagram)
  (sum-over (n diagram)(number-of-predecessors n)))

;; Removes the existing evidence in the diagram and adds evidence to a
;; randomly chosen set of nodes. The max size of this set is
;; <max-number-of-evidence-nodes> Node predicate is used to restrict 
;;; evidence generation to only those nodes which satisfy the predicate e.g
;;; #'barren-node-p will only generate evidence for nodes w/o successor

(defun create-random-evidence (diagram &optional max-number-of-evidence-nodes
			       (node-predicate #'node-p))
  (remove-evidence diagram)
  (let ((chosen-node-set (choose-random-subset (remove-if-not node-predicate diagram)
					       max-number-of-evidence-nodes)))
    (dolist (n chosen-node-set)
      (setf (node-state n)  (nth (random (length (state-labels n)))
					 (state-labels n))))
    (values chosen-node-set)))


; Setting the distribution of a diagram randomly. Useful sometimes


(defun randomize-probabilities (existing-belief-net)
  (mapc #'set-node-distribution-randomly existing-belief-net))

;; This function guarantees non-zero probabilities

(defun set-node-distribution-randomly (node &optional (random-seed 100))
	; Changed the following line 18 Jun 93 on bug report
        ; from Scott Mussmn
	; The function returns no values and is sideeffecting now.
	;(setf (distribution-repn node) (create-empty-distribution node))
  (create-empty-distribution node)
  (for-all-cond-cases (pred-case (node-predecessors node))
    (let ((total 0))
      (for-all-cond-cases (node-case node)
	(incf total (setf (prob-of node-case pred-case) (+ 1 (random random-seed)))))
      (for-all-cond-cases (node-case node)
	(setf (prob-of node-case pred-case)
	      (float (/ (prob-of node-case pred-case) total)))))))



