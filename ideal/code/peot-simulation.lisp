;;; -*- Base: 10; Syntax: Common-Lisp; Default-character-style: (:FIX :ROMAN :NORMAL); Package: IDEAL; -*-					       

(in-package :ideal)

;;;;********************************************************
;;;;  Copyright (c) 1989, 1992 Rockwell International -- All rights reserved.
;;;;  Rockwell International Science Center Palo Alto Lab
;;;;********************************************************

;;;;;;;;;;;;;;;;;;;;;;;; Mark Peot ;;;;;;;;;;;;;;;;;;;;


(export '(FORWARD-SIMULATION-INFER))

;--------------------------------------------------------

;;;This is the redesigned version of the simulation algorithm.  
;;;Hopefully, this time the style is much closer to the style of IDEAL.

;;;In the original algorithm there were lots of hacks to make it run super fast.
;;;These have been removed entirely.  Sorry.  The strategy I am using is to glue
;;;the importance sampling matrix, the simulation state, and the simulation belief
;;;array into the extra slot on nodes.  

;;;The new version runs completely within the framework of IDEAL.

;;;This version does not use any fancy techniques other than sampling from an importance
;;;dist'n and markov scoring.  You can't turn off importance sampling, but that doesn't 
;;;matter.  Computation of the Markov blanket scores can be done on the fly or in advance.

;;;That is it!

;;;Algorithms are :NORMAL, :MBS, :CACHED-MBS, and :LOGIC.

;;;There are three flavors of restart for this algorithm.  There is 
;;;initialize which completely builds everything from scratch; reset which resets
;;;the data structures for a new decision; and continue which doesn't reset anything.


;;;Important routines:
;;;  FORWARD-SIMULATION-INFER (node-list iterations 
;;;                            &key (diagram nil) (algorithm :normal) (error-node ()))
;;;      runs the simulation routine on diagram--the goal is to have the simplest interface
;;;      possible to the simulation routine.  If a diagram is supplied, it will be used; 
;;;      otherwise, a new diagram will be constructed and used.
;;;  FORWARD-SIMULTION-INITIALIZE (node-list &key (algorithm :normal) (error-node ()))
;;;      Makes a new diagram from node-list.
;;;  SIM-CONTINUE (diagram iterations)  continues inference on a DIAGRAM (not a node list).
;;;      This allows you to run inference for a while, stop and sample the running belief
;;;      to determine whether you wish to continue.  Running belief is sampled using 
;;;      RUNNING-BELIEF-OF.  
;;;  SIM-INITIALIZE (diag algorithm)  Inits a diagram for a new algorithm.
;;;  SIM-RESET (diag)  Resets the running belief and evidence for a network.  When the evidence
;;;      in a network changes, you may update a diagram which has already been constructed 
;;;      to reset the 'belief accumulators' and evidence for a new inference run.


(defun forward-simulation-infer (node-list iterations 
				            &key (diagram nil) 
				                 (algorithm :normal)
						 (error-node ()))
  (let ((diag1 (if diagram 
		   (sim-continue diagram iterations)
		   (sim-infer node-list iterations 
			      :algorithm algorithm :error-node error-node))))
    (values node-list diag1)))

(defun forward-simulation-initialize (node-list 
				      &key (algorithm :normal)
				           (error-node ()))
  (let ((diag (make-diag :algorithm algorithm
			 :initializations nil
			 :node-list (order node-list))))
    (sim-initialize diag algorithm)
    (setf (error-node diag) error-node)
    diag))

(defun sim-infer (node-list iterations &key algorithm (error-node ()))
  (let ((diag (make-diag :algorithm algorithm
			 :initializations nil
			 :node-list (order node-list))))
    (sim-initialize diag algorithm)
    (setf (error-node diag) error-node)
    (sim-continue diag iterations)
    diag))

(defun sim-continue (diag iterations)
  ;computes belief using simulation.  There is no argument for the type of simulation
  ;as this function is intended for continuing a computation in progress.
  (ecase (diag-algorithm diag)
    (:normal (dotimes (i iterations)
	       (declare (ignore i))
	       (sim-sample diag)
	       (sim-score diag)))
    (:mbs (dotimes (i iterations)
	    (declare (ignore i))
	    (sim-sample diag)
	    (sim-mbs-score diag)))
    (:logic (dotimes (i iterations)
	      (declare (ignore i))
	      (sim-score diag (logic-sample diag))))
    (:cached-mbs (if (initialized-for diag :cached-mbs)
		     (dotimes (i iterations)
		       (declare (ignore i))
		       (sim-sample diag)
		       (sim-cached-mbs-score diag))
		     (error "~A not initialized for :cached-mbs" diag))))
  (sim-estimate-errors diag)
  (normalize-and-set-beliefs diag)
  diag)

(defun sim-mbs-score (diag)
  (sim-cached-mbs-score diag))

(defun sim-cached-mbs-score (diag)
  (let ((nominal-score (score-diag diag)))
    (dolist (node (diag-sampling-list diag))
      (mbs-score-node node nominal-score (diag-conditioning-case diag)))))

;;;This version does not score a node if the nominal markov score is zero or the nominal-score
;;;is zero (surprise.).
(defun mbs-score-node (node nominal-score master-cond-case)
  (cond ((zerop nominal-score))
	(t (let ((old-state (simulation-node-state node)))
	     (dolist (label (state-labels node))
	       (setf (simulation-node-state node) label)
	       (incf (running-belief-of (simulation-node-case node))
		     (* (transition-prob-of (simulation-node-case node) master-cond-case)
			nominal-score)))
	     (setf (simulation-node-state node) old-state)))))
					
(defun sim-score (diag &optional (score (score-diag diag))) 
    (dolist (node (diag-sampling-list diag))
      (incf (running-belief-of (simulation-node-case node))
	    score)))

(defun score-diag (diag)
  (let ((total-score 1.0)
	(cond-case (diag-conditioning-case diag)))

    (dolist (node (diag-sampling-list diag))
      (setf total-score (* total-score
			   (/ (prob-of (simulation-node-case node)
				       cond-case)
			      (importance-prob-of (simulation-node-case node)
						  cond-case)))))
    (dolist (node (diag-evidence-list diag))
      (setf total-score (* total-score (prob-of (simulation-node-case node) cond-case))))
    total-score))

(defun logic-sample (diag)
 (let ((conditioning-case (diag-conditioning-case diag)))
    (dolist (node (diag-sampling-list diag) (score-diag diag))
      (sample node conditioning-case)
      (if (and (specific-evidence-node-p node)
	       (not (eq (node-state node)
			(simulation-node-state node))))
	  (return 0.0)))))
      
(defun sim-sample (diag)
   ;IDEAL does not care how many elements are in a cond-case.
   ;I save on cons cells by building a master cond-case and modifying it as I sample.
   ;The algorithm iterates through all of the nodes setting successive elements of the
   ;'master' cond case.  This master cond case is used when conditioning all other nodes.
   ;This sounds complicated, but is actually easier than building a cond case on every
   ;probability access.
  (let ((conditioning-case (diag-conditioning-case diag)))
    (dolist (node (diag-sampling-list diag))
      (sample node conditioning-case))))

(defun sample (node conditioning-case)
  (let ((random-number (random 1.0))
	(total 0.0))
    (dolist (state (state-labels node))
      (setf (simulation-node-state node) state)
      (if (>= (incf total (importance-prob-of (simulation-node-case node)
					      conditioning-case))
	      random-number)
	  (return)
	  nil))))

(defun sim-initialize (diag algorithm)
  (reset-inits diag)
  (dolist (node (diag-node-list diag))
    (setf (node-bel node) (make-probability-array node)))
  (dolist (node (diag-node-list diag))
    (setf (node-sim-node node) 
	  (make-sim-node
	    :importance-sampling-array
	    (copy-node-probability-array node)
	    :belief
	    (make-belief-array node)
	    :diag diag)))
  (let ((full-cond-case
	  (make-conditioning-case
	    (mapcan #'(lambda (node) (make-arbitrary-node-case node))
		    (diag-node-list diag)))))
    (setf (diag-conditioning-case diag)    ;see sim-sample for an explanation.
	  full-cond-case)
    (dangerous-ideal-structure-sharing-copy full-cond-case))   
			   		        ;This function takes advantage of a
						;feature of IDEAL which is not SUPPOSED to
						;change.  If the representation of cond-cases
						;changes, this function needs to be changed.
						;You have been warned.
  (add-to-initializations diag algorithm)
  (sim-reset diag))

(defun add-to-initializations (diag algorithm)
  (setf (initialized-for diag) algorithm)
  (case algorithm
    ((:cached-mbs :mbs)
      (dolist (node (diag-node-list diag))
	(setf (markov-blanket node) (find-markov-blanket node))
	(setf (transition-array node) (make-transition-array node))
	(initialize-transition-probabilities node)))))


;;;This function makes the node-case inside a simulation node and the main algorithm 
;;;conditioning case EQ.  The idea is that one would modify the local node state during 
;;;simulation and have the main node case automatically change.

(defun dangerous-ideal-structure-sharing-copy (full-cond-case)
    (dolist (cond-caselet full-cond-case)
      (setf (simulation-node-case (first cond-caselet))
	    (list cond-caselet))))

;;;just a stub for now...
(defun sim-estimate-errors (diag)
  (declare (ignore diag)))

(defun make-belief-array (node)
  (make-array (number-of-states node)
	      :initial-element 0.0))


(defun sim-reset (diag)
  (dolist (node (diag-node-list diag))
    (reset-belief node))
  (update-evidence diag))

(defun update-evidence (diag)
  (cond ((initialized-for diag :logic)
	 (setf (diag-sampling-list diag) (diag-node-list diag)))
	(t (let ((parameter-node-list nil)
		 (evidence-node-list nil))
	     (dolist (node (diag-node-list diag))
	       (if (specific-evidence-node-p node)
		   (progn (setf (simulation-node-state node)
				(node-state node))
			  (reset-belief node)
			  (push node evidence-node-list)
			  (setf (running-belief-of
				  (make-conditioning-case 
				    (list (cons node (node-state node)))))
				1.0))
		   (push node parameter-node-list)))
	     (setf (diag-evidence-list diag)
		   evidence-node-list)
	     (setf (diag-sampling-list diag)
		   (nreverse parameter-node-list))))))

(defun reset-belief (node)
  (for-all-cond-cases (node-case (list node))
    (setf (running-belief-of node-case) 0.0)))

(defun normalize-and-set-beliefs (diag)
  (dolist (node (diag-node-list diag))
    (normalize-and-set-beliefs-of-node node)))

(defun normalize-and-set-beliefs-of-node (node)
  (let ((total 0.0))
    (for-all-cond-cases (node-case (list node))
      (incf total (running-belief-of node-case)))
    (if (zerop total)
	(error "ZERO JOINT PROBABILITY IN NODE ~A." node)
	(for-all-cond-cases (node-case (list node))
	  (setf (belief-of node-case)
		(/ (running-belief-of node-case)
		   total))))))

(defun copy-vector (vector)
  (let* ((dimension (first (array-dimensions vector)))
	 (new-vector (make-array dimension)))
    (dotimes (loc dimension new-vector)
      (setf (aref new-vector loc)
	    (aref vector loc)))))

(defun copy-node-probability-array (node)
  ;Sam claims that probability arrays are vectors.  If this changes, change this function.
  (copy-vector (distribution-repn node)))

;;;The running belief is the belief during the simulation...  Needs to be normalized before
;;;it is truly useful to the outside consumer.  The normalized version is returned by 
;;;sim-belief-of.  sim-belief-of is not settable.  This function is designed to run without
;;;interleaving so that it can get a valid simulation belief from another process...

(defun sim-belief-of (node-case)
  (#+GENERA scl:without-interrupts #-GENERA progn
    (let ((total 0.0))
      (for-all-cond-cases (inner-node-case (list (node-in node-case)))
	(incf total (running-belief-of inner-node-case)))
      (if (zerop total)
	  (error "ZERO JOINT PROBABILITY IN NODE ~A." (node-in node-case))
	  (/ (running-belief-of node-case) total)))))









