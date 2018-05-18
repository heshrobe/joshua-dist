;;; -*- Mode: Lisp; Package: JOSHUA-USER; Syntax: Joshua-cloe -*-
;;;>
;;;> *****************************************************************************************
;;;> ** (c) Copyright 1990 Symbolics, Inc.  All rights reserved.
;;;> ** Portions of font library Copyright (c) 1984 Bitstream, Inc.  All Rights Reserved.
;;;>
;;;>    The software, data, and information contained herein are proprietary 
;;;> to, and comprise valuable trade secrets of, Symbolics, Inc., which intends 
;;;> to keep such software, data, and information confidential and to preserve 
;;;> them as trade secrets.  They are given in confidence by Symbolics pursuant 
;;;> to a written license agreement, and may be used, copied, transmitted, and 
;;;> stored only in accordance with the terms of such license.
;;;> 
;;;> Symbolics, Symbolics 3600, Symbolics 3670 (R), Symbolics 3675 (R), Symbolics 3630,
;;;> Symbolics 3640, Symbolics 3645 (R), Symbolics 3650 (R), Symbolics 3653, Symbolics
;;;> 3620 (R), Symbolics 3610 (R), Symbolics Common Lisp (R), Symbolics-Lisp (R),
;;;> Zetalisp (R), Genera (R), Wheels (R), Dynamic Windows (R), Showcase, SmartStore (R),
;;;> Semanticue (R), Frame-Up (R), Firewall (R), MACSYMA (R), COMMON LISP MACSYMA (R),
;;;> CL-MACSYMA (R), LISP MACHINE MACSYMA (R), MACSYMA Newsletter (R), PC-MACSYMA, Document
;;;> Examiner (R), Delivery Document Examiner, S-DYNAMICS (R), S-GEOMETRY (R), S-PAINT (R),
;;;> S-RECORD, S-RENDER (R), Displacement Animation, FrameThrower, PaintAmation, "Your Next
;;;> Step in Computing" (R), Ivory, MacIvory, MacIvory model 2, XL400, Symbolics UX400S, 
;;;> Symbolics C, Symbolics Pascal (R), Symbolics Prolog, Symbolics Fortran (R), CLOE (R),
;;;> CLOE Application Generator, CLOE Developer, CLOE Runtime, Common Lisp Developer,
;;;> Symbolics Concordia, Joshua, and Statice (R) are trademarks of Symbolics, Inc.
;;;> 
;;;> RESTRICTED RIGHTS LEGEND
;;;>    Use, duplication, and disclosure by the Government are subject to restrictions 
;;;> as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data and Computer 
;;;> Software Clause at DFAR 52.227-7013.
;;;> 
;;;>      Symbolics, Inc.
;;;>      8 New England Executive Park, East
;;;>      Burlington, Massachusetts  01803
;;;>      United States of America
;;;>      617-221-1000
;;;> *****************************************************************************************
;;;>
;;; Created 3/08/89 13:11:47 by HES running on MERLIN at SCRC.

(defvar *time* nil "The Global Clock")

;;; now we need rules stating what the production time for 
;;; each object type is for each factory type

(defvar *production-times* (make-hash-table :test #'equal :size 3))

(defun initialize-production-times ()
  (clrhash *production-times*)
  (loop for object-type in '(widget-1 widget-2 widget-3)
	do (loop for factory-types in '(factory-1 factory-2 factory-3)
		 for time = (loop for foo = (random 5) when (not (zerop foo)) return foo)
		 do (setf (gethash (list object-type factory-types) *production-times*)
			  time))))

(initialize-production-times)

(defun production-time (widget factory)
  (gethash (list (type-of widget) (type-of factory)) *production-times*))



(define-object-type widget
   :slots ((production-completion-time :equalities nil)))

(define-object-type variable-size-buffer
   :slots ((input :initform nil)
	    (output :initform nil)
	    (output-request-acknowledged :initform nil)
	    (queue :set-valued t :initform nil :equalities nil)
	    (clock :initform nil)))

(define-object-type manufacturing-site
   :slots ((production-capacity :initform 3 :equalities nil)
	   (clock :initform nil)
	   (input-request :initform nil)
	   (input-request-acknowledge :initform nil)
	   (output :attached-actions t :initform nil))
   :other-instance-variables ((things-being-produced :initform (make-heap)
						     :reader things-being-produced))
   )



(define-object-type widget-1
   :included-object-types (widget))

(define-object-type widget-2
   :included-object-types (widget))

(define-object-type widget-3
   :included-object-types (widget))

(define-object-type factory-1
   :included-object-types (manufacturing-site))

(define-object-type factory-2
   :included-object-types (manufacturing-site))

(define-object-type factory-3
   :included-object-types (manufacturing-site))




(defrule variable-size-buffer-input (:forward)
  If [and [object-type-of ?buffer variable-size-buffer]
	  [value-of (?buffer clock) HI]
	  [value-of (?buffer input) ?Thing]
	  (not (null ?Thing))]
  Then `[value-of (?buffer queue) ?Thing])

(defrule variable-size-buffer-output (:forward)
  If [and [object-type-of ?buffer variable-size-buffer]
	  [value-of (?buffer clock) HI]]
  Then (let ((stuff (queue ?buffer)))
	 (when stuff
	   (tell `[value-of (?buffer output) ,(car (last stuff))]))))

(defrule variable-size-buffer-output-remove-from-queue (:forward)
  If [and [object-type-of ?buffer variable-size-buffer]
	  [value-of (?buffer clock) HI]
	  [value-of (?buffer output) ?output]
	  [value-of (?buffer queue) ?output] :support ?f1
	  [value-of (?buffer output-request-acknowledged) HI]]
  then (untell ?f1))

;;; On the low phase clear all external signals
;;; Set output-request-acknowledged  LOW
;;;                           INPUT  NIL
;;;                           OUTPUT NIL

(defrule variable-size-buffer-clear (:forward)
  If [and [object-type-of ?buffer variable-size-buffer]
	  [value-of (?buffer clock) LOW]]
  then [and [value-of (?buffer output-request-acknowledged) LOW]
	    [value-of (?buffer input) NIL]
	    [value-of (?buffer output) NIL]])



;;; On the Hi phase of a clock, the machine accepts
;;; an input if there is one, by raising input-request-acknowledge

(defrule manufacturing-site-input (:forward)
  If [and [object-type-of ?machine manufacturing-site]
	  [value-of (?machine clock) HI]
	  [value-of (?machine input-request) ?widget]]
  then (when
	 (and (not (null ?widget))
	      (< (heap-active-size (things-being-produced ?machine)) (production-capacity ?machine)))
	 (tell `[value-of (?widget production-completion-time) ,(+ *time* (production-time ?widget ?machine))])
	 (heap-insert (things-being-produced ?machine) ?widget (production-completion-time ?widget))
	 (tell [value-of (?machine input-request-acknowledge) HI])))

;;; On the LOW phase
;;; the machine clears any input requests
;;; the machine clears any input-request-acknowledge
;;; the machine clears any outputs

(defrule manufacturing-site-clear (:forward)
  If [and [object-type-of ?machine manufacturing-site]
	  [value-of (?machine clock) LOW]]
  then [and [value-of (?machine input-request) NIL]
	    [value-of (?machine output) NIL]
	    [value-of (?machine input-request-acknowledge) LOW]])

;;; On the High Phase of a Clock
;;; Output anything whose production time has reached 0
;;; This is buggy since, two things can be and then we
;;; have a race condition
(defrule manufacturing-site-output (:forward)
  If [and [object-type-of ?machine manufacturing-site]
	  [value-of (?machine clock) HI]]
  then (multiple-value-bind (thing production-completion-time)
	   (heap-top (things-being-produced ?machine))
	 (when (and thing (> *time* production-completion-time))
	   (tell `[value-of (?machine output) ,thing])
	   (heap-remove (things-being-produced ?machine)))))


(clim:define-application-frame widgetsim
			  ()
    ((initialized :initform nil)
     (number-of-iterations :initform 20)
     (machine-array :initform nil)
     (buffer-array :initform nil)
     (widget-array :initform nil)
     (output-produced :initform nil))
  (:panes
    ((title :title
	    :display-string "Factory Simulator"
	    :height (1 :lines)
	    :display-after-commands nil)
     (viewer :application
	     :incremental-redisplay t
	     :display-function update-widget-display
	     :scroll-bars nil
	     )
     (output-view :application
		  :incremental-redisplay t
		  :display-function update-production-display
		  :height (5 :lines)
		  )
     (production-table :application
		       :incremental-redisplay t
		       :display-function update-production-table
		       :height (5 :lines)
		       :scroll-bars nil
		       )
     (commands :command-menu
	       ;; :center-p t
	       ;; :columns 2 
	       :menu-level :simulation-commands
	       :height (5 :lines)
	       :scroll-bars nil
	       )
     (interactor :interactor :height 4)))
  (:layout
	((main
	   (:column 1
	    (title :compute) (viewer :rest)
	    (:row 1/8
	     (output-view :rest)
	     (commands :compute)
	     (production-table :compute))
	    (interactor 1/12))))))



(defmethod initialize-instance :after ((self widgetsim) &key &allow-other-keys)
  (with-slots (initialized) self
    (unless initialized
      (initialize-state self)
      (setq initialized t))))

(define-widgetsim-command (com-set-simulation-time-limit :menu t :name t)
    ((limit 'integer :prompt "Time To Stop Simulation" :default nil))
  (clim:with-frame-state-variables (widgetsim)
    (setf number-of-iterations limit)))

(defmethod refresh-all-panes ((self widgetsim))
  (mapc #'(lambda (pane-name)
	      (clim:redisplay-frame-pane self pane-name :force-p t))
	  '(viewer output-view commands production-table interactor)))

(define-widgetsim-command (initialize :menu t :name t)
    ()
  (clim:with-frame-state-variables (widgetsim)
    (initialize-state clim:*application-frame*)
    (refresh-all-panes clim:*application-frame*)))

(define-widgetsim-command (refresh :menu t :name t)
    ()
  (refresh-all-panes clim:*application-frame*))

(defmethod run-a-step ((self widgetsim))
  (clim:with-frame-state-variables (widgetsim)
    (incf *time*)
    (tell [value-of (buffer1 clock) HI])
    (let ((new-widget (and (evenp (random 10))
			   (make-object (aref widget-array (random 3))
					:name (intern (format nil "~a-~2,'0d" 'widget *time*))
					:superpart-object nil))))
      (when new-widget
	(tell `[value-of (buffer1 input) ,new-widget]))
      (tell [value-of (machine1 clock) LOW]))
    (clim:redisplay-frame-pane clim:*application-frame* 'viewer)
    (clim:redisplay-frame-pane clim:*application-frame* 'output-view)))

(define-widgetsim-command (run :menu t :name t)
    ()
  (clim:with-frame-state-variables (widgetsim)
    (loop until (>= *time* number-of-iterations)
	  doing (run-a-step clim:*application-frame*))))

(define-widgetsim-command (do-steps :menu t :name t)
    ((how-many 'integer :prompt "How Many Steps to Perform" :default nil))
   (loop for i below how-many
	 doing (progn i)
	       (run-a-step clim:*application-frame*)))

(defmethod initialize-state ((self widgetsim))
  (with-slots (machine-array buffer-array output-produced widget-array) self
   (clear)
   (setq *time* 0)
   (setq output-produced nil)
   (setq widget-array #(widget-1 widget-2 widget-3))
   (let ((buffer1 (make-object 'variable-size-buffer :name 'buffer1))
	 (machine1 (make-object 'factory-1 :name 'machine1))
	 (buffer2 (make-object 'variable-size-buffer :name 'buffer2))
	 (machine2 (make-object 'factory-2 :name 'machine2))
	 (buffer3 (make-object 'variable-size-buffer :name 'buffer3))
	 (machine3 (make-object 'factory-3 :name 'machine3)))
     (setq machine-array (coerce (list machine1 machine2 machine3) 'array)
	   buffer-array (coerce (list buffer1 buffer2 buffer3) 'array))

     ;; Connect buffers to machines
     (loop for i below 3
	   for last-machine = nil then machine
	   for buffer = (aref buffer-array i)
	   for machine = (aref machine-array i)
	   do (tell `[equated (,buffer output) (,machine input-request)])
	      (tell `[equated (,buffer output-request-acknowledged)
		    (,machine input-request-acknowledge)])
	   when last-machine
	     do (tell `[equated (,last-machine output) (,buffer input)]))

     ;; Connect all the clocks
     (loop with first-buffer = (aref buffer-array 0)
	   for i below 3
	   for buffer = (aref buffer-array i)
	   for machine = (aref machine-array i)
	   unless (= i 0)
	     do (tell `[equated (,buffer clock) (,first-buffer clock)])
	   do (tell `[equated (,machine clock) (,first-buffer clock)]))

     ;; Randomly generate production capacities
     (flet ((non-zero-random (n)
	      (loop for x = (random n)
		    when (not (zerop x))
		      return x)))
       (tell `[value-of (machine1 production-capacity) ,(non-zero-random 5)])
       (tell `[value-of (machine2 production-capacity) ,(non-zero-random 5)])
       (tell `[value-of (machine3 production-capacity) ,(non-zero-random 5)]))

     ;; Attach noticer to last factory
     (flet ((notice-machine-output (ignore current-value current-predication old-truth-value)
	       (declare (ignore old-truth-value))
	       (when (and (eql (predication-truth-value current-predication) *true*)
			  (typep current-value 'widget))
		 (setq output-produced (append output-produced (list current-value))))))
	(add-action '(machine3 output) #'notice-machine-output))
     )))

(clim:define-presentation-type capacity-presentation ())

;;   :inherit-from '(slot-presentation) which doesn't yet exist in JP's object system

(clim:define-presentation-method clim:present (capacity (type capacity-presentation)
							stream (view clim:textual-view)
							&key &allow-other-keys)
  (format stream "Capacity of ~s" (first (path-name capacity))))



(defmethod update-widget-display ((self widgetsim) stream)
  (clim:with-frame-state-variables (widgetsim)
    (clim:updating-output (stream
			    :unique-id 'cycle
			    :cache-value *time*)
      (format stream "~40tCycle ~d~%~%" *time*))
    (flet ((do-warehouse-column (stream i)
	     (let ((buffer (aref buffer-array (1- i))))
	       (clim:formatting-column (stream)
		 (clim:formatting-cell (stream :align-x :center :align-y :bottom)
		   (clim:updating-output (stream
					   :cache-value t
					   :unique-id (list 'warehouse-header i)
					   :id-test #'equal)
		     (format stream "~%Inventory")))
		 (loop for thing in (reverse (queue buffer))
		       for counter from 0
		       do (clim:formatting-cell (stream :align-x :center :align-y :bottom)
			    (clim:updating-output (stream
						    :cache-value (string (role-name thing))
						    :cache-test #'string-equal
						    :unique-id (list 'warehouse i counter)
						    :id-test #'equal)
			      (format stream "~%~a" (role-name thing))))))))
	   (do-factory-column (stream i)
	     (let ((machine (aref machine-array (1- i))))
	       (clim:formatting-column (stream)
		 (clim:formatting-cell (stream :align-x :center :align-y :bottom)
		   (clim:updating-output (stream
					   :cache-value t
					   :unique-id (list 'factory-header i)
					   :id-test #'equal)
		     (format stream "Production")))
		 (loop for thing being the heap-elements of (things-being-produced machine)
		       for counter from 0
		       do (clim:formatting-cell (stream :align-x :center :align-y :bottom)
			    (clim:updating-output (stream
						    :cache-value (string (role-name thing))
						    :cache-test #'string-equal
						    :unique-id (list 'factory i counter)
						    :id-test #'equal)
			      (format stream "~%~a" (role-name thing))))))))
	   (do-time-column (stream i)
	     (let ((machine (aref machine-array (1- i))))
	       (clim:formatting-column (stream)
		 (clim:formatting-cell (stream :align-x :center :align-y :bottom)
		   (clim:updating-output (stream
					   :cache-value t
					   :unique-id (list 'completion-header i)
					   :id-test #'equal)
		     (format stream "~%Ship It!")))
		 (loop for thing being the heap-elements of (things-being-produced machine)
		       for counter from 0
		       do (clim:formatting-cell (stream :align-x :center :align-y :bottom)
			    (clim:updating-output
			      (stream
				:cache-value (production-completion-time thing)
				:cache-test #'eql
				:unique-id (list 'completion i counter)
				:id-test #'equal)
			      (format stream "~%~d"
				      (production-completion-time thing)))))))))
      (clim:formatting-table (stream)
	(loop for i from 1 upto 3
	      do (do-warehouse-column stream i)
		 (do-factory-column stream i)
		 (do-time-column stream i))))))


(define-widgetsim-command (com-change-capacity-value :menu t :name t) 
    ((the-slot 'capacity-presentation :prompt "A Factories Capacity" :default nil)
     (new-value 'clim:expression :prompt "New capacity" :default nil))
   (tell `[value-of ,the-slot ,new-value]))



(defmethod update-production-display ((self widgetsim) stream)
  (clim:with-frame-state-variables (widgetsim)
    (when output-produced
      (clim:formatting-item-list (stream :n-columns 4)
	(loop for i from 0
	      for thing in output-produced
	      do (clim:formatting-cell (stream :align-x :center)
		   (clim:updating-output (stream
					   :unique-id i
					   :cache-value thing)
		     (format stream "~%~a" (role-name thing)))))))))



(clim:define-presentation-type production-matrix-entry ()
   :inherit-from '(clim:expression))

(define-widgetsim-command (com-set-production-matrix-entry)
    ((the-entry 'production-matrix-entry))
   (setf (gethash the-entry *production-times*)
	 (accept 'number :prompt "New Production matix entry" :default nil)))

(clim:define-presentation-to-command-translator production-matrix-entry-to-change-production-matrix-entry
   (production-matrix-entry com-set-production-matrix-entry widgetsim
    :documentation "Modify this production matrix entry"
    :gesture :modify
    :priority 10)
   (object)
 `(,object))

(define-widgetsim-command (com-change-production-time :menu t :name t )
    ((the-entry 'production-matrix-entry :prompt "Production Matrix Entry" :default nil)
     (new-value 'number :prompt "New Value" :default nil))
   (setf (gethash the-entry *production-times*) new-value))

(defmethod update-production-table ((self widgetsim) stream)
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream)
	(clim:updating-output (stream
				:Unique-id 'column-headings
				:cache-value t)
	  (write-string " " stream)))
      (loop for factory-type in '(factory-1 factory-2 factory-3)
	    do (clim:formatting-cell (stream :align-x :center)
		 (clim:updating-output (stream
					 :unique-id (list 'name factory-type)
					 :id-test #'equal
					 :cache-value factory-type)
		   (write-string (string factory-type) stream)))))
    (loop for object-type in '(widget-1 widget-2 widget-3)
	  do (clim:formatting-row (stream)
	       (clim:formatting-cell (stream)
		 (clim:updating-output (stream
					 :Unique-id (list 'row-heading object-type)
					 :id-test #'equal
					 :cache-value object-type)
		   (write-string (string object-type) stream)))
	       (loop for factory-type in '(factory-1 factory-2 factory-3)
		     for key = (list object-type factory-type)
		     for time = (gethash key *production-times*)
		     do (clim:formatting-cell (stream :align-x :center)
			  (clim:updating-output (stream
						  :Unique-id (cons 'cell key)
						  :id-test #'equal
						  :cache-value time)
			    (clim:with-output-as-presentation (:stream stream
							       :object key
							       :type 'production-matrix-entry)
			      (format stream "~d" time)))))))))



;;; Drawing truck
;;; Bounding box 174,278  349,206
(DEFUN DRAW-TRUCK (&OPTIONAL (*STANDARD-OUTPUT* *STANDARD-OUTPUT*))
  ;; Rectangle-60
  (PROGN
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 174 277 348 206 :GRAY-LEVEL 0))
  ;; Rectangle-61
  (PROGN
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 225 233 256 213 :GRAY-LEVEL 0.25)
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 225 233 256 213 :FILLED NIL))
  ;; Rectangle-62
  (PROGN
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 258 226 270 213 :GRAY-LEVEL 0.25)
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 258 226 270 213 :FILLED NIL))
  ;; Circular Arc/Sector-1
  (PROGN
    (CLIM:DRAW-CIRCLE* *STANDARD-OUTPUT* 461/2
		      421/2
		      4.3011622
		      :START-ANGLE
		      2.5213432
		      :END-ANGLE
		      0.6202495
		      :GRAY-LEVEL
		      0.25)
    (CLIM:DRAW-CIRCLE* *STANDARD-OUTPUT* 461/2
		       421/2
		       4.3011622
		       :START-ANGLE
		       2.5213432
		       :END-ANGLE
		       0.6202495
		       :FILLED
		       NIL))
  ;; Circular Arc/Sector-2
  (PROGN
    (CLIM:DRAW-CIRCLE* *STANDARD-OUTPUT* 483/2
		       419/2
		       4.3011622
		       :START-ANGLE
		       2.5213432
		       :END-ANGLE
		       0.6202495
		       :GRAY-LEVEL
		       0.25)
    (CLIM:DRAW-CIRCLE* *STANDARD-OUTPUT* 483/2
		       419/2
		       4.3011622
		       :START-ANGLE
		       2.5213432
		       :END-ANGLE
		       0.6202495
		       :FILLED
		       NIL))
  ;; Circular Arc/Sector-3
  (PROGN
    (CLIM:DRAW-CIRCLE* *STANDARD-OUTPUT* 523/2
		       419/2
		       4.3011622
		       :START-ANGLE
		       2.5213432
		       :END-ANGLE
		       0.6202495
		       :GRAY-LEVEL
		       0.25)
    (CLIM:DRAW-CIRCLE* *STANDARD-OUTPUT* 523/2
		       419/2
		       4.3011622
		       :START-ANGLE
		       2.5213432
		       :END-ANGLE
		       0.6202495
		       :FILLED
		       NIL)))


;;; Drawing factory
;;; Bounding box 170,380  345,278
(DEFUN DRAW-FACTORY (&OPTIONAL (*STANDARD-OUTPUT* *STANDARD-OUTPUT*))
  ;; Rectangle-1
  (PROGN
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 170 337 344 278 :GRAY-LEVEL 0.25)
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 170 337 344 278 :FILLED NIL))
  ;; Rectangle-2
  (PROGN
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 179 379 191 338 :GRAY-LEVEL 0.25)
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 179 379 191 338 :FILLED NIL))
  ;; Rectangle-3
  (PROGN
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 214 379 226 338 :GRAY-LEVEL 0.25)
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 214 379 226 338 :FILLED NIL))
  ;; Rectangle-4
  (PROGN
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 280 379 292 338 :GRAY-LEVEL 0.25)
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 280 379 292 338 :FILLED NIL))
  ;; Rectangle-5
  (PROGN
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 315 379 327 338 :GRAY-LEVEL 0.25)
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 315 379 327 338 :FILLED NIL))
  ;; Rectangle-6
  (PROGN
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 246 378 258 337 :GRAY-LEVEL 0.25)
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 246 378 258 337 :FILLED NIL))
  ;; Rectangle-7
  (PROGN
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 181 329 188 291 :GRAY-LEVEL 0)
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 181 329 188 291 :FILLED NIL))
  ;; Rectangle-8
  (PROGN
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 197 329 204 291 :GRAY-LEVEL 0)
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 197 329 204 291 :FILLED NIL))
  ;; Rectangle-9
  (PROGN
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 224 329 231 291 :GRAY-LEVEL 0)
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 224 329 231 291 :FILLED NIL))
  ;; Rectangle-10
  (PROGN
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 240 329 247 291 :GRAY-LEVEL 0)
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 240 329 247 291 :FILLED NIL))
  ;; Rectangle-11
  (PROGN
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 265 329 272 291 :GRAY-LEVEL 0)
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 265 329 272 291 :FILLED NIL))
  ;; Rectangle-12
  (PROGN
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 281 329 288 291 :GRAY-LEVEL 0)
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 281 329 288 291 :FILLED NIL))
  ;; Rectangle-13
  (PROGN
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 301 329 308 291 :GRAY-LEVEL 0)
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 301 329 308 291 :FILLED NIL))
  ;; Rectangle-14
  (PROGN
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 317 329 324 291 :GRAY-LEVEL 0)
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 317 329 324 291 :FILLED NIL)))

;;; Drawing warehouse
;;; Bounding box 338,297  513,225
(DEFUN DRAW-WAREHOUSE (&OPTIONAL (*STANDARD-OUTPUT* *STANDARD-OUTPUT*))
  ;; Rectangle-34
  (PROGN
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 338 284 512 225 :GRAY-LEVEL 0.25)
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 338 284 512 225 :FILLED NIL))
  ;; Rectangle-17
  (PROGN
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 352 296 375 285 :GRAY-LEVEL 0.25)
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 352 296 375 285 :FILLED NIL))
  ;; Rectangle-16
  (PROGN
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 400 253 431 225 :GRAY-LEVEL 0)
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 400 253 431 225 :FILLED NIL))
  ;; Rectangle-48
  (PROGN
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 414 296 437 285 :GRAY-LEVEL 0.25)
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 414 296 437 285 :FILLED NIL))
  ;; Rectangle-49
  (PROGN
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 474 296 497 285 :GRAY-LEVEL 0.25)
    (CLIM:DRAW-RECTANGLE* *STANDARD-OUTPUT* 474 296 497 285 :FILLED NIL)))




(defvar *clim-root* (clim:open-root-window :sheet))

(defvar *factory-demo-frame* (clim:make-application-frame 'widgetsim :parent *clim-root*))

#||
()
(clim:run-frame-top-level *factory-demo-frame*)
||#





