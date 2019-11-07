;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;;; Copyright (c) 1994-2000, Scott McKay.
;;; Copyright (c) 2001-2003, Scott McKay and Howard Shrobe.
;;; All rights reserved.  No warranty is expressed or implied.
;;; See COPYRIGHT for full copyright and terms of use.

(in-package :clim-internals)

;;; Genera-like "text scrolling" windows

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(clim-internals::text-scroll-output-history
	    clim-internals::add-text-scroll-element
	    clim-internals::delete-text-scroll-element
	    clim-internals::replace-text-scroll-element
	    clim-internals::find-text-scroll-element
	    clim-internals::output-record-text-scroll-item
	    clim-internals::with-text-scrolling-delayed
	    clim-internals::add-item
	    clim-internals::append-item
	    clim-internals::delete-item
	    clim-internals::delete-item-at-index
	    clim-internals::replace-item
	    clim-internals::find-item
	    clim-internals::clear-items
	    clim-internals::map-over-items)
	  'clim))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(clim::text-scroll-output-history
	    clim::add-text-scroll-element
	    clim::delete-text-scroll-element
	    clim::replace-text-scroll-element
	    clim::find-text-scroll-element
	    clim::output-record-text-scroll-item
	    clim::with-text-scrolling-delayed
	    clim::add-item
	    clim::append-item
	    clim::delete-item
	    clim::delete-item-at-index
	    clim::replace-item
	    clim::find-item
	    clim::clear-items
	    clim::map-over-items)
	  'clim))

;; The "text scrolling" output record
(defclass text-scroll-output-record
	  (output-record-mixin output-record-element-mixin output-record)
    ((elements :initform nil)
     (fill-pointer :initform 0 :type fixnum)
     ;; For optimizing BITBLT operations
     (delay-bitblt :initform nil)
     (delay-bitblt-state :initform nil)))

;; We recommend that this only be used for the top-level output record...
(defclass text-scroll-output-history
	  (stream-output-history-mixin text-scroll-output-record)
    ())


;;; Implement the output record protocol

(defmethod initialize-instance :after ((record text-scroll-output-record) &key)
  (setf (slot-value record 'fill-pointer) 0))

(defmethod output-record-children ((record text-scroll-output-record))
  (with-slots (elements fill-pointer) record
    (typecase elements
      (array
	(let ((result (make-list fill-pointer)))
	  (replace result elements :end1 fill-pointer :end2 fill-pointer)
	  result))
      (otherwise nil))))

(defmethod output-record-element ((record text-scroll-output-record) index)
  (with-slots (elements) record
    (and elements
	 (svref elements index))))

(defmethod output-record-count ((record text-scroll-output-record) &key fastp)
  (declare (ignore fastp))
  (with-slots (elements fill-pointer) record
    (typecase elements
      (array fill-pointer)
      (otherwise 0))))

(defmethod clear-output-record ((record text-scroll-output-record))
  (with-slots (elements fill-pointer) record
    (typecase elements
      (array (setq fill-pointer 0))
      (otherwise nil))))

(defmethod add-output-record (child (record text-scroll-output-record))
  (declare (ignore child))
  (error "You can't add children to this output record this way;~
	  use ~S instead" 'add-text-scroll-element))

(defmethod delete-output-record
	   (child (record text-scroll-output-record) &optional (errorp t))
  (declare (ignore child errorp))
  (error "You can't delete elements from this output record this way;~
	  use ~S instead" 'delete-text-scroll-element))

(defmethod map-over-output-records-overlapping-region
	   (function (record text-scroll-output-record) region 
	    &optional (x-offset 0) (y-offset 0) &rest continuation-args)
  (declare (dynamic-extent function continuation-args))
  (declare (fixnum x-offset y-offset))
  (declare (optimize (safety 0)))
  (with-slots (elements fill-pointer) record
    (typecase elements
      (array
	(if (or (null region) (eql region +everywhere+))
	  (dovector (element elements :start 0 :end fill-pointer :simple-p t)
	    (apply function element continuation-args))
	  (with-bounding-rectangle* (left1 top1 right1 bottom1) region
	    (translate-positions x-offset y-offset left1 top1 right1 bottom1)
	    ;; Subtract out the record offset from the region, to make comparison fair
	    (multiple-value-bind (xoff yoff) (output-record-position record)
	      (translate-positions (- xoff) (- yoff) left1 top1 right1 bottom1))
	    (let ((start (text-scroll-output-record-index
			   elements fill-pointer top1 :lower)))
	      (dovector (element elements :start start :end fill-pointer :simple-p t)
		(with-bounding-rectangle* (left2 top2 right2 bottom2) element
		  (cond ((ltrb-overlaps-ltrb-p left1 top1 right1 bottom1
					       left2 top2 right2 bottom2)
			 (apply function element continuation-args))
			((> top2 bottom1)
			 ;; Y is out of range, just give up
			 (return-from map-over-output-records-overlapping-region nil)))))))))
      (otherwise nil)))
  nil)

(defmethod map-over-output-records-containing-position
	   (function (record text-scroll-output-record) x y 
	    &optional (x-offset 0) (y-offset 0) &rest continuation-args)
  (declare (dynamic-extent function continuation-args))
  (declare (fixnum x y x-offset y-offset))
  (declare (optimize (safety 0)))
  (translate-positions x-offset y-offset x y)
  (with-slots (elements fill-pointer) record
    (typecase elements
      (array
	(multiple-value-bind (xoff yoff) (output-record-position record)
	  (translate-positions (- xoff) (- yoff) x y))
	(let ((end (text-scroll-output-record-index
		     elements fill-pointer y :upper)))
	  (if (zerop end)
	    (let ((element (svref elements 0)))
	      (with-bounding-rectangle* (left top right bottom) element
		(when (ltrb-contains-position-p left top right bottom x y)
		  (apply function element continuation-args))))
	    (dovector (element elements :start 0 :end end :from-end t :simple-p t)
	      (with-bounding-rectangle* (left top right bottom) element
		(cond ((ltrb-contains-position-p left top right bottom x y)
		       (apply function element continuation-args)
		       ;; (X,Y) can be only in a single element, by definition
		       (return-from map-over-output-records-containing-position nil))
		      ((< y top)
		       ;; Y is out of range, just give up
		       (return-from map-over-output-records-containing-position nil))))))))
      (otherwise nil)))
  nil)

(defun text-scroll-output-record-index (vector fill-pointer y bound)
  (declare (type simple-vector vector) (fixnum y fill-pointer))
  (declare (optimize (speed 3) (safety 0)))
  (check-type bound (member :lower :upper))
  (let ((below 0)
	(above fill-pointer))
    (declare (fixnum below above))
    (assert (<= below above))			;Binary search will loop otherwise.
    (let (#+(or Genera Minima) (vector vector))
      (declare (type vector vector))
      (loop
	(when (= above below)
	  (return above))
	(let* ((index (the fixnum (ash (the fixnum (+ above below)) -1)))
	       (element (svref vector index)))
	  (with-bounding-rectangle* (left top right bottom) element
	    (declare (ignore left right))
	    (case bound
	      (:lower
		(cond ((< y top)
		       (setq above index))
		      ((> y top)
		       (if (= below index)
			 (return (max (1- above) 0))
			 (setq below index)))
		      (t
		       (return (max (1- index) 0)))))
	      (:upper
		(cond ((< y bottom)
		       (setq above index))
		      ((> y bottom)
		       (if (= below index)
			 (return (min (1+ above) fill-pointer))
			 (setq below index)))
		      (t
		       (return (min (1+ index) fill-pointer))))))))))))

;; Given an output record, find the text scroll item that contains it, that
;; is, the output record whose parent is the text scroll output record
(defun output-record-text-scroll-item (record)
  (loop
    (let ((parent (output-record-parent record)))
      (typecase parent
	(text-scroll-output-record 
	  (return-from output-record-text-scroll-item record))
	(null
	  (return-from output-record-text-scroll-item nil))
	(otherwise
	  (setq record parent))))))


;;; The text scrolling output history protocol

(defmethod with-text-scrolling-delayed-2 ((record text-scroll-output-history) 
					  continuation stream)
  (with-slots (delay-bitblt delay-bitblt-state) record
    (let ((old-delay-bitblt delay-bitblt)
	  (old-delay-bitblt-state delay-bitblt-state))
      (unwind-protect
	  (progn
	    (setq delay-bitblt t
		  delay-bitblt-state nil)
	    (funcall continuation stream))
	(process-delayed-text-scrolling record :flush nil nil nil)
	(setq delay-bitblt old-delay-bitblt
	      delay-bitblt-state old-delay-bitblt-state)))))

;; Inserts the output record element at INDEX, pushing the following elements down
;; ELEMENT is an output record
(defmethod add-text-scroll-element ((record text-scroll-output-history) element index)
  (with-slots (elements fill-pointer delay-bitblt) record
    (when (null elements)
      (setq elements (make-array 50)))
    (when (null index)
      (setq index fill-pointer))
    ;; Insert the new element into the tscroll record
    (multiple-value-setq (elements fill-pointer)
      (simple-vector-insert-element element index elements fill-pointer 50))
    ;; Update the records positions and the screen
    (if delay-bitblt
      (process-delayed-text-scrolling record :add index 0 1)
      (shift-text-scroll-elements record index 0 1))))

;; Deletes the output record element at INDEX, pulling the following elements up
(defmethod delete-text-scroll-element ((record text-scroll-output-history) index)
  (with-slots (elements fill-pointer stream delay-bitblt) record
    (let* ((element (svref elements index))
	   (old-height (+ (bounding-rectangle-height element)
			  (stream-vertical-spacing stream))))
      ;; Delete the element from the tscroll record
      (let ((new-fp (the fixnum (1- fill-pointer)))
	    (vector elements))
	(declare (type simple-vector vector) (fixnum new-fp))
	(unless (= (the fixnum index) new-fp)
	  (do ((i (the fixnum index) (1+ i)))
	      ((= i new-fp))
	    (declare (fixnum i) (optimize (speed 3) (safety 0)))
	    (setf (svref vector i) (svref vector (1+ i)))))
	(setf fill-pointer new-fp))
      ;; Update the records positions and the screen
      (if delay-bitblt
	(process-delayed-text-scrolling record :delete index old-height 0)
	(shift-text-scroll-elements record index old-height 0)))))

;; Replace the output record element at INDEX, moving the following elements
;; ELEMENT is an output record
(defmethod replace-text-scroll-element ((record text-scroll-output-history) element index)
  (with-slots (elements fill-pointer stream delay-bitblt) record
    (let ((old-height (+ (bounding-rectangle-height (aref elements index))
			 (stream-vertical-spacing stream))))
      (setf (svref elements index) element)
      ;; Update the records positions and the screen
      (if delay-bitblt
	(process-delayed-text-scrolling record :replace index old-height 1)
	(shift-text-scroll-elements record index old-height 1)))))

;; Operation is one of :ADD, :DELETE, :REPLACE or :FLUSH.
;; INDEX is the index into elements of the first affected element.
;; OLD-HEIGHT is the height of the elements being deleted or replaced (0 for :ADD).
;; N-NEW is the number of new elements (already at position INDEX in elements) (0 for :DELETE).
(defmethod process-delayed-text-scrolling ((record text-scroll-output-history) 
					   operation index old-height n-new)
  (with-slots (delay-bitblt-state) record
    (cond ((eql operation :flush)
	   ;; Carry out pending operations immediately
	   (when delay-bitblt-state
	     (destructuring-bind (op i old-h n) delay-bitblt-state
	       (declare (ignore op))
	       (shift-text-scroll-elements record i old-h n))))
	  ((null delay-bitblt-state)
	   ;; No pending ops, just save this one
	   (setq delay-bitblt-state (list operation index old-height n-new)))
	  (t
	   (destructuring-bind (op i old-h n) delay-bitblt-state
	     ;; Attempt to merge this operation with the pending ones.
	     ;; Try to merge for same operation that immediately precedes
	     ;; or follows the pending one.
	     (unless (and (eql op operation)
			  (case operation
			    (:add
			      (when (<= i index (+ i n))
				(incf (fourth delay-bitblt-state) n-new)))
			    (:delete
			      (when (<= (1- i) index i)
				(when (= index (1- i))
				  (decf (second delay-bitblt-state)))
				(incf (third delay-bitblt-state) old-height)))
			    (:replace
			      (when (<= (- i n-new) index (+ i n 1))
				(cond ((= index (- i n-new))
				       (decf (second delay-bitblt-state) n-new)
				       (incf (fourth delay-bitblt-state) n-new))
				      ((= index (+ i n 1))
				       (incf (fourth delay-bitblt-state) n-new)))
				(incf (third delay-bitblt-state) old-height)))))
	       ;; Couldn't merge, so carry out pending ones now and save this one
	       (shift-text-scroll-elements record i old-h n)
	       (setq delay-bitblt-state (list operation index old-height n-new))))))))

(defmethod shift-text-scroll-elements ((record text-scroll-output-history)
				       index old-height n-new)
  (with-slots (left top right bottom elements fill-pointer stream) record
    (let* ((y0 (if (zerop index)		;top of changed region
		 0
		 (bounding-rectangle-bottom (svref elements (1- index)))))
	   (y1 y0)				;(to be) bottom of changed region, or just Y0
	   (new-right right))
      (unless (zerop n-new)
	;; Set parent and position of new elements, and compute Y1
	(let ((vsp (stream-vertical-spacing stream)))
	  (dovector (elt elements :start index :end (+ index n-new) :simple-p t)
	    (setf (output-record-parent elt) record)
	    (note-output-record-attached elt stream)
	    (output-record-set-position elt 0 (incf y1 vsp))
	    (shift-old-cursor-position elt y1)
	    (multiple-value-bind (width height) (bounding-rectangle-size elt)
	      (maxf new-right width)
	      (incf y1 height)))))
      (let* ((new-height (- y1 y0))
	     (dheight (- new-height old-height)))
	;; Update positions of records following affected area.
	(unless (zerop dheight)
	  (dovector (elt elements :start (+ index n-new) :end fill-pointer :simple-p t)
	    (multiple-value-bind (x y) (output-record-position elt)
	      (output-record-set-position elt x (+ y dheight)))
	    (shift-old-cursor-position elt dheight)))
	;; Update bounding box of the scroll record
	(bounding-rectangle-set-edges record left top new-right (+ bottom dheight))
	;; Now shift the display up or down
	(with-bounding-rectangle* (sleft stop sright sbottom) (pane-viewport-region stream)
	  (declare (ignore stop))
	  (let ((medium (sheet-medium stream)))
	    (unless (or (zerop dheight) (<= sbottom y1))
	      (when (or (minusp dheight)
			(/= (1+ index) fill-pointer))
		;; Shift the visible output on the screen
		(let* ((from-y (+ y0 old-height))
		       (height (- sbottom (max 0 dheight) from-y)))
		  (when (plusp height)
		    (copy-area medium sleft from-y (- sright sleft) height sleft y1))))
	      (when (minusp dheight)
		(let ((top (max (+ sbottom dheight) y0)))
		  ;; When shifting up, we may need to replay some records at the bottom
		  (medium-clear-area medium sleft top sright sbottom)
		  (stream-replay stream (make-bounding-rectangle sleft top sright sbottom)))))
	    (when (and (plusp n-new) (<= y0 sbottom))
	      (medium-clear-area medium sleft y0 sright y1)
	      (dovector (elt elements :start index :end (+ index n-new) :simple-p t)
		(replay elt stream)))))))))

;; Returns the index associated with the output record element
;; ELEMENT is an output record
(defmethod find-text-scroll-element ((record text-scroll-output-history) element)
  (with-slots (elements fill-pointer) record
    (and elements
	 (dotimes (i fill-pointer nil)
	   (when (eql (svref elements i) element)
	     (return-from find-text-scroll-element i))))))

(defmethod find-text-scroll-element ((record text-scroll-output-history) (function function))
  (with-slots (elements fill-pointer) record
    (and elements
	 (dotimes (i fill-pointer nil)
	   (when (funcall function (svref elements i))
	     (return-from find-text-scroll-element i))))))


;;; The text-scroll machinery moves the records and does the screen
;;; updating.  But UPDATING-OUTPUT records maintain an absolute screen
;;; position used during incremental redisplay to determine if they need
;;; to be updated.  The following methods change the remembered position
;;; whenever we move an output record that contains any updating records.

(defmethod shift-old-cursor-position ((record output-record-element-mixin) dy)
  (declare (ignore dy))
  nil)

(defmethod shift-old-cursor-position :after ((record output-record-mixin) dy)
  (map-over-output-records
    #'(lambda (child) (shift-old-cursor-position child dy)) record))

(defmethod shift-old-cursor-position :after ((record standard-updating-output-record) dy)
  (let ((position (output-record-old-cursor-position record)))
    (incf (point-y position) dy)))


;;; The stream interface to the above, compatible with ITEM-LIST-MANAGER

(defmacro with-text-scrolling-history ((stream history-var) &body body)
  (check-type history-var symbol)
  `(let ((,history-var (stream-output-history ,stream)))
     (check-type ,history-var text-scroll-output-history)
     ,@body))

(defmacro with-text-scrolling-delayed ((&optional stream) &body body)
  (default-output-stream stream with-text-scrolling-delayed)
  `(flet ((text-scrolling-delayed-body (,stream) ,@body))
     (declare (dynamic-extent #'text-scrolling-delayed-body))
     (with-text-scrolling-delayed-1 ,stream #'text-scrolling-delayed-body)))

(defmethod with-text-scrolling-delayed-1 ((stream window-stream) continuation)
  (with-text-scrolling-history (stream history)
    (with-text-scrolling-delayed-2 history continuation stream)))

(defmethod add-item ((stream window-stream) item
		     &key before-item after-item before-index after-index)
  (with-text-scrolling-history (stream history)
    (let ((index (cond (before-index before-index)
		       (after-index (1+ after-index))
		       (before-item (find-text-scroll-element history before-item))
		       (after-item (1+ (find-text-scroll-element history after-item))))))
      (add-text-scroll-element history item index))))

(defmethod append-item ((stream window-stream) item)
  (with-text-scrolling-history (stream history)
    (add-text-scroll-element history item nil)))

#+Genera (scl:fundefine 'delete-item)
#+Lispworks (clos::fundefine 'delete-item)
#-(or Genera Lispworks)
(progn (let* ((gf (symbol-function 'delete-item))
	      (methods (aisl-clos:generic-function-methods gf)))
	 (loop for method in methods do (remove-method gf method)))
       (defgeneric delete-item (stream item &key errorp)))

(defmethod delete-item ((stream window-stream) item &key (errorp t))
  (with-text-scrolling-history (stream history)
    (let ((index (find-text-scroll-element history item)))
      (if (null index)
	(when errorp
	  (error "The element ~S was not found in ~S" item history))
	(delete-text-scroll-element history index)))))

(defmethod delete-item-at-index ((stream window-stream) index)
  (with-text-scrolling-history (stream history)
    (delete-text-scroll-element history index)))

(defmethod replace-item ((stream window-stream) item index)
  (with-text-scrolling-history (stream history)
    (replace-text-scroll-element history item index)))

(defmethod find-item ((stream window-stream) item)
  (with-text-scrolling-history (stream history)
    (find-text-scroll-element history item)))

(defmethod clear-items ((stream window-stream))
  (with-text-scrolling-history (stream history)
    (window-clear stream)))

(defmethod map-over-items ((stream window-stream) function)
  (with-text-scrolling-history (stream history)
    (dovector (item (slot-value history 'elements) 
	       :end (slot-value history 'fill-pointer) :simple-p t)
      (funcall function item))))

;; No need for DISPLAY-ITEMS, DISPLAY-ITEMS-INCREMENTALLY, REDISPLAY-ITEMS
;; since that happens automatically

