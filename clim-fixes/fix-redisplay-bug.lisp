;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

(in-package :clim-internals)

;; from recording protocol
(defmethod invoke-with-new-output-record
	   ((stream output-recording-mixin) continuation record-type constructor
	    &rest initargs &key parent &allow-other-keys)
  (declare (dynamic-extent initargs))
  (with-keywords-removed (initargs initargs '(:parent))
    (let* ((current-output-record (stream-current-output-record stream))
	   (new-output-record (and (stream-redisplaying-p stream)
				   current-output-record
				   (apply #'find-child-output-record-1
					  current-output-record record-type initargs))))
      (multiple-value-bind (cursor-x cursor-y)
	  (stream-cursor-position stream)
	(declare (type coordinate cursor-x cursor-y))
	(multiple-value-bind (x y)
	    (multiple-value-bind (px py)
		(point-position
		  (stream-output-history-position stream))
	      (declare (type coordinate px py))
	      (position-difference cursor-x cursor-y px py))
	  (declare (type coordinate x y))
	  (if new-output-record
            (progn 
              (copy-display-state new-output-record nil))
            (progn
	      (setq new-output-record
		    ;; Note that we set the x- and y-positions to 0.  We
		    ;; do this because setting the cursor position to (x,y)
		    ;; has the appropriate effect, and setting the other
		    ;; positions can cause the record to be mis-positioned
		    ;; if we don't actually do any output into it.
		    (if constructor
			(apply constructor
			       :x-position 0 :y-position 0 initargs)
			(apply #'construct-output-record-1 record-type
			       :x-position 0 :y-position 0 initargs)))
              (SETF (OUTPUT-RECORD-GENERATION-TICK NEW-OUTPUT-RECORD) *GENERATION-TICK*)
              ))
	  (output-record-set-start-cursor-position new-output-record x y)
          (#+mcl with-screen-flashing-disabled ;; << let it finish and be added before something comes along and replays without it
		  #-mcl progn
            (with-output-record-1 continuation 
              stream new-output-record cursor-x cursor-y)
            
	    (when (stream-redisplaying-p stream)
	      (recompute-contents-ok new-output-record))
	    ;; We set the parent after doing everything else so that calls
	    ;; to RECOMPUTE-CONTENTS-OK inside the dynamic extent of the
	    ;; continuation won't take forever.
            (let ((parent (or parent
			      current-output-record
			      (stream-output-history stream))))
	      (when parent 
	        (add-output-record new-output-record parent))))
	  new-output-record)))))

(defmethod output-record-set-position :around ((record output-record-element-mixin) new-x new-y)
  (with-bounding-rectangle* (oleft otop oright obottom) record
    (let ((motion? (not (and (= new-x oleft) (= new-y otop))))
	  (parent (output-record-parent record)))
      (when parent
	(multiple-value-bind (xoff yoff) (convert-from-descendant-to-ancestor-coordinates record parent)
	  (declare (type coordinate xoff yoff))
	  (translate-coordinates xoff yoff oleft otop oright obottom)))
      (when motion?
        ;; if it's actually moving save the state
        ;; for incremental redisplay state
        (unless  (or 
		  ;; Text records don't have output-record-mixin
		  ;; and so don't have generation ticks
		  ;; not sure this is right.
		  (not (typep record 'output-record-mixin))
		  (zerop (output-record-generation-tick record))
		  (= (output-record-generation-tick record) *generation-tick*))
          ;; it was just created, don't save it's display state
          ;; which isn't ok anyhow.
          ;; it wasn't new but it's definitely moving so save the state
          (copy-display-state record t)
          )
        (call-next-method)
        (when parent
          (recompute-extent-for-changed-child parent record oleft otop oright obottom))
        (with-bounding-rectangle* (nleft ntop nright nbottom) record
          (let ((dx1 (coordinate (- nleft oleft)))
                (dy1 (coordinate (- ntop otop)))
                (dx2 (coordinate (- nright oright)))
                (dy2 (coordinate (- nbottom obottom))))
            (note-output-record-moved record 
                                      #+mcl (output-record-parent record)
                                      dx1 dy1 dx2 dy2)))))))

(defmethod bounding-rectangle-set-edges :around ((record output-record-mixin) 
						 left top right bottom)
  (with-bounding-rectangle* (oleft otop oright obottom) record
    (let* ((old-width (- oright oleft))
           (old-height (- obottom otop))
           (new-width (- right left))
           (new-height (- bottom top))
           (dx1 (coordinate (- left oleft)))
           (dy1 (coordinate (- top otop)))
           (dx2 (coordinate (- right oright)))
           (dy2 (coordinate (- bottom obottom)))
           (change? (or (not (eql left oleft)) (not (eql right oright)) 
                        (not (eql top otop)) (not (eql bottom obottom))))
           (rigid? (and (eql new-width old-width) (eql new-height old-height)))
           (reshape? (and change? (not rigid?)))
           (soleft oleft) 
           (soright oright) 
           (sotop otop) 
           (sobottom obottom) 
           (parent (output-record-parent record)))
      (declare (type coordinate dx1 dy1 dx2 dy2))
      (when parent
        (multiple-value-bind (xoff yoff) (convert-from-descendant-to-ancestor-coordinates record parent)
          (declare (type coordinate xoff yoff))
          (translate-coordinates xoff yoff soleft sotop soright sobottom)))
      (when (and change? rigid?)
        (unless  (or (zerop (output-record-generation-tick record))
                     (= (output-record-generation-tick record) *generation-tick*))
          ;; it was just created, don't save it's display state
          ;; which isn't ok anyhow.
          ;; it wasn't new but it's definitely moving so save the state
          (copy-display-state record t)))
      (call-next-method)
      ;; if contents-ok is already off none of this matters
      (when reshape?
        ;; it's an honest reshaping, turn off output-ok
        (when (output-record-contents-ok record)
          (setf (output-record-contents-ok record) nil)))
      (when change?
        (when parent
          (recompute-extent-for-changed-child parent record soleft sotop soright sobottom))
        (note-output-record-moved record #+mcl (output-record-parent record)
                                  dx1 dy1 dx2 dy2)))))



;;; from incremental-redisplay
(defmethod compute-difference-set
	   ((record output-record-element-mixin)
	    &optional (check-overlapping nil)
		      (x-offset (coordinate 0)) (y-offset (coordinate 0))
		      (old-x-offset (coordinate 0)) (old-y-offset (coordinate 0)))
  (declare (type coordinate x-offset y-offset old-x-offset old-y-offset)
           (ignore check-overlapping))
  (declare (values erases moves draws erase-overlapping move-overlapping))
  (let ((erases nil)
	(moves nil)
	(draws nil)
	(erase-overlapping nil)
	(move-overlapping nil))
    (flet ((erase (record region)
	     ;; REGION is the bounding rectangle
	     (when region
	       (multiple-value-bind (width height)
		   (bounding-rectangle-size region)
		 (declare (type coordinate width height))
		 (unless (and (zerop width) (zerop height))
		   (push (list record
			       (bounding-rectangle-shift-position
				 region old-x-offset old-y-offset))
			 erases)))))
	   (move (record old-bounding-rectangle)
	     ;;--- It's probably a bug if OLD-BOUNDING-RECTANGLE is NIL
	     ;;--- but do we want to penalize the user?
	     (when old-bounding-rectangle
	       (multiple-value-bind (e-x e-y)
		   (bounding-rectangle-position record)
		 (declare (type coordinate e-x e-y))
		 (multiple-value-bind (old-e-x old-e-y)
		     (bounding-rectangle-position old-bounding-rectangle)
		   (declare (type coordinate old-e-x old-e-y))
		   (unless (and (= (+ x-offset e-x) (+ old-x-offset old-e-x))
				(= (+ y-offset e-y) (+ old-y-offset old-e-y)))
		     (push (list record
				 (bounding-rectangle-shift-position
				   old-bounding-rectangle old-x-offset old-y-offset)
				 (bounding-rectangle-shift-position
				   record x-offset y-offset))
			   moves))))))
	   (draw (record region)
	     (push (list record
			 (bounding-rectangle-shift-position
			   region x-offset y-offset))
		   draws)))
      (declare (dynamic-extent #'erase #'move #'draw))
      (with-slots (old-bounding-rectangle contents-ok) record
	(cond (contents-ok
	       ;; just check position, we know bounding-rect is ok if contents is ok.
	       (move record old-bounding-rectangle))
	      (t
	       (when (displayed-output-record-p record)
		 ;; It's a displayed output record element, erase and redraw it.
		 (erase record old-bounding-rectangle)
		 (draw record (bounding-rectangle record)))
	       (when (output-record-p record)
		 ;; We have to look at the children.
		 (multiple-value-bind (start-x start-y)
		     (output-record-start-cursor-position record)
		   (declare (type coordinate start-x start-y))
		   (multiple-value-bind (o-start-x o-start-y)
		       (output-record-old-start-cursor-position record)
		     (declare (type coordinate o-start-x o-start-y))
		     (dolist (child (output-record-old-children record))
                       (UNLESS (MEMBER CHILD (OUTPUT-RECORD-CHILDREN RECORD))
		         (erase child (bounding-rectangle-shift-position
				       child o-start-x o-start-y))))
		     (let ((x-offset (+ x-offset start-x))
			   (y-offset (+ y-offset start-y))
			   (old-x-offset (+ old-x-offset o-start-x))
			   (old-y-offset (+ old-y-offset o-start-y)))
		       (flet ((compute-diffs (child)
				(multiple-value-bind (nerases nmoves ndraws
						      nerase-overlapping nmove-overlapping)
				    (compute-difference-set
				      child nil
				      x-offset y-offset old-x-offset old-y-offset)
				  (setq erases (nconc erases nerases))
				  (setq moves (nconc moves nmoves))
				  (setq draws (nconc draws ndraws))
				  (setq erase-overlapping
					(nconc erase-overlapping nerase-overlapping))
				  (setq move-overlapping
					(nconc move-overlapping nmove-overlapping)))))
			 (declare (dynamic-extent #'compute-diffs))
			 (map-over-output-records #'compute-diffs record))))))))))
    (values erases moves draws erase-overlapping move-overlapping)
    ))

