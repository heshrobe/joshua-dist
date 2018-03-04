;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

(in-package :clim-internals)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (export (intern (string-upcase "ruled-table-output-record") :clim) :clim)
  (export (intern (string-upcase "draw-rules") :clim) :clim))

(defmethod adjust-table-cells ((table standard-table-output-record) stream)
  (let* ((nrows 0)
         (ncells nil)
         (cells 0)
         (row-table-p (row-table-p table))
         (table-mapper (if row-table-p #'map-over-table-rows #'map-over-table-columns))
         (x-spacing (slot-value table 'x-spacing))
         (y-spacing (slot-value table 'y-spacing))
         (equalize-column-widths (slot-value table 'equalize-column-widths)))
    (declare (type fixnum nrows cells))
    (declare (type coordinate x-spacing y-spacing))
    (labels ((count-rows (row)
               (incf nrows)
               (setq cells 0)
               (map-over-row-cells #'count-cells row)
               (assert (not (zerop cells)) ()
		 "Row or column in table does not contain any cells")
               (cond ((null ncells)
                      (setq ncells cells))
                     (t
                      (maxf ncells cells))))
             (count-cells (cell)
               (assert (cell-output-record-p cell))
               (incf cells)))
      (declare (dynamic-extent #'count-rows #'count-cells))
      ;; Calculate nrows & ncells (= ncells per row)
      (funcall table-mapper #'count-rows table))
    ;; If there are no rows, COUNT-ROWS won't get invoked and NCELLS
    ;; will be NIL.  If all the rows and columns are empty, NCELLS will
    ;; be 0.  In either case, that means we're done.
    (when (or (null ncells) (= ncells 0))
      (return-from adjust-table-cells
        (tree-recompute-extent table)))
    (with-stack-array (row-array nrows :initial-element 0)
      (with-stack-array (column-array ncells :initial-element 0)
        (let ((x-pos nil)
              (y-pos nil)
              (row-count 0)
              (column-count 0)
              (total-width (coordinate 0))
              (total-height (coordinate 0)))
          (declare (type fixnum row-count column-count))
          (declare (type coordinate total-width total-height))
	  ;; We always want the table to start at its START-X and START-Y positions.
          (multiple-value-setq (x-pos y-pos) (output-record-position table))
          (macrolet (#-CCL-2 (row-max-height (row-number)
                               `(svref row-array ,row-number))
			     #-CCL-2 (column-max-width (column-number)
				       `(svref column-array ,column-number)))
	    ;; Figure out max height for each row,
	    ;;            max width for each column.
	    ;; Collect row heights and column widths into temp arrays.
	    ;; We need to remember for each row its total height and
	    ;; the difference between the smallest top and the largest top.
	    ;; For each row remember the total height and then remember the maximum
	    ;; difference between the row top and the y-position of the row.
	    ;; Rows and columns are pretty symmetric, but we need to arrange
	    ;; for a few things to work out right...
            (unless row-table-p
              (rotatef row-array column-array))
            (if row-table-p (setq row-count -1) (setq column-count -1))
            (flet ((row-mapper (row)
                     (if row-table-p (incf row-count) (incf column-count))
                     (if row-table-p (setq column-count -1) (setq row-count -1))
                     (adjust-table-cells row stream)
                     (flet ((cell-mapper (cell)
                              (if row-table-p (incf column-count) (incf row-count))
                              (multiple-value-bind (width height)
                                  (bounding-rectangle-size cell)
                                (declare (type coordinate width height))
                                (maxf (row-max-height row-count)
                                      (max height (cell-min-height cell)))
                                (maxf (column-max-width column-count)
                                      (max width (cell-min-width cell))))))
                       (declare (dynamic-extent #'cell-mapper))
                       (map-over-row-cells #'cell-mapper row))))
              (declare (dynamic-extent #'row-mapper))
              (funcall table-mapper #'row-mapper table))
            (when equalize-column-widths
              (let ((column-width (coordinate 0))
                    (n-columns (1+ column-count)))
                (declare (type fixnum n-columns))
                (declare (type coordinate column-width))
                (dotimes (i n-columns)
                  (maxf column-width (column-max-width i)))
                (dotimes (i n-columns)
                  (setf (column-max-width i) column-width))))
            (if row-table-p (setq row-count -1) (setq column-count -1))
            (flet ((row-mapper (row)
                     (if row-table-p (incf row-count) (incf column-count))
                     (let ((this-row-height (row-max-height row-count))
                           (this-column-width (column-max-width column-count)))
                       (declare (type coordinate this-row-height this-column-width))
		       ;; All numbers are in (output-record-parent table) coordinates
                       (if row-table-p (setq column-count -1) (setq row-count -1))
                       (setq total-width x-pos
                             total-height y-pos)
                       (flet ((cell-mapper (cell)
                                (if row-table-p (incf column-count) (incf row-count))
                                (let ((column-width (column-max-width column-count))
                                      (row-height (row-max-height row-count))
                                      (cell-width (bounding-rectangle-width cell))
                                      (cell-height (bounding-rectangle-height cell))
                                      (x-alignment-adjust 0)
                                      (y-alignment-adjust 0))
                                  (declare (type coordinate column-width row-height
						 cell-width cell-height))
                                  (ecase (slot-value cell 'x-alignment)
                                    (:left )
                                    (:right
				     (setq x-alignment-adjust
				       (- column-width cell-width)))
                                    (:center
				     (setq x-alignment-adjust
				       (floor (- column-width cell-width) 2))))
                                  (ecase (slot-value cell 'y-alignment)
                                    (:top )
                                    (:bottom
				     (setq y-alignment-adjust
				       (- row-height cell-height)))
                                    (:center
				     (setq y-alignment-adjust
				       (floor (- row-height cell-height) 2))))
                                  (multiple-value-bind (x-offset y-offset)
                                      (convert-from-ancestor-to-descendant-coordinates
				       (output-record-parent table) (output-record-parent cell))
                                    (declare (type coordinate x-offset y-offset))
				    ;;; Make sure output-record of a row fills
				    ;;; the entire row height.
				    ;;; The reason for this is that the code that
				    ;;; calculates the total-extent of the table
				    ;;; uses the size and position of the child-cells.
				    ;;; As a result, the bottom row is drawn only
				    ;;; to its minimal height.
				    (when row-table-p
				      (let ((old-width (bounding-rectangle-width cell))) 
					(bounding-rectangle-set-size 
					 cell
					 old-width
					 this-row-height)))
                                    (output-record-set-position
				     cell
				     (+ x-offset total-width  x-alignment-adjust)
				     (+ y-offset total-height y-alignment-adjust)))
                                  (if row-table-p
                                      (incf total-width (+ column-width x-spacing))
                                    (incf total-height (+ row-height y-spacing))))))
                         (declare (dynamic-extent #'cell-mapper))
                         (map-over-row-cells #'cell-mapper row))
                       (if row-table-p
                           (incf y-pos (+ this-row-height y-spacing))
			 (incf x-pos (+ this-column-width x-spacing))))))
              (declare (dynamic-extent #'row-mapper))
              (funcall table-mapper #'row-mapper table)))
	  ;; at this point you could draw lines around the cells easily
	  ;; because you have the row and column arrays and total-height and total-width
	  (draw-rules table column-array row-array stream)
      ))))
  (tree-recompute-extent table))

(defmethod draw-rules ((table standard-table-output-record) column-array row-array stream)
  (declare (ignore column-array row-array stream))
  (values))

(defclass ruled-table-output-record (standard-table-output-record) ())

(defmethod draw-rules ((table ruled-table-output-record) column-array row-array stream)
  (let* ((x-spacing (slot-value table 'x-spacing))
	 (y-spacing (slot-value table 'y-spacing))
	 (max-x (loop for x across column-array sum (+ x x-spacing)))
	 (max-y (loop for y across row-array sum (+ y y-spacing))))
    (add-output-record
     (with-output-to-output-record (stream 'standard-sequence-output-record ;; record
					   )
       ;; (setq cl-user::foobar record)
       (clim:draw-line* stream 0 max-y  max-x max-y)
       (clim:draw-line* stream max-x 0 max-x max-y)
       (let ((last-x 0))
	 (loop for x across column-array
	     for first = t then nil
	     do (draw-line* stream last-x 0 last-x max-y)
		(incf last-x x)
		(if first 
		    (incf last-x (floor x-spacing 2))
		  (incf last-x x-spacing))
		))
       (let ((last-y 0))
	 (loop for y across row-array
	     for first = t then nil
	     do (draw-line* stream 0 last-y max-x last-y)
		(incf last-y y)
		(if first
		    (incf last-y (floor y-spacing 2))
		  (incf last-y y-spacing)))))
     table)))