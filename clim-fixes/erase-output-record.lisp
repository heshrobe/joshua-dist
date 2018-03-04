;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: clim-internals; Base: 10; Patch-File: T -*-
;;; CLOS:METHOD CLIM:ERASE-OUTPUT-RECORD (T CLIM-INTERNALS::OUTPUT-RECORDING-MIXIN)):  
;;; Calculate the region before deleting the parent.   In the current
;;; version, the deleted record is used as the replay region, but its
;;; region is relative to its parent which is lost if its deleted.
;;; Written by HES, 9/15/98 10:24:22


(in-package :clim-internals)


#-allegro
(defmethod erase-output-record (record (stream output-recording-mixin) 
				       &optional (errorp t) (x-offset 0) (y-offset 0))
  (if (listp record)
      (erase-output-records record stream errorp)
      (let ((parent (output-record-parent record)))
	(multiple-value-bind (xoff yoff)
	    (convert-from-relative-to-absolute-coordinates stream parent)
	  (incf xoff x-offset)
	  (incf yoff y-offset)
	  (with-bounding-rectangle* (left top right bottom) record
	    (with-output-recording-options (stream :record nil)
	      (if (or (= left right) (= top bottom))
		  ;; Handle specially, since a thin line is wider than a
		  ;; rectangle of zero width or height
		  (draw-line-internal stream xoff yoff
				      left top right bottom
				      +background-ink+ nil)
		  (draw-rectangle-internal stream xoff yoff
					   left top right bottom
					   +background-ink+ nil)))))
	;; Preserve the region of the record we're about to delete so that we can use it for
	;; the frame-replay below.  The unpatched code uses the record itself which won't
	;; work if it's deleted from it's parent.
	(let ((region (with-bounding-rectangle* (left top right bottom) record
			(multiple-value-bind (xoff yoff)
			    (convert-from-relative-to-absolute-coordinates 
			      stream (output-record-parent record))
			  (translate-coordinates xoff yoff left top right bottom))
			(make-bounding-rectangle left top right bottom))))
	  (when parent
	    (delete-output-record record parent errorp))
	  ;; Use the output record itself as the replay region, and replay
	  ;; the stuff that might have been obscured by the erased output.
	  ;; Note that FRAME-REPLAY sets up a clipping region using the
	  ;; bbox of the original record, so we don't end up erroneously
	  ;; redrawing anything outside the erasure region (which could
	  ;; clobber useful output).
	  (frame-replay *application-frame* stream region)))))

#+allegro
(defmethod erase-output-record (record (stream output-recording-mixin) &optional (errorp t))
  (macrolet ((draw-it ()
               `(if (or (= left right) (= top bottom))
                    ;; Handle specially, since a thin line is wider than a
                    ;; rectangle of zero width or height
                    (draw-line-internal stream xoff yoff
                                        left top right bottom
                                        +background-ink+ nil)
                  (draw-rectangle-internal stream xoff yoff
                                           left top 
                                           #-(or aclpc acl86win32) right
                                           #+(or aclpc acl86win32)
                                           (if (let () 
                                                 (declare (special *editting-field-p*))
                                                 ;;mm: defined in accept-v.lsp later
                                                 *editting-field-p*)
                                               (+ right 6) right)
                                           bottom
                                           +background-ink+ nil))))
    (if (listp record)
        (let ((replay-region +nowhere+))
          (with-output-recording-options (stream :record nil)
            (dolist (record record)
              (let ((parent (output-record-parent record)))
                (multiple-value-bind (xoff yoff)
                    (convert-from-relative-to-absolute-coordinates stream parent)
                  (with-bounding-rectangle* (left top right bottom) record
                    (draw-it)
                    (when parent
                      (delete-output-record record parent errorp))
                    (translate-coordinates xoff yoff left top right bottom)
		    #-ignore ;; may be faster to send one big rectangle
		    (if (eq replay-region +nowhere+)
			(setq replay-region
			  (make-bounding-rectangle left top right bottom))
		      (let ((lf2 left) (tp2 top) (rt2 right) (bt2 bottom))
			(with-slots (left top right bottom) replay-region
			  (minf left lf2) (minf top tp2)
			  (maxf right rt2) (maxf bottom bt2))))
		    #+ignore
                    (setq replay-region
                      (region-union
                       replay-region
                       (make-bounding-rectangle left top right bottom))))))))
          (frame-replay *application-frame* stream replay-region))
      (let ((parent (output-record-parent record)))
        (multiple-value-bind (xoff yoff)
            (convert-from-relative-to-absolute-coordinates stream parent)
          (with-bounding-rectangle* (left top right bottom) record
            (with-output-recording-options (stream :record nil)
              (draw-it))))
	;; This is the patch code
	(let ((region (with-bounding-rectangle* (left top right bottom) record
			(multiple-value-bind (xoff yoff)
			    (convert-from-relative-to-absolute-coordinates 
			      stream (output-record-parent record))
			  (translate-coordinates xoff yoff left top right bottom))
			(make-bounding-rectangle left top right bottom))))
	  (when parent
	    (delete-output-record record parent errorp))
	  ;; Use the calculated region as the replay region, since we may be deleted
	  ;; from the parent and replay
	  ;; the stuff that might have been obscured by the erased output
	  (frame-replay *application-frame* stream region))))))


