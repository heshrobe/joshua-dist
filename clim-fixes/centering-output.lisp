;;; -*- Mode: Common-lisp; Package: clim-internals -*-

(in-package :clim-internals)

(eval-when (:compile-toplevel :load-toplevel)
  (export 'clim::centering-output 'clim)
  (export 'clim::with-output-centered 'clim))

(defclass centering-output-record
          (standard-sequence-output-record)
    ())

(define-output-record-constructor centering-output-record
                                  (&key x-position y-position size)
  :x-position x-position :y-position y-position :size size)

(defun invoke-centering-output (stream continuation &key (move-cursor t))
  (let* ((centering-record
           (with-output-recording-options (stream :draw nil :record t)
             (with-new-output-record (stream 'centering-output-record nil)
                (funcall continuation stream))))
	 (parent-middle nil))
    (multiple-value-bind (left top right bottom) (bounding-rectangle* centering-record)
      (declare (ignore top bottom))
      (setq parent-middle (/ (+ left right) 2)))
    (map-over-output-records 
     #'(lambda (record)
	 (multiple-value-bind (left top right bottom) (bounding-rectangle* record)
	   (declare (ignore  bottom))
	   (let* ((his-middle (/ (+ left right) 2))
		  (offset (- parent-middle his-middle)))
	     (clim:output-record-set-position record (+ left offset) top))))
     centering-record)
    (replay centering-record stream)
    (when move-cursor
      (move-cursor-beyond-output-record stream centering-record))
    centering-record))

;;; for some reason, emacs formats this correctly but not centering-output
(defmacro with-output-centered ((stream &key (move-cursor t)) &body body)
  (default-output-stream stream centering-output)
  `(flet ((centering-output-body (,stream) ,@body))
     (declare (dynamic-extent #'centering-output-body))
     (invoke-centering-output ,stream #'centering-output-body 
			      :move-cursor ,move-cursor)))

(defmacro centering-output ((stream &key (move-cursor t)) &body body)
  (default-output-stream stream centering-output)
  `(flet ((centering-output-body (,stream) ,@body))
     (declare (dynamic-extent #'centering-output-body))
     (invoke-centering-output ,stream #'centering-output-body 
			      :move-cursor ,move-cursor)))

#|
(defun test-1 (&optional (stream *standard-output*))
  (with-output-centered (stream)
    (print "A lot longer string" stream)
    (print "foobar" stream)))
|#