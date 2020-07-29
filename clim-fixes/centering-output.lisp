;;; -*- Mode: Common-lisp; Package: clim-internals -*-

(in-package :clim-internals)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export (intern (string-upcase "with-output-centered") 'clim) 'clim)
  (export (intern (string-upcase "centering-output") 'clim) 'clim))

(defclass centering-output-record
          (standard-sequence-output-record)
    ())

;; (define-output-record-constructor centering-output-record
;;                                   (&key x-position y-position size)
;;   :x-position x-position :y-position y-position :size size)

(defun invoke-centering-output (stream continuation &key (move-cursor t))
  (let* ((centering-record
           (with-output-recording-options (stream :draw nil :record t)
             (with-new-output-record (stream 'centering-output-record)
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
	     (setf (output-record-position record) (values (+ left offset) top)))))
     centering-record)
    (replay centering-record stream)
    (when move-cursor
      (setf (stream-cursor-position stream)
            (values (bounding-rectangle-max-x centering-record)
                        (bounding-rectangle-max-y centering-record))))
    centering-record))

;;; for some reason, emacs formats this correctly but not centering-output
(defmacro with-output-centered ((stream &key (move-cursor t)) &body body)
  ;;(default-output-stream stream centering-output)
  `(flet ((centering-output-body (,stream) ,@body))
     (declare (dynamic-extent #'centering-output-body))
     (invoke-centering-output ,stream #'centering-output-body
			      :move-cursor ,move-cursor)))

(defmacro centering-output ((stream &key (move-cursor t)) &body body)
  ;;(default-output-stream stream centering-output)
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
