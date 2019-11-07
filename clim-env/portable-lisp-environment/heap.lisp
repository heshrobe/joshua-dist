;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-ENV; Base: 10; Lowercase: Yes -*-

;;; Copyright (c) 1994-2000, Scott McKay.
;;; Copyright (c) 2001-2003, Scott McKay and Howard Shrobe.
;;; All rights reserved.  No warranty is expressed or implied.
;;; See COPYRIGHT for full copyright and terms of use.

(in-package :clim-env)

;;; An implementation of a heap (priority queue)

(defparameter *default-heap-size* 100)

(defclass heap ()
    ((size :initarg :size :initform *default-heap-size* :reader heap-size)
     (active-size :initform 0 :reader heap-active-size)
     heap-array							;contains (key . item)
     (test :initarg :test :initform #'<)
     (growth-factor :initarg :growth-factor :initform 1.5)))

(defun make-heap (&rest initargs
		  &key (size 100.) (test #'<) (growth-factor 1.5))
  (declare (dynamic-extent initargs)
	   (ignore size test growth-factor))
  (apply #'make-instance 'heap initargs))

(defmethod initialize-instance :after ((heap heap) &key)
  (with-slots (heap-array growth-factor size) heap
    ;; Make the growth-factor reasonable
    (let ((gf growth-factor))
      (typecase gf
	(null)
	(integer
	  (cond ((< gf 2) (setq gf 2))
		((> gf size) (setq gf size))))
	(float
	  (cond ((<= gf (/ (float (1+ size)) size)) (setq gf 1.5))
		((> gf 10.0) (setq gf 10.0))))
	(t (setq gf 1.5)))
      (setq growth-factor gf))
    (setq heap-array (make-array size))))

(defmethod heap-empty-p ((heap heap))
  (zerop (heap-active-size heap)))

(defmethod clear-heap ((heap heap))
  (setf (slot-value heap 'active-size) 0))

;; For internal use only...
(defmethod adjust-heap ((heap heap) i)
  (let ((heap-array (slot-value heap 'heap-array))
	(test (slot-value heap 'test))
	(active-size (heap-active-size heap)))
    (declare (type simple-vector heap-array))
    (loop with key-and-item = (svref heap-array i)
	  for j = (* 2 i) then (* 2 j)
	  while (<= j active-size)
	  if (and (< j active-size)
		  (funcall test (car (svref heap-array (1+ j)))
			        (car (svref heap-array j))))
	    do (incf j)
	  while (funcall test (car (svref heap-array j))
			      (car key-and-item))
	  do (setf (svref heap-array (floor j 2)) (svref heap-array j))
	  finally (setf (svref heap-array (floor j 2)) key-and-item))))

;; If for some reason the heap is no longer in sorted order (the user
;; could have bashed an entry), rebuild-heap will make it sorted again.
;; For internal use only...
(defmethod rebuild-heap ((heap heap))
  (loop for i from (floor (heap-active-size heap) 2) downto 1
	do (adjust-heap heap i)))

(defmethod add-item-to-heap (key item (heap heap))
  (incf (slot-value heap 'active-size))
  (let ((active-size (heap-active-size heap))
	(test (slot-value heap 'test))
	(growth-factor (slot-value heap 'growth-factor)))
    ;; Copy the heap if it's full up
    (when (>= active-size (heap-size heap))
      (if (null growth-factor)
	(error "The heap ~S overflowed" heap)
	(let* ((new-size (if (integerp growth-factor)
			   (+ (heap-size heap) growth-factor)
			   (floor (* (heap-size heap) growth-factor))))
	       (new-heap-array (make-array new-size)))
	  (replace new-heap-array (slot-value heap 'heap-array))
	  (setf (slot-value heap 'heap-array) new-heap-array
		(slot-value heap 'size) new-size))))
    (let ((heap-array (slot-value heap 'heap-array)))
      (declare (type simple-vector heap-array))
      (loop with j = active-size
	    with i = (floor active-size 2)
	    while (and (> i 0)
		       (funcall test key (car (svref heap-array i))))
	    do (setf (svref heap-array j) (svref heap-array i))
	       (setq j i)
	       (setq i (floor i 2))
	    finally (setf (svref heap-array j) (cons key item))))
    (values item key)))

(defmacro with-top-key-and-item ((heap key item) &body body)
  (let ((key-and-item '#:key-and-item))
    `(let* ((,key-and-item (svref (slot-value ,heap 'heap-array) 1))
	    (,key (car ,key-and-item))
	    (,item (cdr ,key-and-item)))
       ,@body)))

(defmethod heap-top ((heap heap))
  (if (heap-empty-p heap)
    (values nil nil nil)
    (with-top-key-and-item (heap key item)
      (values item key t))))

(defmethod delete-top-of-heap ((heap heap))
  (if (heap-empty-p heap)
    (values nil nil nil)
    (with-top-key-and-item (heap key item)
      (setf (svref (slot-value heap 'heap-array) 1)
	    (svref (slot-value heap 'heap-array) (slot-value heap 'active-size)))
      (decf (slot-value heap 'active-size))
      (adjust-heap heap 1)
      (values item key t))))

(defmethod map-over-heap (function (heap heap))
  (declare (dynamic-extent function))
  (let ((heap-array (slot-value heap 'heap-array))
	(active-size (heap-active-size heap)))
    (declare (type simple-vector heap-array))
    (dotimes (i active-size)
      (funcall function (car (svref heap-array i)) (cdr (svref heap-array i)) i))))

(defmethod describe-heap ((heap heap) &optional (stream *standard-output*))
  (fresh-line stream)
  (format stream "~S is a heap using test ~A, containing ~D element~:P."
    heap (slot-value heap 'test) (heap-active-size heap))
  (when (and (not (heap-empty-p heap))
	     (y-or-n-p "Do you want to see the contents of the heap? "))
    (map-over-heap #'(lambda (key value index) 
		       (declare (ignore index))
		       (format stream "~%Key ~S~20T~S" key value))
		   heap)))

(defmethod delete-heap-item (item (heap heap)
			     &key (equal-test #'eql) (errorp t))
  (let ((heap-array (slot-value heap 'heap-array)) 
	(active-size (heap-active-size heap)))
    (map-over-heap 
      #'(lambda (this-key this-item i)
	  (when (funcall equal-test item this-item)
	    (setf (svref heap-array i) (svref heap-array active-size))
	    (setf (svref heap-array active-size) nil)
	    (decf (slot-value heap 'active-size))
	    (rebuild-heap heap)
	    (return-from delete-heap-item
	      (values this-item this-key))))
      heap))
  (if errorp
    (error "Item ~S not found in heap ~S" item heap)
    (values nil nil)))

(defmethod delete-heap-key (key (heap heap)
			    &key (equal-test #'eql) (errorp t))
  (let ((heap-array (slot-value heap 'heap-array)) 
	(active-size (heap-active-size heap)))
    (map-over-heap
      #'(lambda (this-key this-item i)
	  (when (funcall equal-test key this-key)
	    (setf (svref heap-array i) (svref heap-array active-size))
	    (setf (svref heap-array active-size) nil)
	    (decf (slot-value heap 'active-size))
	    (rebuild-heap heap)
	    (return-from delete-heap-key
	      (values this-item this-key))))
      heap))
  (if errorp
    (error "Key ~S not found in heap ~S" key heap)
    (values nil nil)))

(defmethod find-heap-item (item (heap heap)
			   &key (equal-test #'eql) (errorp t))
  (map-over-heap 
    #'(lambda (this-key this-item i)
	(declare (ignore i))
	(when (funcall equal-test item this-item)
	  (return-from find-heap-item
	    (values this-item this-key))))
    heap)
  (if errorp
    (error "Item ~S not found in heap ~S" item heap)
    (values nil nil)))

(defmethod find-heap-key (key (heap heap)
			  &key (equal-test #'eql) (errorp t))
  (map-over-heap
    #'(lambda (this-key this-item i)
	(declare (ignore i))
	(when (funcall equal-test key this-key)
	  (return-from find-heap-key
	    (values this-item this-key))))
    heap)
  (if errorp
    (error "Key ~S not found in heap ~S" key heap)
    (values nil nil)))
