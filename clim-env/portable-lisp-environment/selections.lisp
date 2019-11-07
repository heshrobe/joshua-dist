;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-ENV; Base: 10; Lowercase: Yes -*-

;;; Copyright (c) 1994-2000, Scott McKay.
;;; Copyright (c) 2001-2003, Scott McKay and Howard Shrobe.
;;; All rights reserved.  No warranty is expressed or implied.
;;; See COPYRIGHT for full copyright and terms of use.

(in-package :clim-env)

;;; Support for a set of selected objects

(define-application-frame selected-object-mixin ()
    ;; A list whose entries are of the form (OBJECT . PRESENTATION)
    ((selected-objects :accessor frame-selected-objects
		       :initform nil))
  (:command-table selected-objects)
  (:command-definer define-selection-command))

(defmethod object-selected-p ((frame selected-object-mixin) object)
  (not (null (assoc object (frame-selected-objects frame)))))

;; Deselect all the current objects and select the new one
(defmethod select-object ((frame selected-object-mixin) object &optional presentation)
  (deselect-all-objects frame)
  (setf (frame-selected-objects frame) (list (cons object presentation)))
  (when presentation
    (highlight-selected-object frame object presentation :highlight)))

;; Add an object to the selection
(defmethod add-object-to-selection ((frame selected-object-mixin) object &optional presentation)
  (setf (frame-selected-objects frame)
	(nconc (frame-selected-objects frame) (list (cons object presentation))))
  (when presentation
    (highlight-selected-object frame object presentation :highlight)))

;; Deselect an current object
(defmethod deselect-object ((frame selected-object-mixin) object &optional presentation)
  (declare (ignore presentation))
  (let ((entry (assoc object (frame-selected-objects frame))))
    (when entry
      (setf (frame-selected-objects frame)
	    (delete entry (frame-selected-objects frame)))
      (when (cdr entry)
	(highlight-selected-object frame (car entry) (cdr entry) :unhighlight)))))

(defmethod deselect-all-objects ((frame selected-object-mixin))
  (dolist (entry (frame-selected-objects frame))
    (when (cdr entry)
      (highlight-selected-object frame (car entry) (cdr entry) :unhighlight)))
  (setf (frame-selected-objects frame) nil))

;; Assume any sort of object is selectable.  Most application frames
;; will restrict this
(defmethod object-selectable-p ((frame selected-object-mixin) object)
  (declare (ignore object))
  t)

;; Some things may want to use this
(define-border-type :selection-rectangle (stream left top right bottom
				          &rest drawing-options 
                                          &key (filled t) &allow-other-keys)
  (declare (dynamic-extent drawing-options))
  (apply #'draw-rectangle* stream
	 left top right bottom
	 :filled filled drawing-options)
  0)

;;--- Doesn't interact correctly with refreshing the window...
(defmethod highlight-selected-object 
    ((frame selected-object-mixin) object (presentation standard-presentation) state)
  (declare (ignore state object))
  (let ((stream (clim-internals::output-record-stream presentation)))
    (when stream
      (with-bounding-rectangle* (left top right bottom) presentation
	(multiple-value-bind (xoff yoff)
	    (convert-from-relative-to-absolute-coordinates
	     stream (output-record-parent presentation))
	  (translate-coordinates xoff yoff left top right bottom))
	(with-output-recording-options (stream :record nil)
	  (draw-rectangle* stream left top right bottom
			   :filled t :ink +flipping-ink+))))))

;;--- ...but we can kludge around it for the time being
(defmethod frame-replay ((frame selected-object-mixin) stream &optional region)
  (declare (ignore stream region))
  (call-next-method)
  (dolist (entry (frame-selected-objects frame))
    (when (cdr entry)
      (highlight-selected-object frame (car entry) (cdr entry) :highlight))))


;;; Commands for selecting and deselecting objects

(define-gesture-name :select-object :pointer-button (:left :control :meta))
(define-gesture-name :add-object-to-selection :pointer-button (:left :control :meta :shift))
(define-gesture-name :deselect-object :pointer-button (:middle :control :meta))
(define-gesture-name :deselect-all-objects :pointer-button (:middle :control :meta :shift))

(defun presentation-type-equalp (object1 object2)
  (let ((type1 (presentation-type-of object1))
	(type2 (presentation-type-of object2)))
    (or (presentation-subtypep type1 type2)
	(presentation-subtypep type2 type1))))

(define-selection-command (com-select-object)
    ((object 't)
     (presentation 'presentation))
  (with-application-frame (frame)
    (select-object frame object presentation)))

(define-presentation-to-command-translator select-object
    (t com-select-object selected-objects
     :gesture :select-object
     :tester ((object frame)
              (object-selectable-p frame object))
     :documentation ((object stream)
                     (write-string "Select Object " stream)
                     (present object (presentation-type-of object) :stream stream))
     :echo nil :maintain-history nil)
    (object presentation)
  (list object presentation))

(define-selection-command (com-add-object-to-selection)
    ((object 't)
     (presentation 'presentation))
  (with-application-frame (frame)
    (when (or (null (frame-selected-objects frame))
	      (presentation-type-equalp
		object (car (first (frame-selected-objects frame)))))
      (add-object-to-selection frame object presentation))))

(define-presentation-to-command-translator add-object-to-selection
    (t com-add-object-to-selection selected-objects
     :tester ((object frame)
	      (and (object-selectable-p frame object)
                   (and (frame-selected-objects frame)
		        (presentation-type-equalp
		          object (car (first (frame-selected-objects frame)))))))
     :gesture :add-object-to-selection
     :documentation ((object stream)
                     (write-string "Add Object To Selection " stream)
                     (present object (presentation-type-of object) :stream stream))
     :echo nil :maintain-history nil)
    (object presentation)
  (list object presentation))

(define-selection-command (com-deselect-object)
    ((object 't)
     (presentation 'presentation))
  (with-application-frame (frame)
    (deselect-object frame object presentation)))

(define-presentation-to-command-translator deselect-object
    (t com-deselect-object selected-objects
     :gesture :deselect-object
     :tester ((object frame)
              (and (object-selectable-p frame object)
                   (object-selected-p frame object)))
     :documentation ((object stream)
                     (write-string "Deselect Object " stream)
                     (present object (presentation-type-of object) :stream stream))
     :echo nil :maintain-history nil)
    (object presentation)
  (list object presentation))

(define-selection-command (com-deselect-all-objects :menu t) ()
  (with-application-frame (frame)
    (deselect-all-objects frame)))

(define-presentation-to-command-translator deselect-all-objects
    ((or t blank-area) com-deselect-all-objects selected-objects
     :tester ((frame)
	      (not (null (frame-selected-objects frame))))
     :gesture :deselect-all-objects
     :echo nil :maintain-history nil)
    ()
  nil)


;;; Functions that make a command from a selected item or items

(defmacro make-command-from-selected-item (command-name presentation-type frame
                                           &rest additional-args)
  `#'(lambda (gesture arg)
       gesture arg
       (make-command-from-selected-item-1 ',command-name ',presentation-type ,frame
                                          ,@additional-args)))

(defun make-command-from-selected-item-1 (command-name presentation-type frame
					  &rest additional-args)
  #+Genera (declare (non-dynamic-extent additional-args))
  (let ((object (car (first (frame-selected-objects frame))))
        (nobjects (length (frame-selected-objects frame))))
    (deselect-all-objects frame)
    (if (and (= nobjects 1)
	     (presentation-typep object presentation-type))
      `(,command-name ,object ,@additional-args)
      `(,command-name ,*unsupplied-argument-marker* ,@additional-args))))

(defmacro make-command-from-selected-items (command-name presentation-type frame
                                           &rest additional-args)
  `#'(lambda (gesture arg)
       gesture arg
       (make-command-from-selected-items-1 ',command-name ',presentation-type ,frame
                                           ,@additional-args)))

(defun make-command-from-selected-items-1 (command-name presentation-type frame
					   &rest additional-args)
  #+Genera (declare (non-dynamic-extent additional-args))
  (let ((objects (mapcar #'car (frame-selected-objects frame))))
    (deselect-all-objects frame)
    (if (presentation-typep objects presentation-type)
      `(,command-name ,objects ,@additional-args)
      `(,command-name ,*unsupplied-argument-marker* ,@additional-args))))

