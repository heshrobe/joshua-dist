;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-ENV; Base: 10; Lowercase: Yes -*-

;;; Copyright (c) 1994-2000, Scott McKay.
;;; Copyright (c) 2001-2003, Scott McKay and Howard Shrobe.
;;; All rights reserved.  No warranty is expressed or implied.
;;; See COPYRIGHT for full copyright and terms of use.

(in-package :clim-env)

;;; Support for tool bars containing typed icons

(defclass tool-bar-icon ()
    ((pattern :initarg :pattern :accessor icon-pattern)
     (object :initarg :object :accessor icon-object)
     (type :initarg :type :accessor icon-type)))

(defclass tool-bar (clim-stream-pane)
    ((icons :initform nil :initarg :icons :accessor tool-bar-icons))
  (:default-initargs 
     :display-function #'display-tool-bar
     :display-after-commands nil
     :scroll-bars nil
     :width :compute :max-width +fill+		;it's OK to grow this horizontally
     :height :compute :max-height :compute	; but not vertically
     :end-of-page-action :allow 
     :end-of-line-action :allow))

(defmethod display-tool-bar (frame stream &key icons)
  (declare (ignore frame))
  (let ((icons (or icons (slot-value stream 'icons))))
    (format-items icons
                  :stream stream
                  :printer
                    #'(lambda (icon stream)
			(let ((pattern (icon-pattern icon))
			      (object (icon-object icon))
			      (type (icon-type icon)))
			  (with-output-as-presentation (stream object type
                                                        :single-box t)
			    (typecase pattern
			      (string
			        (surrounding-output-with-border (stream)
				  (with-text-style (stream '(:sans-serif :bold 10))
				    (write-string pattern stream))))
			      (pattern
			        (draw-pattern* stream pattern 0 0))
			      (pixmap
			        (draw-pixmap* stream pattern 0 0))))))
                  ;; Force a tightly packed, horizontal tool bar
                  :n-rows 1 :x-spacing 2 :initial-spacing nil)))


;;; Simple icons

(defconstant *trashcan* '#:trashcan)
(define-presentation-type trashcan ())
(define-presentation-method presentation-typep (object (type trashcan))
  (eq object *trashcan*))

(defvar *trashcan-icon*
        (make-instance 'tool-bar-icon
          ;;--- We need an icon
          :pattern "Trash"
	  :object *trashcan*
	  :type 'trashcan))


(defconstant *compiler* '#:compiler)
(define-presentation-type compiler ())
(define-presentation-method presentation-typep (object (type compiler))
  (eq object *compiler*))

(defvar *compiler-icon*
        (make-instance 'tool-bar-icon
          ;;--- We need an icon
          :pattern "Compiler"
	  :object *compiler*
	  :type 'compiler))


(defconstant *loader* '#:loader)
(define-presentation-type loader ())
(define-presentation-method presentation-typep (object (type loader))
  (eq object *loader*))

(defvar *loader-icon*
        (make-instance 'tool-bar-icon
          ;;--- We need an icon
          :pattern "Loader"
	  :object *loader*
	  :type 'loader))


(defconstant *editor* '#:editor)
(define-presentation-type editor ())
(define-presentation-method presentation-typep (object (type editor))
  (eq object *editor*))

(defvar *editor-icon*
        (make-instance 'tool-bar-icon
          ;;--- We need an icon
          :pattern "Editor"
	  :object *editor*
	  :type 'editor))
