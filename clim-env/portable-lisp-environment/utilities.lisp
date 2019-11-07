;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-ENV; Base: 10; Lowercase: Yes -*-

;;; Copyright (c) 1994-2000, Scott McKay.
;;; Copyright (c) 2001-2003, Scott McKay and Howard Shrobe.
;;; All rights reserved.  No warranty is expressed or implied.
;;; See COPYRIGHT for full copyright and terms of use.

(in-package :clim-env)

;;; Various utility functions and macros

(defun false (&rest args)
  (declare (ignore args))
  nil)

(defun true (&rest args)
  (declare (ignore args))
  t)


;; Use this to bind STREAM-VAR to something that will be used as *STANDARD-OUTPUT*.
;; Frames with no standard output pane can use a pop-up window, for example.
(defmacro with-frame-standard-output ((stream-var &optional (frame '*application-frame*))
				      &body body)
  `(flet ((with-frame-standard-output-body (,stream-var) ,@body))
     (declare (dynamic-extent #'with-frame-standard-output-body))
     (invoke-with-frame-standard-output ,frame #'with-frame-standard-output-body)))

(defmethod invoke-with-frame-standard-output ((frame standard-application-frame) continuation)
  (declare (dynamic-extent continuation))
  (let ((stream *standard-output*))
    (funcall continuation stream)))

;;--- This should pop up a non-AVV window so that all of the translators
;;--- from the parent frame continue to work
(defmacro with-pop-up-window ((stream-var frame
			       &key width height (scroll-bars ':vertical))
			      &body body)
  `(let ((,stream-var (frame-standard-output ,frame)))
     (accepting-values (,stream-var
			:own-window t
			:exit-boxes '((:exit "Click here to remove this display"))
                        :scroll-bars ,scroll-bars
                        ,@(and width `(:width ,width))
                        ,@(and height `(:height ,height)))
       ,@body)))


;; Shadowed by the package declaration so we can get input editing
(defun yes-or-no-p (format-string &rest format-args)
  (declare (dynamic-extent format-args))
  (eql (clim:accept '(member yes no)
		    :prompt-mode :raw
		    :prompt (apply #'format nil format-string format-args))
       'yes))

#|| original from swm
(defun yes-or-no-p (format-string &rest format-args)
  (declare (dynamic-extent format-args))
  (with-input-editing (*query-io*)
    (apply #'cl:yes-or-no-p format-string format-args)))

||#

;; Ditto

(defun y-or-n-p (format-string &rest format-args)
  (declare (dynamic-extent format-args))
  (eql (clim:accept '(member y n) 
		    :prompt-mode :raw
		    :prompt (apply #'format nil format-string format-args))
       'y))

#||  original version from swm doesn't deal with newline right
(defun y-or-n-p (format-string &rest format-args)
  (declare (dynamic-extent format-args))
  (with-input-editing (*query-io*)
    (apply #'cl:y-or-n-p format-string format-args)))
||#

#+Genera
(defun pointer-yes-or-no-p (message
			    &key (associated-window 
				  (frame-top-level-sheet *application-frame*))
				 foreground background text-style)
  (let* ((choices '(("Yes" :value t)
		   ("No"  :value nil)))
	 (item (menu-choose choices
			    :text-style (or text-style '(:serif :roman :large))
                            :foreground foreground :background background
			    :default-item (first choices)
                            :cell-align-x :center
			    :label (list message :text-style '(:fix :roman :large))
			    :associated-window associated-window)))
    (menu-item-value item)))

#-Genera
(defun pointer-yes-or-no-p (message
			    &key (associated-window 
				  (frame-top-level-sheet *application-frame*))
				 foreground background text-style)
  (notify-user (pane-frame associated-window) message
               :style :question
               :title "Confirm"
               :foreground foreground :background background
               :text-style text-style))



;;; FORMAT directives for presentations

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'presentation ':common-lisp-user))

;; Example: (format stream "Class ~/presentation/" (list class 'clos:class))
(defun cl-user::presentation (stream list &optional colon atsign)
  (declare (ignore atsign colon))
  (destructuring-bind (object type) list
    (present object type :stream stream))) 


;;; Generic file types for compilers and loaders

(defclass file-type ()
  ((name :accessor file-type-name :initarg :name)
   (object-types :accessor file-type-object-types :initarg :object-types)
   (compiler :accessor file-type-compiler :initarg :compiler)
   (source-loader :accessor file-type-source-loader :initarg :source-loader)
   (object-loader :accessor file-type-object-loader :initarg :object-loader)
   (source-extensions :accessor file-type-source-extensions :initarg :source-extensions)
   (object-extensions :accessor file-type-object-extensions :initarg :object-extensions)))

(defvar *object-file-types* (make-hash-table :test #'equalp))
(defvar *source-file-types* (make-hash-table :test #'equalp))

;; NAME, COMPILER, SOURCE-LOAD, and OBJECT-LOADER are not evaluated
;; SOURCE-EXTENSIONS and OBJECT-EXTENSIONS are evaluated
(defmacro define-file-type (name &key source-extensions object-extensions
                                      compiler source-loader object-loader)
  `(let ((file-type (make-instance 'file-type
                      :name ',name
                      :source-extensions ,source-extensions
                      :object-extensions ,object-extensions
                      :compiler ',compiler
                      :source-loader ',source-loader
                      :object-loader ',object-loader)))
     (dolist (type ,source-extensions)
       (setf (gethash type *source-file-types*) file-type))
     (dolist (type ,object-extensions)
       (setf (gethash type *object-file-types*) file-type))
     file-type))

(defun find-compiler-and-loader (pathname)
  (declare (values compiler object-loader source-loader))
  (let* ((type (pathname-type pathname))
         (src (gethash type *source-file-types*))
         (obj (gethash type *object-file-types*)))
    (cond (src
           (values (file-type-compiler src)
		   (file-type-object-loader src)
		   (file-type-source-loader src)))
          (obj
           (values (file-type-compiler obj)
		   (file-type-object-loader obj)
		   (file-type-source-loader obj)))
          (t
           ;; Lisp-centered world view
           (values 'compile-file 'load 'load)))))

(define-file-type lisp-file  
  :source-extensions '("lisp" "lsp" #+Allegro "cl" #+Genera :lisp)
  :object-extensions (list #+Genera si:*default-binary-file-type*
			   #+Lispworks compiler:*fasl-extension-string*
			   #+Allegro excl:*fasl-default-type*
			   #+MCL (pathname-type ccl:*.fasl-pathname*))
  :compiler          compile-file
  :source-loader     load
  :object-loader     load)


;;; Utilities for hacking lambda lists

(defparameter *print-lambda-keyword-style* '(nil :italic nil))
(defparameter *print-lambda-keyword-case* :downcase)
(defparameter *print-lambda-type-style* '(nil :italic nil))
(defparameter *print-lambda-type-case* :downcase)

(defmacro with-optional-argument-destructured
	  ((list-cons type arg-var &optional default-var supplied-p-var keyword-var)
	   &body body)
  (let ((object-var '#:object-var))
    `(let ((,arg-var nil)
	   ,@(and keyword-var `((,keyword-var nil)))
	   ,@(and default-var `((,default-var nil)))
	   ,@(and supplied-p-var `((,supplied-p-var nil)))
	   (,object-var (car ,list-cons)))
       (cond ((symbolp ,object-var)
	      (setq ,arg-var ,object-var
		    ,@(and keyword-var 
			   `(,keyword-var (intern (symbol-name ,arg-var) :keyword)))))
	     (t
	      (if (eql ,type :optional)
		(setq ,arg-var (first ,object-var))
		(if (consp (first ,object-var))
		  ,(if keyword-var
		     `(setq ,keyword-var (first (first ,object-var))
			    ,arg-var (second (first ,object-var)))
		     `(setq ,arg-var (second (first ,object-var))))
		  (setq ,arg-var (first ,object-var)
			,@(and keyword-var 
			       `(,keyword-var (intern (symbol-name ,arg-var) :keyword))))))
	      ,@(when default-var
		  `((when (>= (length ,object-var) 2)
		      (setq ,default-var (second ,object-var)))))
	      ,@(when supplied-p-var
		  `((when (>= (length ,object-var) 3)
		      (setq ,supplied-p-var (third ,object-var)))))))
       ,@body)))

;; This does very little error checking...
(defun map-over-lambda-list (ll function &key handle-macros)
  (declare (dynamic-extent function))
  (flet ((check-macro-keyword (word)
	   (unless handle-macros
	     (error "~S is not permitted in the lambda list ~S" word ll))))
    (declare (dynamic-extent #'check-macro-keyword))
    (loop with state = 'required
	  with special-saved-state = nil
	  with key-required = nil
	  for arg-point on ll
	  for arg = (and (consp arg-point) (car arg-point))
	  as lambda&-p = (and (symbolp arg) (eql (aref (symbol-name arg) 0) #\&))
	  doing
      (cond ((symbolp arg-point)
	     (unless handle-macros
	       (error "The lambda list ~S is a dotted list" ll))
	     (funcall function arg-point :dotted-tail)
	     (return))
	    (lambda&-p
	     (setq key-required nil)
	     (case arg
	       (&optional
		 (setq state 'optional))
	       (&rest
		 (case state
		   ((required optional))
		   ((key allow-other-keys)
		    ;; &REST follows &KEY, but who cares
		    )
		   (otherwise
		     (error "&REST must not follow &AUX or &KEY in ~S" ll)))
		 (setq state 'rest))
	       (&key
		 (unless (member state '(required optional rest))
		   (error "&KEY in invalid location in ~S" ll))
		 (setq state 'key))
	       (&aux
		 (setq state 'aux))
	       (&allow-other-keys
		 (unless (eql state 'key)
		   (error "&ALLOW-OTHER-KEYS must follow &KEY in ~S" ll))
		 (setq state 'allow-other-keys)
		 (setq key-required t))
	       (&whole
		 (setq special-saved-state state)
		 (check-macro-keyword '&whole)
		 (unless (member state '(required optional key allow-other-keys))
		   (error "&WHOLE in invalid location in ~S" ll))
		 (setq state 'whole))
	       (&environment
		 (setq special-saved-state state)
		 (check-macro-keyword '&environment)
		 (unless (member state '(required optional key allow-other-keys))
		   (error "&ENVIRONMENT in invalid location in ~S" ll))
		 (setq state 'environment))
	       (&body
		 (setq special-saved-state state)
		 (check-macro-keyword '&body)
		 (unless (member state '(required optional key allow-other-keys))
		   (error "&BODY in invalid location in ~S" ll))
		 (setq state 'body))
	       (otherwise (funcall function arg-point :&-key))))
	    ((not (or (symbolp arg) (consp arg)))
	     (error "The lambda lists ~S may contain only symbols and lists, not ~S" ll arg))
	    (t
	     (when key-required
	       (error "Non-lambda-keyword ~S where lambda keyword required in ~S" arg ll))
	     (case state
	       (required
		 (funcall function arg-point :required))
	       (optional
		 (funcall function arg-point :optional))
	       (rest
		 (setq key-required t)
		 (funcall function arg-point :rest))
	       (aux
		 (funcall function arg-point :aux))
	       (key
		 (funcall function arg-point :key))
	       (whole
		 (setq state special-saved-state)
		 (funcall function arg-point :whole))
	       (environment
		 (setq state special-saved-state)
		 (funcall function arg-point :environment))
	       (body
		 (setq key-required t)
		 (setq state special-saved-state)
		 (funcall function arg-point :body)))))))
  nil)

;; This assumes that it gets called on well-structured lambda lists...
(defun print-lambda-list (lambda-list stream 
			  &key brief types (parenthesis t))
  (let (#+Genera (scl:*print-abbreviate-quote* 'si:backquote)
	(*print-pretty* t)
	(print-space nil)
	(state 'required))
    (labels ((print-or-recurse (item &optional no-space)
	       (unless no-space (space))
	       (if (symbolp item)
		 (print-symbol item)
		 (print-lambda-list item stream :brief brief :types types)))
	     (print-type (item)
	       (let ((type (cadr (assoc item types)))
		     (*print-case* *print-lambda-type-case*))
		 (when type
		   (with-text-style (stream *print-lambda-type-style*)
		     (format stream "<~A>" type)))))
	     (print-symbol (item)
	       (if (special-variable-p item)
		 (prin1 item stream)
		 (princ item stream))
	       (print-type item))
	     (print-lambda-keyword (keyword)
	       (space)
	       (with-text-style (stream *print-lambda-keyword-style*)
		 (let ((*print-case* *print-lambda-keyword-case*))
		   (prin1 keyword stream))))
	     (space ()
	       (if print-space
		 (write-char #\space stream)
		 (setq print-space t)))
	     (special-variable-p (var)
	       #+Genera (lt:global-special-variable-p var)
	       #-Genera (and (symbolp var)
                             (boundp var))))
      (when parenthesis
	(write-char #\( stream))
      (map-over-lambda-list
	lambda-list
	#'(lambda (list-point type)
	    (case type
	      (:required (print-or-recurse (car list-point)))
	      (:body
		(print-lambda-keyword '&body)
		(print-or-recurse (car list-point)))
	      (:rest
		(print-lambda-keyword '&rest)
		(print-or-recurse (car list-point)))
	      (:whole
		(print-lambda-keyword '&whole)
		(print-or-recurse (car list-point)))
	      (:environment
		(print-lambda-keyword '&environment)
		(print-or-recurse (car list-point)))
	      (:allow-other-keys
		(print-lambda-keyword '&allow-other-keys))
	      ((:optional :key)
	       (if (eql type ':optional)
		 (when (not (eql state 'optional))
		   (setq state 'optional)
		   (print-lambda-keyword '&optional))
		 (when (not (eql state 'key))
		   (setq state 'key)
		   (print-lambda-keyword '&key)))
	       (with-optional-argument-destructured
		   (list-point type arg default supplied key)
		 (space)
		 (if (or default (and (not brief) supplied))
		   (progn
		     (write-char #\( stream)
		     (if (eql type ':optional)
		       (print-or-recurse arg t)
		       ;; key
		       (cond ((or brief (string= (symbol-name arg)
						 (symbol-name key)))
			      (prin1 key stream)
			      (print-type arg))
			     (t 
			      (write-char #\( stream)
			      (prin1 key stream)
			      (write-char #\space stream)
			      (print-or-recurse arg t)
			      (write-char #\) stream))))
		     (write-char #\space stream)
		     (prin1 default stream)
		     (when (and (not brief) supplied)
		       (write-char #\space stream)
		       (print-or-recurse supplied t))
		     (write-char #\) stream))
		   (if (eql type ':optional)
		     (print-or-recurse arg t)
		     (progn
		       (prin1 key stream)
		       (print-type arg))))))
	      (:aux
		(unless brief
		  (unless (eql state 'aux)
		    (print-lambda-keyword '&aux)
		    (setq state 'aux))
		  (let ((whatsit (car list-point)))
		    (if (symbolp whatsit)
		      (print-or-recurse whatsit)
		      (progn
			(write-char #\( stream)
			(prin1 (car whatsit) stream)
			(write-char #\space stream)
			(prin1 (second whatsit))
			(write-char #\) stream))))))
	      (:dotted-tail
		(print-lambda-keyword '&rest)
		(space)
		(print-symbol list-point))
	      (:&-key
		(print-lambda-keyword (car list-point)))	;the &thing itself
	      ))
	:handle-macros t)
      ;; MAP-OVER-LAMBDA-LIST didn't show trailing lambda-list-keywords
      ;; so print the ones that are meaningful in the trailing position,
      ;; but watch out for dotted lists
      (loop for i from 1
	    as list = (last lambda-list i)
	    as item = (car list)
	    while (member item '(&key &allow-other-keys))
	    do (print-lambda-keyword item)
	    until (eql list lambda-list))
      (when parenthesis
	(write-char #\) stream)))))

