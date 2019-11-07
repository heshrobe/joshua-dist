;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-ENV; Base: 10; Lowercase: Yes -*-

"Copyright (c) 1994 Harlequin, Inc.  All rights reserved."

(in-package :clim-env)


;;; Dylan file support

(define-file-type dylan-file
  :source-extensions '("dylan" "dyl" #+Genera :dylan)
  :object-extensions (list #+Lispworks compiler:*fasl-extension-string*
                           #+Genera si:*default-binary-file-type*)
  :compiler          dylan::compile-dylan-file
  :object-loader     dylan::load-dylan-file)


;;; Dylan expressions and commands

;; Dylan expressions
(define-presentation-type dylan-expression ()
  :inherit-from 'expression)

;;--- This is the worst imaginable kludge for doing infix printing, but
;;--- we need it due to the lack of an infix printer in the translator
(defvar *dylan-expression-table* (make-hash-table :test #'equal))

(define-presentation-method accept
    ((type dylan-expression) stream (view textual-view) &key)
  (multiple-value-bind (expr input)
      (let ((*package* (find-package :dylan))
            (*readtable* dylan::*infix-dylan-load-eval-readtable*)
	    (eof clim-internals::*end-of-file-marker*)
	    (input "")
            (newline (string #\Newline)))
	(loop
	  ;;--- Clicking on an unreadable object in the middle of an
	  ;;--- input line doesn't work.  Conses like mad, too.
	  (let ((line (read-line stream nil eof)))
	    (setq input (concatenate 'string input line))
	    (let ((expr (handler-case
                            (read-from-string input)
                          (end-of-file () eof))))
	      (unless (eq expr eof)
	        (return (values expr input))))
            (unless (stream-rescanning-p stream)
              (replace-input stream newline))
            (setq input (concatenate 'string input newline)))))
    ;; Stash the input string for later printing
    (let ((expr `(dylan::begin ,expr)))
      (setf (gethash expr *dylan-expression-table*) input)
      expr)))

(define-presentation-method present
    (object (type dylan-expression) stream (view textual-view)
     &key (acceptably *print-readably*))
  (let ((string (gethash object *dylan-expression-table*)))
    (if (typep (class-of object) 'clos::dylan-class)
      (if acceptably
	(signal 'print-not-readable :object object)
	(dylan::dylan-print object :stream stream))
      (if string
        ;; If we stashed a printable infix expression, print it now
	(write-string string stream)
	(print-dylan-object object stream
			    :make-presentation nil :readably acceptably)))))

(defun print-dylan-object (object stream
                           &key (make-presentation t)
			        (length *print-length*)
			        (level *print-level*)
			        (readably *print-readably*)
                                (array *print-array*))
  (let ((*print-length* length)
	(*print-level* level)
	(*print-readably* readably)
	(*print-array* array))
    (print-dylan-object-1 object stream length level make-presentation)))

(defun print-dylan-object-1 (object stream length level make-presentation)
  (flet ((body (object stream)
	   (cond ((and *print-array*
                       (and (vectorp object) (not (stringp object))))
		  (write-string "#[" stream)
		  (let ((index 0)
                        (last-index (length object)))
		    (clim-utils:dovector (element object)
                      (incf index)
		      (when (and length (> index length))
			(write-string "..." stream)
			(return))
		      (let ((new-level (and level (1- level))))
			(when (and new-level (zerop new-level))
			  (write-string "..." stream)
			  (return))
			(print-dylan-object-1 element stream length new-level t)
                        (unless (= index last-index)
			  (write-string ", " stream)))))
                  (write-string "]" stream))
                 ((symbolp object)
                  ;; Symbols look nicest in lowercase in Dylan...
                  (format stream "\#\"~(~A~)\"" (symbol-name object)))
                 ((characterp object)
                  (format stream "'~:C'" object))
                 ((atom object)
                  (dylan::dylan-print object :stream stream))
		 ((eq (first object) 'quote)
		  (write-string "'" stream)
		  (print-dylan-object-1 (second object) stream length level t))
		 ((and (or (eq (first object) 'function)
			   (eq (first object) clim-utils::*function-symbol*)
			   #+Genera (eq (first object) 'lisp:function))
		       (listp (cdr object)))
		  (write-string "#'" stream)
		  (print-dylan-object-1 (second object) stream length level t))
		 (t
		  (write-string "#(" stream)
		  (let ((count 0))
		    (clim-utils:dorest (elements object)
		      (when (and length (> (incf count) length))
			(write-string "..." stream)
			(return))
		      (let ((element (first elements))
			    (new-level (and level (1- level))))
			(when (and new-level (zerop new-level))
			  (write-string "..." stream)
			  (return))
			(print-dylan-object-1 element stream length new-level t)
			(cond ((null (rest elements)))
			      ((atom (setq element (rest elements)))
			       (write-string " . " stream)
			       (print-dylan-object-1 element stream length new-level t)
			       (return))
			      (t (write-string ", " stream))))))
		  (write-string ")" stream)))))
    ;;--- WITH-OUTPUT-AS-PRESENTATION should actually do this for us!
    (if (and make-presentation
	     (stream-recording-p stream))	;default method returns NIL
      (with-output-as-presentation (stream object 'dylan-expression
				    ;; Better highlighting performance!
				    :single-box :highlighting
				    :type-expanded-p t)
	(body object stream))
      (body object stream))))


;; Dylan forms
(define-presentation-type dylan-form ()
  :inherit-from 'dylan-expression)

(define-presentation-method presentation-type-history ((type clim-env::dylan-form))
  (clim-internals::presentation-type-history-for-frame 'dylan-expression *application-frame*))

(define-presentation-method map-over-presentation-type-supertypes ((type dylan-form) function)
  (with-presentation-type-decoded (name) type
    (funcall function name type)
    (clim-utils:with-stack-list (new-name 'command-or-dylan-form 
			                  :command-table (frame-command-table *application-frame*))
      (clim-utils:with-stack-list (new-type new-name)
	(funcall function 'command-or-dylan-form new-type))
      (clim-utils:with-stack-list (new-name 'dylan-expression)
	(clim-utils:with-stack-list (new-type new-name)
	  (map-over-presentation-type-supertypes new-type function))))))


;; Dylan's version of COMMAND-OR-FORM
(define-presentation-type command-or-dylan-form
    (&key (command-table (frame-command-table *application-frame*)))
  :inherit-from `(command-or-form :command-table ,command-table))

(define-presentation-method accept ((type command-or-dylan-form) stream (view textual-view)
				    &rest args)
  (declare (dynamic-extent args))
  (let ((command-type `(command :command-table ,command-table))
	(form-type 'dylan-form)
	(start-position (and (input-editing-stream-p stream)
			     (stream-scan-pointer stream)))
	(replace-input-p nil))
    (multiple-value-bind (object type)
	(with-input-context (command-type) (command command-presentation-type nil options)
	     (with-input-context (form-type) (form form-presentation-type nil options)
                  (let ((gesture (stream-read-gesture stream :peek-p t)))
		    (cond ((and (characterp gesture)
				(find gesture clim-internals::*command-dispatchers*
                                      :test #'char-equal))
			   (stream-read-gesture stream)	;get out the colon
			   (apply #'accept command-type
				  :stream stream :prompt nil :view view
				  :history type args))
			  (t (apply #'accept form-type
				    :stream stream :prompt nil :view view
				    :history type args))))
		(t (when (getf options :echo t)
		     (setq replace-input-p t))
		   (values form form-presentation-type)))
	   (t (when (getf options :echo t)
		(setq replace-input-p t))
	      (when (partial-command-p command)
		(setq command (funcall *partial-command-parser*
				       command command-table stream start-position)))
	      (when (and replace-input-p start-position)
		(unless (stream-rescanning-p stream)
                  ;; Paper over a problem whereby the COMMAND-NAME type might
                  ;; have already inserted a ":" for us...
                  (unless (member (aref (input-editor-buffer stream) start-position)
                                  clim-internals::*command-dispatchers*)
		    (replace-input stream (string (first clim-internals::*command-dispatchers*))
				   :buffer-start start-position)
		    (incf start-position))))
	      (values command command-presentation-type)))
      (when replace-input-p
	(presentation-replace-input stream object type view
				    :buffer-start start-position))
      (values object type))))

(define-presentation-method present (thing (type command-or-dylan-form) stream view &rest args)
  (declare (dynamic-extent args))
  (setq command-table (find-command-table command-table))
  (apply #'present thing (if (clim-internals::object-is-command-p thing command-table)
			   `(command :command-table ,command-table)
			   'clim-env::dylan-form)
	 :stream stream :view view :for-context-type type args))



;;; Dylan libraries and modules

;; Dylan libraries
(define-presentation-type dylan-library ()
  :inherit-from 'symbol)

(define-presentation-method accept
    ((type dylan-library) stream (view textual-view) &key)
  (values
    (let ((libraries (let ((libraries nil))
		       (dolist (registry dylan::*library-registries*)
			 (when (probe-file registry)
			   (dolist (file (directory registry))
			     (push (pathname-name file) libraries))))
                       (sort libraries #'string-lessp))))
      (completing-from-suggestions (stream :partial-completers '(#\-))
        (dolist (library libraries)
	  (suggest library (intern library :dylan)))))))

(define-presentation-method present
    (library (type dylan-library) stream (view textual-view) &key)
  (princ library stream))


;; Dylan modules
(define-presentation-type dylan-module ())

(define-presentation-method accept
    ((type dylan-module) stream (view textual-view) &key)
  (values
    (let ((modules (let ((modules nil))
		     (maphash #'(lambda (name module)
                                  (declare (ignore module))
                                  (push (list (symbol-name name)
                                              (dylan::find-translator-module name))
                                        modules))
                              (dylan+dylan/internal::table-table
                                dylan+dylan/internal::$translator-modules))
                     (sort modules #'string-lessp :key #'first))))
      (completing-from-suggestions (stream :partial-completers '(#\-))
        (dolist (module modules)
	  (suggest (first module) (second module)))))))

(define-presentation-method present
    (module (type dylan-module) stream (view textual-view) &key)
  (princ (dylan::module-name module) stream))

(define-presentation-method presentation-typep (object (type dylan-module))
  (typep object 'dylan::<translator-module>))

