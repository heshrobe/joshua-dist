;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-ENV; Base: 10; Lowercase: Yes -*-

;;; Copyright (c) 1994-2000, Scott McKay.
;;; Copyright (c) 2001-2003, Scott McKay and Howard Shrobe.
;;; All rights reserved.  No warranty is expressed or implied.
;;; See COPYRIGHT for full copyright and terms of use.

(in-package :clim-env)

;;; Directory browser

(define-command-table fsedit-files :inherit-from (files systems editing))

(add-menu-item-to-command-table
  'fsedit-files "Compile" 
  :function (make-command-from-selected-items 
	      com-compile-file (sequence pathname) *application-frame*)
  :errorp nil)

(add-menu-item-to-command-table
  'fsedit-files "Compile&Load" 
  :function (make-command-from-selected-items 
	      com-compile-file (sequence pathname) *application-frame*
	      :load t)
  :errorp nil)

(add-menu-item-to-command-table
  'fsedit-files "Load" 
  :function (make-command-from-selected-items 
	      com-load-file (sequence pathname) *application-frame*)
  :errorp nil)

(add-menu-item-to-command-table
  'fsedit-files "Edit" 
  :function (make-command-from-selected-item
	      com-edit-file pathname *application-frame*)
  :errorp nil)

(add-menu-item-to-command-table
  'fsedit-files "Print" 
  :function (make-command-from-selected-item
	      com-hardcopy-file pathname *application-frame*)
  :errorp nil)

(add-menu-item-to-command-table 'fsedit-files "destructive-items" 
				:divider nil
				:errorp nil)

(add-menu-item-to-command-table
  'fsedit-files "Delete" 
  :function (make-command-from-selected-items 
	      com-delete-file (sequence pathname) *application-frame*)
  :errorp nil)

(add-menu-item-to-command-table
  'fsedit-files "Undelete" 
  :function (make-command-from-selected-items 
	      com-undelete-file (sequence pathname) *application-frame*)
  :errorp nil)

(define-application-frame fsedit (selected-object-mixin)
    ((pathname :initarg :pathname :initform nil
	       :accessor fsedit-pathname)
     (truename-string :initform nil
	              :accessor fsedit-truename-string)
     (sort-by :initform :name
	      :accessor fsedit-sort-by))
  (:command-table (fsedit :inherit-from (activity
					 editing
					 files
                                         printers
                                         systems
					 fsedit-files
					 selected-objects
					 accept-values-pane)
			  :menu (("Activity" :menu activity)
				 ("File" :menu fsedit-files)
				 ("Selections" :menu selected-objects))))
  (:top-level (fsedit-top-level))
  (:pointer-documentation t)
  (:panes
    (pathname :accept-values
	      :height :compute
	      :display-function
	        '(accept-values-pane-displayer
		   :displayer read-fsedit-pathname)
	      :text-style '(:fix :roman :large)
	      :scroll-bars nil)
    (sort-by (make-pane 'option-pane
			:label "Sort by"
			:items '(("Name" . :name)
				 ("Type" . :type)
				 ("Size" . :size)
				 ("Date" . :date))
			:name-key #'car :value-key #'cdr
			:value :name
			:value-changed-callback 'fsedit-sort-changed))
    (display :application
	     :background +white+
	     :display-after-commands nil
	     :scroll-bars :vertical
             :end-of-line-action :allow
	     :output-record (make-instance 'text-scroll-output-history)))
  (:layouts
    (main
      (vertically () 
	(horizontally ()
	  (:fill #+Genera (outlining () (spacing () pathname))
		 #-Genera pathname)
	  (1/5 sort-by))
	(:fill display)))))

;; We can only select pathnames in this application
(defmethod object-selectable-p ((frame fsedit) object)
  (presentation-typep object 'pathname))

(defmethod invoke-with-frame-standard-output ((frame fsedit) continuation)
  (declare (dynamic-extent continuation))
  (with-pop-up-window (stream frame)
    (let ((*standard-output* stream))
      (funcall continuation stream))))

(defun fsedit-get-truename-string (pathname)
  (let ((tn (ignore-errors (truename pathname))))
    (cond (tn (namestring tn))
	  ((typep pathname 'logical-pathname) (translate-logical-pathname pathname))
	  (t (namestring pathname)))))

(defmethod fsedit-top-level ((frame fsedit) &key)
  (enable-frame frame)
  (setf (fsedit-pathname frame) (pathname (fsedit-pathname frame)))
  (setf (fsedit-truename-string frame) (fsedit-get-truename-string (fsedit-pathname frame)))
  (compute-directory-pane (fsedit-pathname frame))
  (default-frame-top-level frame))

(defmethod read-fsedit-pathname ((frame fsedit) stream)
  (with-slots (pathname truename-string) frame
    (when (null pathname)
      (setq pathname (or (let ((element (yank-from-history 
					 (presentation-type-history 'pathname))))
			   (and element 
				(presentation-history-element-object element)))
			 (make-pathname :name :wild
					:type #+Unix :unspecific #-Unix :wild
					:version #+Genera :unspecific #-Genera :newest
					:defaults (user-homedir-pathname)))))
    (multiple-value-bind (new-pathname type changed-p)
	(accept '((pathname) 
		  :default-type #+Unix :unspecific #-Unix :wild 
		  :default-version #+Genera :newest #-Genera :unspecific)
		:default pathname :prompt nil
		:stream stream)
      (declare (ignore type))
      (when changed-p
	(setq pathname #+Genera new-pathname
	               #-Genera (merge-pathnames new-pathname
						 #.(make-pathname :name :wild :type :wild :version :unspecific))
	      truename-string (fsedit-get-truename-string pathname))
	(compute-directory-pane pathname)))))

(defun fsedit-sort-changed (pane value)
  (let ((frame (pane-frame pane)))
    (setf (fsedit-sort-by frame) value)
    (compute-directory-pane (fsedit-pathname frame))))


(defclass pathname-line ()
   ((pathname :initarg :pathname :reader pathname-line-pathname)
    ;; Parent and child pathname lines
    (parent :initarg :parent :reader pathname-line-parent)
    (children :initform nil :accessor pathname-line-children)
    ;; The output record representing this line
    (record :initform nil :accessor pathname-line-record)
    (indentation :initarg :indentation :reader pathname-line-indentation)
    (properties :initarg :properties :reader pathname-line-properties)))

(defmethod print-object ((pline pathname-line) stream)
  (print-unreadable-object (pline stream :type t :identity t)
    (princ (pathname-line-pathname pline) stream)))

(declaim (inline make-pathname-line))

(defun make-pathname-line (pathname parent indentation &rest properties)
  #+Genera (declare (non-dynamic-extent properties))
  (make-instance 'pathname-line
		 :pathname pathname
		 :parent parent
		 :indentation indentation
		 :properties properties))

(define-presentation-type pathname-line ())

(define-presentation-method present
    (pline (type pathname-line) stream (view textual-view) &key)
  (flet ((write-directory-namestring (pathname stream)
	   #+Genera
	   (let ((dir (pathname-as-directory pathname)))
	     (format stream "~A:~A" (host-namestring dir) (directory-namestring dir)))
	   #-(and Genera (not Unix))
	   (format stream "~A" (car (last (pathname-directory pathname))))
	   #+Unix
	   (let* ((name (format nil "~A" pathname))
		  (root (fsedit-truename-string *application-frame*))
		  (length (length root)))
	     (if (string-equal name root :end1 length :end2 length)
	       (write-string name stream :start length)
	       (write-string name stream))))
	 (write-file-namestring (pathname stream)
	   #+Genera
	   (format stream "~A" pathname)
	   #-(and Genera (not Unix))
	   (let ((name (pathname-name pathname))
		 (type (pathname-type pathname)))
	     (format stream "~A~:[~;~:*.~A~]" name (when (stringp type) type)))
	   #+Unix
	   (let* ((name (format nil "~A" pathname))
		  (root (fsedit-truename-string *application-frame*))
		  (length (length root)))
	     (if (string-equal name root :end1 length :end2 length)
	       (write-string name stream :start length)
	       (write-string name stream)))))
    (declare (dynamic-extent #'write-directory-namestring #'write-file-namestring))
    (let ((pathname (pathname-line-pathname pline))
	  (pathname-plist (pathname-line-properties pline)))
      (with-output-as-presentation (stream pathname 'pathname :single-box t)
	(write-string (make-string (pathname-line-indentation pline) :initial-element #\Space)
		      stream)
	(if (getf pathname-plist :directory)
	  (write-directory-namestring pathname stream)
	  (write-file-namestring pathname stream))
	(multiple-value-bind (cx cy) 
	    (stream-cursor-position stream)
	  (stream-set-cursor-position stream (max 400 (+ cx 30)) cy)
	  (if (getf pathname-plist :directory)
	    (destructuring-bind (&key (modification-date 0) (author "") &allow-other-keys)
		pathname-plist
	      (format stream "Directory  ~A  ~A"
		(with-output-to-string (s)
		  (print-universal-time modification-date :stream s)) author))
	    (destructuring-bind (&key (length-in-bytes 0) (byte-size 8) (modification-date 0) (author "") &allow-other-keys)
		pathname-plist
	      (format stream "~6D(~D)  ~A  ~A"
		length-in-bytes byte-size
		(with-output-to-string (s)
		  (print-universal-time modification-date :stream s)) author)))))))) 

(defun compute-directory-pane (directory)
  ;; Bind *ORIGINAL-STREAM* to NIL so that this can get called from
  ;; inside of accept-values panes.  Yetch!
  (let ((*original-stream* nil))
    (let ((stream (get-frame-pane *application-frame* 'display)))
      (clear-items stream)
      (add-pathname-lines directory stream))))

(defun add-pathname-lines (directory-pathname stream &key (indentation 0) index pline)
  (when directory-pathname
    (loop for (file . properties)
	    in (sorted-directory-list directory-pathname
				      (fsedit-sort-by *application-frame*))
	  do (let* ((child-pline
		     (apply #'make-pathname-line file pline indentation properties))
		    (record (with-output-to-output-record (stream)
			      (present child-pline 'pathname-line
				       :stream stream :single-box t))))
	       (setf (pathname-line-record child-pline) record)
	       (add-item stream record :after-index index)
	       ;; Intentionally maintained in reverse order so that
	       ;; removing the items later is speedier
	       (when pline (push child-pline (pathname-line-children pline)))
	       (when index (incf index))))))

(define-fsedit-command com-expand-line
    ((pline 'pathname-line)
     (presentation 'presentation))
  (let ((stream (get-frame-pane *application-frame* 'display))
	(pathname (pathname-line-pathname pline)))
    (when (directory-pathname-p pathname)
      (let* ((item (output-record-text-scroll-item presentation))
	     (index (and item (find-item stream item)))
	     (directory
	       (make-pathname :name :wild 
                              :type #+Unix nil #-Unix :wild
                              :version #+Unix nil #-Unix :newest
			      :defaults (pathname-as-directory pathname)))
	     (indentation (+ (pathname-line-indentation pline) 2)))
	(when index
	  (with-text-scrolling-delayed (stream)
            (add-pathname-lines directory stream 
                                :indentation indentation
                                :index index :pline pline)))))))

(define-presentation-to-command-translator expand-line
    (pathname-line com-expand-line fsedit
     :gesture :select
     :priority 2				;win over other translators
     :tester ((object)
	      (and (null (pathname-line-children object))
		   (directory-pathname-p (pathname-line-pathname object))))
     :documentation "Expand this line"
     :echo nil :maintain-history nil)
    (object presentation)
  (list object presentation))

(define-fsedit-command com-contract-line
    ((pline 'pathname-line))
  (let ((stream (get-frame-pane *application-frame* 'display)))
    (with-text-scrolling-delayed (stream)
      (labels ((delete-children (pline)
                 (dolist (child (pathname-line-children pline))
                   (mapc #'delete-children (pathname-line-children child))
                   (delete-item stream (pathname-line-record child)))))
        (declare (dynamic-extent #'delete-children))
        (delete-children pline)))
    (setf (pathname-line-children pline) nil)))

(define-presentation-to-command-translator contract-line
    (pathname-line com-contract-line fsedit
     :gesture :select
     :priority 2				;win over other translators
     :tester ((object)
	      (pathname-line-children object))
     :documentation "Contract this line"
     :echo nil :maintain-history nil)
    (object)
  (list object))

