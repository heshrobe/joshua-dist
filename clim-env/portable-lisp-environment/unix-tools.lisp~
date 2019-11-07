;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-ENV; Base: 10; Lowercase: Yes -*-

;;; Copyright (c) 1994-2000, Scott McKay.
;;; Copyright (c) 2001-2003, Scott McKay and Howard Shrobe.
;;; All rights reserved.  No warranty is expressed or implied.
;;; See COPYRIGHT for full copyright and terms of use.

(in-package :clim-env)

;;; Unix-y applications


;;; Command shell

(define-application-frame shell ()
  ((input-stream :accessor shell-input-stream
		 :initform nil :initarg :stream)
   (output-stream :accessor shell-output-stream
		  :initform nil :initarg :stream))
  (:command-table (shell :inherit-from (activity)
			 :menu (("Activity" :menu activity))))
  (:command-definer t)
  (:top-level (shell-top-level))
  (:panes
    (interactor :interactor
		:background +white+
		:scroll-bars :both))
  (:layouts (default
	      (vertically ()
		(:fill interactor)))))

(define-presentation-type unix-command ()
  :inherit-from 'string
  :history t)

(defmethod shell-top-level ((frame shell))
  (unless (and (shell-input-stream frame)
	       (shell-output-stream frame))
    (start-shell frame))
  (enable-frame frame)
  (let* ((stream (frame-standard-input frame))
	 (command-table (frame-command-table frame))
         (command-type `(command :command-table ,command-table)))
    (with-input-focus (stream)
      (let* (#-Genera (*pointer-documentation-output*
	                (frame-pointer-documentation-output frame)))
	(condition-restart-loop (#+Genera (sys:error sys:abort)
				 #-Genera (error)
				 "Restart shell")
	  (shell-command-reader frame stream command-type))))))

;;--- We really need an extra asynchronous process to grab the output
(defun shell-command-reader (frame stream command-type)
  (catch-abort-gestures ("Return to ~A command level" (frame-pretty-name frame))
    (do ((output (get-unix-output frame) (get-unix-output frame)))
	((null output))
      (write-line output stream))
    (multiple-value-bind (command type)
	(with-input-context (command-type)
	    (command type)
	    (let ((command (accept 'unix-command 
                                   :stream stream
                                   :prompt nil :provide-default t)))
	      (values command 'unix-command))
	  (t
           (values command type)))
      (cond ((eq type 'unix-command)
             (send-unix-command frame command))
	    (t
	     (execute-frame-command frame command))))))

(defmethod start-shell ((frame shell))
  #+Lispworks
  (let ((stream (sys::open-pipe "/usr/local/bin/bash"
				:direction :io
				:buffered nil
				:use-pty t)))
    (setf (shell-input-stream frame) stream
	  (shell-output-stream frame) stream)))

(defmethod send-unix-command ((frame shell) command)
  #+Lispworks
  (write-line command (shell-output-stream frame)))

(defmethod get-unix-output ((frame shell) &key (timeout 1/4))
  #+Lispworks
  (let ((stream (shell-input-stream frame))
        (buffer (make-array 256 :element-type 'base-char
                                :adjustable t :fill-pointer 0)))
    (clim-sys:process-wait-with-timeout "Unix output"
      timeout #'(lambda () (listen stream)))
    (unless (listen stream)
      (return-from get-unix-output nil))
    (loop for char = (read-char stream nil nil)
	  while char
	  do (vector-push-extend char buffer))
    buffer))


;;; "Grep"

(define-application-frame grep (selected-object-mixin)
    ((pathname :initarg :pathname :initform nil
	       :accessor grep-pathname)
     (strings :initform nil
	      :accessor grep-strings)
     (conjunction :initform 'or
		  :accessor grep-conjunction))
  (:command-table (grep :inherit-from (activity
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
  (:top-level (grep-top-level))
  (:pointer-documentation t)
  (:panes
    (pathname :accept-values
	      :height :compute
	      :display-function
	        '(accept-values-pane-displayer
		   :displayer read-grep-pathname)
	      :text-style '(:fix :roman :large)
	      :scroll-bars nil)
    (strings :accept-values
	     :height :compute
	     :display-function
	       '(accept-values-pane-displayer
		  :displayer read-grep-strings)
	     :text-style '(:fix :roman :large)
	     :scroll-bars nil)
    (conjunction (make-pane 'option-pane
			    :label "Direction"
			    :items '(("Or" . or)
				     ("And" . and))
			    :name-key #'car :value-key #'cdr
			    :value 'or
			    :value-changed-callback 'grep-conjunction-changed))
    (display :application
	     :background +white+
	     :display-after-commands nil
	     :scroll-bars :vertical
             :end-of-page-action :allow
	     :end-of-line-action :allow))
  (:layouts
    (main
      (vertically () 
	#+Genera (outlining () (spacing () pathname))
	#-Genera pathname
	(horizontally ()
	  (:fill #+Genera (outlining () (spacing () strings))
		 #-Genera strings)
	  (1/5 conjunction))
	(:fill display)))))

;; We can only select pathnames in this application
(defmethod object-selectable-p ((frame grep) object)
  (presentation-typep object 'pathname))

(defmethod invoke-with-frame-standard-output ((frame grep) continuation)
  (declare (dynamic-extent continuation))
  (with-pop-up-window (stream frame)
    (let ((*standard-output* stream))
      (funcall continuation stream))))

(defmethod grep-top-level ((frame grep) &key)
  (enable-frame frame)
  (setf (grep-pathname frame) (pathname (grep-pathname frame)))
  (display-grep-results (grep-pathname frame) (grep-strings frame))
  (default-frame-top-level frame))

(defmethod read-grep-pathname ((frame grep) stream)
  (with-slots (pathname) frame
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
						 #.(make-pathname :name :wild :type :wild :version :unspecific)))
	(display-grep-results (grep-pathname frame) (grep-strings frame))))))

(defmethod read-grep-strings ((frame grep) stream)
  (with-slots (strings) frame
    (multiple-value-bind (new-strings type changed-p)
	(accept '(sequence string)
		:default strings :prompt nil
		:stream stream)
      (declare (ignore type))
      (when changed-p
	(setq strings new-strings)
	(display-grep-results (grep-pathname frame) (grep-strings frame))))))

(defun display-grep-results (pathname strings)
  (let ((*original-stream* nil))
    (let ((stream (get-frame-pane *application-frame* 'display)))
      (window-clear stream)
      (when strings
	(ignore-errors
	  (if (wild-pathname-p pathname)
	    (dolist (p (expand-wildcard-pathname pathname))
	      (search-file p stream strings
			   (grep-conjunction *application-frame*)))
	    (search-file pathname stream strings
			 (grep-conjunction *application-frame*))))))))

(defun grep-conjunction-changed (pane value)
  (let ((frame (pane-frame pane)))
    (setf (grep-conjunction frame) value)
    (display-grep-results (grep-pathname frame) (grep-strings frame))))
