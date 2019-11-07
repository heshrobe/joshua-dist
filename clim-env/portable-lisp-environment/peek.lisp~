;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-ENV; Base: 10; Lowercase: Yes -*-

;;; Copyright (c) 1994-2000, Scott McKay.
;;; Copyright (c) 2001-2003, Scott McKay and Howard Shrobe.
;;; All rights reserved.  No warranty is expressed or implied.
;;; See COPYRIGHT for full copyright and terms of use.

(in-package :clim-env)

;;; Process browser

;;--- It would be nice if this had a few other modes (e.g., Windows, etc.)
(define-application-frame peek (selected-object-mixin)
  ((mode :initform :help :accessor peek-mode)
   (sleep-time :initform 10 :accessor peek-sleep-time)
   (items :initform nil :accessor peek-processes-items)
   (name-width :initform nil :accessor peek-processes-name-width))
  (:command-table (peek :inherit-from (activity
				       processes
				       selected-objects)
			:menu (("Activity" :menu activity)
			       ("Selections" :menu selected-objects))))
  (:top-level (peek-top-level))
  (:pointer-documentation t)
  (:panes
   (display :application
	    :background +white+
	    :scroll-bars :vertical
	    :output-record (make-instance 'text-scroll-output-history)
	    :end-of-line-action :allow
	    :end-of-page-action :allow))
  (:layouts
   (main
    (vertically () 
      display))))

(defmethod invoke-with-frame-standard-output ((frame peek) continuation)
  (declare (dynamic-extent continuation))
  (with-pop-up-window (stream frame)
    (let ((*standard-output* stream))
      (funcall continuation stream))))

(defmethod peek-top-level ((frame peek) &key)
  ;; Enable the frame now
  (unless (eql (frame-state frame) :enabled)
    (enable-frame frame))
  (let* ((stream (get-frame-pane frame 'display))
	 (*standard-output* stream)
	 (*standard-input* stream)
	 (*query-io* stream)
	 (*error-output* stream)
	 (*command-parser* #'peek-command-parser)
	 (*command-unparser* #'command-line-command-unparser)
	 (*partial-command-parser* #'menu-read-remaining-arguments-for-partial-command))
    #+++IGNORE
    (unless (peek-mode frame)
      (com-display-processes))
    (loop (redisplay-peek-display frame)
      (catch-abort-gestures
	  ("Return to ~A command level" (frame-pretty-name frame))
	(let ((command (read-frame-command frame :stream *standard-input*)))
	  ;; Need this check in case the user aborted out of a command menu
	  (when command
	    (execute-frame-command frame command)))))))

(defun peek-command-parser (command-table stream)
  (menu-command-parser
    command-table stream
    :timeout (peek-sleep-time *application-frame*)))


(defvar *peek-mode-alist* nil)

(defmacro define-peek-mode (mode displayer help-string)
  #+Genera (declare (zwei:indentation 2 1))
  `(let ((old-entry (assoc ',mode *peek-mode-alist*)))
     (cond (old-entry
	    (setf (second old-entry) ',displayer)
	    (setf (third old-entry) ,help-string))
	   (t
	    (setq *peek-mode-alist*
		  (nconc *peek-mode-alist*
			 (list (list ',mode ',displayer ,help-string))))))))

(defun redisplay-peek-display (frame)
  (let* ((stream (get-frame-pane frame 'display))
	 (displayer (second (assoc (peek-mode frame) *peek-mode-alist*))))
    (with-simple-restart (nil "Skip redisplaying pane ~S" stream)
      (loop
	(with-simple-restart (nil "Retry displaying pane ~S" stream)
	  (return
	    (funcall displayer frame stream))))))) 

;; Help 
(define-peek-command (com-display-help :menu "Help" :keystroke #\?) ()
  (let ((stream (get-frame-pane *application-frame* 'display)))
    (clear-items stream)
    (setf (peek-mode *application-frame*) :help)
    (let ((record (with-output-to-output-record (stream)
		    (dolist (entry *peek-mode-alist*)
		      (write-string (third entry) stream)
		      (terpri stream)
		      (terpri stream)))))
      (add-item stream record))))

(defun display-peek-help (frame stream)
  (declare (ignore frame stream)))

(define-peek-mode :help display-peek-help
  "Help (?):
   Show a brief help message for Peek.")


;; Force a redisplay (not from scratch)
(define-peek-command (com-do-redisplay :keystroke #\space) ()
  ;; Do nothing, let the command loop do it
  ) 


;;; Processes 

(define-peek-command (com-display-processes :menu "Processes" :keystroke :p) ()
  (with-application-frame (frame)
    (let ((stream (get-frame-pane frame 'display)))
      (clear-items stream)
      (setf (peek-mode frame) :processes)
      (setf (peek-processes-items frame) nil)
      (let* ((name-width
	      (reduce #'(lambda (size process)
			  (max size (length (clim-sys:process-name process))))
		      (clim-sys:all-processes)
		      :initial-value 0))
	     (record 
	      (with-output-to-output-record (stream)
		(with-text-face (stream :italic)
		  (write-string
		   (format nil "~vA~25A~10A~9A~14A~%~%" name-width
		     "Process Name" "State" "Priority" "  CPU" "  Idle" " % Utilization")
		   stream)))))
	(add-item stream record :before-index 0)
	(setf (peek-processes-name-width frame) name-width)))))

(defun display-peek-processes (frame stream)
  (let* ((processes (copy-list (clim-sys:all-processes)))
	 (name-width (peek-processes-name-width frame))
	 (index 0))
    ;; Remove all of the processes that are no longer with us
    (let ((done (remove-if #'(lambda (pair) (member (car pair) processes)) (peek-processes-items frame))))
      (dolist (entry done)
	(setf (peek-processes-items frame)
	  (delete entry (peek-processes-items frame)))
	(delete-item stream (output-record-text-scroll-item (second entry)))))
    ;; Display the current processes
    (silica:inhibit-updating-scroll-bars #+Allegro (stream)
     (dolist (process processes)
       (let ((old-process (assoc process (peek-processes-items frame))))
	 (cond (old-process
		;; We've displayed this process, just redisplay it
		(redisplay (second old-process) stream))
	       ;; New process, display it freshly
	       (t (let* ((process process) ;new contour for this, please
			 redisplay-record
			 (record
			  (with-output-to-output-record (stream)
			    (setq redisplay-record
			      (updating-output (stream :unique-id process)
				(multiple-value-bind (name whostate priority cpu-time idle-time percent)
				    (process-information process)
				  (with-output-as-presentation (stream process 'process :single-box t)
				    (updating-output (stream :unique-id 'name :cache-value name)
				      (write-string (format nil "~vA" name-width name) stream))
				    (updating-output (stream :unique-id 'whostate :cache-value whostate)
				      (write-string (format nil "~25A" whostate) stream))
				    (updating-output (stream :unique-id 'priority :cache-value priority :cache-test #'equal)
				      (write-string (format nil "~10A" priority)
						    stream))
				    (updating-output (stream :unique-id 'cpu-time :cache-value cpu-time)
				      (write-string (format nil "~9A" (format-cpu-time cpu-time)) stream))
				    (updating-output (stream :unique-id 'idle-time :cache-value idle-time)
				      (write-string (format nil "~9A" (format-idle-time idle-time)) stream))
				    (updating-output (stream :unique-id 'percent :cache-value percent)
				      (write-string (format nil "~14A" (format-percent-utilization percent)) stream)))))))))
		    (add-item stream record :after-index index)
		    (pushnew (list process redisplay-record) (peek-processes-items frame)
			     :key #'first)
		    (incf index)))))))))

(defun process-information (process)
  (declare (values name whostate priority idle-time percent))
  #+Genera (values (clim-sys:process-name process)
		   (tv:peek-process-whostate process)
		   (tv:peek-process-priority process)
		   (process::process-cpu-time process)
		   (process::process-idle-time process)
		   (process::percent-utilization process))
  #-Genera
  (values (clim-sys:process-name process)
	  (clim-sys:process-state process)
	  (clim-sys::process-priority process)
	  (clim-sys::process-cpu-time process)
	  (clim-sys::process-idle-time process)
	  (clim-sys::process-percent-utilization process))) 

(defun format-cpu-time (cpu-time)
  (format nil "~D" cpu-time))

(defun format-idle-time (idle-time)
  (let ((time (and idle-time (floor idle-time 60.))))
    (cond ((null time) "forever")
	  ((zerop time) "")
	  ((< time 60) (format nil "~3D sec" time))
	  ((< time 3600) (format nil "~3D min" (floor time 60.)))
	  (t (format nil "~4D hr" (floor time 3600.))))))

(defun format-percent-utilization (percent)
  (format nil "~:[     ~;~:*~1,1,5$%~]" percent))

(define-peek-mode :processes display-peek-processes
  "Processes (P):
   Show all active processes, their state, priority, and so forth.")
