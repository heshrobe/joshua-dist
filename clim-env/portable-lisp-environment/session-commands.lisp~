;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-ENV; Base: 10; Lowercase: Yes -*-

;;; Copyright (c) 1994-2000, Scott McKay.
;;; Copyright (c) 2001-2003, Scott McKay and Howard Shrobe.
;;; All rights reserved.  No warranty is expressed or implied.
;;; See COPYRIGHT for full copyright and terms of use.

(in-package :clim-env)

;;;  Session commands

;;--- Halt Session
;;--- Login
;;--- Logout
;;--- Show Herald 


;;; Select activity 

#+MCL
(defvar *activity-alist* nil)

#+MCL
(defun sort-activity-alist ()
  (setq *activity-alist* (sort *activity-alist* #'string< :key #'car)))

#+MCL
(ccl:require "Processes" "ccl:examples;processes")

#+MCL
(defun add-activity (name selection-function &optional key-binding documentation (sort-p t))
  (let ((entry (assoc name *activity-alist* :test #'equalp)))
    (when key-binding			; add FRED key binding
      (ccl::comtab-set-key ccl::*comtab* key-binding selection-function documentation))
    (cond (entry
	   (setf (second entry) selection-function
		 (third entry) documentation
		 (fourth entry) key-binding)
	   entry)
	  (t (setq entry `(,name ,selection-function,documentation ,key-binding))
	     (push entry *activity-alist*)
	     (when sort-p (sort-activity-alist))
	     (values entry t)))))

#+MCL
(defun remove-activity (name)
  (let ((entry (assoc name *activity-alist* :test #'equalp)))
    (when entry
      (when (third entry)
	(ccl::comtab-set-key ccl::*comtab* (third entry) nil)) ; remove key binding from FRED
      (setq *activity-alist* (delete entry *activity-alist*))
      (values t entry))))

#+MCL
(defun %select-activity (activity)
   (funcall (second activity)))

#+MCL
(defmacro define-activity (name (pretty-name &key key-binding documentation) &body body)
   `(progn
       (defun ,name (&optional ignore)
          (declare (ignore ignore))
          ,@body)
       (add-activity ,pretty-name ',name ,key-binding ,documentation t)
       ',name))

#+MCL
(progn
(define-activity select-finder ("Finder" :documentation "Select the Finder.")
   (ccl::select-finder))

(define-activity select-eudora ("Eudora" :documentation "Select the Eudora.")
  (ccl::select-process :|CSOm|))

(define-activity select-netscape ("Netscape" :documentation "Select Netscape Navigator")
  (ccl::select-process :|MOSS|))

(define-activity select-internet-explorer ("Explorer" :documentation "Select Microsoft Internet Explorer.")
  (ccl::select-process :|MSIE|))

(define-activity select-msword ("Word" :documentation "Select Microsoft Word.")
  (ccl::select-process :|MSWD|))

(define-activity select-talk ("Talk" :documentation "Select Talk.")
  (ccl::select-process :|tALK|))

(define-activity select-fetch ("Fetch" :documentation "Select Fetch.")
  (ccl::select-process :|FTCh|))

(define-activity select-trace-route ("TraceRoute" :documentation "Select Trace Route.")
  (ccl::select-process :|TRCE|))

(define-activity select-power-point ("PowerPoint" :documentation "Select Microsoft Power Point.")
  (ccl::select-process :|PPT3|))

(define-activity select-acrobat-reader ("Acrobat" :documentation "Select Adobe Acrobat Reader.")
  (ccl::select-process :|CARO|))

(define-activity select-timbuktu ("Timbuktu" :documentation "Select Netopia Timbuktu.")
  (ccl::select-process :|T$2a|))

(define-activity select-ssh ("SSH" :documentation "Select F-Secure SSH")
  (ccl::select-process :|FSSH|))

(define-activity select-exodus ("Exodus" :documentation "Select Exodus X-WIndows.")
  (ccl::select-process :|EXOD|))

(define-activity select-telnet ("Telnet" :documentation "Select NCSA Telnet.")
  (ccl::select-process :|NCSA|)) 

;;; CLIM Environment Windows

(define-activity select-lisp-listener ("Lisp Listener" :documentation "Select Lisp Listener.")
  (clim:find-application-frame 'lisp-listener))

(define-activity select-filesystem-editor ("FSEdit" :documentation "Select Filesystem Editor.")
  (clim:find-application-frame 'fsedit))

(define-activity select-class-browser ("Class Browser" :documentation "Select Class Browser.")
  (clim:find-application-frame 'class-browser))

)

#+MCL
(define-command (com-select-activity :command-table windows :name t) 
    ((activity `((completion ,*activity-alist* :test equalp)
		 :name-key first
		 :documentation-key third)
	       :documentation "an activity name"))
  (%select-activity activity))

;;; GC commands 

#-Genera
(defun run-gc-cleanups ()
  ;; Cleanup MCL windows
  #+MCL
  (flet ((maybe-close-window (window)
	   (typecase window
	     ;; Save windows to close without danger of losing state
	     ((or ccl::callers-dialog ccl::doc-output-class ccl::apropos-dialog ccl::menu-of-defs-dialog
	       ccl::defs-select-dialog ccl::dialog inspector::inspector-window ccl::ed-help-window)
	      (ccl:window-close window)))))
    (ccl::map-windows #'maybe-close-window :include-invisibles t))
  ;; -- presentations
  ;; -- histories
  ) 

(define-command (com-start-gc :command-table gc :name "Start GC")
    (&key
     #+(or Genera MCL)
     (generational 'boolean :default t
		   :documentation "Start up the generational GC")
     #+Genera
     (dynamic 'boolean :default nil :mentioned-default t
	      :documentation "Start up the dynamic GC")
     #+(or Genera MCL)
     (immediately 'boolean :default nil :mentioned-default t
		  :documentation "Perform a garbage collection now")
     #+(or Genera MCL)
     (cleanup 'boolean :default nil :mentioned-default t
	      :documentation "Perform a cleanup before the garbage collection"))
  #+Genera (cond (immediately
		  (when cleanup
		    (si:run-gc-cleanups))
		  (si:gc-immediately))
		 (t (si:gc-on :dynamic dynamic :ephemeral generational)))
  #+MCL (cond (immediately
	       (when cleanup
		 (run-gc-cleanups))
	       (ccl:gc))
	      ;; There is no obvious way to turn off the GC in MCL
	      ((and generational (not (ccl:egc-active-p)))
	       (ccl:egc generational)))
  #+Lispworks (system:gc)
  #+Allegro (excl:gc))

(define-command (com-stop-gc :command-table gc :name "Halt GC") ()
  #+Genera (si:gc-off)
  #+MCL (ccl:egc nil))

(define-command (com-show-gc-status :command-table gc :name "Show GC Status") 
    (&key
     (detailed 'boolean :default nil :mentioned-default t
	       :documentation "Display detailed information."))
  (with-frame-standard-output (stream)
    (fresh-line stream)
    #+Genera (zl:gc-status)
    #+MCL (format stream "The dynamic garbage colletors is on.~&The generational garbage collector is ~:[off~;on~].~%"
		  (ccl:egc-active-p))
    #-Genera (room detailed)
    (fresh-line stream)))

#|
(define-command (com-set-default-text-style :command-table session  :name t) 
    ((text-style 'text-style
		 :default *default-text-style* :provide-default t
		 :documentation "Change the default text style used by CLIM applications."))
  (cond ((eq text-style *default-text-style*))
	(t (setq *default-text-style* text-style))))
|#
