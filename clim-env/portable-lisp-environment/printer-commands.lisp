;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-ENV; Base: 10; Lowercase: Yes -*-

;;; Copyright (c) 1994-2000, Scott McKay.
;;; Copyright (c) 2001-2003, Scott McKay and Howard Shrobe.
;;; All rights reserved.  No warranty is expressed or implied.
;;; See COPYRIGHT for full copyright and terms of use.

(in-package :clim-env)

;;; Printer commands

;;; Managing the default printer 

;;;  MCL does not provide infrastructure for manipulating printers -- JCMa 12/12/2001.
#-MCL
(defvar *default-printer* nil)

#-MCL
(defvar *printer-icon* (make-instance 'tool-bar-icon
			 ;;--- We need an icon
			 :pattern "Printer"
			 :object *default-printer*
			 :type 'printer)) 

#-MCL
(define-command (com-set-default-printer :command-table printers :name t)
    ((printer '(null-or-type printer)
	      :default *default-printer*))
  (setq *default-printer* printer)
  (setf (icon-object *printer-icon*) printer))

#-MCL
(define-command (com-show-default-printer :command-table printers :name t) ()
  (with-frame-standard-output (stream)
    (cond (*default-printer*
           (fresh-line stream)
	   (write-string "The default printer is " stream)
	   (present *default-printer* 'printer :stream stream))
	  (t
           (fresh-line stream)
	   (write-string "There is no default printer" stream))))) 


;;; Printing files 

(defun hardcopy-file (pathname printer &rest options &key query &allow-other-keys)
  (declare (dynamic-extent options)
	   #+MCL (ignore printer))
  #+(or Genera (and Lispworks Unix) (and Allegro Unix))
  (clim-utils:with-keywords-removed (options options '(:query))
    #+(or (and unix allegro) (and lispworks unix))
    (declare (ignore options))
    (when (or (not query)
	      (y-or-n-p "Print file ~A? " pathname))
      #+Genera 
      (apply #'hardcopy:hardcopy-file pathname printer options)
      #+(and Lispworks Unix)
      (let ((print-string (if (string-equal (pathname-type pathname) "ps")
			    "lpr -P~A ~A"
			    "print -P~A -p 10 ~A")))
	(if printer
	  (ccl::call-system (format nil print-string printer pathname))
	  (ccl::call-system (format nil print-string pathname))))
       #+(and Unix Allegro)
      (let ((print-string (format nil "lpr -P~A ~A" printer pathname)))
	(excl:shell print-string)
	)))
  #+MCL
  (prog1 (handler-case
	     (ccl::hardcopy-file pathname query)
	   (file-error (err)
	     (beep *standard-output*)
	     (fresh-line *standard-output*) 
	     (ccl::report-condition err *standard-output*)))
    (when (and (getf options :delete)
	       (or (not query)
		   (yes-or-no-p "Delete file ~A? " pathname)))
      (delete-file pathname))))

(define-command (com-hardcopy-file :command-table printers :name t)
    ((pathnames '(sequence pathname)
		:provide-default t
		:prompt "file(s)")
     &key
     #-MCL (printer '(null-or-type printer)
		    :default *default-printer*
		    :documentation "Name of printer on which to do hardcopy")
     #-MCL (copies 'integer
		   :default 1
		   :prompt "number of copies"
		   :documentation "Number of copies to print")
     (delete 'boolean
	     :default nil :mentioned-default t
	     :prompt "delete after printing"
	     :documentation "Delete file after printing")
     #-MCL (orientation '(member :landscape :portrait)
			:default :portrait
			:documentation "Orientation to use with respect to the paper")
     (query 'boolean
	    :default nil :mentioned-default t
	    :documentation #-MCL "Ask before printing each file"
	                   #+MCL "Ask for parameters before printing each file."))
  (dolist (pathname pathnames)
    (if (wild-pathname-p pathname)
      (dolist (p (expand-wildcard-pathname pathname))
	#-MCL (hardcopy-file p printer :copies copies :delete delete :orientation orientation :query query)
	#+MCL (hardcopy-file p nil :delete delete :query query))
      #-MCL (hardcopy-file pathname printer
			   :copies copies :delete delete :orientation orientation :query query)
      #+MCL (hardcopy-file pathname nil
			   :delete delete :query query))))

(define-presentation-to-command-translator com-hardcopy-file
    (pathname com-hardcopy-file printers
     :gesture nil
     :tester ((object)
	      (not (directory-pathname-p object))))
  (object)
  (list (list object)))

(define-drag-and-drop-translator d&d-hardcopy-file
    (pathname command printer printers)
    (object destination-object)
  `(com-hardcopy-file ,(list object) :printer ,destination-object)) 

#+Genera
(define-command (com-hardcopy-system :command-table systems :name t)
    ((system '(type-or-string system)
	     :default-type 'system
	     :provide-default t)
     &key
     (printer 'printer
	      :default *default-printer*
	      :documentation "Name of printer on which to do hardcopy")
     (include-components 'boolean
			 :default t
			 :documentation "Hardcopy files in component systems")
     (query '(member :yes :no :confirm)
	    :default :no :mentioned-default :yes
	    :documentation "Query about hardcopying files")
     (silent 'boolean
	     :default nil :mentioned-default t
	     :documentation "Suppress all terminal output"))
  (let ((system #+Genera (sct:find-system-named system nil nil t) 
		#-Genera system))
    (assert (and (not (null system)) #-Genera (not (stringp system))) ()
      "There is no system named ~A" system)
    #+Genera (sct:hardcopy-system system
				  :hardcopy-device printer
				  :include-components include-components
				  :query (case query
					   (:yes t)
					   (:no nil)
					   (:confirm :confirm))
				  :silent silent)))



;;--- Delete Printer Request
;;--- Show Printer Status

