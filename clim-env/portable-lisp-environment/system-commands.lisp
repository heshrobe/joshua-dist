;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-ENV; Base: 10; Lowercase: Yes -*-

;;; Copyright (c) 1994-2000, Scott McKay.
;;; Copyright (c) 2001-2003, Scott McKay and Howard Shrobe.
;;; All rights reserved.  No warranty is expressed or implied.
;;; See COPYRIGHT for full copyright and terms of use.

(in-package :clim-env)

;;; System configuration management commands

;;; System utilities 

#-(or Genera Allegro)
(defun clim-defsystem::find-system-named (system)
  (etypecase system
    (clim-defsystem::system system)
    ((or string symbol) (clim-defsystem::find-system system))))

#-(or Genera Allegro)
(defun clim-defsystem::system-map-files
    (system function &key (file-type :source) (include-components t))
  (flet ((relevant-subsystems (s)
           (remove-if-not #'(lambda (s) (clim-defsystem::system-loaded-p (clim-defsystem::find-system-named s)))
                          (clim-defsystem::system-needed-systems (clim-defsystem::find-system-named s))))
         (map-files (system function file-type)
           (dolist (module (clim-defsystem::system-module-list (clim-defsystem::find-system-named system)))
             (let ((pathname (ecase file-type
                               (:source (clim-defsystem::module-src-path module))
                               (:binary (clim-defsystem::module-bin-path module)))))
               (when pathname
                 (funcall function pathname))))))
    (declare (dynamic-extent #'relevant-subsystems #'map-files))
    (if include-components 
      (dolist (sys (clim-defsystem::expand-subsystems (list system) #'relevant-subsystems))
	(map-files sys function file-type))
      (map-files system function file-type)))) 

#-(or Genera Allegro)
(defun clim-defsystem::edit-system (system &key query silent (include-components t))
  (labels ((report-activity (file &optional (stream *standard-output*))
             (format stream "~&Reading ")
             (present file 'pathname :stream stream)
             (write-string " ..." stream))
           (edit-file (file)
             #+MCL (unless (ccl::pathname-to-window file)
		     (when (or (not query)
			       (y-or-n-p "Edit file ~A? " file))
		       (unless silent
			 (report-activity file))
		       (make-instance ccl::*default-editor-class*
			 :filename file :window-layer 1000)))	;keep it buried
             #-MCL (prog2 
		     (unless silent (report-activity file))
		     (ed file))))
    (declare (dynamic-extent #'report-activity #'edit-file))
    (clim-defsystem::system-map-files system #'edit-file
				      :include-components include-components :file-type :source))) 


;;; System commands 

;;--- Implement :Output File keyword
(define-command (com-compile-file :command-table systems :name t)
    ((pathnames '(sequence pathname)
		:provide-default t
		:prompt "file(s)")
     &key
     (output-file '(null-or-type pathname)
		  :default nil
		  :mentioned-default (make-pathname :name :unspecific 
						    :type :unspecific
						    :version #+Unix nil #-Unix :unspecific
						    :defaults (first pathnames))
		  :documentation "Output file pathname")
     (load 'boolean
	   :default nil :mentioned-default t 
	   :documentation "Load the file after compiling it")
     (query 'boolean
	    :default nil :mentioned-default t
	    :documentation "Ask before loading each file")
     (silent 'boolean
	     :default nil :mentioned-default t
	     :documentation "Suppress all terminal output"))
  (when (or (> (length pathnames) 1)
	    (some #'wild-pathname-p pathnames))
    (unless (or (null output-file)
		(or (null (pathname-name output-file))
		    (eql (pathname-name output-file) :unspecific)
		    (eql (pathname-name output-file) :wild)))
      (return-from com-compile-file
	(with-frame-standard-output (stream)
	  (format stream "The output file ~A must not have a fixed name component"
		  output-file)))))
  (let ((default-type 
	 (and output-file
	      (if (or (null (pathname-type output-file))
		      (eql (pathname-type output-file) :unspecific)
		      (eql (pathname-type output-file) :wild))
		#+Lispworks compiler:*fasl-extension-string*
		#+Allegro excl:*fasl-default-type*
		#+MCL (pathname-type ccl:*.fasl-pathname*)
		#-(or Lispworks Allegro MCL) :unspecific 
		(pathname-type output-file)))))
    (flet ((compilef (pathname)
	     (when (or (not query)
		       (y-or-n-p "Compile the file ~A? " pathname))
	       (multiple-value-bind (compiler loader)
		   (find-compiler-and-loader pathname)
		 (let* ((output (make-pathname :name (pathname-name pathname)
					       :type default-type
					       :defaults pathname))
			(binary (if output-file
				  (funcall compiler pathname 
					   :output-file output
					   :verbose (not silent) :print (not silent))
				  (funcall compiler pathname 
					   :verbose (not silent) :print (not silent)))))
		   (when load
		     (funcall loader (or binary output) :verbose (not silent))))))))
      (declare (dynamic-extent #'compilef))
      (dolist (pathname pathnames)
	(if (wild-pathname-p pathname)
	  (dolist (p (expand-wildcard-pathname pathname))
	    (compilef p))
	  (compilef pathname))))))

(define-presentation-to-command-translator compile-file
    (pathname com-compile-file systems
     :gesture nil)
    (object)
  (list (list object)))

(define-drag-and-drop-translator d&d-compile-file
    (pathname command compiler systems)
    (object)
  `(com-compile-file ,(list object))) 

(define-command (com-load-file :command-table systems :name t)
    ((pathnames `(sequence ((pathname)
			    :default-type #+Genera ,si:*default-binary-file-type*
					  #+Lispworks ,compiler:*fasl-extension-string*
					  #+Allegro ,excl:*fasl-default-type*
					  #+MCL ,(pathname-type ccl:*.fasl-pathname*)))
		:provide-default nil :prompt "file(s)")
     &key
     #+mcl (external-format '(member :unix nil) :default nil
                            :documentation "Load in unix format?")
     (query 'boolean :default nil :mentioned-default t
	    :documentation "Ask before loading each file")
     (silent 'boolean :default nil :mentioned-default t
	     :documentation "Suppress all terminal output"))
  (flet ((loadf (pathname)
	   (when (or (not query)
		     (y-or-n-p "Load the file ~A? " pathname))
	     (multiple-value-bind (compiler loader)
		 (find-compiler-and-loader pathname)
	       (declare (ignore compiler))
	       (funcall loader pathname :verbose (not silent) #+mcl :external-format #+mcl external-format)))))
    (declare (dynamic-extent #'loadf))
    (dolist (pathname pathnames)
      (if (wild-pathname-p pathname)
	(dolist (p (expand-wildcard-pathname pathname))
	  (loadf p))
	(loadf pathname)))))

(define-presentation-to-command-translator load-file
    (pathname com-load-file systems
	      :gesture nil)
    (object)
  (list (list object)))

(define-drag-and-drop-translator d&d-load-file
    (pathname command loader systems)
    (object)
  `(com-load-file ,(list object))) 


(defun find-system (system-name)
  (or
   #+Allegro
   (typecase system-name 
     (defsys:default-system system-name)
     ((or string symbol)
      (find system-name (defsys:list-all-systems) 
	    :test #'string-equal 
	    :key #'defsys:pretty-name
	    )))
   #+asdf
   (asdf/find-system:find-system system-name)
   #+genera
   (sct:find-system-named system-name nil nil t)
   #-(or Allegro Genera) 
   (clim-defsystem::find-system-named system-name)))

(define-command (com-compile-system :command-table systems :name t)
    ((system 'system
	     :default-type 'system
	     :provide-default t
	     )
     &key
     (condition '(member :always :changed-sources)
		:default :changed-sources
		:documentation "Which files to compile")
     (load '(member :everything :newly-compiled :only-for-dependencies :nothing)
	   :default :newly-compiled
	   :documentation "Which files to load.")
     (redefinitions-ok 'boolean
		       :default nil :mentioned-default t
		       :documentation "Whether to proceed through redefinition warnings")
     #+Genera
     (increment-version 'boolean
			:default t
			:documentation "Increment the major version number of the system")
     (include-components 'boolean
			 :default t
			 :documentation "Load component systems")
     #-Allegro
     (query '(member :yes :no :confirm)
	    :default :no :mentioned-default :yes
	    :documentation "Query about compiling files")
     (silent 'boolean
	     :default nil :mentioned-default t
	     :documentation "Suppress all terminal output"))
  (let ((system (find-system system)))
    (assert (and (not (null system)) #-Genera (not (stringp system))) () "There is no system named ~A" system)
    #+Genera (sct:compile-system (sct:system-name system)
				 :recompile (eql condition :always)
				 :reload (eql load :everything)
				 :no-load (eql load :only-for-dependencies)
				 :never-load (eql load :nothing)
				 :include-components include-components
				 :no-warn redefinitions-ok
				 :increment-version increment-version
				 :query (case query
					  (:yes t)
					  (:no nil)
					  (:confirm :confirm))
				 :silent silent)
     #+Allegro (typecase system
		 (defsystem:default-system
		     (excl:compile-system system
					  :recompile (eql condition :always)
					  :reload (eql load :everything)
					  :include-components include-components
					  :no-warn redefinitions-ok
					  :silent silent))
		 (asdf/system:system
		  (asdf:compile-system system 
				       :force (eql condition :always)
				       :verbose (not silent)
				       )))		 
    #-(or Allegro Genera)
    (let ((*compile-print* (if silent nil *compile-print*))
	  (*compile-verbose* (if silent nil *compile-print*))
	  (*load-print* (if silent nil *load-print*))
	  (*load-verbose* (if silent nil *load-verbose*)) 
	  (query (case query
		   ((:yes :confirm) t)
		   (:no nil)))
	  #+LispWorks (sys::*handle-warn-on-redefinition* (if redefinitions-ok :warn :error))
	  #+MCL (ccl:*warn-if-redefine* (not redefinitions-ok))
	  #+MCL (*error-output* *standard-output*))
      (clim-defsys:compile-system (clim-defsys::system-name system)
				  :recompile (eql condition :always)
				  :reload (eql load :everything)
				  :include-components include-components
				  :propagate nil
				  :query query)
      (when (eql load :everything)
	(clim-defsys:load-system (clim-defsys::system-name system)
				 :query query)))))

(define-presentation-to-command-translator compile-system
    (system com-compile-system systems
     :gesture nil)
    (object)
  (list object))

(define-drag-and-drop-translator d&d-compile-system
    (system command compiler systems)
    (object)
  `(com-compile-system ,object)) 

(define-command (com-load-system :command-table systems :name t)
    ((system 'system
	     :default-type 'system
	     :provide-default t)
     &key
     (condition '(member :always :changed-sources)
		:default :changed-sources
		:documentation "Which files to load")
     (redefinitions-ok 'boolean
		       :default nil :mentioned-default t
		       :documentation "Whether to proceed through redefinition warnings")
     #+(or Genera Allegro)
     (include-components 'boolean
			 :default t
			 :documentation "Load component systems")
     #+Genera
     (version '(or integer symbol)
	      :default :released
	      :documentation "Which major version of the system to load")
     #+Genera
     (load-patches 'boolean
		   :default t
		   :documentation "Load patches after loading the system")
     #+Allegro
     (simulate 'boolean
	       :default nil :mentioned-default t
	       :documentation "Just print the actions that would be taken")
     #-Allegro
     (query '(member :yes :no :confirm)
	    :default :no :mentioned-default :yes
	    :documentation "Query about loading files") 
     (silent 'boolean
	     :default nil :mentioned-default t
	     :documentation "Suppress all terminal output"))
  (let ((system (find-system system)))
    (assert (and (not (null system)) #-Genera (not (stringp system))) ()
      "There is no system named ~A" system)
    #+Genera (sct:load-system (sct:system-name system)
			      :reload (eql condition :always)
			      :include-components include-components
			      :no-warn redefinitions-ok
			      :load-patches load-patches
			      :version version
			      :query (case query
				       (:yes t)
				       (:no nil)
				       (:confirm :confirm))
			      :silent silent)
     #+Allegro
      (typecase system
	(defsystem:default-system
	    (excl:load-system (defsys:system-name system)
			      :reload (eql condition :always)
			      :include-components include-components
			      :simulate simulate
			      :no-warn redefinitions-ok
			      :silent silent))
	(asdf/system:system (asdf:load-system 
			     system
			     :force (eql condition :always)
			     :verbose (not silent))))
					      
    #-(or Genera Allegro)
    (let ((*compile-print* (if silent nil *compile-print*))
	  (*compile-verbose* (if silent nil *compile-print*))
	  (*load-print* (if silent nil *load-print*))
	  (*load-verbose* (if silent nil *load-verbose*))
	  (query (case query
		   ((:yes :confirm) t)
		   (:no nil)))
	  #+Lispworks (sys::*handle-warn-on-redefinition* (if redefinitions-ok :warn :error))
	  #+MCL (ccl:*warn-if-redefine* (not redefinitions-ok))
	  #+MCL(*error-output* *standard-output*))
      (clim-defsys:load-system (clim-defsys::system-name system) :reload (eql condition :always) :query query))))

(define-presentation-to-command-translator load-system
    (system com-load-system systems
     :gesture nil)
    (object)
  (list object))

(define-drag-and-drop-translator d&d-load-system
    (system command loader systems)
    (object)
  `(com-load-system ,object)) 

(define-command (com-show-system-files :command-table systems :name t)
    ((system 'system
	     :default-type 'system
	     :provide-default t)
     &key
     (include-components 'boolean
			 :default t
			 :documentation "Show files in component systems"))
  (declare (ignore #+Allegro query))
  (let ((system (find-system system)))
    (assert (and (not (null system)) #-Genera (not (stringp system))) ()
      "There is no system named ~A" system)
    (with-frame-standard-output (stream)
      #+Genera nil
      #+Allegro
       (formatting-table (stream)
	 (loop for pathname in (all-files-in-system system include-components)
	     do (formatting-row (stream)
		  (formatting-cell (stream)
		    (present (typecase pathname
			       (logical-pathname (translate-logical-pathname pathname))
			       (t pathname)) 
			     'pathname
			     :stream stream)))))
      #-(or Allegro Genera)
      (formatting-table (stream)
	(clim-defsystem::system-map-files
	  system 
	  #'(lambda (pathname)
	      (formatting-row (stream)
		(formatting-cell (stream)
		  (present (typecase pathname
			     (logical-pathname (translate-logical-pathname pathname))
			     (t pathname)) 'pathname
			   :stream stream))))
	  :include-components include-components))
      nil)))

(define-command (com-edit-system :command-table systems :name t)
    ((system 'system
	     :default-type 'system
	     :provide-default t)
     &key
     (include-components 'boolean
			 :default t
			 :documentation "Edit files in component systems")
     #+genera
     (query '(member :yes :no :confirm)
	    :default :no :mentioned-default :yes
	    :documentation "Query about editing files")
     (silent 'boolean
	     :default t :mentioned-default t
	     :documentation "Suppress all terminal output"))
  #+Allegro (declare (ignore silent))
  (let ((system (find-system system)))
    (assert (and (not (null system)) #-Genera (not (stringp system))) ()
      "There is no system named ~A" system)
    #+Genera
     (sct:edit-system system :include-components include-components :silent silent
		      :query (case query
			       (:yes t)
			       (:no nil)
			       (:confirm :confirm)))
     #+Allegro
      (let ((modules (all-files-in-system system include-components)))
	(setq modules (multiple-choose-modules modules))
	(loop for module in modules
	    do (ed (if (typep module 'pathname) module (defsys:source-pathname module)))))
      #-(or Allegro Genera)
       (clim-defsystem::edit-system system :include-components include-components 
				    :silent silent :query query))) 
  
(defgeneric all-files-in-system (asdf-or-built-in-system &optional (include-components? nil)))

#+asdf
(defmethod all-files-in-system ((system asdf/system:system) &optional (include-components? nil))
  (let ((answer nil) (traversed nil))
    (labels ((do-one (component)
	       (unless (member component traversed)
		 (push component traversed)
		 (typecase component
		  (asdf/lisp-action:cl-source-file 
		   (pushnew (translate-logical-pathname (asdf/component:component-pathname component)) answer))
		  ((or asdf/system:system asdf/component:component)
		   (when (or (typep component 'asdf/component:component) include-components?)
		     (let ((children (asdf/component:component-children component)))
		       (loop for thing in children do (do-one thing)))))))))
      (do-one system))
    answer))

#+Allegro
(defmethod all-files-in-system ((system defsystem:default-system) &optional (include-components? nil))
  (let ((pathnames nil))
    (excl:map-system system
		     #'(lambda (module)
			 (pushnew (translate-logical-pathname (defsys:source-pathname module))
				  pathnames
				  :test #'(lambda (x y) (string= (namestring x) (namestring y)))))
		  :include-components include-components?)
    (reverse pathnames)))

(defun multiple-choose-modules (modules &optional (stream *standard-output*))
  (let ((ht (make-hash-table :test #'equal)))
    (loop for module in modules
	do (setf (gethash module ht) nil))
    (clim:accepting-values (stream :own-window t :align-prompts :left)
      (loop for module in modules
	  for pathname = (if (typep module 'pathname) (namestring module) (defsys:source-pathname module))
	  for new-value = (clim:accept 'boolean :prompt (format nil "~a" pathname) 
				       :default (gethash module ht)
				       :stream stream)
	  do (terpri stream)
	  do (setf (gethash module ht) new-value)))
      (loop for module in modules
	  when (gethash module ht)
	  collect module)))

(define-presentation-to-command-translator edit-system
    (system com-edit-system systems
     :gesture :edit)
    (object)
  (list object)) 

(define-command (com-show-system-definition :command-table systems :name t)
    ((system 'system :default-type 'system
	     :documentation "System for which to display description and files")
     &key
     #+Genera
     (detailed 'boolean :default nil :mentioned-default t
	       :prompt "describe systems to all levels"
	       :documentation "Whether to describe the plans for component systems")
     #+Genera
     (version '(or integer symbol)
	      :default :released
	      :documentation "What version of the system for which to construct plans")
     #+Genera
     (use-journals 'boolean :default nil :mentioned-default t
		   :documentation "Prefer the system's journals to the running world"))
  (let ((system #+Genera (sct:find-system-named system nil nil t)
		#+ Allegro  (if (stringp system) (find system (defsys:list-all-systems) 
						     :test #'string-equal 
						     :key #'defsys:pretty-name
						     ) 
			    system)
		#-(or Genera Allegro) system))
    #+Genera
    (sct:describe-system system :show-files t :system-op nil :version version :detailed detailed :use-journals use-journals)
    #+Allegro 
    (excl:show-system system)
    #-(or Allegro Genera)
    (clim-defsystem::describe-system system)))

;;--- Load Patches
;;--- Show Loaded Patches

;;--- Show System Components 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Counting lines in files or systems
;;;
;;;   This is dependent on Allegro only in so far as it depends on the specific
;;;    facility for calling the "wc" command in a Unix environment.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+allegro
(defun line-count-file (pathname)
  (when (stringp pathname)
    (setq pathname (pathname pathname)))
  (let* ((name (namestring (truename pathname)))
	 (lines (excl.osi:command-output (coerce `("wc" "-l" ,name) 'simple-vector) )))
    (prog1 (read-from-string (first lines)))))

#+allegro
(defun line-count-system (system-name &key show-each-file)
  (let* ((system (etypecase system-name
		   (defsys:default-system system-name)
		   ((symbol string) (excl:find-system system-name))))
	 (alist nil)
	 (count 0))
    (defsys:map-system system 
	#'(lambda (module)
	    (let* ((pathname (namestring (defsys:source-pathname module)))
		   (his-count (line-count-file pathname)))
	      (incf count his-count)
	      (when show-each-file
		(push (list pathname his-count) alist)
		))))
    (when show-each-file
      (setq alist (sort alist #'< :key #'second))
      (let ((stream *standard-output*))
	(clim:formatting-table (stream)
	  (clim:formatting-row (stream)
	    (clim:formatting-cell (stream :align-x :left) (write-string "File Name" stream))
	    (clim:formatting-cell (stream :align-x :center) (write-string "Size" stream)))
	  (loop for (pathname size) in alist
	      do (clim:formatting-row (stream)
		   (clim:formatting-cell (stream :align-x :left) (write-string pathname stream))
		   (clim:formatting-cell (stream :align-x :right) (format stream "~:d" size))))
	  (clim:formatting-row (stream)
	    (clim:formatting-cell (stream :align-x :left) (write-string "Total" stream))
	    (clim:formatting-cell (stream :align-x :right) (format stream "~:d" count))))))
    count))

#+allegro
(define-command (com-count-lines-in-system :command-table systems :name t)
    ((system 'system :default-type 'system
	     :documentation "System for which to count lines in all files")
     &key (show-each-file 'boolean :documentation "Show the count for each file?")
     )
  (let ((count (line-count-system system :show-each-file show-each-file)))
    (unless show-each-file
      (format t "~%There are ~:d lines in system ~a" count (defsys:system-name system)))))

#+allegro
(define-command (com-table-system-line-count :command-table systems :name t)
    ((systems '((sequence system)) :documentation "A sequence of system names"))
  (let ((alist nil)
	(total-count 0)
	(stream *standard-output*))
    (loop for system in systems
	for his-count = (line-count-system system)
	for his-name = (string (defsys:system-name system))
	do (push (list his-name his-count) alist)
	   (incf total-count his-count))
    (setq alist (sort alist #'< :key #'second))
    (terpri stream)
    (clim:formatting-table (stream)
      (clim:formatting-row (stream)
	(clim:formatting-cell (stream) (write-string "System" stream))
	(clim:formatting-cell (stream) (write-string "Line Count" stream)))
      (loop for (system-name system-count) in alist
	  do (clim:formatting-row (stream)
	       (clim:formatting-cell (stream) (write-string system-name stream))
	       (clim:formatting-cell (stream :align-x :right) (format stream "~:d" system-count))))
      (clim:formatting-row (stream)
	(clim:formatting-cell (stream) (write-string "Total" stream))
	(clim:formatting-cell (stream :align-x :right) (format stream "~:d" total-count))))
    (terpri stream)))
	


