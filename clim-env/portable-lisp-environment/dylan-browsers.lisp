;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-ENV; Base: 10; Lowercase: Yes -*-

"Copyright (c) 1994 Harlequin, Inc.  All rights reserved."

(in-package :clim-env)


;;; Extensions to the inspector

(defmethod display-object ((object dylan::<stretchy-vector>) stream)
  (formatting-table (stream :x-spacing '(2 :character))
    (let ((index -1))
      (map nil
	   #'(lambda (elt)
	       (formatting-row (stream)
		 (formatting-cell (stream :align-x :right)
		   (format stream "~D:" (incf index)))
		 (formatting-cell (stream)		;--- modifier?
		   (present elt 'location :stream stream))))
	   (slot-value object 'dylan+dylan/internal::stretchy-vector-data)))))

(defmethod display-object ((object dylan::<deque>) stream)
  (formatting-table (stream :x-spacing '(2 :character))
    (let ((index -1)
          (start (slot-value object 'dylan+dylan/internal::deque-start-node)))
      (do ((node start (slot-value node 'dylan+dylan/internal::node-next)))
	  ((null node))
        (let ((value (slot-value node 'dylan+dylan/internal::node-value)))
	  (formatting-row (stream)
	    (formatting-cell (stream :align-x :right)
	      (format stream "~D:" (incf index)))
	    (formatting-cell (stream)		;--- modifier?
	      (present value 'location :stream stream))))))))


;;; Dylan library browser

(define-application-frame library-browser (browser-frame)
    ((library :initarg :library :initform nil
	      :accessor library-browser-library))
  (:default-initargs :graph-type :graphical
		     :browser-type :library
		     :browser-subtype :used-by		;or :USES...
		     :browser-options nil
		     :presentation-type 'dylan-library-descriptor
		     :node-maker #'make-library-call-node
		     :root-node-maker #'make-library-browser-root
                     :browser-depth 100)
  (:command-table (library-browser :inherit-from (browser-activity
						  editing
                                                  files
						  inspection
                                                  libraries
						  browser-frame
						  selected-objects
						  accept-values-pane)
				   :menu (("Activity" :menu browser-activity)
					  ("Selections" :menu selected-objects))))
  (:top-level (library-browser-top-level))
  (:pointer-documentation t)
  (:panes
    (library :accept-values
	     :height :compute
	     :display-function
	       '(accept-values-pane-displayer
		  :displayer read-library-browser-library)
	     :text-style '(:fix :roman :large)
	     :scroll-bars nil)
    (direction (make-pane 'option-pane
		 :label "Direction"
		 :items '(("Uses" . :uses)
			  ("Used by" . :used-by))
		 :name-key #'car :value-key #'cdr
		 :value :used-by
		 :value-changed-callback 'library-browser-direction-changed))
    (graph :application
	   :display-function 'display-graph-pane
	   :display-after-commands nil
	   :incremental-redisplay t
	   :scroll-bars :both
	   :end-of-page-action :allow
	   :end-of-line-action :allow)
    (files :application
	   :display-after-commands nil
	   :scroll-bars :both
	   :end-of-page-action :allow 
	   :end-of-line-action :allow)
    (modules :application
	     :display-after-commands nil
	     :scroll-bars :both
	     :end-of-page-action :allow 
	     :end-of-line-action :allow))
  (:layouts
    (main
      (vertically () 
	(horizontally ()
	  (:fill #+Genera (outlining () (spacing () library))
		 #-Genera library)
	  (1/5 direction))
	(:fill graph)
	(1/4 (horizontally ()
	       (1/2 files)
	       (1/2 modules)))))))

(defmethod library-browser-top-level ((frame library-browser))
  (enable-frame frame)
  (load-dylan-library-descriptors)
  (configure-for-browser-type frame (browser-subtype frame))
  (default-frame-top-level frame))

(defmethod browser-display-pane ((frame library-browser))
  (get-frame-pane frame 'graph))

(defmethod frame-redisplay :after ((browser library-browser))
  (redisplay-frame-pane browser 'files)
  (redisplay-frame-pane browser 'modules))

(defmethod read-library-browser-library ((frame library-browser) stream)
  (with-slots (library) frame
    (when (null library)
      (setq library :all))
    (multiple-value-bind (new-lib type changed-p)
	(accept '(token-or-type (:all) (sequence dylan-library-descriptor))
		:default library :prompt nil
		:stream stream)
      (declare (ignore type))
      (when changed-p
	(setq library new-lib)
        (generate-browser-display
          frame (library-browser-libraries frame) (browser-display-pane frame))))))

(defmethod library-browser-libraries ((frame library-browser))
  (with-slots (library) frame
    (if (eql library ':all)
      (let ((libs nil))
	(maphash #'(lambda (name desc)
		     (when (if (eql (browser-subtype frame) :uses)
			       (null (library-used-by desc))
			       (null (library-uses desc)))
		       (push name libs)))
		 *dylan-libraries*)
	(nreverse libs))
      library)))

(defun library-browser-direction-changed (pane value)
  (let ((frame (pane-frame pane)))
    (setf (browser-subtype frame) value)
    (configure-for-browser-type frame (browser-subtype frame))))

(defmethod note-browser-root-changed ((browser library-browser) objects)
  (setf (library-browser-library browser) objects)
  (redisplay-frame-pane browser 'library :force-p t))

(defmethod generate-browser-display :before ((browser library-browser) objects stream)
  (declare (ignore objects stream))
  (window-clear (get-frame-pane browser 'files))
  (window-clear (get-frame-pane browser 'modules)))


;;; Library descriptors

(defvar *dylan-libraries* (make-hash-table))

(defclass dylan-library-descriptor ()
    ((name :initarg :name :reader library-name)
     (lid-file :initarg :lid-file :reader library-lid-file)
     (files :initarg :files :reader library-files)
     (uses :initarg :uses :reader library-uses)
     (used-by :initform nil :accessor library-used-by)
     (modules :initarg :modules :reader library-modules)))

(defmethod print-object ((lib dylan-library-descriptor) stream)
  (print-unreadable-object (lib stream :type t :identity t)
    (format stream "~S" (library-name lib))))

(define-presentation-type dylan-library-descriptor ())

(define-presentation-method accept
    ((type dylan-library-descriptor) stream (view textual-view) &key)
  (values
    (let ((libraries (let ((libraries nil))
		       (dolist (registry dylan::*library-registries*)
			 (when (probe-file registry)
			   (dolist (file (directory registry))
			     (push (pathname-name file) libraries))))
                       (sort libraries #'string-lessp))))
      (completing-from-suggestions (stream :partial-completers '(#\-))
        (dolist (library libraries)
	  (suggest library (find-library-descriptor library)))))))

(define-presentation-method present
    (library (type dylan-library-descriptor) stream (view textual-view) &key)
  (princ (library-name library) stream))

(defun find-library-descriptor (name)
  (when (typep name 'dylan-library-descriptor)
    (return-from find-library-descriptor name))
  (let ((name (if (symbolp name) (symbol-name name) name)))
    ;; Work hard to find a match -- sigh
    (or (gethash (intern name :dylan) *dylan-libraries*)
        (gethash (intern (string-downcase name) :dylan) *dylan-libraries*)
        (gethash (intern (string-upcase name) :dylan) *dylan-libraries*))))

(defun load-dylan-library-descriptors ()
  (clrhash *dylan-libraries*)
  (flet ((load-one (file &optional lid-file)
           (let ((lib-file nil)
                 (files nil)
                 (uses nil))
             (ignore-errors
	       (unless lid-file
                 (with-open-file (s file :direction :input :if-does-not-exist nil)
                   (when s
                     (setq lid-file (pathname (read-line s))))))
               ;; The LID file contains a list of all the files
               (when lid-file
	         (with-open-file (s lid-file  :direction :input :if-does-not-exist nil)
		   (when s
		     (loop
		      (let ((line (read-line s nil nil)))
		        (when (or (null line) (zerop (length line)))
			  (return))
		        (if lib-file
			  (push (translate-logical-pathname
				 (make-pathname
				  :defaults lid-file
				  :name (pathname-name (pathname line))
				  :type "dylan")) files)
			  (when (string-equal line "Files:" :end1 6)
			    ;;--- Assume the 'define library' form is in the very first file
			    (setq lib-file (translate-logical-pathname
					    (make-pathname
					     :defaults lid-file
					     :name (pathname-name (pathname (subseq line 7)))
					     :type "dylan")))
			    (setq files (list lib-file)))))))))
               ;; Figure out the library's use list
               (when lib-file
	         (with-open-file (s lib-file :direction :input :if-does-not-exist nil)
                   (when s
		     (let ((mode ':infix)
                           (dylan-package (find-package :dylan)))
		       (loop
		        (let ((line (read-line s nil nil)))
			  (when (or (null line) (zerop (length line)))
			    (return))
			  (when (string-equal line "Language:" :end1 9)
			    (let ((language (string-left-trim '(#\Space #\Tab) (subseq line 10))))
			      (when (string-equal language "Prefix-Dylan")
			        (setq mode ':prefix))))))
		       (case mode
		         (:infix
		          (let ((found-start nil))
			    (loop
			     (let ((line (read-line s nil nil)))
			       (if (null line)
				 (return)
				 (setq line (string-left-trim '(#\Space #\Tab) line)))
			       (if found-start
				 (when (string-equal line "use" :end1 3)
				   (push (let ((*package* dylan-package))
					   (read-from-string line nil nil :start 4))
					 uses))
				 (cond ((string-equal line "define library" :end1 14)
					(setq found-start t))
				       ((string-equal line "end" :end1 3)
					(return))))))))
		         (:prefix
		          (let ((form (let ((*package* dylan-package))
                                        (read s nil nil))))
			    (when (and (listp form)
				       (eql (car form) 'dylan::define-library))
			      (dolist (clause (cddr form))
			        (when (eql (car clause) 'dylan::use)
			          (push (cadr clause) uses)))))))))))
               (let* ((lib-name (intern (string-upcase 
                                         (if (stringp file) file (pathname-name file)))
                                        :dylan))
                      (lib-desc (make-instance 'dylan-library-descriptor
                                               :name lib-name
			                       :lid-file lid-file
			                       :files (nreverse files)
			                       :uses (nreverse uses)
			                       :modules nil)))
                 (setf (gethash lib-name *dylan-libraries*) lib-desc))))))
    (dolist (registry dylan::*library-registries*)
      (when (probe-file registry)
        (dolist (file (directory registry))
          (load-one file))))
    ;; Handle some that are mysteriously omitted.  Barf.
    (dolist (file '(("dylan" "~dylan/lib/dylan/dylan.lid")
                    ("parser" "~dylan/lib/parser-run-time/parser.lid")
                    ("variable-search" "~dylan/lib/doss/variable-search.lid")))
      (load-one (first file) (second file))))
  ;; Generate stubs for libraries that aren't in the registry
  (let ((unknowns nil))
    (maphash #'(lambda (name desc)
                 (declare (ignore name))
                 (dolist (use (library-uses desc))
                   (unless (find-library-descriptor use)
                     (pushnew use unknowns))))
             *dylan-libraries*)
    (dolist (unknown unknowns)
      (let* ((lib-name (intern (string-upcase (symbol-name unknown)) :dylan))
	     (lib-desc (make-instance 'dylan-library-descriptor
                         :name lib-name
			 :lid-file nil
			 :files nil
			 :uses nil
			 :modules nil)))
	(setf (gethash lib-name *dylan-libraries*) lib-desc))))
  ;; Fill in the used-by slots, and generate stubs for libraries
  ;; that aren't in the registry
  (maphash #'(lambda (name desc)
               (dolist (use (library-uses desc))
                 (let ((use-desc (find-library-descriptor use)))
                   (pushnew name (library-used-by use-desc)))))
           *dylan-libraries*))


;;; Library browser call nodes, etc.

(defclass library-call-node (call-node) ())

(declaim (inline make-library-call-node))
(defun make-library-call-node (object)
  (make-instance 'library-call-node :object (find-library-descriptor object)))

(defmethod node-object-name ((node library-call-node))
  (library-name (node-object node)))

(defmethod display-node ((node library-call-node) stream)
  (with-output-as-presentation (stream node 'library-call-node)
    (present (node-object node) 'dylan-library-descriptor :stream stream)))

(defun make-library-browser-root (object)
  (let ((lib-desc (find-library-descriptor object)))
    (when lib-desc
      (make-library-call-node lib-desc))))


(defmethod node-generate-inferior-objects ((node library-call-node) (type (eql ':uses)))
  (loop for lib in (library-uses (node-object node))
        as lib-desc = (find-library-descriptor lib)
        when lib-desc
          collect lib-desc))

(defmethod node-any-inferior-objects-p ((node library-call-node) (type (eql ':uses)))
  (not (null (library-uses (node-object node)))))

(defmethod node-generate-inferior-objects ((node library-call-node) (type (eql ':used-by)))
  (loop for lib in (library-used-by (node-object node))
        as lib-desc = (find-library-descriptor lib)
        when lib-desc
          collect lib-desc))

(defmethod node-any-inferior-objects-p ((node library-call-node) (type (eql ':used-by)))
  (not (null (library-used-by (node-object node)))))


(defmethod node-generate-superior-objects ((node library-call-node) (type (eql ':uses)))
  (node-generate-inferior-objects node ':used-by))

(defmethod node-any-superior-objects-p ((node library-call-node) (type (eql ':uses)))
  (node-any-inferior-objects-p node ':used-by))

(defmethod node-generate-superior-objects ((node library-call-node) (type (eql ':used-by)))
  (node-generate-inferior-objects node ':uses))

(defmethod node-any-superior-objects-p ((node library-call-node) (type (eql ':used-by)))
  (node-any-inferior-objects-p node ':uses))


;;; Library browser commands

(defmethod configure-for-browser-type ((browser library-browser) subtype)
  (ecase subtype
    (:uses
      (setf (browser-grapher-args browser)
            '(:arc-drawer draw-arrow-arc :arc-drawing-options (:from-head t :to-head nil))))
    (:used-by
      (setf (browser-grapher-args browser)
	    '(:arc-drawer draw-arrow-arc))))
  (setf (browser-subtype browser) subtype)
  ;; Now regenerate the display
  (generate-browser-display
    browser (library-browser-libraries browser) (browser-display-pane browser)))


(define-library-browser-command (com-show-library-uses :name "Show Library Uses")
    ((library 'dylan-library-descriptor :gesture :select))
  (with-application-frame (frame)
    ;; Select all the libraries used by this library
    (deselect-all-objects frame)
    (let ((path nil))
      (labels ((add-library (lib)
	         (let* ((desc (find-library-descriptor lib))
			(node (find-node-for-object frame desc)))
		   ;; Be robust against possible circular library definitions
                   (unless (member desc path)
		     (push desc path)
		     (when node
		       (add-object-to-selection frame node))
		     (dolist (use (library-uses desc))
		       (add-library use))))))
	(declare (dynamic-extent #'add-library))
	(add-library library)))))

(define-library-browser-command (com-show-library-details :name "Show Library Details")
    ((library 'dylan-library-descriptor :gesture :describe))
  (with-application-frame (frame)
    (display-files-pane frame library)
    (display-modules-pane frame library)))

(defmethod display-files-pane ((browser library-browser) library)
  (let ((stream (get-frame-pane browser 'files)))
    (window-clear stream)
    (silica:inhibit-updating-scroll-bars
      (format-items (library-files library)
                    :stream stream
                    :presentation-type 'pathname))))

(defmethod display-modules-pane ((browser library-browser) library)
  (let ((stream (get-frame-pane browser 'modules)))
    (window-clear stream)
    (silica:inhibit-updating-scroll-bars
      ;;--- How do we figure out what modules are there?
      )))

(define-presentation-to-command-translator compile-library-from-descriptor
    (dylan-library-descriptor com-compile-library library-browser
     :gesture nil)
    (object)
  (list (library-name object)))

(define-presentation-to-command-translator load-library-from-descriptor
    (dylan-library-descriptor com-load-library library-browser
     :gesture nil)
    (object)
  (list (library-name object)))

(add-command-to-command-table 'com-compile-file 'library-browser)
(add-command-to-command-table 'com-load-file 'library-browser)

(add-presentation-translator-to-command-table
  'library-browser (find-presentation-translator 'compile-file 'systems))
(add-presentation-translator-to-command-table
  'library-browser (find-presentation-translator 'load-file 'systems))


;;; Dylan module browser

(define-application-frame module-browser (browser-frame)
    ((module :initarg :module :initform nil
	     :accessor module-browser-module))
  (:default-initargs :graph-type :graphical
		     :browser-type :module
		     :browser-subtype :uses		;or :USED-BY...
		     :browser-options nil
		     :presentation-type 'dylan-module
		     :node-maker #'make-module-call-node
		     :root-node-maker #'make-module-browser-root
                     :browser-depth 2)
  (:command-table (module-browser :inherit-from (browser-activity
						 editing
						 inspection
						 browser-frame
						 selected-objects
						 accept-values-pane)
				   :menu (("Activity" :menu browser-activity)
					  ("Selections" :menu selected-objects))))
  (:top-level (module-browser-top-level))
  (:pointer-documentation t)
  (:panes
    (module :accept-values
	    :height :compute
	    :display-function
	      '(accept-values-pane-displayer
		 :displayer read-module-browser-module)
	     :text-style '(:fix :roman :large)
	     :scroll-bars nil)
    (direction (make-pane 'option-pane
		 :label "Direction"
		 :items '(("Uses" . :uses)
			  ("Used by" . :used-by))
		 :name-key #'car :value-key #'cdr
		 :value :uses
		 :value-changed-callback 'module-browser-direction-changed))
    (graph :application
	   :display-function 'display-graph-pane
	   :display-after-commands nil
	   :incremental-redisplay t
	   :scroll-bars :both
	   :end-of-page-action :allow
	   :end-of-line-action :allow)
    (exports :application
	     :display-after-commands nil
	     :scroll-bars :both
	     :end-of-page-action :allow 
	     :end-of-line-action :allow)
    (symbols :application
	     :display-after-commands nil
	     :scroll-bars :both
	     :end-of-page-action :allow 
	     :end-of-line-action :allow))
  (:layouts
    (main
      (vertically () 
	(horizontally ()
	  (:fill #+Genera (outlining () (spacing () module))
		 #-Genera module)
	  (1/5 direction))
	(:fill graph)
	(1/4 (horizontally ()
	       (1/2 exports)
	       (1/2 symbols)))))))

(defmethod module-browser-top-level ((frame module-browser))
  (enable-frame frame)
  (configure-for-browser-type frame (browser-subtype frame))
  (default-frame-top-level frame))

(defmethod browser-display-pane ((frame module-browser))
  (get-frame-pane frame 'graph))

(defmethod frame-redisplay :after ((browser module-browser))
  (redisplay-frame-pane browser 'exports)
  (redisplay-frame-pane browser 'symbols))

(defmethod read-module-browser-module ((frame module-browser) stream)
  (with-slots (module) frame
    (when (null module)
      (setq module (dylan::find-translator-module 'dylan::dylan)))
    (multiple-value-bind (new-module type changed-p)
	(accept 'dylan-module
		:default module :prompt nil
		:stream stream)
      (declare (ignore type))
      (when changed-p
	(setq module new-module)
        (generate-browser-display frame module (browser-display-pane frame))))))

(defun module-browser-direction-changed (pane value)
  (let ((frame (pane-frame pane)))
    (setf (browser-subtype frame) value)
    (configure-for-browser-type frame (browser-subtype frame))))

(defmethod note-browser-root-changed ((browser module-browser) objects)
  (setf (module-browser-module browser) objects)
  (redisplay-frame-pane browser 'module :force-p t))

(defmethod generate-browser-display :before ((browser module-browser) objects stream)
  (declare (ignore objects stream))
  (window-clear (get-frame-pane browser 'exports))
  (window-clear (get-frame-pane browser 'symbols)))


;;; Library browser call nodes, etc.

(defclass module-call-node (call-node) ())

(declaim (inline make-module-call-node))
(defun make-module-call-node (object)
  (if (symbolp object)
    (make-instance 'module-call-node :object (dylan::find-translator-module object))
    (make-instance 'module-call-node :object object)))

(defmethod node-object-name ((node module-call-node))
  (dylan::module-name (node-object node)))

(defmethod display-node ((node module-call-node) stream)
  (with-output-as-presentation (stream node 'module-call-node)
    (present (node-object node) 'dylan-module :stream stream)))

(defun make-module-browser-root (object)
  (let ((module (if (symbolp object)
		  (dylan::find-translator-module object)
		  object)))
    (make-module-call-node module)))


(defmethod node-generate-inferior-objects ((node module-call-node) (type (eql ':uses)))
  (module-uses (node-object node)))

(defmethod node-any-inferior-objects-p ((node module-call-node) (type (eql ':uses)))
  (not (null (module-uses (node-object node)))))

(defmethod node-generate-inferior-objects ((node module-call-node) (type (eql ':used-by)))
  (module-used-by (node-object node)))

(defmethod node-any-inferior-objects-p ((node module-call-node) (type (eql ':used-by)))
  (not (null (module-used-by (node-object node)))))


(defmethod node-generate-superior-objects ((node module-call-node) (type (eql ':uses)))
  (node-generate-inferior-objects node ':used-by))

(defmethod node-any-superior-objects-p ((node module-call-node) (type (eql ':uses)))
  (node-any-inferior-objects-p node ':used-by))

(defmethod node-generate-superior-objects ((node module-call-node) (type (eql ':used-by)))
  (node-generate-inferior-objects node ':uses))

(defmethod node-any-superior-objects-p ((node module-call-node) (type (eql ':used-by)))
  (node-any-inferior-objects-p node ':uses))


;;; Module browser commands

(defmethod configure-for-browser-type ((browser module-browser) subtype)
  (ecase subtype
    (:uses
      (setf (browser-grapher-args browser)
            '(:arc-drawer draw-arrow-arc :arc-drawing-options (:from-head t :to-head nil))))
    (:used-by
      (setf (browser-grapher-args browser)
	    '(:arc-drawer draw-arrow-arc))))
  (setf (browser-subtype browser) subtype)
  ;; Now regenerate the display
  (generate-browser-display
    browser (module-browser-module browser) (browser-display-pane browser)))


(define-module-browser-command (com-show-module-details :name "Show Module Details")
    ((module 'dylan-module :gesture :describe))
  (with-application-frame (frame)
    (display-exports-pane frame module)
    (display-symbols-pane frame module)))

(defmethod display-exports-pane ((browser module-browser) module)
  (let ((stream (get-frame-pane browser 'exports)))
    (window-clear stream)
    (silica:inhibit-updating-scroll-bars
      (map nil #'(lambda (symbol) (format stream "~S~%" symbol))
           (module-exported-symbols module)))))

(defmethod display-symbols-pane ((browser module-browser) module)
  (let ((stream (get-frame-pane browser 'symbols)))
    (window-clear stream)
    (silica:inhibit-updating-scroll-bars
      (map nil #'(lambda (symbol) (format stream "~S~%" symbol))
           (module-internal-symbols module)))))


;;; The following several functions are hacks that are meant to extract
;;; information from module objects, which are Dylan objects and thus
;;; have no Lisp accessors

(defun module-uses (module)
  )

(defun module-used-by (module)
  )

(defun module-exported-symbols (module)
  )

(defun module-internal-symbols (module)
  )
  
(defun dylan-slot-value (instance slot-name)
  (let ((slots (clos:class-slots (class-of instance))))
    (dolist (slot slots)
      (when (string-equal (symbol-name slot-name)
                          (symbol-name (clos:slot-definition-name slot))
                          :end2 (length (symbol-name slot-name)))
        (return (slot-value instance (clos:slot-definition-name slot)))))))

