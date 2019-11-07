;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

"Copyright (c) 2001-2003 Franz, Inc.  All rights reserved."


;;;; --------------------------------------------------------------------------------
;;;; Fix up CLIM's implementation of Allegro process state stuff

(in-package :clim-utils)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (excl:package-definition-lock (find-package :clim-sys)) nil))

(excl:without-redefinition-warnings

(defun clim-sys:process-state (process)
  (cond ((not (mp::process-active-p process)) "Inactive")
	((mp::process-runnable-p process) "Runnable")
	#+os-threads
	((eq (mp::process-wait-function process)
	     #'mp:gate-open-p) "Gated")
	#+os-threads
	((or (eq (mp::process-wait-function process)
		 #'excl::read-no-hang-p)) "I/O Gated")
	((mp::process-wait-function process) "Waiting")
	(t "???")))

(defun clim-sys:process-whostate (process)
  (mp:process-whostate process))

)	;without-redefinition-warnings

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (excl:package-definition-lock (find-package :clim-sys)) t))


;;;; --------------------------------------------------------------------------------
;;;; Franz seems to have left these out of its implementation.

(in-package :clim-internals)

;; Make the CLIM completer more friendly
(define-gesture-name :complete :keyboard (:i :control) :unique nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(key-press-event-p command-menu-enabled) :clim-internals)
  (import '(clim-internals::command-menu-enabled clim-internals::key-press-event-p) :clim)
  (export '(key-press-event-p command-menu-enabled) :clim))

(declaim (inline key-press-event-p))
(defun key-press-event-p (x)
  (or (characterp x)
      (typep x 'key-press-event)))

(defmethod command-menu-enabled (command-table (frame standard-application-frame))
  (with-slots (disabled-commands) frame
    (or *assume-all-commands-enabled*
	(let ((comtab (if (command-table-p command-table)
			  command-table
			  (find-command-table command-table :errorp nil))))
	  (not (member comtab disabled-commands))))))

(defmethod (setf command-menu-enabled)
	   (enabled command-table (frame standard-application-frame))
  (with-slots (disabled-commands) frame
    (let* ((comtab (if (command-table-p command-table)
		       command-table
		       (find-command-table command-table :errorp nil)))
	   (name (command-table-name comtab)))
      ;;--- NOTE-COMMAND-ENABLED/DISABLED doesn't manage to increment the
      ;;--- command table tick when we are using the non-gadget frame manager
      (cond (enabled
	     (setf disabled-commands (delete comtab disabled-commands))
	     (note-command-enabled (frame-manager frame) frame name))
	    (t
	     (pushnew comtab disabled-commands)
	     (note-command-disabled (frame-manager frame) frame name)))
      enabled)))


;;;; --------------------------------------------------------------------------------
;;;; Franz seems to have not gotten this far.

(in-package :clim-internals)

;; The :radio-box and :check-box types are a bit odd:
;;  (add-menu-item-to-command-table
;;    <command-table> <symbol> :radio-box	;or :check-box
;;    '(:items (("True" t) ("False" nil))
;;      :callback (lambda (x) (format *trace-output* "Value changed to ~S" x))))
#||
(defun add-menu-item-to-command-table (command-table string type value
				       &key documentation (after ':end) 
					    keystroke mnemonic
					    text-style (errorp t))
  (check-type type (member :command :function :menu :divider
                           ;; Some ports allow radio/check-boxes in menus
                           :radio-box :check-box))
  (if (member type '(:divider :radio-box :check-box))
      (check-type string (or string symbol null))
      (check-type string (or string null)))
  (when keystroke
    (assert (keyboard-gesture-spec-p keystroke) (keystroke)
	    "~S is not a keyboard gesture spec" keystroke)
    (multiple-value-bind (keysym modifiers)
	(decode-gesture-spec keystroke)
      #+Genera (when (and (characterp keysym)
			  (not (zerop (si:char-bits keysym))))
		 (error "The keystroke ~S is no longer legal" keysym))
      (setq keystroke (cons keysym modifiers))))
  (check-type documentation (or string null))
  (setq command-table (find-command-table command-table))
  (let ((old-item (and string (find-menu-item string command-table :errorp nil))))
    (when old-item
      (when errorp
	(cerror "Remove the menu item and proceed"
		'command-already-present
		:format-string "Menu item ~S already present in ~S"
		:format-args (list string command-table)))
      (remove-menu-item-from-command-table command-table string)))
  (when (eq type ':command)
    ;; Canonicalize command name to a command with the right number of
    ;; unsupplied argument markers.
    (unless (listp value)
      (setq value (list value)))
    (let ((n-required (get (first value) 'n-required))
	  (n-supplied (1- (length value))))
      (when (and n-required
		 (not (zerop n-required))
		 (< n-supplied n-required))
	(setq value (append value 
			    (make-list (- n-required n-supplied)
				       :initial-element *unsupplied-argument-marker*))))))
  (with-slots (menu menu-tick commands keystrokes) command-table
    (incf menu-tick)
    (setq keystrokes nil)
    (let* ((item `(,type ,value 
		   ,@(and documentation `(:documentation ,documentation))
		   ,@(and text-style `(:text-style ,text-style))
		   ,@(and mnemonic `(:mnemonic ,mnemonic))))
	   ;; Entries are of the form (MENU-NAME KEYSTROKE MENU-ITEM)
	   (entry (list string keystroke item)))
      (when (null menu)
	(setq menu (make-array *command-table-size*
			       :fill-pointer 0 :adjustable t)))
      (case after
	((:start)
	 (vector-push-extend nil menu)		;extend the vector by 1
	 (replace menu menu :start1 1 :start2 0)
	 (setf (aref menu 0) entry))
	((:end nil)
	 (vector-push-extend entry menu))
	((:sort)
	 (vector-push-extend entry menu)
	 (flet ((menu-name-lessp (x y)
		  (cond ((null x) t)
			((null y) nil)
			(t (string-lessp x y)))))
	   (setq menu (sort menu #'menu-name-lessp :key #'first))))
	(otherwise
	  (if (stringp after)
	      (let ((index (position after menu
				     :test #'menu-name-equal :key #'first)))
		(if index
		    (cond ((= index (1- (fill-pointer menu)))
			   ;; Just add at end
			   (vector-push-extend entry menu))
			  (t (vector-push-extend nil menu)
			     (replace menu menu :start1 (+ index 2) :start2 (+ index 1))
			     (setf (aref menu (+ index 1)) entry)))
		  (error 'command-not-present
			 :format-string "Menu item ~S not present in ~S for :AFTER"
			 :format-args (list after command-table))))
	    (error "The value for :AFTER is not a string, :START, :END, or :SORT"))))
      ;; Now that the command is accessible via a menu (or keystroke),
      ;; make sure that we've really imported it
      (when (eq type ':command)
	(let ((old-name (gethash (first value) commands)))
	  (setf (gethash (first value) commands) (or old-name t))))
      entry)))
||#


;;;; --------------------------------------------------------------------------------
;;;; Oops, Franz missed that this is supposed to be a generic function.

(in-package :clim-internals)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (excl:package-definition-lock (find-package :clim-internals)) nil))

(excl:without-redefinition-warnings

(fmakunbound 'frame-document-highlighted-presentation-1)
(defmethod frame-document-highlighted-presentation-1
    ((frame standard-application-frame) presentation input-context window x y stream)
  (let ((modifier-state (port-modifier-state (port window))))
    (declare (type fixnum modifier-state))
    (multiple-value-bind (left   left-presentation   left-context
			  middle middle-presentation middle-context
			  right  right-presentation  right-context)
	(find-applicable-translators-for-documentation presentation input-context
						       frame window x y modifier-state)
      (let* ((*print-length* 3)
	     (*print-level* 2)
	     (*print-circle* nil)
	     (*print-array* nil)
	     (*print-readably* nil)
	     (*print-pretty* nil))
	(flet ((document-translator (translator presentation context-type
				     button-name separator)
		 ;; Assumes 5 modifier keys and the reverse ordering of *MODIFIER-KEYS*
		 (let ((bit #o20)
		       (shift-name '("h-" "s-" "m-" "c-" "sh-")))
		   (declare (type fixnum bit))
		   (repeat 5			;length of shift-name
		     (unless (zerop (logand bit modifier-state))
		       (write-string (car shift-name) stream))
		     (pop shift-name)
		     (setq bit (the fixnum (ash bit -1)))))
		 (write-string button-name stream)
		 (document-presentation-translator translator presentation context-type
						   frame nil window x y
						   :stream stream
						   :documentation-type :pointer)
		 (write-string separator stream)))
	  (declare (dynamic-extent #'document-translator))
	  ;;--- The button names should be hard-wired in.  Consider 1-button
	  ;;--- Macs and 2-button PCs...
	  (when left
	    (let ((button-name (cond ((and (eq left middle)
					   (eq left right))
				      (setq middle nil
					    right nil)
				      "L,M,R: ")
				     ((eq left middle) 
				      (setq middle nil)
				      "L,M: ")
				     (t "L: "))))
	      (document-translator left left-presentation left-context
				   button-name (if (or middle right) "; " "."))))
	  (when middle
	    (let ((button-name (cond ((eq middle right)
				      (setq right nil)
				      "M,R: ")
				     (t "M: "))))
	      (document-translator middle middle-presentation middle-context
				   button-name (if right "; " "."))))
	  (when right
	    (document-translator right right-presentation right-context
				 "R: " "."))
	  ;; Return non-NIL if any pointer documentation was produced
	  (or left middle right))))))

)	;without-redefinition-warnings

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (excl:package-definition-lock (find-package :clim-internals)) t))


;;;; --------------------------------------------------------------------------------
;;;; Fix some pathname merging problems with empty components.

(in-package :clim-internals)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (excl:package-definition-lock (find-package :clim-internals)) nil))

(excl:without-redefinition-warnings

(define-presentation-method presentation-default-preprocessor
                            (default (type pathname) &key default-type)
  (when (null default)
    (setq default *default-pathname-defaults*))
  (with-presentation-type-options (pathname default-type)
    (when merge-default
      (setq default
            (make-pathname :host (pathname-host default)
                           :device (pathname-device default)
                           :directory (pathname-directory default)
                           :name (or (pathname-name default) #+Allegro :wild)
                           :type (or default-type (pathname-type default) #+Allegro :wild)
                           :version default-version))))
  (values default default-type))

(define-presentation-method accept ((type pathname) stream (view textual-view) &key default)
  (when (null default)
    (setq default *default-pathname-defaults*))
  (when (and merge-default default-type)
    (setq default (make-pathname :host (pathname-host default)
                                 :device (pathname-device default)
                                 :directory (pathname-directory default)
                                 :name (or (pathname-name default) #+Allegro :wild)
                                 :type (or default-type #+Allegro :wild)
                                 :version default-version)))
  (let ((buffer-start (stream-scan-pointer stream)))
    (multiple-value-bind (pathname success string)
        (complete-input stream #'(lambda (string action)
                                   (pathname-complete string action default))
                        :allow-any-input t :help-displays-possibilities nil)
      (declare (ignore success))
      (handler-bind ((error
                       #'(lambda (anerror)
                           (declare (ignore anerror))
                           (simple-parse-error "Error parsing pathname string ~A" string))))
        (unless pathname
          (setq pathname (parse-namestring string nil default)))
        (when merge-default
          (setq pathname (merge-pathnames pathname default default-version))))
      (unless (stream-rescanning-p stream)
        (presentation-replace-input stream pathname type view
                                    :buffer-start buffer-start))
      (values pathname type))))

)	;without-redefinition-warnings

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (excl:package-definition-lock (find-package :clim-internals)) t))


;;;; --------------------------------------------------------------------------------
;;;; Integrate 'describe' with CLIM presentations

(in-package :excl)

(excl:without-redefinition-warnings

(defun default-structure-describe (structure stream)
  (with-stream-class (stream)
    (let* ((real-stream (if (xp-stream-p stream) (sm output-handle stream)))
	   (clim-stream-p (typep real-stream 'clim:clim-stream-pane))
	   (type (structure-ref structure 0))
	   desc)
      (when (consp type)
	(setq type (caar type)))
      (setq desc  (get type '%structure-definition))
      (let ((*print-structure* nil))
	(format stream
		"~@<~w is a structure of type ~w.  ~
  ~:[It has no description~;It has these slots:~]~:@>~%"
		structure type desc))
      (when desc
	;; Assume *new-structure-style*
	(do ((slots (nreverse (excl::object-instance-slot-names structure)) (cdr slots))
	     (i 1 (1+ i)))
	    ((null slots))
	  (declare (optimize speed) (fixnum i))
	  (if clim-stream-p
	    (clim:with-output-as-presentation
		(real-stream (structure-ref structure i) 'clim:expression
			     :allow-sensitive-inferiors t)
	      (format stream
		    " ~@<~3I~A ~20T~_~W~:>~%"
		    (car slots)
		    (structure-ref structure i)))
	    (format stream
		    " ~@<~3I~A ~20T~_~W~:>~%"
		    (car slots)
		    (structure-ref structure i))))))))

(defmethod describe-object ((h hash-table) stream)
  (with-stream-class (stream)
    (let* ((real-stream (if (xp-stream-p stream) (sm output-handle stream)))
	   (clim-stream-p (typep real-stream 'clim:clim-stream-pane)))
      (when clim-stream-p
	(let ((stream real-stream))
	  (format stream "~@<~2i~s~:@>~%" h )
	  (terpri stream)
	  (clim:formatting-table (stream)
	    (clim:formatting-row (stream)
	      (clim:formatting-cell (stream :min-width '(30 :character))
		(clim:surrounding-output-with-border (stream :shape :underline)
		  (princ "KEY" stream)))
	      (clim:formatting-cell (stream)
		(clim:surrounding-output-with-border (stream :shape :underline)
		  (princ "VALUE" stream))))
	    (maphash #'(lambda (key value)
			 (clim:formatting-row (stream)
			   (clim:formatting-cell (stream)
			     (clim:present key 'clim:expression :stream stream))
			   (clim:formatting-cell (stream)
			     (clim:present value 'clim:expression :stream stream))))
		     h)))))
	(values)))

(defmethod describe-object ((object standard-object) stream)
  (with-stream-class (stream)
    (let* ((real-stream (if (xp-stream-p stream) (sm output-handle stream)))
	   (clim-stream-p (typep real-stream 'clim:clim-stream-pane))
	   (class (class-of object))
	   (slotds (mop:class-slots class))
	   (max-slot-name-length 0)
	   (instance-slotds ())
	   (class-slotds ())
	   (other-slotds ()))
      (flet ((adjust-slot-name-length (name)
	       (setq max-slot-name-length
		 (max max-slot-name-length
		      (length (the string (symbol-name name))))))
	     (describe-slot (name &optional (allocation () alloc-p))
	       (let* ((boundp (slot-boundp object name))
		      (value (and boundp (slot-value object name))))
		 (if clim-stream-p
		     (clim:with-output-as-presentation
			 (real-stream (and boundp value) 'clim:expression
				      :allow-sensitive-inferiors t)
		       (cond 
			(boundp
			 (if alloc-p
			     (format real-stream "  ~A ~S ~VT"
				     name allocation (+ max-slot-name-length 7))
			   (format real-stream "  ~A~VT" name max-slot-name-length))
			 (clim-env::print-acceptably value real-stream 'clim:expression)
			 (format real-stream "~%"))
			(t
			 (if alloc-p
			     (format real-stream "  ~A ~S ~VT  <unbound>~%"
				     name allocation (+ max-slot-name-length 7))
			   (format real-stream "  ~A~VT  <unbound>~%"
				   name max-slot-name-length)))))
		   (if alloc-p
		     (format real-stream
			     "  ~A ~S ~VT  ~:[<unbound>~*~;~S~]~%"
			     name allocation (+ max-slot-name-length 7)
			     boundp value)
		     (format stream
			     "  ~A~VT  ~:[<unbound>~*~;~S~]~%"
			     name max-slot-name-length
			     boundp value))))))
	;; Figure out a good width for the slot-name column.

	(dolist (slotd slotds)
	  (adjust-slot-name-length (mop:slot-definition-name slotd))
	  (let ((x (slotd-allocation slotd)))
	    (cond
	     ((eq x :instance)
	      (push slotd instance-slotds))
	     ((typep x 'class)
	      (push slotd class-slotds))
	     (t (push slotd other-slotds)))))

	(setq max-slot-name-length  (min (+ max-slot-name-length 3) 30))
	(format stream "~&~@<~S~4I is an instance of ~S:~:@>~%" object class)

	(when instance-slotds
	  (format stream " The following slots have ~s allocation:~%" :instance)
	  (dolist (slotd (nreverse instance-slotds))
	    (describe-slot (mop:slot-definition-name slotd))))

	(when class-slotds
	  (format stream " The following slots have class allocation as shown:~%")
	  (dolist (slotd (nreverse class-slotds))
	    (describe-slot (mop:slot-definition-name slotd) (class-name (slotd-allocation slotd)))))
	(when other-slotds
	  (format stream " The following slots have nonstandard allocation as shown:~%")
	  (dolist (slotd (nreverse other-slotds))
	    (describe-slot (mop:slot-definition-name slotd) (slotd-allocation slotd))))
	;; [bug11228]: add fwrap description for gf:
	(when (function-object-p object)
	  (let ((ent (gethash object *fwrap-hash-table*)))
	    (when ent
	      (format stream "  It has the following indicator/fwrapper pairs, from outer to inner:~%")
	      (do ((wraps ent (cddr wraps)))
		  ((null wraps))
		(format t "~16s  ~s~%" (car wraps) (cadr wraps))))))
	(values)))))

)	;without-redefinition-warnings
