;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-ENV; Base: 10; Lowercase: Yes -*-

;;; Copyright (c) 1994-2000, Scott McKay.
;;; Copyright (c) 2001-2003, Scott McKay and Howard Shrobe.
;;; All rights reserved.  No warranty is expressed or implied.
;;; See COPYRIGHT for full copyright and terms of use.

(in-package :clim-env)

;;; Lisp Listener
;;; Is it Live, or is it Memorex?

(define-command-table listener-files :inherit-from (files systems editing))

(add-menu-item-to-command-table
  'listener-files "Compile" 
  :function (make-command-from-selected-items 
	      com-compile-file (sequence pathname) *application-frame*)
  :errorp nil)

(add-menu-item-to-command-table
  'listener-files "Compile&Load" 
  :function (make-command-from-selected-items 
	      com-compile-file (sequence pathname) *application-frame*
	      :load t)
  :errorp nil)

(add-menu-item-to-command-table
  'listener-files "Load" 
  :function (make-command-from-selected-items 
	      com-load-file (sequence pathname) *application-frame*)
  :errorp nil)

(add-menu-item-to-command-table
  'listener-files "Edit" 
  :function (make-command-from-selected-item
	      com-edit-file pathname *application-frame*)
  :errorp nil)

(define-command-table listener-systems :inherit-from (systems))

(add-menu-item-to-command-table
  'listener-systems "Compile" 
  :function (make-command-from-selected-item
	      com-compile-system system *application-frame*)
  :errorp nil)

(add-menu-item-to-command-table
  'listener-systems "Load" 
  :function (make-command-from-selected-item
	      com-load-system system *application-frame*)
  :errorp nil)

(add-menu-item-to-command-table 'listener-systems "setting-divider" 
				:divider nil
				:errorp nil)

(add-menu-item-to-command-table
  'listener-systems "Settings" 
  :command `(com-change-compiler-settings)
  :errorp nil)


(define-command-table listener-restarts)

(add-command-to-command-table 'com-resume 'listener-restarts
                              :name "Resume" :menu "Resume"
			      :errorp nil)
(add-command-to-command-table 'com-abort 'listener-restarts
			      :name "Abort" :menu "Abort" 
			      :errorp nil)
(add-command-to-command-table 'com-enter-display-debugger 'listener-restarts
			      :errorp nil
			      :name "Debug" :menu "Debug")


;;--- How the heck are we going to interface to the IE yanking commands?
(define-command-table listener-history)


;; This base class can be specialized by other types of listeners
(define-application-frame basic-listener (selected-object-mixin)
    ()
  (:command-table basic-listener)
  (:command-definer define-listener-command))

(define-application-frame lisp-listener (basic-listener)
    ()
  (:command-table (lisp-listener :inherit-from (activity
						listener-files 
						listener-systems
						listener-restarts
						listener-history
                                                basic-listener
						selected-objects
						global)
				 :menu (("Activity" :menu activity)
					("File" :menu listener-files)
					("Systems" :menu listener-systems)
					("Restarts" :menu listener-restarts)
					("History" :menu listener-history)
					("Selections" :menu selected-objects))))
  (:command-definer t)
  (:top-level (lisp-listener-top-level))
  (:pointer-documentation t)
  (:panes
    (interactor :interactor 
		:background +white+
		:scroll-bars :both))
  (:layouts (default
	      (vertically ()
		interactor))))

(defmethod frame-maintain-presentation-histories ((frame basic-listener)) t)

(defmethod clim-internals::frame-document-highlighted-presentation-1
    ((frame lisp-listener) presentation input-context window x y stream)
  (declare (ignore x y window input-context presentation))
  (format stream "{~A} " (package-name *package*))
  (call-next-method))


(defvar *listener-depth* -1)

(defvar *listener-frame*)
(defvar *listener-io*)

(defparameter *use-native-debugger* #-CCL nil #+CCL T)	;---until debugger ported to MCL

(defparameter *lisp-listener-prompt* *prompt-arrow*)


(defmethod lisp-listener-top-level ((frame lisp-listener))
  (enable-frame frame)
  (let* ((*listener-frame* frame)
	 (window (frame-standard-input frame))
         #+CCL (mirror (clim:sheet-mirror window))
         #+CCL (menu-item (ccl:window-menu-item mirror))
	 (command-table (frame-command-table frame))
	 (presentation-type `((command-or-form :command-table ,command-table)
			      :auto-activate ,*auto-activate-expressions*)))
    #+CCL (ccl:set-command-key menu-item #\L)
    (with-input-focus (window)
      (let* ((*listener-io* window)
	     (*listener-depth* (1+ *listener-depth*))
	     (*standard-input* *listener-io*)
	     (*standard-output* *listener-io*)
	     (*query-io* *listener-io*)
	     (*debugger-hook* #'listener-debugger-hook)
	     #+(or Minima Allegro) (*error-output* *listener-io*)
	     #+Allegro (*trace-output* *listener-io*)
	     #+Minima (*debug-io* *listener-io*)
	     #-Genera (*pointer-documentation-output*
	                (frame-pointer-documentation-output frame))
	     (*package* *package*)
	     (*** nil) (** nil) (* nil)
	     (/// nil) (// nil) (/ nil)
	     (+++ nil) (++ nil) (+ nil)
	     (- nil))
	(terpri *listener-io*)
	(with-command-table-keystrokes (keystrokes command-table)
	  (condition-restart-loop (#+Genera (sys:error sys:abort)
				   #-Genera (error)
				   "Restart CLIM Lisp listener")
            (setf (command-menu-enabled 'listener-restarts frame) nil)
	    (listener-command-reader
	      frame *standard-input* command-table presentation-type
	      :keystrokes keystrokes
	      :listener-depth *listener-depth*
	      :prompt *lisp-listener-prompt*)))))))

(defun listener-command-reader (frame stream command-table presentation-type 
				&key keystrokes listener-depth (prompt *lisp-listener-prompt*)
				     (expression-type 'expression)
				     (evaluator #'eval)
				     (debugger-hook #'listener-debugger-hook))
  (setf (medium-ink *query-io*) (medium-foreground *query-io*))
  (catch-abort-gestures ("Return to ~A command level ~D"
			 (frame-pretty-name frame) listener-depth)
    ;; Eat any abort gestures that might be hanging around.
    ;; We need to do this because COMMAND-OR-FORM is wierd.
    (let* ((abort-gestures *abort-gestures*)
	   (*abort-gestures* nil))
      ;;--- What test does this really need?
      (when (member (stream-read-gesture stream :timeout 0 :peek-p t) abort-gestures)
	(stream-read-gesture stream :timeout 0)))
    (fresh-line stream)
    (typecase prompt
      (string
       (write-string prompt stream))
      (clim-utils:pattern
       (multiple-value-bind (x y) (stream-cursor-position stream)
	   (draw-pattern* stream prompt x y)
	 (stream-increment-cursor-position stream (pattern-width prompt) nil)))
      (t
       (funcall prompt frame stream)))
    (multiple-value-bind (command-or-form type numeric-arg)
	(block keystroke
	  (handler-bind ((accelerator-gesture
			   #'(lambda (c)
			       ;; The COMMAND-OR-FORM type is peeking for the
			       ;; first character, looking for a ":", so we
			       ;; have to manually discard the accelerator
			       (stream-read-gesture stream :timeout 0)
			       (return-from keystroke
				 (values
				   (accelerator-gesture-event c)
				   :keystroke
				   (accelerator-gesture-numeric-argument c)))))
                         ;; Too bad the I.E. establishes another handler further in
			 (simple-parse-error
			   #'(lambda (c)
			       (let ((args (clim-internals::parse-error-format-arguments c)))
				 (when (and (= (length args) 1) ;<== 2 I think -jga
					    (equal (first args) ""))
				   ;; Hmm, user must have just hit <Return>
				   (return-from listener-command-reader)))
			       ;; Otherwise decline to handle this
			       nil)))
	    (let ((*accelerator-gestures* keystrokes))
	      (accept presentation-type
		      :stream stream
		      :prompt nil :prompt-mode :raw
		      :additional-activation-gestures '(#+Genera #\End)))))
      (when (eql type :keystroke)
	(let ((command (lookup-keystroke-command-item command-or-form command-table 
						      :numeric-argument numeric-arg)))
	  (unless (clim-internals::key-press-event-p command)
	    (when (partial-command-p command)
	      (setq command (funcall *partial-command-parser*
				     command command-table stream nil
				     :for-accelerator t)))
	    (setq command-or-form command
		  type 'command))))
      (cond ((eql type ':keystroke)
	     (beep))
	    ((eql (presentation-type-name type) 'command)
	     (terpri)
	     (let ((*debugger-hook* 
		     (unless *use-native-debugger*
		       (and (zerop listener-depth) debugger-hook))))
	       (apply (command-name command-or-form)
		      (command-arguments command-or-form)))
	     (terpri))
	    (t
	     (terpri)
	     (let ((values 
		     (multiple-value-list
		       (let ((*debugger-hook* 
			       (unless *use-native-debugger*
				 (and (zerop listener-depth) debugger-hook))))
			 (funcall evaluator command-or-form)))))
	       (fresh-line)
	       (dolist (value values)
		 (print-acceptably value stream expression-type)
		 (terpri))
	       (setq - command-or-form)
	       (shiftf +++ ++ + -)
	       (when values
		 ;; Don't change this stuff if no returned values
		 (shiftf /// // / values)
		 (shiftf *** ** * (first values)))))))))

(defun listener-debugger-hook (condition hook)
  (declare (ignore hook))
  (let* ((*application-frame* *listener-frame*)
	 #+Minima (*debug-io* (frame-query-io *application-frame*))
	 (*error-output* (frame-standard-output *application-frame*))
	 (stream *listener-io*))
    (stream-close-text-output-record stream)
    (clim-utils:letf-globally
        (((stream-current-output-record stream) (stream-output-history stream))
	 ((stream-recording-p stream) t)
	 ((stream-drawing-p stream) t)
	 ((clim-internals::stream-current-redisplay-record stream) nil))
      (setf (command-menu-enabled 'listener-restarts *application-frame*) t)
      (enter-debugger condition *listener-io*))))

(define-listener-command (com-use-native-debugger :name t)
    ((boolean 'boolean
	      :default (not *use-native-debugger*)))
  (setq *use-native-debugger* boolean))


;;; Lisp-y stuff

(defun quotify-object-if-necessary (object)
  (if (or (consp object)
	  (and (symbolp object)
	       (not (keywordp object))
	       (not (eql object nil))
	       (not (eql object t))))
    (list 'quote object)
    object))

(define-presentation-translator describe-lisp-object
    (expression form lisp-listener
     :documentation
       ((object stream)
	(let ((*print-length* 3)
	      (*print-level* 3)
	      (*print-pretty* nil)
              (*print-array* nil))
	  (present `(describe ,(quotify-object-if-necessary object)) 'expression
		   :stream stream :view +pointer-documentation-view+)))
     :gesture :describe)
    (object)
  `(describe ,(quotify-object-if-necessary object)))

;;; In MCL the following makes describe behave the way you'd like in a lisp listener
;;; i.e. every value is sensitive as an expression.  Perhaps this should be on some more general
;;; stream than just interactor panes, but I'm being cautious for the moment.

#+MCL
(defmethod inspector::prin1-value :around (i (stream clim:interactor-pane) value &optional label type)
  (clim:with-output-as-presentation (stream value 'clim:expression)
    (call-next-method i stream value label type)))

(define-presentation-translator expression-identity
    (expression nil lisp-listener
     :tester
       ((object context-type)
        ;;--- It might be nice to have a similar hack for SUBSET-COMPLETION
	(if (and (eql (presentation-type-name context-type) 'sequence)
		 (or (listp object)
		     (vectorp object)))
	  (clim-utils:with-stack-list
	    (type 'sequence (reasonable-presentation-type (elt object 0)))
	    (clim-internals::presentation-subtypep-1 type context-type))
	  (clim-internals::presentation-subtypep-1
	    (reasonable-presentation-type object) context-type)))
     :tester-definitive t
     :documentation ((object stream)
		     (let ((*print-length* 3)
			   (*print-level* 3)
			   (*print-pretty* nil)
                           (*print-array* nil))
		       (present object 'expression 
				:stream stream :view +pointer-documentation-view+)))
     :gesture :select)
    (object)
  object)

(defun print-acceptably (value stream expression-type)
  (let ((ptype (if (and (not (symbolp value))
			(reasonable-presentation-type value))
		   (reasonable-presentation-type value)
		 expression-type)))
    ;; present is as both expression and whatever it is
    (with-output-as-presentation (stream value 'expression 
					 :allow-sensitive-inferiors t)
      (with-output-as-presentation (stream value ptype
					   :allow-sensitive-inferiors t)
	(cond 
	 ((listp value)
	  (write-char #\( stream)
	  (loop for things on value
	      for thing = (first things)
	      do (with-output-as-presentation (stream thing 'expression)
		   (prin1 thing stream))
		 (when (rest things)
		   (if (atom (rest things))
		       (let ((thing (rest things)))
			 (write-string " . " stream)
			 (with-output-as-presentation (stream thing 'expression)
			   (prin1 thing stream)))
		     (write-char #\space stream))))
	  (write-char #\) stream))
	 (t (prin1 value stream)))))))

(defun reasonable-presentation-type (object)
  (let* ((class (class-of object))
	 (class-name (class-name class)))
    (when (presentation-type-specifier-p class-name)
      ;; Don't compute precedence list if we don't need it
      (return-from reasonable-presentation-type class-name))
    (dolist (class (class-precedence-list class))
      (when (presentation-type-specifier-p (class-name class))
	(return-from reasonable-presentation-type (class-name class))))
    nil))

(define-gesture-name :inspect-object :pointer-button (:left :control))
(define-presentation-to-command-translator inspect-lisp-object
    (expression com-invoke-inspector lisp-listener
     :documentation "Inspect this object"
     :gesture :inspect-object)
    (object)
  (list object))


(define-lisp-listener-command (com-show-undefined-symbols :name t)
    ((package 'package :default *package*))
  (with-frame-standard-output (stream)
    (let ((undefined
	   (let ((undefined ()))
	     (do-symbols (sym package)
	       (when (and (eql (symbol-package sym) package)
			  (not (fboundp sym))
			  (not (boundp sym))
			  (not (find-class sym nil))
			  (not (find-presentation-type-class sym nil))
			  (not (gethash sym clim-internals::*presentation-type-abbreviation-table*))
			  (not (member sym '(* ** *** + ++ +++ - / // ///))))
		 (push sym undefined)))
	     undefined)))
      (filling-output (stream :fill-width '(72 :character))
        (format stream "The following are undefined in the package ~A: "
          (package-name package))
        (format-textual-list (sort undefined #'string-lessp) #'princ
                             :stream stream :conjunction "and")
	(write-string "." stream)))))

(define-lisp-listener-command (com-change-compiler-settings :name t) ()
  (let ((source-debugging t)
	(mode :default)
	(speed 1)	;3 when in production mode, 1 in debugging mode
	(safety 3)	;1 when in production mode, 3 in debugging mode
	(debug 2)	;1 when in production mode, 3 in debugging mode
	(space 1)
	(compilation-speed 1)
	#+Lispworks (float 1)
	#+Lispworks (fixnum-safety 3)
	#+Lispworks (gc-safety 3)
	#+Lispworks (interruptable 0)
	(stream *standard-input*))
    (accepting-values (stream :own-window t
			      :align-prompts t
			      #-Genera :view #-Genera +gadget-dialog-view+)
      (setq source-debugging (accept 'boolean 
				     :prompt "Emit source debugging info"
				     :default source-debugging
				     :stream stream))
      (multiple-value-bind (new-mode type changed)
	  (accept '(member :default :debugging :production)
		  :prompt "Compilation mode"
		  :default mode
		  :stream stream)
	(declare (ignore type))
        (setq mode new-mode)
	(when changed
	  (multiple-value-setq (speed safety debug)
              (ecase mode
                (:default    (values 1 3 2))
                (:debugging  (values 1 3 3))
		(:production (values 3 1 1))))))
      (flet ((get-setting (prompt default)
               (accept '(member :none 0 1 2 3)
		       :prompt prompt :default default :stream stream)))
        (setq speed  (get-setting "Speed" speed))
        (setq safety (get-setting "Safety" safety))
        (setq debug  (get-setting "Debug" debug))
        (setq space  (get-setting "Space" space))
        (setq compilation-speed (get-setting "Compilation speed" compilation-speed))
        #+Lispworks (setq float (get-setting "Float" float))
        #+Lispworks (setq fixnum-safety (get-setting "Fixnum safety" fixnum-safety))
        #+Lispworks (setq gc-safety (get-setting "GC safety" gc-safety))
        #+Lispworks (setq interruptable (get-setting "Interruptable" interruptable))))
    #+Lispworks (compiler::toggle-source-debugging source-debugging)
    (proclaim `(optimize ,@(unless (eq speed :none)  `((speed ,speed)))
		         ,@(unless (eq safety :none) `((safety ,safety)))
		         ,@(unless (eq debug :none)  `((debug ,debug)))
		         ,@(unless (eq space :none)  `((space ,space)))
		         ,@(unless (eq compilation-speed :none)
                             `((compilation-speed ,compilation-speed)))
		         #+Lispworks ,@(unless (eq float :none)
                                         `((system::float ,float)))
		         #+Lispworks ,@(unless (eq fixnum-safety :none)
                                         `((system::fixnum-safety ,fixnum-safety)))
		         #+Lispworks ,@(unless (eq gc-safety :none)
                                         `((system::gc-safety ,gc-safety)))
		         #+Lispworks ,@(unless (eq interruptable :none)
                                         `((system::interruptable ,interruptable)))))))


(define-listener-command (com-demonstrate-clim :name "Demonstrate CLIM") ()
  (when (find-package :clim-demo)
    (funcall (intern (symbol-name 'start-demo) (find-package :clim-demo))
             :port (port *application-frame*))))


#+Genera
(define-genera-application lisp-listener
			   :pretty-name "CLIM Lisp Listener"
			   :select-key #\ˆ 
			   :width +fill+ :height +fill+)

(clim-env:define-lisp-listener-command (com-undefine-method :name t)
    ((generic-function 'clim-env::generic-function)
     (specializers `(generic-function-specializers ,generic-function))
     &key
     (qualifiers '(clim:sequence symbol)
		 :default nil))
  (let ((methods (find-applicable-methods generic-function specializers)))
    (loop for method in methods 
	unless (set-exclusive-or (method-qualifiers method) qualifiers)
	do (remove-method generic-function method))))

(defun undefmethod (generic-function-name qualifiers class-names)
  (let* ((class-list (mapcar #'find-class class-names))
	 (generic-function (symbol-function generic-function-name))
	 (method (find-method generic-function
			      qualifiers
			      class-list)))
    (remove-method generic-function method)))

(defun get-current-size-alist (stream)
  (let* ((medium (clim:sheet-medium stream))
	 (text-style (clim:medium-merged-text-style medium))
	 (family (clim:text-style-family text-style))
	 (face (clim:text-style-face text-style)))
    (let* ((family-entry (assoc family clim-silica::*text-style-intern-table*))
	   (face-entry (assoc (clim-silica::face->face-code face) (cdr family-entry))))
      (sort 
       (loop for (size) in (cdr face-entry)
	   when (integerp size) collect (cons (format nil "~d" size) size)
	   when (symbolp size) collect (cons (string-capitalize (string size)) size))
       #'(lambda (a b)
	   (cond
	    ((and (numberp (cdr a)) (numberp (cdr b))) (< (cdr a) (cdr b)))
	    ((and (symbolp (cdr a)) (symbolp (cdr b))) (string-lessp (car a) (car b)))
	    ((and (symbolp (cdr a)) (numberp (cdr b))) t)
	    (t nil))))
      )))

(defun get-current-face-alist (stream)
  (let* ((medium (clim:sheet-medium stream))
	 (text-style (clim:medium-merged-text-style medium))
	 (family (clim:text-style-family text-style)))
    (let* ((family-entry (assoc family clim-silica::*text-style-intern-table*)))
      (sort 
       (loop for (face-code) in (cdr family-entry)
	   for face = (clim-silica::face-code->face face-code)
	   for string = (if (symbolp face) 
			    (string face)
			  (format nil "~{~a~^-~}" face))			
	   collect (cons (string-capitalize string) face))
       #'string-lessp
       :key #'first)
      )))
   
(define-lisp-listener-command (com-set-size :name "Set Text Size")
    ((size `(clim:member-alist ,(get-current-size-alist *standard-output*))))
  (let* ((medium (clim:sheet-medium *standard-output*))
	 ;; (text-style (clim:medium-text-style medium))
	 (merged-text-style (clim:medium-merged-text-style medium)))
    (setf (clim:medium-merged-text-style medium) 
      (clim:parse-text-style (list (clim:text-style-family merged-text-style)
				   (clim:text-style-face merged-text-style) 
				  size)))))

(define-lisp-listener-command (com-set-face :name "Set Text Face")
    ((face `(clim:member-alist ,(get-current-face-alist *standard-output*))))
  (let* ((medium (clim:sheet-medium *standard-output*))
	 ;; (text-style (clim:medium-text-style medium))
	 (merged-text-style (clim:medium-merged-text-style medium)))
    (setf (clim:medium-merged-text-style medium) 
      (clim:parse-text-style (list (clim:text-style-family merged-text-style)
				   face 
				   (clim:text-style-size merged-text-style))))))