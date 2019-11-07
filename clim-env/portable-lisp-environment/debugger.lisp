;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-ENV; Base: 10; Lowercase: Yes -*-

;;; Copyright (c) 1994-2000, Scott McKay.
;;; Copyright (c) 2001-2003, Scott McKay and Howard Shrobe.
;;; All rights reserved.  No warranty is expressed or implied.
;;; See COPYRIGHT for full copyright and terms of use.

(in-package :clim-env)

;;; Debugger

(define-application-frame debugger (selected-object-mixin)
    ((condition :reader debugger-condition :initarg :condition)
     (restarts :accessor debugger-restarts :initform nil)
     (initial-frame :accessor debugger-initial-frame :initarg :initial-frame)
     (current-frame :accessor debugger-current-frame :initform nil)
     (previous-frame :accessor debugger-previous-frame :initform nil)
     (current-language :accessor debugger-current-language :initform nil)
     (stack-frame-pdl :reader debugger-stack-frame-pdl :initarg :stack-frame-pdl)
     (use-frame-environment :accessor debugger-use-frame-environment :initform t))
  (:command-definer t)
  (:command-table (debugger :inherit-from (activity
					   editing
					   file-system
					   programming-tools
					   selected-objects)))
  (:menu-bar nil)
  (:top-level (debugger-top-level)))

(defmethod (setf debugger-current-frame) :around (stack-frame (debugger debugger))
  (multiple-value-prog1
      (call-next-method)
    (setf (debugger-current-language debugger) (stack-frame-language stack-frame))))

(defmethod frame-maintain-presentation-histories ((frame debugger)) t)


(define-command-table debugger-stack-frames)
(define-command-table debugger-restarts)


;;--- How do we dynamically fill in the restarts menu?
(define-application-frame debugger-frame (debugger)
    ((current-frame-item :accessor debugger-current-frame-item :initform nil))
  (:command-definer nil)
  (:command-table (debugger-frame
                    :inherit-from (activity
				   debugger-stack-frames 
				   debugger-restarts
				   selected-objects
				   debugger)
		    :inherit-menu :keystrokes
		    :menu (("Activity" :menu activity)
			   ("Frames" :menu debugger-stack-frames)
			   ("Restarts" :menu debugger-restarts)
			   ("Selections" :menu selected-objects))))
  (:top-level (debugger-top-level))
  (:pointer-documentation t)
  (:panes
    (proceeds :application
	      :background +white+
	      :output-record (make-instance 'text-scroll-output-history)
	      :display-after-commands nil
	      :end-of-line-action :allow
	      :end-of-page-action :allow)
    (backtrace :application
	       :background +white+
	       :output-record (make-instance 'text-scroll-output-history)
	       :display-after-commands nil
	       :end-of-line-action :allow
	       :end-of-page-action :allow)
    (interactor :interactor
		:background +white+)
    (code :application
	  :background +white+
	  :display-after-commands nil
	  :end-of-line-action :allow
	  :end-of-page-action :allow)
    (locals :application
	    :background +white+
	    :display-after-commands nil
	    :end-of-line-action :allow
	    :end-of-page-action :allow))
  (:layouts
   (main (vertically ()
	   (1/5 proceeds)
	   (4/5 (horizontally ()
		  (1/2 (vertically ()
			 (1/2 backtrace)
			 (1/2 interactor)))
		  (1/2 (vertically ()
			 (1/5 code)
			 (4/5 locals)))))))))

(defmethod frame-standard-input ((frame debugger-frame))
  (get-frame-pane frame 'interactor))

(defmethod frame-standard-output ((frame debugger-frame))
  (get-frame-pane frame 'interactor))

;; Get these translators from the Lisp Listener

(add-presentation-translator-to-command-table
  'debugger (find-presentation-translator 'describe-lisp-object 'lisp-listener)
  :errorp nil)

(add-presentation-translator-to-command-table
  'debugger (find-presentation-translator 'expression-identity 'lisp-listener)
  :errorp nil)


;;; Definitions and presentation types

(defparameter *debugger-prompt* *prompt-arrow*)

(defmacro print-carefully ((stream type) &body body)
  `(flet ((print-carefully-body ()
	    ,@body))
     (declare (dynamic-extent #'print-carefully-body))
     (invoke-print-carefully ,stream ,type #'print-carefully-body)))
    
(defun invoke-print-carefully (stream type continuation)
  (declare (dynamic-extent continuation))
  (multiple-value-bind (value error)
      (ignore-errors (funcall continuation))
    (when error
      (multiple-value-bind (value recursive-error)
	  (ignore-errors
	    (write-string "<<Error printing " stream)
	    (if (stringp type)
	      (write-string type stream)
	      (princ type stream))
	    (write-string ">>" stream))
	(declare (ignore value))
	(when recursive-error
	  ;;--- Do something even more cautious
	  )))
    value))


;; Stack frame
(define-presentation-type stack-frame ())

;;--- Use multiple views to handle different languages
(define-presentation-method present
    (object (type stack-frame) stream (view textual-view) &key)
  (let ((function (stack-frame-function object))
	(*print-length* nil)
	(*print-level* nil))
    (with-output-as-presentation (stream function 'function-spec
				  :single-box :highlighting)
      (prin1 (function-name function) stream))))

(define-presentation-method presentation-typep (object (type stack-frame))
  #+Genera (and (sys:%pointerp object)
		(not (or (sys:%pointer-lessp object sys:%control-stack-low)
			 (sys:%pointer-lessp (sys:%read-internal-register
					       sys:%register-control-stack-limit)
					     object))))
  #+Lispworks (typep object 'dbg::call-frame)
  #+Allegro (integerp object))

(define-presentation-translator stack-frame-to-function-spec
   (stack-frame function-spec debugger)
   (object)
  (stack-frame-function object))


;; Restart
(define-presentation-type restart-name ())

(define-presentation-method presentation-typep (object (type restart-name))
  (typep object 'restart))

(define-presentation-method present
    (object (type restart-name) stream (view textual-view) &key)
  (prin1 (restart-name object) stream))


;;; Debugger entry and command loop(s)

(defparameter *use-display-debugger* nil)

;; Create a debugger for the condition CONDITION on the stream STREAM.
;;--- Resource the debugger frames!
(defun enter-debugger (condition stream 
		       &key (own-frame *use-display-debugger*)
		            (stack-frame nil)
                            (debugger-class 'debugger)
                            (display-debugger-class 'debugger-frame))
  (let* ((stack-frame
	   (or stack-frame
	       #+Genera (sys:frame-previous-frame (sys:%stack-frame-pointer))))
	 (clim-stream-p (typep stream 'clim-stream-pane))
	 (calling-frame (and clim-stream-p (pane-frame stream)))
	 #-Allegro
	 (framem (if clim-stream-p (frame-manager stream) (find-frame-manager))))
    (unless clim-stream-p
      (setq own-frame t))
    ;; Create the debugger, run it, and return its values
    (if own-frame
      (let ((debugger (make-application-frame display-debugger-class
			:calling-frame calling-frame
			:condition condition
			:initial-frame stack-frame
			:stack-frame-pdl (make-array 20 :fill-pointer t)
			#-Allegro :parent #-Allegro framem
			:width 1000 :height 800)))
	(unwind-protect
	    (run-frame-top-level debugger)
	  ;;--- Flush this when we are using resourced frames?
	  (disown-frame (frame-manager debugger) debugger)))
      (let ((debugger (make-application-frame debugger-class 
			:calling-frame calling-frame
			:condition condition
			:initial-frame stack-frame
			:stack-frame-pdl (make-array 20 :fill-pointer t)
			;; Share i/o buffers will the calling frame
			;; in the case of "inlined" debuggers
			:top-level-sheet (frame-top-level-sheet calling-frame))))
	(unwind-protect
	    (with-output-recording-reset (stream)
	      (declare (ignore stream))
	      (run-frame-top-level debugger))
	  ;; Since this frame isn't really adopted, we don't really want
	  ;; to disown it since that will end up disowning the calling
	  ;; frame.  Flush it from the frame manager manually.  Barf.
	  (let ((framem (frame-manager debugger)))
	    (setf (frame-manager-frames framem)
		    (delete debugger (frame-manager-frames framem))
		  (slot-value debugger 'frame-manager) nil)))))))

(defvar *debugger-level* 0)
(defvar *debugger-level-threshhold* 10)

;; This is where some of the initial platform dependencies go
(defmacro with-debugger-initialized ((debugger) &body body)
  `(let ((*debugger-level* (1+ *debugger-level*))
	 ;; Glue to the rest of the Genera debugger
	 #+Genera (dbg:*error* (debugger-condition ,debugger))
	 #+Genera (dbg:*innermost-visible-frame* (debugger-initial-frame ,debugger))
	 #+Genera (dbg:*reason-debugger-entered* nil)
	 #+Genera (dbg:*current-frame* dbg:*innermost-visible-frame*)
	 #+Genera (dbg:*previous-current-frame* nil)
	 #+Genera (dbg:*innermost-interesting-frame* dbg:*innermost-visible-frame*)
	 #+Genera (dbg:*proceed-dispatch-table* nil)
	 #+(and Genera Imach) (dbg:*frame-array-index-cached-frame* nil)
	 #+(and Genera Imach) (dbg:*frame-array-index-cached-index* nil)
	 ;; Glue to the rest of the LispWorks debugger
         #+Lispworks (dbg::*debug-level* (1+ *debugger-level*))
         #+Lispworks (dbg::*old-read-suppress* dbg::*read-suppress*)
         ;;--- Do we need to bind *IN-STEPPER*?
	 #+Lispworks (compiler::*in-compiler-handler* nil)
	 #+Lispworks (conditions::*restart-clusters* conditions::*restart-clusters*)
	 #+Lispworks (conditions::*handler-clusters* conditions::*handler-clusters*)
	 #+Lispworks (dbg::*debug-abort* nil) ; computed below
         #+Lispworks (dbg::*debug-continue* dbg::*debug-continue*) ; recomputed below
	 #+Lispworks (dbg::*debug-window* nil)
	 #+Lispworks (dbg::*debug-listener* dbg::*debug-listener*)
	 #+Lispworks (dbg::*debugger-stack* (dbg::grab-stack nil :how-many 5))
	 #+Lispworks (dbg::*debug-condition* nil)
	 #+Lispworks (dbg::*listener-is-reading* nil)
	 ;; Glue to the rest of the Allegro debugger
	 #+Allegro (top-level::*top-top-frame-pointer* (debugger-initial-frame ,debugger))
	 #+Allegro (top-level::*top-frame-pointer* (debugger-initial-frame ,debugger))
	 #+Allegro (top-level::*current-frame-pointer* (debugger-initial-frame ,debugger))
	 #+Allegro (top-level::*stepping* nil )
	 #+Allegro (top-level::*focus-process* nil)
	 #+Allegro (top-level::*tpl-current-thread* (multiprocessing:process-thread sys:*current-process*))
	 #+Allegro (sys::*compile-to-file* nil)
	 #+Allegro (sys::*compile-to-core* nil)
	 #+Allegro (top-level::*break-level* (1+ top-level::*break-level*))
	 #+Allegro (top-level::*break-level-symbol* (excl::gentag))
	 #+Allegro (excl::*circularity-hash-table* nil)
	 #+Allegro (sys::*current-context* sys::*current-context*)
	 ;; Bind some environment variables
	 #+Lispworks (*print-length* #+Lispworks dbg::*debug-print-length*)
	 #+Lispworks (*print-level* #+Lispworks dbg::*debug-print-level*)
	 #+Allegro (excl::.throwing-to-tag. nil)
	 #+Allegro (excl::*source-file-info* nil)
	 #+Allegro (excl::*recursive-read-list* t)
	 (*print-pretty* nil)
	 (*read-base* 10)
	 (*print-base* 10)
	 (*read-suppress* nil)
	 (*debug-io* *debug-io*)
         #+Lispworks
	 (*readtable* (if (> dbg::*debug-level* 4)
			(copy-readtable nil)
			*readtable*)))
      #+allegro (declare (ignore excl::.throwing-to-tag.))
     (#+Genera scl:using-resource #+Genera (dbg:*stack-frame-array* dbg:stack-frame-array)
      #-Genera progn
       (initialize-debugger-state ,debugger)
       ,@body)))

;; This is where more of the initial platform dependencies go
(defmethod initialize-debugger-state ((frame debugger))
  #+Genera 
  (progn
    (dbg:relocate-frame-pointers dbg:*error*)
    (setf (debugger-current-frame frame) (debugger-initial-frame frame)))
  #+Lispworks
  (let* ((initial-frame
	   (block find-frame
	     ;; cf DBG::GOTO-FIRST-INTERESTING-FRAME
	     (dbg::with-interesting-frames 
		(:hidden-packages dbg::*boring-packages*)
	      (dbg::for-next-frames stack-frame (dbg::debugger-stack-current-frame dbg::*debugger-stack*)
		(when (dbg::interesting-frame-p stack-frame)
		  (return-from find-frame stack-frame))))))
         (current-frame
           (do ((frame initial-frame (stack-frame-next-call-frame frame)))
	       ((eql (function-name (stack-frame-function frame)) 'enter-debugger)
		(stack-frame-next-call-frame frame)))))
    (setf (debugger-initial-frame frame) initial-frame
	  (debugger-current-frame frame) current-frame))
  (setf (fill-pointer (debugger-stack-frame-pdl frame)) 0)
  (let ((condition (debugger-condition frame)))
    (setf (debugger-restarts frame) (compute-restarts condition))
    #+Genera 
    (progn
      (setq dbg:*current-frame* (dbg:innermost-interesting-frame dbg:*current-frame*))
      (multiple-value-setq (dbg:*current-frame* dbg:*innermost-interesting-frame*)
	(dbg:current-and-innermost-frames-for-debugger 
	  dbg:*error* dbg:*current-frame*))
      (setf (debugger-current-frame frame) dbg:*current-frame*))
    #+Lispworks
    (progn
      (setq dbg::*debug-restarts* (conditions::compute-restarts condition))
      (setq dbg::*debug-abort* (find 'abort dbg::*debug-restarts* 
				     :key #'restart-name))
      (setq dbg::*debug-continue*
	    (or (let ((c (find 'continue dbg::*debug-restarts*
			       :key #'restart-name)))
		  (if (or (not dbg::*debug-continue*)
			  (not (eq dbg::*debug-continue* c)))
		    c
		    nil))
		(let ((c (first dbg::*debug-restarts*)))
		  (if (not (eq c dbg::*debug-abort*))
		    c
		    nil))))
      #+Lispworks3 (setq dbg::*debug-condition* condition))
    #+Allegro 
    (let ((debugger::*hidden-packages*
	   (append debugger::*hidden-packages*
		   '(:keyword :clim-environment :clim-internals :clim)))
	  (debugger::*hidden-package-internals*
	   (append debugger::*hidden-package-internals*
		   '(:keyword :clim-environment :clim-internals :clim)))
	  (debugger::*hidden-frames*
	   (append debugger::*hidden-frames*
		   '(:effective-method :internal)))
	  (debugger::*hidden-functions*
	   (append debugger::*hidden-functions*
		   '(:effective-method :internal))))
      (setq top-level::*tpl-current-thread* (multiprocessing:process-thread sys:*current-process*))
      (setq top-level::*top-top-frame-pointer*
	    (excl::int-newest-frame (multiprocessing:process-thread sys:*current-process*)
				    :visible-only-p nil))
      (setq top-level::*top-frame-pointer*
	    (or (unless (excl::package-not-yet-loaded :debugger)
		  (excl::funcall-in-package :find-interesting-frame :debugger
					    top-level::*top-top-frame-pointer*))
		top-level::*top-top-frame-pointer*))
      (setq top-level::*current-frame-pointer* top-level::*top-frame-pointer*)
      (setf (debugger-initial-frame frame) top-level::*top-frame-pointer*
	    
	    (debugger-current-frame frame) 
	    ;; int-next-older-frame visits invisible-frames in contast to
	    ;; the portable stack-frame-previous-frame
	    ;; Finds enter-debugger and then goes 2 visible frames further
	    (or (loop for fp = (debugger-initial-frame frame) then fp-next
		    for fp-next = (excl::int-next-older-frame fp)
		    for function = (stack-frame-function fp)
		    for function-name = (function-name function)
		    when (eql function-name 'enter-debugger)
		    return (stack-frame-previous-frame (stack-frame-previous-frame fp))
		    until (null fp-next))
		top-level::*current-frame-pointer*)))))

(defmethod debugger-top-level ((frame debugger))
  (with-debugger-initialized (frame)

    (let ((stream (frame-standard-output frame)))
      (when (> *debugger-level* *debugger-level-threshhold*)
	;;--- Handle highly recursive errors
	)
      (describe-error frame stream :highlight-error t)
      (let ((process (clim-sys:current-process)))
	(when process
          (fresh-line stream)
	  (format stream "In process ~A." process)))
      (debugger-top-level-1 frame))))

(defmethod debugger-top-level ((frame debugger-frame))
  (enable-frame frame)
  (with-debugger-initialized (frame)
    (when (> *debugger-level* *debugger-level-threshhold*)
      ;;--- Handle highly recursive errors
      )
    ;; Display the proceed options
    (let ((stream (get-frame-pane frame 'proceeds))
	  (condition (debugger-condition frame))
	  (restarts (debugger-restarts frame)))
      (with-text-scrolling-delayed (stream)
	(add-item stream (with-output-to-output-record (stream)
			   (with-text-face (stream :italic)
                             (fresh-line stream)
			     (format stream "Error: ~A" condition))))
	(when restarts
	  (dolist (restart restarts)
	    (add-item stream (with-output-to-output-record
				 (stream 'standard-presentation presentation
				  :object restart
				  :type 'restart-name
				  :single-box t)
			       presentation
			       (let ((clim-internals::*allow-sensitive-inferiors* nil))
				 (format stream "~A" restart)
				 (when (restart-name restart)
				   (format stream " (~S)" (restart-name restart))))))))))
    (let ((stream (get-frame-pane frame 'backtrace)))
      (with-text-scrolling-delayed (stream)
	(map-backtrace
	  nil 0
	  #'(lambda (stack-frame count index)
	      (declare (ignore index count))
              (let* ((current (stack-frame-eql stack-frame (debugger-current-frame frame)))
                     (item (stack-frame-text-scroll-item
			     stack-frame stream (if current :bold :roman))))
                (when current
                  (setf (debugger-current-frame-item frame) item))
	        (add-item stream item))))))
    (display-debugger-show-args-and-locals frame)
    (display-debugger-source-code frame)
    (debugger-top-level-1 frame)))

(defmethod debugger-top-level-1 ((frame debugger))
  (let* ((interactor (frame-standard-input frame))
	 (command-table (frame-command-table frame)))
    (let* ((*standard-output*
	     (or (frame-standard-output frame) *standard-output*))
	   (*standard-input* 
	     (or (frame-standard-input frame) *standard-output*))
	   (*query-io* 
	     (or (frame-query-io frame) *standard-input*))
	   (*error-output* 
	     (or (frame-error-output frame) *standard-output*))
	   #+Allegro (*trace-output* *error-output*)
	   (*pointer-documentation-output*
	     (frame-pointer-documentation-output frame))
	   #+Genera (sys:trace-conditions nil)
	   (*break-on-signals* nil)
	   (*package* *package*)
	   (*** ***) (** **) (* *)
	   (/// ///) (// //) (/ /)
	   (+++ +++) (++ ++) (+ +)
	   (- -))
      (terpri interactor)
      (with-command-table-keystrokes (keystrokes command-table)
	(condition-restart-loop (#+Genera (sys:error sys:abort)
				 #-Genera (error)
				 "Restart this debugger")
	  (debugger-command-reader
	    frame *standard-input* command-table
	    :keystrokes keystrokes))))))

(defmethod debugger-command-reader ((frame debugger) stream command-table 
				    &key keystrokes (prompt *debugger-prompt*))
  (setf (medium-ink *query-io*) (medium-foreground *query-io*))
  #+Genera (dbg:relocate-debugger-frame-pointers)
  (catch-abort-gestures ("Return to ~A command level" (frame-pretty-name frame))
    ;; Eat any abort gestures that might be hanging around.
    ;; We need to do this because COMMAND-OR-FORM is wierd.
    (let* ((abort-gestures *abort-gestures*)
	   (*abort-gestures* nil))
      ;;--- What test does this really need?
      (when (member (stream-read-gesture stream :timeout 0 :peek-p t) abort-gestures)
	(stream-read-gesture stream :timeout 0)))
    (let* ((current-frame (debugger-current-frame *application-frame*))
           (evaluator
             (stack-frame-eval-function current-frame *application-frame*))
	   (expression-type
             (stack-frame-expression-type current-frame *application-frame*))
           (presentation-type
             (stack-frame-command-or-form-type current-frame *application-frame*)))
      (fresh-line stream)
      (typecase prompt
        (string
         (write-string (additional-prompt-string frame current-frame) stream)
         (write-char #\Space stream)
         (write-string prompt stream))
        (clim-utils:pattern
         (multiple-value-bind (x y) (stream-cursor-position stream)
           (let* ((string (additional-prompt-string frame current-frame))
                  (width (stream-string-width stream string)))
	     (draw-text* stream string
		         x (+ y (floor (pattern-height prompt) 2))
		         :align-y :center
		         :text-style '(nil :bold :smaller))
	     (draw-pattern* stream prompt (+ x width) y)
	     (stream-increment-cursor-position stream
                                               (+ (pattern-width prompt) width) nil))))
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
			   (simple-parse-error
			     #'(lambda (c)
				 (let ((args (clim-internals::parse-error-format-arguments c)))
				   (when (and (= (length args) 1)
					      (equal (first args) ""))
				     ;; Hmm, user must have just hit <Return>
				     (return-from debugger-command-reader)))
				 ;; Otherwise decline to handle this
				 nil)))
	      (let ((*accelerator-gestures* keystrokes))
		(with-input-focus (stream)
		  (accept presentation-type
			  :stream stream
			  :prompt nil :prompt-mode :raw
			  :additional-activation-gestures '(#+Genera #\End))))))
	(when (eql type :keystroke)
	  (let ((command (lookup-keystroke-command-item command-or-form command-table 
							:numeric-argument numeric-arg)))
	    (unless (clim-internals::key-press-event-p command)
	      (when (partial-command-p command)
		(setq command (funcall *partial-command-parser*
				       command command-table stream nil
				       :for-accelerator t)))
	      (with-text-face (stream :italic)
		(present command 'command 
			 :for-context-type presentation-type
			 :stream stream))
	      (setq command-or-form command
		    type 'command))))
	(cond ((eql type ':keystroke)
	       (beep))
	      ((eql (presentation-type-name type) 'command)
	       (terpri)
	       (let ((*debugger-hook* nil))
		 (apply (command-name command-or-form)
			(command-arguments command-or-form)))
	       (terpri))
	      (t
	       (terpri)
	       (let ((values 
		       (let ((*debugger-hook* nil))
			 (funcall evaluator
				  command-or-form (debugger-current-frame frame)))))
		 (fresh-line *standard-output*)
		 (dolist (value values)
		   (present value expression-type
			    :stream *standard-output*
			    :single-box :highlighting)
		   (terpri))
		 (setq - command-or-form)
		 (shiftf +++ ++ + -)
		 (when values
		   ;; Don't change this stuff if no returned values
		   (shiftf /// // / values)
		   (shiftf *** ** * (first values))))))))))

(defmethod additional-prompt-string ((frame debugger) stack-frame)
  (declare (ignore stack-frame))
  (format nil "~D" *debugger-level*))


(defun evaluate-in-frame-environment (expression stack-frame)
  #+Genera
  (let ((dbg:*current-frame* stack-frame)
	(dbg:*inherit-lexical-environment* 
	 (debugger-use-frame-environment *application-frame*)))
    (dbg:eval-in-error-environment t t expression))
  #+Lispworks
  (multiple-value-list (dbg::dbg-eval expression stack-frame))
  #+allegro
  ;; Fix: The globals bound by the command loop aren't seen when you eval-form-in-context
  ;; This isn't really good enough because (+ * 2) will still use the * from the stack-frame
  ;; problem is that I don't have the code for eval-form-in-context
  (multiple-value-list (cond 
			((member expression '(*** ** * /// // / +++ ++ + -))
			 (eval expression))
			(t (db:eval-form-in-context expression stack-frame))))
  #-(or Genera Lispworks allegro)
  (multiple-value-list (eval expression)))

;; Start with a Lisp view of the world...
;;--- The next four should really be methods on a "language" object
(defmethod stack-frame-expression-type (stack-frame (frame debugger))
  (declare (ignore stack-frame))
  'expression)

(defmethod stack-frame-form-type (stack-frame (frame debugger))
  (declare (ignore stack-frame))
  'form)

(defmethod stack-frame-command-or-form-type (stack-frame (frame debugger))
  (declare (ignore stack-frame))
  `((command-or-form) :auto-activate ,*auto-activate-expressions*))

(defmethod stack-frame-eval-function (stack-frame (frame debugger))
  (declare (ignore stack-frame))
  #'evaluate-in-frame-environment)


;;; Conditions and restarts

(defun describe-error (frame stream &key highlight-error (describe-restarts t))
  (let ((condition (debugger-condition frame)))
    (with-output-as-presentation (stream condition 'form
				  :single-box t)
      (with-text-face (stream (and highlight-error :bold))
	(format stream "~2&Error: ~A" condition))))
  (when describe-restarts
    (describe-restarts frame stream)))

(defun describe-restarts (frame stream)
  (let ((restarts (debugger-restarts frame)))
    (when restarts
      (let ((actions '(invoke-restart)))
	(dolist (restart (reverse restarts))
	  (let ((action (member (restart-name restart)
				'(abort continue muffle-warning store-value use-value))))
	    (when action
	      (pushnew (first action) actions))))
        (fresh-line stream)
	(format stream "Use~?to resume~:[~; or abort~] execution:"
		       "~#[~; ~S~; ~S or ~S~:;~@{~#[~; or~] ~S~^,~}~] "
	  actions (member 'abort actions)))
      (fresh-line stream)
      (formatting-table (stream :x-spacing '(2 :character))
	(dolist (restart restarts)
	  (with-output-as-presentation (stream restart 'restart-name
					:single-box t :allow-sensitive-inferiors nil)
	    (formatting-row (stream)
	      (formatting-cell (stream) stream)
	      (formatting-cell (stream)
		(when (restart-name restart)
		  (format stream "~A:" (restart-name restart))))
	      (formatting-cell (stream)
		(format stream "~A" restart)))))))))


;;; Stack frame hacking

(defun stack-frame-function (stack-frame)
  #+Genera (sys:frame-function stack-frame)
  #+Lispworks (dbg::get-frame-functions stack-frame)
  #+Allegro (debug::frame-function stack-frame))

(defun function-name (function)
  #+Genera (sys:function-name function)
  #+Lispworks (sys::function-name function)
  #+MCL (ccl::function-name function)
  #+Allegro (xref::object-to-function-name function))

(defun short-backtrace (&key (nframes most-positive-fixnum)
			     (interpreter-frames nil)
			     (stream *standard-output*))
  (fresh-line stream)
  (let* ((line-length (window-inside-width stream))
	 (current-line-length 0)
	 (separator #+Genera "  " #-Genera " <- ")
	 (separator-length (stream-string-width stream separator)))
    (map-backtrace
      nframes 0
      #'(lambda (stack-frame count index)
	  (declare (ignore index))
	  (let* ((name (function-name (stack-frame-function stack-frame)))
		 ;;--- Can we avoid all this consing?
		 (name-length (+ (stream-string-width stream (format nil "~S" name))
				 separator-length)))
	    (when (> (+ current-line-length name-length) line-length)
	      (terpri stream)
	      (setq current-line-length 0))
	    (when (plusp count) 
	      (write-string separator stream))
	    (print-carefully (stream "function name")
	      (present stack-frame 'stack-frame
		       :stream stream))
	    (incf current-line-length name-length)))
      :interpreter-frames interpreter-frames)))


;;; Primitive frame motion

#+Lispworks
(declaim (inline stack-frame-next-call-frame stack-frame-previous-call-frame))

#+Lispworks
(defun stack-frame-next-call-frame (stack-frame &optional (test #'true))
  (loop for frame = (dbg::frame-next stack-frame)
          then (dbg::frame-next frame)
        when (or (null frame)
                 (and (typep frame 'dbg::call-frame)
                      (funcall test frame)))
          return frame))

#+Lispworks
(defun stack-frame-previous-call-frame (stack-frame &optional (test #'true))
  (loop for frame = (dbg::frame-prev stack-frame)
          then (dbg::frame-prev frame)
        when (or (null frame)
                 (and (typep frame 'dbg::call-frame)
                      (funcall test frame)))
          return frame))

;; The next several functions return NIL if they go off the end of the stack.
;; Note that Genera and LispWork have differing ideas of "next" and "previous".
(defun stack-frame-previous-frame (stack-frame)
  #+Genera (sys:frame-previous-frame stack-frame)
  #+Lispworks (stack-frame-next-call-frame stack-frame)
  #+Allegro (top-level::down-frame stack-frame))

(defun stack-frame-previous-active-frame (stack-frame)
  #+Genera (dbg:frame-previous-active-frame stack-frame)
  #+Lispworks (stack-frame-next-call-frame stack-frame)
  #+Allegro (top-level::down-frame stack-frame))

(defun stack-frame-previous-interesting-frame (stack-frame)
  #+Genera (dbg:frame-previous-interesting-frame stack-frame nil t)
  #+Lispworks (stack-frame-next-call-frame stack-frame #'dbg::interesting-frame-p)
  #+Allegro (top-level::down-frame stack-frame))

(defun stack-frame-next-active-frame (stack-frame)
  #+Genera (dbg:frame-next-active-frame stack-frame)
  #+Lispworks (stack-frame-previous-call-frame stack-frame)
  #+Allegro (top-level::up-frame stack-frame))


;;; Primitive frame predicates

(defun stack-frame-eql (frame1 frame2)
  #-Allegro (eql frame1 frame2)
  ;; Allegro frame motion primitives cons a new FD each time!
  #+Allegro (and frame1 frame2
		 (excl::int-frame-reference-eq  frame1 frame2)))

(defun stack-frame-interesting-p (stack-frame)
  #+Allegro (declare (ignore stack-frame))
  #+Genera (dbg:frame-interesting-p stack-frame)
  #+Lispworks (dbg::interesting-frame-p stack-frame)
  #+Allegro t)

(defun stack-frame-active-p (stack-frame)
  #+(or Lispworks Allegro) (declare (ignore stack-frame))
  #+Genera (dbg:frame-active-p stack-frame)
  #+Lispworks t
  #+Allegro t)

(defun stack-frame-value-disposition (stack-frame) 
  #+(or Lispworks Allegro) (declare (ignore stack-frame))
  #+Genera (dbg:frame-real-value-disposition stack-frame)
  ;;--- Until we can figure out how to get a real disposition
  #+Lispworks :multiple
  #+Allegro :multiple)

(defun stack-frame-relative-exit-pc (stack-frame)
  #+Genera (sys:frame-relative-exit-pc stack-frame)
  #+Lispworks (dbg::call-frame-call-address stack-frame)
  #+Allegro 0)

(defmacro with-language-for-stack-frame ((stack-frame) &body body)
  #-Genera (declare (ignore stack-frame))
  #+Genera `(dbg:with-language-for-frame (,stack-frame) ,@body)
  #-Genera `(progn ,@body))

(defun stack-frame-language (stack-frame)
  #-Genera (declare (ignore stack-frame))
  #+Genera (dbg:find-language-for-frame stack-frame)
  #-Genera nil)


;;; Frame arguments and variables

#+Lispworks
;; This utility is used by all kinds of things that want to extract
;; information from stack frames. 
;;--- It would be nice to cache this information in the debugger's
;;--- call-frame objects so that we don't have to cons all the time. 
(defun lispworks-stack-frame-info (stack-frame)
  ;; ARGS-AND-LOCALS is of the form (name kind varspec value)
  (declare (values args-and-locals-alist n-args specials-alist))
  (let* ((lambda-list (aref (dbg::call-frame-constants stack-frame) 3))
	 (var-specs (dbg::call-frame-vars-in-scope stack-frame))
         (all-vars nil)
	 (args-and-locals nil)
	 (n-args 0))
    (dolist (c var-specs)
      (let* ((name (dbg::name-from-var-spec c stack-frame))
	     (value (dbg::value-from-var-spec c stack-frame)))
	(push (list name c value) all-vars)))
    ;; Disgusting special case for macros.  Apparently, when we're
    ;; looking at the stack frame of a macro, and we get to this
    ;; point, ALL-VARS doesn't contain varspecs with the real 
    ;; names, but rather some special stuff which looks 
    ;; suspiciously like &WHOLE and &ENV.  Try to get past that
    ;; by doing arglist matching by hand.
    (when (eq (caar all-vars) 'definition::%%macroarg%%)
      (setq lambda-list '(definition::%%macroarg%% &rest environment)))
    ;; LW seems to sometimes hide extra cruft in lambda lists, marked
    ;; with preceding keyword-oid things.  Strip them off here.
    (loop while (keywordp (car lambda-list))
          do (setq lambda-list (cddr lambda-list)))
    ;; At this point, ALL-VARS contains an alist of all the locally
    ;; accessable vars, in order of definition (I think).  Map over
    ;; the lambda list, extracting the variables that are defined there.
    ;; Walk the rest, extracting all the ones that aren't special.  
    (flet ((special-variable-p (symbol)
             (boundp symbol)))
      (declare (dynamic-extent #'special-variable-p))
      (map-over-lambda-list
	lambda-list
	#'(lambda (args kind)
	    (let* ((name (car args))
		   (elt (assoc name all-vars)))
	      (when (and (eql kind ':required)
			 ;;--- Kludge?  LW lambda lists seem to require this
			 (not (or (keywordp name)
				  (member name '(t nil)))))
		(assert (not (null elt)) ()
			"Argument ~A not found" name))
	      (when elt
		(unless (special-variable-p name)
		  ;; Remove non-specials from all-vars
		  (setq all-vars (delete elt all-vars)))
		;; Add it to args-and-locals
		(setq args-and-locals 
		      (nconc args-and-locals 
			     (list (list (first elt) kind (second elt) (third elt)))))
		(incf n-args)))))
      ;; Got the args; anything else non-special is a local
      (loop for elt in all-vars
	    for (arg spec val) = elt
	    unless (special-variable-p arg)
	      do (setq all-vars (delete elt all-vars)
		       args-and-locals (nconc args-and-locals
					      (list (list arg :local spec val))))))
    (values args-and-locals n-args all-vars)))

#+Allegro
(defun allegro-stack-frame-info (stack-frame)
  (declare (values required optional rest keys n-params n-args))
  (let* ((function (stack-frame-function stack-frame))
	 (lambda-list (excl:arglist function))
	 (required ())
	 (optional ())
	 (rest     nil)
	 (keys     ()))
    (map-over-lambda-list
      lambda-list
      #'(lambda (args kind)
	  (let ((name (car args)))
	    (case kind
	      (:required
	       (push name required))
	      (:optional
	       (push (if (consp name) (car name) name) optional))
	      (:rest
	       (setq rest name))
	      (:key
	       (push (if (consp name) (car name) name) keys))))))
    (values required optional rest keys
	    (+ (length required) (length optional) (length keys) (if rest 1 0))
	    ;;---*** Return the number of passed arguments
	    (+ (length required) (length optional) (length keys) (if rest 1 0)))))

(defmethod stack-frame-argument-value (stack-frame n &optional (error-p t))
  (declare (values value location))
  #+(or Lispworks Allegro) (declare (ignore error-p))
  #+Genera
  (let ((dbg:*printing-monitor-message* t))
    (dbg:frame-arg-value stack-frame n nil (not error-p)))
  #+Lispworks
  (multiple-value-bind (args-and-locals n-args)
      (lispworks-stack-frame-info stack-frame)
    (when (< n n-args)
      ;; No locatives here, just return index as the location
      (values (nth 3 (nth n args-and-locals)) n)))
  #+Allegro
  (multiple-value-bind (required optional rest keys n-params n-args)
      (allegro-stack-frame-info stack-frame)
    (declare (ignore required optional rest keys n-params))
    (when (< n n-args)
      ;; No locatives here, just return index as the location
      (values (debugger:frame-var-value stack-frame n) n))))

(defmethod (setf stack-frame-argument-value) (value stack-frame n)
  #+Genera
  (let ((dbg:*printing-monitor-message* t))
    (multiple-value-bind (nil loc)
	(dbg:frame-arg-value stack-frame n)
      ;;--- Redisplay the args pane if we're in the display debugger
      (setf (sys:location-contents loc) value)
      value))
  #+Lispworks
  (multiple-value-bind (args-and-locals n-args)
      (lispworks-stack-frame-info stack-frame)
    (when (< n n-args)
      (let* ((elt (nth n args-and-locals))
             (var-spec (nth 2 elt)))
        (dbg::dbg-set-value-of var-spec value)
        value)))
  #+Allegro
  (multiple-value-bind (required optional rest keys n-params n-args)
      (allegro-stack-frame-info stack-frame)
    (declare (ignore required optional rest keys n-params))
    (when (< n n-args)
      (setf (debugger:frame-var-value stack-frame n) value))))

(defmethod stack-frame-local-value (stack-frame n &optional (error-p t))
  (declare (values value location))
  #+(or Lispworks Allegro) (declare (ignore error-p))
  #+Genera
  (let ((dbg:*printing-monitor-message* t))
    (dbg:frame-local-value stack-frame n (not error-p)))
  #+Lispworks 
  (multiple-value-bind (args-and-locals n-args)
      (lispworks-stack-frame-info stack-frame)
    (declare (ignore n-args))
    (when (< n (length args-and-locals))
      ;; No locatives here, just return index as the location
      (values (nth 3 (nth n args-and-locals)) n)))
  #+Allegro
  (multiple-value-bind (required optional rest keys n-params n-args)
      (allegro-stack-frame-info stack-frame)
    (declare (ignore required optional rest keys n-params))
    (when (< n (debugger:frame-number-vars stack-frame))
      ;; No locatives here, just return index as the location
      (values (debugger:frame-var-value stack-frame (+ n n-args)) (+ n n-args)))))

(defmethod (setf stack-frame-local-value) (value stack-frame n)
  #+Genera
  (let ((dbg:*printing-monitor-message* t))
    (multiple-value-bind (nil loc)
	(dbg:frame-local-value stack-frame n :force)
      ;;--- Redisplay the locals pane if we're in the display debugger
      (setf (sys:location-contents loc) value)
      value))
  #+Lispworks
  (multiple-value-bind (args-and-locals n-args)
      (lispworks-stack-frame-info stack-frame)
    (declare (ignore n-args))
    (when (< n (length args-and-locals))
      (let* ((elt (nth n args-and-locals))
             (var-spec (nth 2 elt)))
        (dbg::dbg-set-value-of var-spec value)
        value)))
  #+Allegro
  (multiple-value-bind (required optional rest keys n-params n-args)
      (allegro-stack-frame-info stack-frame)
    (declare (ignore required optional rest keys n-params))
    (when (< n n-args)
      (setf (debugger:frame-var-value stack-frame (+ n n-args)) value))))

(defmethod stack-frame-number-of-locals (stack-frame)
  #+Genera
  (dbg:frame-number-of-locals stack-frame)
  #+Lispworks
  (multiple-value-bind (args-and-locals n-args)
      (lispworks-stack-frame-info stack-frame)
    (- (length args-and-locals) n-args))
  #+Allegro
  (multiple-value-bind (required optional rest keys n-params n-args)
      (allegro-stack-frame-info stack-frame)
    (declare (ignore required optional rest keys n-params))
    (- (debugger:frame-number-vars stack-frame) n-args)))

(defun stack-frame-rest-arg-value (stack-frame)
  (declare (values rest-arg rest-arg-p))
  #+Genera
  (let* ((dbg:*printing-monitor-message* t)
	 (function (stack-frame-function stack-frame))
	 (control-register (sys:frame-own-control-register stack-frame))
	 (args-info (sys:%args-info function))
	 (uses-lp (ldb-test sys:%%arg-desc-rest-arg args-info))
	 (first-local-offset (ldb sys:%%cr.argument-size control-register))
	 (rest-arg-slot (and uses-lp first-local-offset)))
    (if rest-arg-slot
      (let* ((loc (sys:%pointer-plus stack-frame rest-arg-slot))
	     (val (sys:location-contents loc)))
	(values val loc))
      (values nil nil)))
  #+Lispworks
  (multiple-value-bind (args-and-locals n-args)
      (lispworks-stack-frame-info stack-frame)
    (loop for i below n-args
	  for (nil kind nil value) in args-and-locals
	  when (eql kind ':rest)
	    return (values value t)))
  #+Allegro
  (multiple-value-bind (required optional rest keys n-params n-args)
      (allegro-stack-frame-info stack-frame)
    (declare (ignore required optional keys n-params))
    (when rest
      (loop for i from 0 below n-args
	    when (eq (debugger:frame-var-name stack-frame i) rest)
	      return (debugger:frame-var-value stack-frame i)))))

(defun stack-frame-argument-name (stack-frame n)
  #+Genera
  (dbg:language-argument-name 
    (dbg:find-language-for-frame stack-frame) stack-frame n)
  #+Lispworks
  (multiple-value-bind (args-and-locals n-args)
      (lispworks-stack-frame-info stack-frame)
    (and (< n n-args)
	 (nth 0 (nth n args-and-locals))))
  #+Allegro
  (multiple-value-bind (required optional rest keys n-params n-args)
      (allegro-stack-frame-info stack-frame)
    (declare (ignore required optional rest keys n-params))
    (when (< n n-args)
      (values (debugger:frame-var-name stack-frame n) n))))

(defun stack-frame-rest-arg-name (stack-frame)
  #+Genera
  (dbg:language-rest-argument-name
    (dbg:find-language-for-frame stack-frame) stack-frame)
  #+Lispworks
  (multiple-value-bind (args-and-locals n-args)
      (lispworks-stack-frame-info stack-frame)
    (loop for (name kind nil nil) in args-and-locals
	  repeat n-args
	  when (eql kind ':rest)
	    return name))
  #+Allegro
  (multiple-value-bind (required optional rest keys n-params n-args)
      (allegro-stack-frame-info stack-frame)
    (declare (ignore required optional keys n-params))
    (when rest
      (loop for i from 0 below n-args
	    when (eq (debugger:frame-var-name stack-frame i) rest)
	      return (debugger:frame-var-name stack-frame i)))))

(defun stack-frame-local-name (stack-frame n &optional pc)
  #+Allegro (declare (ignore pc))
  #+Genera
  (dbg:language-local-name 
    (dbg:find-language-for-frame stack-frame) stack-frame n pc)
  #+Lispworks
  (multiple-value-bind (args-and-locals n-args)
      (lispworks-stack-frame-info stack-frame)
    (declare (ignore n-args))
    (nth 0 (nth n args-and-locals)))
  #+Allegro
  (multiple-value-bind (required optional rest keys n-params n-args)
      (allegro-stack-frame-info stack-frame)
    (declare (ignore required optional rest keys n-params))
    (when (< n (debugger:frame-number-vars stack-frame))
      (values (debugger:frame-var-name stack-frame (+ n n-args)) (+ n n-args)))))


;;; Frame motion

(defun stack-frame-next-nth-open-frame (stack-frame n)
  #+Genera
  (dbg:frame-next-nth-open-frame stack-frame n nil)
  #+Lispworks
  (dbg::with-interesting-frames
     (:hidden-packages dbg::*boring-packages*)
    (cond ((zerop n)
	   stack-frame)
	  ((plusp n)
	   (loop for last-frame = stack-frame then frame
		 for frame = (stack-frame-next-active-frame stack-frame)
		   then (stack-frame-next-active-frame frame)
		 do (cond ((null frame)
			   (return-from stack-frame-next-nth-open-frame
			     last-frame))
			  ((zerop n)
			   (return-from stack-frame-next-nth-open-frame
			     frame))
			  (t
			   (decf n)))))
	  (t	;(minusp n)
	   (loop for last-frame = stack-frame then frame
		 for frame = (stack-frame-previous-active-frame stack-frame)
		   then (stack-frame-previous-active-frame frame)
		 do (cond ((null frame)
			   (return-from stack-frame-next-nth-open-frame
			     last-frame))
			  ((zerop n)
			   (return-from stack-frame-next-nth-open-frame
			     frame))
			  (t
			   (incf n)))))))
  #+Allegro
  (cond ((zerop n)
	 stack-frame)
	((plusp n)
	 (or (top-level::up-frame stack-frame n) stack-frame))
	(t	;(minusp n)
	 (or (top-level::down-frame stack-frame (- n)) stack-frame))))

(defun stack-frame-next-nth-interesting-frame (stack-frame n)
  #+Genera
  (dbg:frame-next-nth-interesting-frame stack-frame n nil t)
  #+Lispworks
  (dbg::with-interesting-frames
     (:hidden-packages dbg::*boring-packages*)
    (cond ((zerop n)
	   stack-frame)
	  ((plusp n)
	   (loop for last-frame = stack-frame then frame
		 for frame = (stack-frame-next-active-frame stack-frame)
		   then (stack-frame-next-active-frame frame)
		 do (cond ((null frame)
			   (return-from stack-frame-next-nth-interesting-frame
			     last-frame))
			  ((zerop n)
			   (return-from stack-frame-next-nth-interesting-frame
			     frame))
			  ((dbg::interesting-frame-p frame)
			   (decf n)))))
	  (t	;(minusp n)
	   (loop for last-frame = stack-frame then frame
		 for frame = (stack-frame-previous-active-frame stack-frame)
		   then (stack-frame-previous-active-frame frame)
		 do (cond ((null frame)
			   (return-from stack-frame-next-nth-interesting-frame
			     last-frame))
			  ((zerop n)
			   (return-from stack-frame-next-nth-interesting-frame
			     frame))
			  ((dbg::interesting-frame-p frame)
			   (incf n)))))))
  #+Allegro
  (cond ((zerop n)
	 stack-frame)
	((plusp n)
	 (or (top-level::up-frame stack-frame n) stack-frame))
	(t	;(minusp n)
	 (or (top-level::down-frame stack-frame (- n)) stack-frame))))

(defun map-backtrace (nframes nskip function
		     &key (interpreter-frames nil))
 (when (null nframes)
   (setq nframes most-positive-fixnum))
 (macrolet ((move-frame (stack-frame)
	      `(if interpreter-frames
		 (stack-frame-previous-active-frame ,stack-frame)
		 (stack-frame-previous-interesting-frame ,stack-frame))))
   (loop with *print-pretty* = nil		;two things filling is too many
	 with count = 0
	 for stack-frame = (debugger-current-frame *application-frame*)
           then (move-frame stack-frame)
	 for index upfrom (- nskip) below nframes
	 until (null stack-frame)
	 do (unless (minusp index)
	      (with-language-for-stack-frame (stack-frame)
		;; In backtraces, always censor uninteresting frames, no
		;; matter what c-N or c-P will do
		(when (or interpreter-frames (stack-frame-interesting-p stack-frame))
		  (funcall function stack-frame count index)
		  (incf count)))))))

(defmethod show-frame-arguments (stack-frame 
				 &key (stream *standard-output*) (indentation 0))
  (declare (values n-args))
  #+Genera
  (let ((*standard-output* stream))
    (dbg:print-frame-args stack-frame indentation t))
  #+Lispworks
  (multiple-value-bind (args-and-locals n-args)
      (lispworks-stack-frame-info stack-frame)
    (loop with expr-type = (stack-frame-expression-type stack-frame *application-frame*)
	  with spaces = (make-string indentation :initial-element #\space)
	  repeat n-args
	  for (name nil nil value) in args-and-locals
	  do (write-string spaces stream)
	     (present name 'form :stream stream)
	     (write-string ": " stream)
	     (present value expr-type :stream stream)
	     (terpri stream)))
  #+Allegro
  (multiple-value-bind (required optional rest keys n-params n-args)
      (allegro-stack-frame-info stack-frame)
    (declare (ignore required optional rest keys n-params))
    (loop with expr-type = (stack-frame-expression-type stack-frame *application-frame*)
	  with spaces = (make-string indentation :initial-element #\space)
	  for i from 0 below n-args
	  as name =  (debugger:frame-var-name stack-frame i)
	  as value = (debugger:frame-var-value stack-frame i)
	  do (unless (and (consp name)
			  (eq (car name) :DEAD))
	       (write-string spaces stream)
	       (present name 'form :stream stream)
	       (write-string ": " stream)
	       (present value expr-type :stream stream)
	     (terpri stream)))))

(defmethod show-frame-locals (stack-frame #+Genera local-start
			      &key (stream *standard-output*) (indentation 0))
  #+Genera
  (let ((*standard-output* stream))
    (dbg:print-frame-locals stack-frame local-start indentation))
  #+Lispworks
  (multiple-value-bind (args-and-locals n-args)
      (lispworks-stack-frame-info stack-frame)
    (loop with expr-type = (stack-frame-expression-type stack-frame *application-frame*)
	  with spaces = (make-string indentation :initial-element #\space)
	  with locals = (nthcdr n-args args-and-locals)
	  for (name nil nil value) in locals
	  do (write-string spaces stream)
	     (present name 'form :stream stream)
	     (write-string ": " stream)
	     (present value expr-type :stream stream)
	     (terpri stream)))
  #+Allegro
  (multiple-value-bind (required optional rest keys n-params n-args)
      (allegro-stack-frame-info stack-frame)
    (declare (ignore required optional rest keys n-params))
    (loop with expr-type = (stack-frame-expression-type stack-frame *application-frame*)
	  with spaces = (make-string indentation :initial-element #\space)
	  for i from 0 below (- (debugger:frame-number-vars stack-frame) n-args)
	  as name =  (debugger:frame-var-name stack-frame (+ i n-args))
	  as value = (debugger:frame-var-value stack-frame (+ i n-args))
	  do (unless (and (consp name)
			  (eql (car name) :DEAD))
	       (write-string spaces stream)
	       (present name 'form :stream stream)
	       (write-string ": " stream)
	       (present value expr-type :stream stream)
	       (terpri stream)))))

(defmethod show-stack-frame-after-motion :around
    ((frame debugger) &key detailed stream)
  (declare (ignore detailed stream))
  #+Genera (let ((stack-frame (debugger-current-frame frame)))
	     (setq dbg:*current-frame* stack-frame
		   dbg:*current-language* (dbg:find-language-for-frame stack-frame)))
  #+Allegro (top-level::set-current-context (debugger-current-frame frame) t)
  (call-next-method))

(defmethod show-stack-frame-after-motion :after
    ((frame debugger) &key detailed stream)
  (declare (ignore detailed stream))
  ;; Set the previous frame for the next time around
  (setf (debugger-previous-frame frame) (debugger-current-frame frame)))

(defmethod show-stack-frame-after-motion
    ((frame debugger) &key detailed (stream *standard-output*))
  (if detailed
    (show-detailed-stack-frame (debugger-current-frame frame)
			       :stream stream)
    (show-stack-frame (debugger-current-frame frame)
		      :stream stream :show-source-file t)))

(defun show-stack-frame (stack-frame
			 &key show-source-file show-pc
			      (stream *standard-output*))
  (with-language-for-stack-frame (stack-frame)
    (let* (#+Genera (dbg:*printing-monitor-message* t)
	   (*print-level* 3)
	   (*print-length* 3)
	   (function (stack-frame-function stack-frame)))
      (print-carefully (stream "function name")
	(terpri stream)
	(show-stack-frame-header stack-frame :show-pc show-pc :stream stream)
	(with-text-face (stream :italic)
	  (loop for func = function then (symbol-function (cadr encaps))
		for delimiter = "  (encapsulated for " then ", "
		as encaps = #+Genera (and (dbg:legitimate-function-p func)
					  (si:debugging-info-user-entry
					    func 'si:encapsulated-definition))
			    #-Genera nil
		while encaps
		do (write-string delimiter stream)
		   (princ (caddr encaps) stream)
		finally (or (eql func function)
			    (write-string ")" stream))))
	(when show-source-file
	  (show-function-source-file function stream)))
      (terpri stream)
      (show-frame-arguments stack-frame :stream stream :indentation 3))))

(defun show-compiled-stack-frame (stack-frame
				  &key show-source-file show-pc
				       (stream *standard-output*))
  (let* (#+Genera (dbg:*printing-monitor-message* t)
	 (function (stack-frame-function stack-frame)))
    (show-stack-frame-header stack-frame :show-pc show-pc :stream stream)
    (when show-source-file
      (show-function-source-file function stream))
    (format stream "~2%")
    ;; Print the arguments, including the rest-arg which is a local
    (let ((local-start (show-frame-arguments stack-frame 
					     :stream stream :indentation 1)))
      #-Genera (declare (ignore local-start))
      (show-frame-locals stack-frame #+Genera local-start
			 :stream stream :indentation 1)
      (show-code-for-stack-frame stack-frame 
				 :headerp :brief
				 :stream stream))))

(defun show-stack-frame-header (stack-frame 
				&key show-pc (stream *standard-output*))
  (let ((function (stack-frame-function stack-frame)))
    (with-text-face (stream :bold)
      (present stack-frame 'stack-frame :stream stream))
    (when (and show-pc
	       (compiled-function-p function))
      (let ((pc-now (stack-frame-relative-exit-pc stack-frame)))
	(when pc-now
	  (format stream #+Genera "  (pc = ~O)"
			 #-Genera "  (pc = ~X)"
	    pc-now))))))

(defun show-detailed-stack-frame (stack-frame 
				  &key clear-window show-source-file 
				       (stream *standard-output*))
  (when clear-window
    (window-clear stream))
  (with-language-for-stack-frame (stack-frame)
    (when clear-window
      (window-clear stream))
    #+Genera
    (let ((cr (sys:frame-own-control-register stack-frame)))
      (when (ldb-test sys:%%cr.cleanup-catch cr)
	(format stream " Frame has open catch or unwind-protect.~%"))
      (when (ldb-test sys:%%cr.cleanup-bindings cr)
	(format stream " Frame has special variable bindings.~%"))
      (when (ldb-test sys:%%cr.call-started cr)
	(format stream " Frame has started some calls.~%"))
      (when (ldb-test sys:%%cr.trap-mode cr)
	(format stream " Trap mode ~[extra-stack~;I/O~;FEP~].~%"
	  (1- (ldb sys:%%cr.trap-mode cr))))
      (when (si:%funcall-in-aux-stack #'storage::trap-on-exit-flag
	      stack-frame (dbg:frame-next-active-frame stack-frame) sys:%current-thread)
	(if (dbg:debugger-trace-flag stack-frame)
	  (format stream " Trap on exit from this frame.~%")
	  (format stream " Presentation cleanup actions on exit from this frame.~%")))
      (format t " Called ~[for effect~;for value~;to return~;for multiple values~]"
	(ldb sys:%%cr.value-disposition cr))
      (when (ldb-test sys:%%cr.apply cr)
	(write-string ", called by apply" stream))
      (format stream ".~2%"))
    (if (compiled-function-p (stack-frame-function stack-frame))
      (show-compiled-stack-frame stack-frame
				 :show-source-file show-source-file
				 :show-pc t
				 :stream stream)
      (show-stack-frame stack-frame 
			:show-source-file show-source-file
			:show-pc t
			:stream stream))))

(defun show-function-source-file (function &optional (stream *standard-output*))
  (with-text-face (stream :italic)
    #+Genera
    (let ((*standard-output* stream))
      (dbg:print-function-source-file function))
    #+Allegro
    (let ((*standard-output* stream)
	  (pathname (excl:source-file function)))
      (when (and pathname (pathnamep pathname))
	(format stream "~&  From ")
	(present (typecase pathname
		   (logical-pathname (translate-logical-pathname pathname))
		   (t pathname)) 'pathname
		 :stream stream)
	(terpri stream)))))

(defparameter *show-source-code* nil)

(defun show-code-for-function (function
			       &key stack-frame headerp warnp
				    (show-source *show-source-code*)
				    (stream *standard-output*))
  (declare (ignore stack-frame warnp))
   (unless (functionp function)
      (setq function (and (fboundp function)
			  (fdefinition function))))
   (when function
     #+Genera
     (let ((*standard-output* stream))
       (if (or show-source stack-frame)
	 (dbg:lframe-show-code-for-function 
	   dbg:*current-language* stack-frame function
	   show-source headerp warnp)
	 (disassemble function)))
     #+Lispworks
     (let* ((*standard-output* stream)
	    (pc (and stack-frame (stack-frame-relative-exit-pc stack-frame)))
	    #+Sun4 (dis::*disassemble-min-pc* (and pc (- pc #x10)))
	    #+Sun4 (dis::*disassemble-max-pc* (and pc (+ pc #x10))))
       ;;--- What about when the user wants source code?
       (disassemble function))
     #+Allegro
     (let* ((*standard-output* stream))
       ;;--- What about when the user wants source code?
       (when headerp 
	 (with-text-face (stream :italic)
	   (format stream "~&Disassembled code for "))
	 (present (function-name function) 'function-spec :stream stream)
	 (write-string ":" stream))
       (if stack-frame
	 ;;---*** How to show the code within a certain range?
	 (write-string " Too much output for the time being!" stream)
	 (disassemble function)))
     #+MCL
     (let ((*debug-io* stream))
       (cond (show-source
	      (let ((lambda (ccl::function-lambda-expression function)))
		(cond (lambda
			  (when headerp 
			    (with-text-face (stream :italic)
			      (format stream "~&Source code for "))
			    (present function 'function-spec :stream stream)
			    (write-string ":" stream))
			(fresh-line stream)
			(let ((sym (typecase function
				     (ccl::compiled-function (function-name function))
				     (symbol function)
				     (t nil))))
			  (cond (sym 
				 (let ((*package* (symbol-package sym)))
				   (pprint lambda stream)))
				(t (pprint lambda stream)))))
		      (t (format stream "~&No source code for ")
			 (present function 'function-spec :stream stream)
			 (write-string "available in the Lisp image." stream)))))
	     (t (when headerp 
		  (with-text-face (stream :italic)
		    (format stream "~&Disassembled code for "))
		  (present function 'function-spec :stream stream)
		  (write-string ":" stream))
		(fresh-line stream)
		(disassemble function))))))

(defun show-code-for-stack-frame (stack-frame &rest keys)
  (declare (dynamic-extent keys))
  (with-language-for-stack-frame (stack-frame)
    (apply #'show-code-for-function (stack-frame-function stack-frame)
				    :stack-frame stack-frame keys)))


;;; Useful commands

(define-debugger-command (com-simple-debugger-help :name "Help")
    (&key (detailed 'boolean 
		    :default nil :mentioned-default t
		    :documentation "Give detailed help"))
  (if detailed
    (com-debugger-help)
    (with-frame-standard-output (stream)
      (format stream
	  "~%You are in the debugger.  Here are some simple commands:~
	   ~%  <Abort>        Take the ABORT restart.~
	   ~%  <Resume>       Take the CONTINUE restart.~
	   ~%  control-<Help> Display the rest of the debugger commands.~
	   ~%  c-m-W          Enter the display debugger frame."))))

(add-keystroke-to-command-table 
  'debugger ':help 
  :command `(com-simple-debugger-help)
  :errorp nil)
 

(define-debugger-command com-debugger-help ()
  (with-frame-standard-output (stream)
    (format stream
        "~
~%  Commands for examining the current stack frame:~
~%    c-L	Redisplay the error message.~
~%    m-L	Show detailed information about this frame.~
~%~
~%    c-m-A	Display an argument in the current frame (use a numeric argument~
~%		to specify which one).~
~%    c-m-sh-A	Show all of the arguments for the current frame.~
~%~
~%    c-m-L	Display a local in the current frame (use a numeric argument to~
~%		specify which one).~
~%    c-m-sh-L	Show all of the locals for the current frame.~
~%~
~%    c-m-F	Display the function in the current frame.~
~%~
~%    c-sh-A	Display the argument list of the function in the current frame.~
~%~
~%   :Show Source Code~
~%		Show the source code for the function in the current frame.~
~%   :Show Compiled Code~
~%		Show the disassembled code for the function in the current frame.~
~%~
~%  Commands for moving around in the backtrace:~
~%    m-<	Move to the top of the stack (the most recent frame).~
~%~
~%    m->	Move to the bottom of the stack (the oldest frame).~
~%~
~%    c-N	Move down a frame (takes numeric argument).~
~%    m-N	Move down a frame, displaying detailed information about it.~
~%~
~%    c-P	Move up a frame (takes numeric argument).~
~%    m-P	Move up a frame, displaying detailed information about it.~
~%~
~%    c-S	Search down from the current frame for one whose function name~
~%		contains a given substring.  With a negative arg, searches backwards.~
~%~
~%  The stack-frame PDL:~
~%   c-Space		Push the current frame onto the stack-frame PDL.~
~%~
~%   c-U c-Space	Pop the stack-frame PDL, and move to that frame.~
~%~
~%   c-U c-U c-Space	Discard the top of the stack-frame PDL.~
~%~
~%   c-m-Space		Exchange the top of the stack-frame PDL with the current frame.~
~%~
~%   c-0 c-Space	Show the contents of the stack-frame PDL.~
~%~
~%  Commands for general information display:~
~%    c-B	Display a brief backtrace.~
~%    m-B	Display a detailed backtrace of the stack.~
~%~
~%    c-m-D	Describe the last thing that set the value of *.~
~%		With a numeric arg, describes that local variable.~
~%~
~%   :Show Bindings~
~%		Show this frame's special variable bindings.  Any argument means~
~%		display every frame's bindings.~
~%   :Show Condition Handlers~
~%		Show this frame's condition handlers.  Any argument means display~
~%		every frame's handlers.~
~%~
~%  Commands to continue execution:~
~%    c-R	Return from the current frame.  Prompts for values as necessary.~
~%~
~%    c-m-R	Reinvoke the current frame.  A numeric argument means to prompt~
~%		for new arguments.~
~%~
~%    <Resume>	Take the CONTINUE restart.~
~%~
~%    <Abort>	Take the ABORT restart.~
~%~
~%  Trap on frame exit commands:~
~%    c-m-X	Set trap-on-exit for the current frame.  A numeric argument~
~%		sets trap-on-exit for all outer frames, too.~
~%~
~%    c-m-sh-X	Clear trap-on-exit for the current frame.  A numeric argument~
~%		clears trap-on-exit for all outer frames, too.~
~%~
~%  Other commands:~
~%    c-E	Call the editor to edit the current function.~
~%~
~%    c-M	Enter a mail sending context, putting stack frames into the message body.~
~%~
~%    c-m-W	Enter the Display Debugger frame.~%")))

#+Genera (add-keystroke-to-command-table 
           'debugger '(:help :control) 
	   :command `(com-debugger-help)
	   :errorp nil)

#-Genera (add-keystroke-to-command-table 
           'debugger '(:? :shift :meta) 
	   :command `(com-debugger-help)
	   :errorp nil)

(define-debugger-command (com-resume :name t) ()
  (with-frame-standard-output (stream)
    (let ((restarts (debugger-restarts *application-frame*)))
      (dolist (restart (reverse restarts)
	       (format stream "There is no restart for ~S" 'continue))
	(when (eql (restart-name restart) 'continue)
	  (invoke-restart restart))))))

(add-keystroke-to-command-table 
  'debugger ':resume :command `(com-resume)
  :errorp nil)

(define-debugger-command (com-abort :name t) ()
  (with-frame-standard-output (stream)
    (let ((restarts (debugger-restarts *application-frame*)))
      (dolist (restart restarts
		(format stream "There is no restart for ~S" 'abort))
	(when (eql (restart-name restart) 'abort)
	  (invoke-restart restart))))))

(add-keystroke-to-command-table 
  'debugger ':abort :command `(com-abort)
  :errorp nil)

(define-debugger-command (com-invoke-restart :name t)
    ((restart 'restart-name :gesture :select))
  (let* ((current-frame (debugger-current-frame *application-frame*))
         (form-type (stack-frame-form-type current-frame *application-frame*))
	 (evaluator (stack-frame-eval-function current-frame *application-frame*))
         (values nil)
         (known-restart nil))
    #+(or Allegro Genera)
    (case (restart-name (find-restart restart))
      ((#+Allegro excl::return-value
	#+Genera conditions:use-value)
       (setq values
	     (list (funcall evaluator
		            (accept form-type :stream *standard-input*
			            :prompt "Enter a value to return")
			    current-frame)))
       (setq known-restart t))
      #+Genera
      ((conditions:store-value)
       (setq values
	     (list (funcall evaluator
		            (accept form-type :stream *standard-input*
			            :prompt "Enter a value to store")
			    current-frame)))
       (setq known-restart t))
      ((#+Allegro excl::try-a-different-function
	#+Allegro excl::try-a-different-function-setf)
       (setq values
	     (list (funcall evaluator
		            (accept form-type :stream *standard-input*
			            :prompt "Enter a form that evaluates to the function to call")
			    current-frame)))
       (setq known-restart t)))
    (if known-restart
      (apply #'invoke-restart restart values)
      (invoke-restart-interactively restart))))

(define-debugger-command (com-describe-error :name t) ()
  (with-application-frame (frame)
    (with-frame-standard-output (stream)
      (describe-error frame stream))))


(define-debugger-command (com-describe-frame :name t)
    (&key (detailed 'boolean 
		    :default nil :mentioned-default t
		    :documentation "Give a detailed description of the frame"))
  (with-frame-standard-output (stream)
    (cond (detailed
	   (show-detailed-stack-frame (debugger-current-frame *application-frame*)
				      :show-source-file t :stream stream))
	  (t
	   (describe-error *application-frame* stream
			   :describe-restarts nil)
	   (show-stack-frame (debugger-current-frame *application-frame*)
			     :show-source-file t :stream stream)
	   (describe-restarts *application-frame* stream)))))

(add-keystroke-to-command-table 
  'debugger '(:l :control) :command `(com-describe-frame :detailed nil)
  :errorp nil)
 
(add-keystroke-to-command-table 
  'debugger '(:l :meta) :command `(com-describe-frame :detailed t)
  :errorp nil)

(define-debugger-command (com-show-backtrace :name t)
    (&key
     (nframes 'integer
	      :default most-positive-fixnum
	      :documentation "Show of backtrace of the given length")
     (detailed 'boolean 
	       :default nil :mentioned-default t
	       :documentation "Show frame locals and code")
     (interpreter-frames 'boolean
			 :default nil :mentioned-default t
			 :documentation "Show interpreter frames"))
  (with-frame-standard-output (stream)
    (when (= nframes 1)				;--- CLIM needs "numeric-arg-p"
      (setq nframes most-positive-fixnum))
    (if detailed
      (map-backtrace nframes 0
		     #'(lambda (stack-frame count index)
			 (declare (ignore count index))
			 (show-stack-frame stack-frame
					   :show-source-file t :stream stream))
		     :interpreter-frames interpreter-frames)
      (short-backtrace :nframes nframes
		       :interpreter-frames interpreter-frames
		       :stream stream))))

(add-keystroke-to-command-table
  'debugger '(:b :control) 
  :command `(com-show-backtrace :nframes ,*numeric-argument-marker* :detailed nil)
  :errorp nil)

(add-keystroke-to-command-table
  'debugger '(:b :meta) 
  :command `(com-show-backtrace :nframes ,*numeric-argument-marker* :detailed t)
  :errorp nil)


;; Go to the top of the stack, i.e., the newest frame
(define-debugger-command (com-top-of-stack :name "Top of Stack")
    (&key (detailed 'boolean 
		    :default nil :mentioned-default t
		    :documentation "Show frame locals and code"))
  ;; Make sure the innermost frame is interesting to the language
  (with-frame-standard-output (stream)
    (push-stack-frame-pdl *application-frame*)
    (let* ((innermost-frame (debugger-initial-frame *application-frame*))
	   (stack-frame 
	     #-Allegro
	     (if (or #+Genera (eql (stack-frame-function innermost-frame) #'si:*eval)
		     (stack-frame-interesting-p innermost-frame))
	       innermost-frame
	       (stack-frame-previous-interesting-frame innermost-frame))
	     #+Allegro
	     (do ((f innermost-frame (excl::int-next-older-frame f)))
		 ((or (null f) (db:frame-visible-p f))
		  (or f innermost-frame)))))
      (setf (debugger-current-frame *application-frame*) stack-frame)
      (show-stack-frame-after-motion *application-frame*
				     :detailed detailed :stream stream))))

(add-keystroke-to-command-table 
  'debugger '(:< :meta :shift)
  :command `(com-top-of-stack)
  :errorp nil)

;; Go to the bottom of the stack, i.e., the oldest frame
(define-debugger-command (com-bottom-of-stack :name "Bottom of Stack")
    (&key (detailed 'boolean 
		    :default nil :mentioned-default t
		    :documentation "Show frame locals and code"))
  (with-frame-standard-output (stream)
    (push-stack-frame-pdl *application-frame*)
    (let ((stack-frame
	   #+Genera (stack-frame-next-active-frame nil)
	   #+Allegro (excl::int-oldest-frame top-level::*tpl-current-thread*)
	   #-(or Genera Allegro)
	   (stack-frame-next-nth-open-frame
	     (debugger-initial-frame *application-frame*)
	     most-negative-fixnum)))
      (setf (debugger-current-frame *application-frame*) stack-frame)
      (show-stack-frame-after-motion *application-frame*
				     :detailed detailed :stream stream))))

(add-keystroke-to-command-table 
  'debugger '(:> :meta :shift) 
  :command `(com-bottom-of-stack)
  :errorp nil)

;; Go to the next older frame, i.e., towards the bottom of the stack
(define-debugger-command (com-next-frame :name t)
    (&key
     (nframes 'integer
	      :default 1
	      :documentation "Move the specified number of frames")
     (detailed 'boolean 
	       :default nil :mentioned-default t
	       :documentation "Show frame locals and code")
     (interpreter-frames 'boolean
			 :default nil :mentioned-default t
			 :documentation "Show interpreter frames"))
  (with-frame-standard-output (stream)
    (labels ((move-frame (n)
	       (if interpreter-frames
		 (stack-frame-next-nth-open-frame
		   (debugger-current-frame *application-frame*) n)
		 (stack-frame-next-nth-interesting-frame 
		   (debugger-current-frame *application-frame*) n)))
	     (up-stack ()
	       (let ((stack-frame (move-frame (- nframes))))
		 (cond ((stack-frame-eql stack-frame (debugger-current-frame *application-frame*))
                        (fresh-line stream)
			(write-string "You are already at the bottom of the stack." stream))
		       (t
			(setf (debugger-current-frame *application-frame*) stack-frame))))))
      (declare (dynamic-extent #'move-frame #'up-stack))
      (up-stack)
      (show-stack-frame-after-motion *application-frame*
				     :detailed detailed :stream stream))))

(add-keystroke-to-command-table
  'debugger '(:n :control) 
  :command `(com-next-frame :nframes ,*numeric-argument-marker* :detailed nil)
  :errorp nil)

(add-keystroke-to-command-table
  'debugger '(:n :meta) 
  :command `(com-next-frame :nframes ,*numeric-argument-marker* :detailed t)
  :errorp nil)

;; Go to the next newer frame, i.e., towards the top of the stack
(define-debugger-command (com-previous-frame :name t)
    (&key
     (nframes 'integer
	      :default 1
	      :documentation "Move the specified number of frames")
     (detailed 'boolean 
	       :default nil :mentioned-default t
	       :documentation "Show frame locals and code")
     (interpreter-frames 'boolean
			 :default nil :mentioned-default t
			 :documentation "Show interpreter frames"))
  (with-frame-standard-output (stream)
    (labels ((move-frame (n)
	       (if interpreter-frames
		 (stack-frame-next-nth-open-frame 
		   (debugger-current-frame *application-frame*) n)
		 (stack-frame-next-nth-interesting-frame
		   (debugger-current-frame *application-frame*) n)))
	     (down-stack ()
	       (let ((stack-frame (move-frame nframes)))
		 (cond ((stack-frame-eql stack-frame (debugger-current-frame *application-frame*))
                        (fresh-line stream)
			(write-string "You are already at the top of the stack." stream))
		       (t
			(setf (debugger-current-frame *application-frame*) stack-frame))))))
      (declare (dynamic-extent #'move-frame #'down-stack))
      (down-stack)
      (show-stack-frame-after-motion *application-frame*
				     :detailed detailed :stream stream))))

(add-keystroke-to-command-table
  'debugger '(:p :control) 
  :command `(com-previous-frame :nframes ,*numeric-argument-marker* :detailed nil)
  :errorp nil)

(add-keystroke-to-command-table
  'debugger '(:p :meta) 
  :command `(com-previous-frame :nframes ,*numeric-argument-marker* :detailed t)
  :errorp nil)

(define-debugger-command (com-find-frame :name t) 
    ((string '(null-or-type string)
	     :default-type 'string :provide-default t)
     &key
     (detailed 'boolean
	       :default nil :mentioned-default t
	       :documentation "Show locals and disassembled code")
     (reverse 'boolean
	      :default nil :mentioned-default t
	      :documentation "Search backwards for the frame"))
  (when (null string)
    (setq string (accept 'string 
			 :provide-default t
			 :prompt "String to search for")))
  (with-frame-standard-output (stream)
    (flet ((move-frame (frame)
	     (if reverse
	       (stack-frame-next-active-frame frame)
	       (stack-frame-previous-active-frame frame))))
      (declare (dynamic-extent #'move-frame))
      ;; STRING can really be a function or a function-spec, too
      (loop for frame = (move-frame (debugger-current-frame *application-frame*))
	      then (move-frame frame)
	    until (null frame)
	    as function-name = (function-name (stack-frame-function frame))
	    do (when (search string (typecase function-name
				      (string function-name)
				      (symbol (string function-name))
				      (otherwise (format nil "~S" function-name)))
			     :test #'char-equal)
		 (push-stack-frame-pdl *application-frame*)
		 (setf (debugger-current-frame *application-frame*) frame)
		 (show-stack-frame-after-motion *application-frame*
						:detailed detailed :stream stream)
		 (return))
	    finally 
              (fresh-line stream)
	      (format stream "No frame matching ~A." string)))))

(add-keystroke-to-command-table 
  'debugger '(:s :control) 
  :function #'(lambda (gesture arg)
		(declare (ignore gesture))
		(if (minusp arg)
		  `(com-find-frame nil :reverse t)
		  `(com-find-frame nil)))
  :errorp nil)

(define-debugger-command (com-set-frame)
    ((stack-frame 'stack-frame 
		  :gesture (:select :priority 2)))
  (push-stack-frame-pdl *application-frame*)
  (setf (debugger-current-frame *application-frame*) stack-frame)
  (with-frame-standard-output (stream)
    (show-stack-frame-after-motion *application-frame* :stream stream)))


(define-debugger-command (com-show-argument :name t)
    ((n '(token-or-type (:all) integer)
	:default :all))
  (with-application-frame (frame)
    (with-frame-standard-output (stream)
      (cond ((eql n :all)
	     (show-frame-arguments (debugger-current-frame frame) :stream stream))
	    (t
	     (multiple-value-bind (val loc)
		 (stack-frame-argument-value (debugger-current-frame frame) n nil)
	       (print-carefully (stream "argument")
		 (if (null loc)
		   (format stream "Argument number ~D out of range" n)
		   (let ((*print-circle* t))
		     (shiftf *** ** * val)
		     (print val stream))))))))))

(add-keystroke-to-command-table 
  'debugger '(:a :control :meta)
  :command `(com-show-argument ,*numeric-argument-marker*)
  :errorp nil)

(add-keystroke-to-command-table 
  'debugger '(:a :control :meta :shift)
  :command `(com-show-argument :all)
  :errorp nil)

(define-debugger-command (com-show-local :name t)
    ((n '(token-or-type (:all) integer)
	:default :all))
  (with-application-frame (frame)
    (with-frame-standard-output (stream)
      (cond ((eql n :all)
	     (show-frame-locals (debugger-current-frame frame) 0 :stream stream))
	    (t
	     (multiple-value-bind (val loc)
		 (stack-frame-local-value (debugger-current-frame frame) n nil)
	       (print-carefully (stream "local")
		 (if (null loc)
		   (format stream "Local number ~D out of range" n)
		   (let ((*print-circle* t))
		     (shiftf *** ** * val)
		     (print val stream))))))))))

(add-keystroke-to-command-table 
  'debugger '(:l :control :meta)
  :command `(com-show-local ,*numeric-argument-marker*)
  :errorp nil)

(add-keystroke-to-command-table 
  'debugger '(:l :control :meta :shift)
  :command `(com-show-local :all)
  :errorp nil)

(define-debugger-command (com-show-frame-arglist :name t) ()
  (with-application-frame (frame)
    (with-frame-standard-output (stream)
      (print-carefully (stream "arglist")
	(let* ((function (stack-frame-function (debugger-current-frame frame)))
	       (arglist (function-arglist function)))
	  (present (function-name function) 'function-spec :stream stream)
	  (write-string ": " stream)
	  (print-lambda-list arglist stream))))))

(add-keystroke-to-command-table 
  'debugger '(:a :control :shift)
  :command `(com-show-frame-arglist)
  :errorp nil)

(define-debugger-command (com-show-frame-function :name t) ()
  (with-application-frame (frame)
    (with-frame-standard-output (stream)
      (let ((function (stack-frame-function (debugger-current-frame frame))))
	(shiftf *** ** * function)
	(print function stream)))))

(add-keystroke-to-command-table 
  'debugger '(:f :control :meta)
  :command `(com-show-frame-function)
  :errorp nil)

(define-debugger-command (com-edit-frame-function :name t)
    ((stack-frame '(null-or-type stack-frame)
		  :default nil :gesture nil))
  (with-application-frame (frame)
    (when (null stack-frame)
      (setq stack-frame (debugger-current-frame frame)))
    #-Lispworks
    (let* ((function (stack-frame-function stack-frame))
	   (function-name (function-name function)))
      (ed function-name))
    #+Lispworks
    (let ((name (dbg::call-frame-function-name stack-frame))
	  (edit-path (dbg::call-frame-edit-path stack-frame)))
      (if edit-path
	(capi::execute-editor-call 'editor::find-subform name edit-path)
	(capi::execute-editor-call 'editor::find-dspec-command nil name)))))

(add-keystroke-to-command-table 
  'debugger '(:e :control) 
  :command `(com-edit-frame-function nil)
  :errorp nil)

(define-presentation-to-command-translator edit-frame-function
   (stack-frame com-edit-definition debugger
    :gesture :edit
    :priority 2)
   (object)
  (list (function-name (stack-frame-function object))))


(define-debugger-command (com-describe-last :name t) ()
  (with-frame-standard-output (stream)
    (describe * stream)))

(define-debugger-command (com-describe-local :name t)
    ((n 'integer :default 0))
  (with-application-frame (frame)
    (with-frame-standard-output (stream)
      (multiple-value-bind (val loc)
	  (stack-frame-local-value (debugger-current-frame frame) n nil)  
	(if (null loc)
	  (format stream "Local number ~D out of range" n)
	  (describe val stream))))))

(add-keystroke-to-command-table
  'debugger '(:d :control :meta) 
  :function #'(lambda (gesture arg)
		(declare (ignore gesture))
		(if (minusp arg)
		  '(com-describe-last)
		  `(com-describe-local ,arg)))
  :errorp nil)

(define-debugger-command (com-return-from-frame :name t)
    ((stack-frame '(null-or-type stack-frame)
		  :default nil :gesture nil))
  (if (null stack-frame)
    (return-from-frame (debugger-current-frame *application-frame*))
    (return-from-frame stack-frame)))

(add-keystroke-to-command-table 
  'debugger '(:r :control)
  :command `(com-return-from-frame nil)
  :errorp nil)

(defun return-from-frame (frame)
  (let (#+Genera (dbg:*current-frame* frame))
    (with-language-for-stack-frame (frame)
      (cond ((not (stack-frame-active-p frame))
	     (write-string "This frame is not active; you cannot return from it" *error-output*))
	    ((null (stack-frame-previous-active-frame frame))
	     (write-string "This is the bottom frame; you cannot return from it" *error-output*))
	    (t
	     (let* ((name (function-name (stack-frame-function frame)))
		    (values
		      (multiple-value-bind (type maxvals)
			  (stack-frame-value-disposition frame)
			(case type
			  (:ignore
                            (fresh-line *query-io*)
			    (write-string "The caller is not interested in any values" *query-io*)
			    (if (y-or-n-p "Return from ~S? " name)
			      nil
			      (return-from return-from-frame)))
			  (:single
                            (fresh-line *query-io*)
			    (format *query-io* "Return a value from the function ~S.~%" name)
			    (list (read-and-verify-expression 
				    frame :prompt "New value to return")))
			  (:multiple
                            (fresh-line *query-io*)
			    (format *query-io* (if maxvals
						 "The caller expects ~R values"
						 "The caller expects any number of values")
			      maxvals)
			    (if (and maxvals (zerop maxvals))
			      (y-or-n-p "Return from ~S? " name)
			      (multiple-value-bind (arglist foundp values-names)
				  (function-arglist (stack-frame-function frame))
				(declare (ignore arglist foundp))
				(read-multiple-values frame maxvals values-names))))))))
	       #+Genera (dbg:return-from-frame frame values)
               #+Lispworks (lispworks-return-from-frame frame values)
	       #+Allegro (allegro-return-from-frame frame values)))))))
#|
#+Lispworks
(defun lispworks-return-from-frame (frame values)
  (dbg::exiting-debugger)
  (let ((offset
         (sys:raw-int-fixnum
	   (sys:raw-- (dbg::stack-address 
		        (dbg::frame-start (dbg::frame-next frame)) 0)
		      sys:*%current-stack)))
	(preserved-regs-stuff
	 #-harp::wsparc (nconc (multiple-value-list
				 (dbg::find-preserved-registers frame))
			       (list (dbg::call-frame-preserved-registers-count frame)))))
    (sys::throw-to-address (list offset 'dbg::in-in-return-from-frame
				 frame 
				 values
				 preserved-regs-stuff)
			   lw::*debugger-execute-unwind-protect*)))
|#


#+Allegro
(defun allegro-return-from-frame (frame values)
  (if (= 0 top-level::*break-level*)
      (format *terminal-io* "Can't return from break level 0.~%")
    (apply #'db:frame-return frame values))
  )


(define-debugger-command (com-reinvoke-frame :name t)
    ((stack-frame '(null-or-type stack-frame)
		  :default nil :gesture nil)
     &key 
     (new-arguments 'boolean
		    :default nil :mentioned-default t
		    :documentation "Supply new arguments"))
  (if (null stack-frame)
    (reinvoke-frame (debugger-current-frame *application-frame*) new-arguments)
    (reinvoke-frame stack-frame new-arguments)))

(add-keystroke-to-command-table 
  'debugger '(:r :control :meta) 
  :function #'(lambda (gesture arg)
		(declare (ignore gesture))
		(if (= arg 1)			;--- CLIM needs "numeric-arg-p"
		  `(com-reinvoke-frame nil)
		  `(com-reinvoke-frame nil :new-arguments t)))
  :errorp nil)

(defun reinvoke-frame (stack-frame &optional new-args)
  (cond (new-args
	 (multiple-value-bind (function args)
	     (stack-frame-function-and-args stack-frame)
	   (let ((new-args (read-new-arguments stack-frame function args))
		 (*print-level* 3)
		 (*print-length* 3))
	     (when (y-or-n-p "~%Restart call to ~S~@[ on arguments ~S~]? "
			     (function-name function) new-args)
	       #+Genera
	       (loop for i upfrom 0
		     for arg in new-args
		     do (setf (stack-frame-argument-value stack-frame i) arg)
		     finally (sys:%restart-frame stack-frame function))
               #+Lispworks 
               (loop for i upfrom 0
		     for arg in new-args
		     do (setf (stack-frame-argument-value stack-frame i) arg)
		     finally (progn
                               ;; Ensure proper state for LispWorks debugger
                               (setf (dbg::debugger-stack-current-frame dbg::*debugger-stack*)
                                     stack-frame)
                               (dbg::restart-frame stack-frame :same-args t)))
	       #+Allegro
	       (apply #'db:frame-retry stack-frame function new-args)))))
	(t
	 (multiple-value-bind (function args)
	     (stack-frame-function-and-args stack-frame)
	   #+Allegro (declare (ignore args))
	   (let ((*print-level* 3)
		 (*print-length* 3))
	     (when (y-or-n-p "Restart call to ~S~@[ on arguments ~S~]? "
			     (function-name function) #-Allegro args #+Allegro new-args)
	       #+Genera 
               (progn
		 (dbg:restore-implied-arguments stack-frame)
		 (sys:%restart-frame stack-frame function))
                #+Lispworks
                (progn
		  ;; Ensure proper state for LispWorks debugger
		  (setf (dbg::debugger-stack-current-frame dbg::*debugger-stack*)
			stack-frame)
		  (dbg::restart-frame stack-frame :same-args t))
		#+Allegro
		(progn
		  (apply #'db:frame-retry stack-frame function new-args))))))))

;; Return list of the function and args that were invoked (as best as it can).
(defun stack-frame-function-and-args (stack-frame)
  (declare (values function args))
  #+Genera
  (let ((function-and-args (dbg:get-frame-function-and-args stack-frame)))
    (values (dbg:real-function-definition (first function-and-args))
	    (rest function-and-args)))
  #+Lispworks
  (let ((function (stack-frame-function stack-frame)))
    (multiple-value-bind (args-alist n-args)
	(lispworks-stack-frame-info stack-frame)
      (loop repeat n-args
	    for (nil nil nil value) in args-alist
	    collect value into values
	    finally (return (values function values)))))
  #+Allegro
  (values (car (debugger:frame-expression stack-frame))
	  (cdr (debugger:frame-expression stack-frame))))


(define-presentation-type evaluated-expression ()
  :inherit-from 'expression)

(defconstant *newline-marker* '(:newline))
(defconstant *end-marker* '(:end))
(define-presentation-method accept
    ((type evaluated-expression) stream (view textual-view) &key)
  (let ((start-position (stream-scan-pointer stream))
	(char (stream-peek-char stream))
        ;;--- Should use STACK-FRAME-EXPRESSION-TYPE
	(ptype `((expression) :auto-activate ,*auto-activate-expressions*)))
    (with-input-context (ptype) (expression)
	 (progn
	   (when (clim-internals::keyboard-event-matches-gesture-name-p char :newline)
	     (return-from accept *newline-marker*))
	   (when (clim-internals::keyboard-event-matches-gesture-name-p char :end)
	     (return-from accept *end-marker*))
	   (let ((expression (accept ptype
				     :stream stream :prompt nil)))
	     expression))
       (t (presentation-replace-input stream expression type view
				      :buffer-start start-position)
	  expression))))

(defun read-and-verify-expression (stack-frame 
				   &key (stream *query-io*) default prompt)
  (declare (values value flag))
  (loop
    (let ((expression (accept 'evaluated-expression 
			      :stream stream :default default :prompt prompt)))
      (when (or (eql expression *newline-marker*)
		(eql expression *end-marker*))
	(return-from read-and-verify-expression
	  (values nil (first expression))))
      (let ((value (first (funcall (stack-frame-eval-function stack-frame *application-frame*)
				   expression stack-frame))))
	(if (or (constantp expression)
		(y-or-n-p "Value is ~S, OK? " value))
	  (return-from read-and-verify-expression
	    (values value :value))
	  (fresh-line stream))))))

(defun read-multiple-values (stack-frame max-values values-names
			     &key (stream *query-io*))
  (let ((values nil)
	(n-values 0))
    (fresh-line stream)
    (write-string "Enter values, ending with <End>" stream)
    (loop
      (when (and max-values (>= n-values max-values))
	(return-from read-multiple-values (nreverse values)))
      (let ((name (nth n-values values-names)))
	(fresh-line stream)
	(multiple-value-bind (value flag)
	    (read-and-verify-expression 
	      stack-frame
	      :prompt (format nil "Value #~D~@[ (~A)~]" n-values name))
	  (incf n-values)
	  (when (or (eql flag :newline) (eql flag :end))
	    (return-from read-multiple-values (nreverse values)))
	  (push value values))))))

(defun read-new-arguments (frame function old-arguments 
			   &key (stream *query-io*))
  (flet ((args-info (function)
	   (declare (values min-args max-args rest-p))
	   #+Genera
	   (let* ((args-info (sys:args-info function )))
	     (values (ldb sys:%%arg-desc-min-args args-info) 
		     (ldb sys:%%arg-desc-max-args args-info)
		     (ldb-test sys:%%arg-desc-rest-arg args-info)))
	   #-Genera (function-args-info function)))
    (declare (dynamic-extent #'args-info))
    (multiple-value-bind (min-args max-args rest-p) 
	(args-info function)
      ;;--- This is truly dreadful code!
      (loop for arg-no below (if rest-p most-positive-fixnum max-args)
	    for old-args = old-arguments then (cdr old-args)
	    when (and rest-p (= arg-no max-args) old-args
		      (y-or-n-p "~&Use old arguments ~S for rest arg? " old-args))
	      append old-args
	      and do (incf arg-no (length old-args))
		     (setq old-args nil)
	    with form and flag
	    do (fresh-line stream)
	       (multiple-value-setq (form flag)
		 (read-and-verify-expression 
		   frame
		   :default (first old-args)
		   :prompt (format nil "Value for arg #~D~:[~@[ (~A)~]~; (in rest arg)~]~
~:[~; (Or <End> to end arguments)~]"
			     arg-no (>= arg-no max-args)
			     (and (< arg-no max-args)
				  (stack-frame-argument-name frame arg-no))
			     (>= arg-no min-args))))
	    collect (cond ((eql flag :end)
			   (when old-args
			     (write-string "[End]" stream))
			   (loop-finish))
			  ((eql flag :newline)
			   (let ((value (first old-args)))
			     (prin1 value stream)))
			  (t form))))))

#-Genera
(defun function-args-info (function)
  (let ((lambda-list (function-arglist function))
        (n-required 0)
        (n-optional 0)
        (rest-arg-p nil))
    (map-over-lambda-list
      lambda-list
      #'(lambda (args kind)
          (declare (ignore args))
          (case kind
            (:required (incf n-required))
            (:optional (incf n-optional))
            ((:key :rest) (setq rest-arg-p t)))))
    (values n-required (+ n-required n-optional) rest-arg-p)))


(define-debugger-command (com-set-trap-on-exit :name t)
    ((stack-frame '(null-or-type stack-frame)
		  :default nil :gesture nil)
     &key 
     (all 'boolean
	  :default nil :mentioned-default t
	  :documentation "Set trap-on-exit for entire stack"))
  (cond ((null stack-frame)
	 (if all
	   (loop for frame = stack-frame then (stack-frame-previous-active-frame frame)
		 until (null frame)
		 do (set-trap-on-exit frame t))
	   (set-trap-on-exit (debugger-current-frame *application-frame*) t)))
	(t
	 (set-trap-on-exit stack-frame t))))

(add-keystroke-to-command-table 
  'debugger '(:x :control :meta)
  :function #'(lambda (gesture arg)
		(declare (ignore gesture))
		(if (= arg 1)			;--- CLIM needs "numeric-arg-p"
		  `(com-set-trap-on-exit nil)
		  `(com-set-trap-on-exit nil :all t)))
  :errorp nil)

(define-debugger-command (com-clear-trap-on-exit :name t)
    ((stack-frame '(null-or-type stack-frame)
		  :default nil :gesture nil)
     &key 
     (all 'boolean
	  :default nil :mentioned-default t
	  :documentation "Clear trap-on-exit for entire stack"))
  (cond ((null stack-frame)
	 (if all
	   (loop for frame = stack-frame then (stack-frame-previous-active-frame frame)
		 until (null frame)
		 do (set-trap-on-exit frame nil))
	   (set-trap-on-exit (debugger-current-frame *application-frame*) nil)))
	(t
	 (set-trap-on-exit stack-frame nil))))

(add-keystroke-to-command-table 
  'debugger '(:x :control :meta :shift) 
  :function #'(lambda (gesture arg)
		(declare (ignore gesture))
		(if (= arg 1)			;--- CLIM needs "numeric-arg-p"
		  `(com-clear-trap-on-exit nil)
		  `(com-clear-trap-on-exit nil :all t)))
  :errorp nil)

(defun set-trap-on-exit (stack-frame trap-p)
  #+Genera (setf (dbg:debugger-trace-flag stack-frame) trap-p)
  #+Lispworks
  (if trap-p
    (dbg::in-dbg-trap-on-exit stack-frame)
    ;;--- No way to do this right now in LispWorks
    (format *query-io* "~%Clear trap-on-exit not implemented"))
  #+Allegro
  (if trap-p
    (top-level::set-break-on-exit stack-frame t)
    (top-level::unset-break-on-exit stack-frame)))


(define-debugger-command (com-pop-frame-pdl :name "Pop Frame PDL")
    (&key (discard 'boolean
		   :default nil :mentioned-default t
		   :documentation "Discard the top item on the frame PDL"))
  (let ((popped-frame (pop-stack-frame-pdl *application-frame*)))
    (when (and popped-frame (not discard))
      (setf (debugger-current-frame *application-frame*) popped-frame)
      (with-frame-standard-output (stream)
	(show-stack-frame-after-motion *application-frame* :stream stream)))))

(define-debugger-command (com-push-frame-pdl :name "Push Frame PDL") ()
  (push-stack-frame-pdl *application-frame*))

(define-debugger-command (com-exchange-frame-pdl :name "Exchange Frame PDL") ()
  (let* ((pdl (debugger-stack-frame-pdl *application-frame*))
	 (index (fill-pointer pdl)))
    (with-frame-standard-output (stream)
      (cond ((zerop index)
             (fresh-line stream)
	     (write-string "The stack-frame PDL is empty." stream))
	    (t
	     (rotatef (aref pdl index) (debugger-current-frame *application-frame*))
	     (show-stack-frame-after-motion *application-frame* :stream stream))))))

(define-debugger-command (com-show-frame-pdl :name "Show Frame PDL") ()
  (with-frame-standard-output (stream)
    (let ((pdl (debugger-stack-frame-pdl *application-frame*)))
      (cond ((zerop (fill-pointer pdl))
             (fresh-line stream)
	     (write-string "The stack-frame PDL is empty." stream))
            (t
	     (do ((i 1 (1+ i)))
		 ((>= i (fill-pointer pdl)))
	       (fresh-line stream)
	       (present (aref pdl i) 'stack-frame
			:stream stream)))))))

(add-keystroke-to-command-table 
  'debugger '(:space :control) 
  :function #'(lambda (gesture arg)
		(declare (ignore gesture))
		(cond ((= arg 1)
		       `(com-push-frame-pdl))
		      ((= arg 0)
		       `(com-show-frame-pdl))
		      ((= arg 4.)
		       `(com-pop-frame-pdl))
		      ((= arg 16.)
		       `(com-pop-frame-pdl :discard t))
		      (t nil)))
  :errorp nil)

(add-keystroke-to-command-table
  'debugger '(:space :control :meta)
  :command '(com-exchange-frame-pdl)
  :errorp nil)

(defun push-stack-frame-pdl (debugger)
  (let* ((pdl (debugger-stack-frame-pdl debugger))
	 (index (fill-pointer pdl))
	 (highest-index (1- (array-dimension pdl 0))))
    (declare (type vector pdl))
    (when (>= index highest-index)
      ;; We've used up the PDL, push the whole thing back
      (loop for i upfrom 1 below (1- index)
	    do (setf (aref pdl i) (aref pdl (1+ i))))
      (setq index (1- highest-index)))
    (incf index)
    (setf (aref pdl index) (debugger-current-frame debugger))
    (setf (fill-pointer pdl) index)))

(defun pop-stack-frame-pdl (debugger)
  (with-frame-standard-output (stream)
    (let* ((pdl (debugger-stack-frame-pdl debugger))
	   (index (fill-pointer pdl)))
      (cond ((zerop index)
             (fresh-line stream)
	     (write-string "The stack-frame PDL is empty." stream)
	     nil)
            (t
	     (prog1 (aref pdl index)
	            (setf (fill-pointer pdl) (1- index))))))))


(define-debugger-command (com-show-bindings :name t)
    (&key
     (all 'boolean
	  :default nil :mentioned-default t
	  :documentation "Show bindings for entire stack")
     (matching '(null-or-type string)
	       :default nil
	       :documentation "Show only specials matching string"))
  (with-frame-standard-output (stream)
    #+Genera (let ((*standard-output* stream)
		   (dbg:*current-frame* (debugger-current-frame *application-frame*)))
	       (dbg:show-frame-bindings (not all) matching))))

(define-debugger-command (com-show-condition-handlers :name t)
    (&key (all 'boolean
	       :default nil :mentioned-default t
	       :documentation "Show handlers for entire stack"))
  (with-frame-standard-output (stream)
    #+Genera (let ((*standard-output* stream)
		   (dbg:*current-frame* (debugger-current-frame *application-frame*)))
	       (dbg:com-show-condition-handlers :all all))))


(define-debugger-command (com-use-frame-environment :name t)
    ((boolean 'boolean
	      :default (not (debugger-use-frame-environment *application-frame*))))
  (with-frame-standard-output (stream)
    (setf (debugger-use-frame-environment *application-frame*) boolean)
    (fresh-line stream)
    (if boolean
      (write-string "Now inheriting frame's lexical environment" stream)
      (write-string "No longer inheriting frame's lexical environment" stream))))


(define-debugger-command (com-mail-bug-report :name t)
    (&key (nframes '(token-or-type (:all :default) integer)
		   :default :default
		   :documentation "Number of frames in backtrace"))
  (when (eql nframes 1)
    (setq nframes :default))
  (let ((error (debugger-condition *application-frame*)))
    (mail-bug-report 
      :process-name "Mail from debugger"
      :system (or #+Genera dbg:*default-bug-report-recipient-system*
		  (bug-report-recipient-system error))
      :prompt "Please explain the circumstances that led to this bug, and how it can be reproduced"
      :additional-body (with-output-to-string (stream)
			 (bug-report-description error stream nframes))
      :point-before-additional-body t)))

(add-keystroke-to-command-table
  'debugger '(:m :control) 
  :function #'(lambda (gesture arg)
		(declare (ignore gesture))
		(if (eql arg 1)
		  `(com-mail-bug-report)
		  `(com-mail-bug-report :nframes ,*numeric-argument-marker*)))
  :errorp nil)

(defun mail-bug-report (&key process-name system prompt
			     additional-body point-before-additional-body)
  #+Genera (let ((dbg:*printing-monitor-message* t))
	     (dbg:mail-bug-report-1
	       :process-name process-name
	       :system system
	       :prompt prompt
	       :additional-body additional-body
	       :point-before-additional-body point-before-additional-body)))

(defun bug-report-recipient-system (error)
  #+Genera (dbg:bug-report-recipient-system error))

(defun bug-report-description (error stream nframes)
  #+Genera (let ((nframes (case nframes
			    (:all t)
			    (:default nil)
			    (otherwise nframes))))
	     (dbg:bug-report-description error stream nframes)))


;;; Methods for the debugger frame

(defmacro make-command-from-selected-frame (command-name)
  `#'(lambda (gesture arg)
       gesture arg
       (make-command-from-selected-frame-1 ',command-name)))

(defun make-command-from-selected-frame-1 (command-name)
  (with-application-frame (frame)
    (let ((object (car (first (frame-selected-objects frame))))
          (nobjects (length (frame-selected-objects frame))))
      (deselect-all-objects frame)
      (if (and (= nobjects 1)
	       (presentation-typep object 'stack-frame))
	`(,command-name ,object)
	`(,command-name ,(debugger-current-frame frame))))))

(add-menu-item-to-command-table
  'debugger-stack-frames "Next" :command `(com-next-frame)
  :errorp nil)

(add-menu-item-to-command-table
  'debugger-stack-frames "Previous" :command `(com-previous-frame)
  :errorp nil)

(add-menu-item-to-command-table
  'debugger-stack-frames "Top" :command `(com-top-of-stack)
  :errorp nil)

(add-menu-item-to-command-table
  'debugger-stack-frames "Bottom" :command `(com-bottom-of-stack)
  :errorp nil)

(add-menu-item-to-command-table 'debugger-stack-frames "motion-divider" 
				:divider nil
				:errorp nil)

(add-menu-item-to-command-table
  'debugger-stack-frames "Return" 
  :function (make-command-from-selected-frame com-return-from-frame)
  :errorp nil)

(add-menu-item-to-command-table
  'debugger-stack-frames "Reinvoke" 
  :function (make-command-from-selected-frame com-reinvoke-frame)
  :errorp nil)

(add-menu-item-to-command-table
  'debugger-stack-frames "Trap on Exit" 
  :function (make-command-from-selected-frame com-set-trap-on-exit)
  :errorp nil)

(add-menu-item-to-command-table 'debugger-stack-frames "ops-divider" 
				:divider nil
				:errorp nil)

(add-menu-item-to-command-table
  'debugger-stack-frames "Edit" 
  :function (make-command-from-selected-frame com-edit-frame-function)
  :errorp nil)

(add-menu-item-to-command-table
  'debugger-restarts "Resume" :command `(com-resume)
  :errorp nil)

(add-menu-item-to-command-table
  'debugger-restarts "Abort" :command `(com-abort)
  :errorp nil)


(define-debugger-command (com-enter-display-debugger :name t) ()
  (with-application-frame (frame)
    (enter-debugger (debugger-condition frame) *standard-input*
		    :own-frame t 
		    :stack-frame (debugger-initial-frame frame))))

(add-keystroke-to-command-table 
  'debugger '(:w :control :meta)
  :command `(com-enter-display-debugger)
  :errorp nil)

(defmethod show-stack-frame-after-motion
    ((frame debugger-frame) &key detailed stream)
  (declare (ignore detailed stream))
  (let* ((stream (get-frame-pane frame 'backtrace))
	 (history (stream-output-history stream))
	 (old-frame (debugger-previous-frame frame))
	 (old-index (find-text-scroll-element
		      history
		      #'(lambda (presentation)
			  (stack-frame-eql (presentation-object presentation) old-frame))))
	 (new-frame (debugger-current-frame frame))
	 (new-index (find-text-scroll-element
		      history
		      #'(lambda (presentation)
			  (stack-frame-eql (presentation-object presentation) new-frame)))))
    ;; Unhighlight the old frame, highlight the new one
    (when old-index
      (replace-text-scroll-element
        history (stack-frame-text-scroll-item old-frame stream :roman) old-index))
    (when new-index
      (replace-text-scroll-element
        history (stack-frame-text-scroll-item new-frame stream :bold) new-index))
    (display-debugger-show-args-and-locals frame)
    (display-debugger-source-code frame)))

;; Make sure the text scroll item is the stack-frame presentation
;; so it's easy to find later
(defun stack-frame-text-scroll-item (stack-frame stream face)
  (with-output-to-output-record
      (stream 'standard-presentation presentation
       :object stack-frame
       :type 'stack-frame)
    presentation
    (with-text-face (stream face)
      (print-carefully (stream "function name")
	(let ((function (stack-frame-function stack-frame))
	      (*print-length* nil)
	      (*print-level* nil))
	  (with-output-as-presentation (stream function 'function-spec
					:single-box :highlighting)
	    (prin1 (function-name function) stream)))))))

;;--- This should really use incremental redisplay.  When it does, fix up
;;--- (SETF STACK-FRAME-ARGUMENT-VALUE) and (SETF STACK-FRAME-LOCAL-VALUE)
(defmethod display-debugger-show-args-and-locals ((frame debugger-frame))
  (let ((stream (get-frame-pane frame 'locals))
	(stack-frame (debugger-current-frame frame)))
    (window-clear stream)
    (silica:inhibit-updating-scroll-bars #+Allegro (stream)
      (let ((local-start (show-frame-arguments stack-frame :stream stream)))
	#-Genera (declare (ignore local-start))
	(show-frame-locals stack-frame #+Genera local-start :stream stream)))))

(defmethod display-debugger-source-code ((frame debugger-frame))
  (let ((stream (get-frame-pane frame 'code))
	(stack-frame (debugger-current-frame frame)))
    (window-clear stream)
    (silica:inhibit-updating-scroll-bars #+Allegro (stream)
      (show-code-for-stack-frame stack-frame :stream stream))))
