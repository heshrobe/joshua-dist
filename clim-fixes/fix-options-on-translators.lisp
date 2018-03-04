;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The problem: :echo and :main-history options on command translators are lost
;;;
;;;   Analysis: The accept method for commands binds the input-context and gets the 
;;;             click.  The options wind up there and aren't returned to accept-1
;;;   Fix:      Return the options from the accept method and bind them in the
;;;             normal-continuation in accept-1.


;;; from accept.lisp
(defun accept-1 (stream type
		 &key (view (stream-default-view stream))
		      (default nil default-supplied-p)
		      (default-type type)
		      ((:history history-type) type)
		      (insert-default nil) (replace-input t replace-supplied-p)
		      (prompt t)
		      (present-p nil)
		      (query-identifier nil)
		      (activation-gestures nil activation-gestures-p)
		      (additional-activation-gestures nil)
		      (delimiter-gestures nil delimiter-gestures-p)
		      (additional-delimiter-gestures nil)
		      (active-p t)
		 &allow-other-keys)

  ;; Set up the input editing environment
  (let ((the-object nil)
	(the-type nil)
	(the-options nil)
	(activated t)
	(history nil))

    (cond ((typep history-type 'basic-history)
	   (setq history history-type
		 history-type type))
	  (history-type
	   (setq history (presentation-type-history history-type))))

    ;; Inside ACCEPTING-VALUES, ACCEPT can turn into PRESENT
    (when present-p
      (return-from accept-1
	(accept-present-default type stream view default default-supplied-p
				present-p query-identifier
				:prompt prompt :active-p active-p)))

    (block input-editing
      (flet ((input-sensitizer (continuation stream)
	       (declare (dynamic-extent continuation))
	       (if (stream-recording-p stream)
		   (with-output-as-presentation (stream the-object (or the-type type))
		     (funcall continuation stream))
		   (funcall continuation stream))))
	(declare (dynamic-extent #'input-sensitizer))
	(with-input-editing (stream :input-sensitizer #'input-sensitizer
				    :initial-contents (and insert-default
							   default-supplied-p
							   (list default default-type)))
	  (let ((start-position (stream-scan-pointer stream)))
	    (with-input-context (type)
				(object presentation-type nil options)
	      (with-activation-gestures ((if activation-gestures-p
					     activation-gestures
					   (or additional-activation-gestures
					       *standard-activation-gestures*))
					 :override activation-gestures-p)
		(with-delimiter-gestures ((if delimiter-gestures-p
					      delimiter-gestures
					    additional-delimiter-gestures)
					  :override delimiter-gestures-p)
		  (handler-bind
		      ((parse-error
			 #'(lambda (anerror)
			     (declare (ignore anerror))
			     (when (and default-supplied-p
					(check-for-default stream start-position
							   default default-type
							   view))
			       (setq the-object default
				     the-type default-type)
			       (return-from input-editing))
			     ;; Decline to handle the parse error
			     nil)))
		    (flet ((accept-help (stream action string-so-far)
			     (declare (ignore action string-so-far))
			     (write-string "You are being asked to enter " stream)
			     (describe-presentation-type type stream)
			     (write-char #\. stream)))
		      (declare (dynamic-extent #'accept-help))
		      (with-accept-help
			  (((:top-level-help :establish-unless-overridden)
			    ;; :ESTABLISH-... here because we want (SEQUENCE PATHNAME)'s
			    ;; help, not both (SEQUENCE PATHNAME) and PATHNAME.
			    #'accept-help))
			;; Call the presentation type's ACCEPT method
			;; it might return options in case it bound the input context
			;; and got a click
			(multiple-value-setq (the-object the-type the-options)
			  (let ((*presentation-type-for-yanking* (and history history-type)))
			    (if default-supplied-p
				(if history
				    (let ((default-element
					    (make-presentation-history-element
					      :object default :type default-type)))
				      (with-default-bound-in-history history default-element
					(funcall-presentation-generic-function accept
					  type stream view
					  :default default :default-type default-type)))
				    (funcall-presentation-generic-function accept
				      type stream view
				      :default default :default-type default-type))
				(funcall-presentation-generic-function accept
				  type stream view)))))))))

	       ;; A presentation translator was invoked
	       (t 
		 (setq the-object object
		       the-type presentation-type
		       the-options options
		       activated nil)
		 (when (if replace-supplied-p
			   replace-input
			   (getf the-options :echo t))
		   (presentation-replace-input stream the-object the-type view
					       :buffer-start start-position
					       :query-identifier query-identifier)
		   		  ;; spr25912 --PnC
		  ;; Windows seems to handle this slightly differently.
		  ;; As a result, when processing a command, if the user
		  ;; does a command-completion (from a menu), the focus
		  ;; doesn't come back to the window.  Furthermore, 
		  ;; trying to read the gesture goes into an infinite loop,
		  ;; because there's nothing there to read.		
		  #+mswindows
		  (when (eql (if (listp the-type)
				 (first the-type)
			       the-type) 
			     'clim:command-name)
		    (clim:stream-set-input-focus (encapsulating-stream-stream stream))
		    (stream-unread-gesture stream #\space))
		  )))))))

    ;; The input has been parsed, moused, or defaulted.
    ;; If we are still inside a WITH-INPUT-EDITING at an outer level, leave the
    ;; delimiter in the stream.	 But if this was the top level of input, eat
    ;; the activation gesture instead of leaving it in the stream. Don't eat
    ;; the activation gesture on streams that can't ever support input editing,
    ;; such as string streams.
    ;;--- This is really lousy.	 We need a coherent theory here.
    (when activated
      (when (and (not (input-editing-stream-p stream))
		 (stream-supports-input-editing stream))
	(let ((gesture (read-gesture :stream stream :timeout 0)))
	  ;;--- For now, just ignore button release events
	  (when (typep gesture 'pointer-button-release-event)
	    (read-gesture :stream stream :timeout 0)))))
    (when (and history
	       (frame-maintain-presentation-histories *application-frame*)
	       (getf the-options :maintain-history t))
      ;;--- Should this only record stuff that was input via the keyboard?
      (push-history-element history (make-presentation-history-element
				      :object the-object :type (or the-type type))))
    #+compulsive-type-checking
    (when (and the-type (not (eq the-type type)))
      (unless (presentation-subtypep-1 the-type type)
	;; Catch a common bug by verifying that the returned type is a subtype
	;; of the requested type
	(cerror "Return a second value of ~*~*~*~S"
		"The ~S method for the type ~S returned a second value of ~S, ~
		 which is not a subtype of ~S"
		'accept type the-type type)
	(setq the-type type)))
    ;; Ensure that there are no stale highlighting boxes lying around if
    ;; we are exiting via keyboard input
    (when (output-recording-stream-p stream)
      (unhighlight-highlighted-presentation stream t))
    (values the-object (or the-type type))))

;;; from command-processor.lisp
(define-presentation-method accept ((type command) stream (view textual-view) &key)
  (setq command-table (find-command-table command-table))
  (let ((start-position (and (input-editing-stream-p stream)
                             (stream-scan-pointer stream)))
        ;; this also requires some thought, but I suspect that
        ;; we can just kludge it this way in this presentation type,
        ;; because we can't think of any other presentation types
        ;; that would establish a shadowing context within the one
        ;; established by ACCEPT-1.
        (replace-input-p nil))
    (multiple-value-bind (object type options)
        ;; We establish a new input context so that clicks throw to us
        ;; rather than to the input context established in ACCEPT-1.
        ;; This will let's us handle "partial commands" below.
        ;; The "partial" notion could be extended to apply to all
        ;; presentation-types, but there are so few which need this
        ;; treatment, that it does not seem worthwhile.
        (with-input-context (type :override nil)
                            (object presentation-type nil options)
             (funcall *command-parser* command-table stream)
           (t (when (getf options :echo t)
                (setq replace-input-p t))
              (values object type options)))
      (cond ((partial-command-p object)
             (values (funcall *partial-command-parser*
                              object command-table stream start-position)
                     type))
            (t (when replace-input-p
                 (presentation-replace-input stream object type view
                                             :buffer-start start-position
                                             ;;--- We really need to pass the
                                             ;;--- query-identifier to the parser.
                                             ; :query-identifier query-identifier
                                             ))
               (values object type options))))))