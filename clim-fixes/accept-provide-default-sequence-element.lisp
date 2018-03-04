;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

(defun accept (type &rest accept-args
	       &key (stream *standard-input*)
		    (view (stream-default-view stream))
		    (default nil default-supplied-p)
		    (default-type type)
		    (history type)
		    (provide-default nil)
		    (prompt t)
		    (prompt-mode ':normal)
		    (display-default prompt)
		    (query-identifier nil)
		    (activation-gestures nil)
		    (additional-activation-gestures nil)
		    (delimiter-gestures nil)
		    (additional-delimiter-gestures nil)
		    (insert-default nil) (replace-input t)
		    (present-p nil) (active-p t))
  (declare (dynamic-extent accept-args))
  (declare (values object type))
  (declare (ignore prompt-mode display-default
		   activation-gestures additional-activation-gestures
		   delimiter-gestures additional-delimiter-gestures 
		   insert-default replace-input present-p active-p))

  ;; Allow the arguments to be presentation type abbreviations
  (multiple-value-bind (expansion expanded)
      (expand-presentation-type-abbreviation type)
    (when expanded
      (when (eq default-type type)
	(setq default-type expansion))
      (when (eq history type)
	(setq history expansion))
      (setq type expansion)))
  (unless (eq default-type type)
    (multiple-value-bind (expansion expanded)
	(expand-presentation-type-abbreviation default-type)
      (when expanded
	(setq default-type expansion)
	(setq accept-args `(:default-type ,default-type ,@accept-args)))))
  (unless (eq history type)
    (multiple-value-bind (expansion expanded)
	(expand-presentation-type-abbreviation history)
      (when expanded
	(setq history expansion)
	(setq accept-args `(:history ,history ,@accept-args)))))

  (let ((insert-default nil))
    (when (and provide-default (null default-supplied-p))
      ;; If the user wants a default, but provided none, go get it from the history
      (let ((history (if (typep history 'basic-history)
			 history
			 (presentation-type-history history))))
	(when history
	  (let ((element (yank-from-history history
					    :test #'(lambda (element) 
						      (presentation-subtypep (presentation-history-element-type element)
									     type)))))
	    (when element
	      (setq default (presentation-history-element-object element)
		    default-supplied-p t
		    insert-default t))))))
    (when default-supplied-p
      ;; Massage the default
      (multiple-value-bind (new-default new-type)
	  (presentation-default-preprocessor default type :default-type default-type)
	(when (or (not (eq default new-default))
		  (not (eq default-type new-type)))
	  (setq default new-default
		default-type (or new-type default-type)
		insert-default t))))
    (when insert-default
      (setq accept-args `(:default ,default :default-type ,default-type ,@accept-args))))

  (typecase view
    (null)
    (symbol (setq view (make-instance view)))
    (cons   (setq view (apply #'make-instance view))))
  (setq view (decode-indirect-view type view (frame-manager stream)
				   :query-identifier query-identifier))

  ;; Call STREAM-ACCEPT to do the work.	 It would be nice if we could
  ;; call PROMPT-FOR-ACCEPT to generate the real query-id here, but we
  ;; can't because we want to be able to decide exactly how it is called
  ;; on a case-by-case basis.  For example, within ACCEPTING-VALUES...
  (with-keywords-removed (accept-args accept-args '(:stream :view))
    (apply #'stream-accept (encapsulating-stream stream) type
			   :view view :query-identifier query-identifier
			   accept-args)))