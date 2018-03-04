;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for Private version 0.0
;;; Reason: Function CLIM:COMPLETE-INPUT:  map the actual character or event gesture to the gesture
;;; name and pass that as the "action to display-accept-help.
;;;
;;; Reason: Don't set actual-object to a value if the completer function isn't called on this pass.

#+genera
(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "SYS:CLIM;REL-2;CLIM;COMPLETER.LISP.102"
  "SYS:CLIM;REL-2;CLIM;INPUT-EDITOR-COMMANDS.LISP.147")

#+genera
(SCT:NOTE-PRIVATE-PATCH "This Allows various *help-gestures* to be treated in different ways.")


;========================
#+genera(SCT:BEGIN-PATCH-SECTION)
#+genera(SCT:PATCH-SECTION-SOURCE-FILE "SYS:CLIM;REL-2;CLIM;COMPLETER.LISP.102")
#+genera (SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-")

#-genera(in-package :clim-internals)

(defun complete-input (stream function
			      &key partial-completers allow-any-input possibility-printer
			      (help-displays-possibilities t))
  (declare (dynamic-extent function))
  (declare (values answer-object success string))
  (with-temporary-string (stuff-so-far :length 100 :adjustable t)
    (with-delimiter-gestures (partial-completers)
      (with-activation-gestures (*magic-completion-gestures*)
	(flet ((completion-help (stream action string-so-far)
		 (declare (ignore string-so-far))
		 (display-completion-possibilities
		   stream function stuff-so-far
		   :possibility-printer possibility-printer
		   :possibility-type
		   (if (MEMBER ACTION *HELP-GESTURES*)
		       (and help-displays-possibilities :possibilities)
		       action))))
	  (declare (dynamic-extent #'completion-help))
	  (with-accept-help ((:subhelp #'completion-help))
	    ;; Keep the input editor from handling help and possibilities gestures.
	    ;; They will get treated as activation gestures, thus ensuring that 
	    ;; STUFF-SO-FAR will be accurate when we display the possibilities.
	    (let ((*ie-help-enabled* nil)
		  (location (stream-scan-pointer stream))
		  token ch
		  unread return extend
		  completion-mode completion-type
		  answer-object)
	      (flet ((ends-in-char-p (string char)
		       (let ((sl (length string)))
			 (and (plusp sl)
			      (char-equal (aref string (1- sl)) char)))))
		(declare (dynamic-extent #'ends-in-char-p))
		(loop
		  (setq unread nil return nil extend nil)
		  (with-input-context (`(completer :stream ,stream
						   :function ,function
						   :possibility-printer ,possibility-printer
						   :prefix ,stuff-so-far
						   :location ,location)) ()
		       (progn 
			 (setq token (read-token stream))
			 (setq ch (stream-read-gesture stream)))
		     (t (beep stream)))
		  (extend-vector stuff-so-far token)
		  (cond ((null ch)
			 (error "Null character?"))
			((key-press-event-p ch)
			 (cond ((member ch *help-gestures* 
					:test #'keyboard-event-matches-gesture-name-p)
				(setq completion-mode ':help))
			       ((member ch *possibilities-gestures* 
					:test #'keyboard-event-matches-gesture-name-p)
				(setq completion-mode ':possibilities))
			       ((member ch *apropos-possibilities-gestures* 
					:test #'keyboard-event-matches-gesture-name-p)
				(setq completion-mode ':apropos-possibilities))
			       ((member ch *completion-gestures*
					:test #'keyboard-event-matches-gesture-name-p)
				(setq completion-mode ':complete-maximal
				      ;; If the completion fails, unread this char
				      ;; so that a higher level gets the chance to
				      ;; try the completion again.  For example, when
				      ;; several completion types are OR'ed together.
				      unread 'unless-completed))
			       ((member ch partial-completers 
					:test #'keyboard-event-matches-gesture-name-p)
				(setq completion-mode ':complete-limited
				      unread t extend t return 'if-completed))
			       ;; What about "overloaded" partial completers??
			       ((delimiter-gesture-p ch)
				(setq completion-mode (if allow-any-input nil ':complete)
				      unread t extend t return t))
			       ((activation-gesture-p ch)
				(setq completion-mode (if allow-any-input nil ':complete) 
				      unread t return t))))
			((eq ch *end-of-file-marker*)
			 (setq completion-mode (if allow-any-input nil ':complete) 
			       return t))
			(t			;mouse click?
			 (beep stream)))
		  ;; OK, this is a SPECIAL case.  We check to see if the null string
		  ;; was read, and if so, we signal a parse-error (because ACCEPT
		  ;; handles this specially) so that the default value will be filled
		  ;; in by ACCEPT.
		  ;; There is a tension here between wanting to fill in the default and
		  ;; use the maximal left substring when the user types #\End or a field
		  ;; terminator that also does completion.  Putting this check before the
		  ;; completion code means that the default always wins.
		  (when (and return (zerop (fill-pointer stuff-so-far)))
		    (when (eq unread t)
		      (stream-unread-gesture stream ch))
		    (when (input-editing-stream-p stream)
		      (rescan-if-necessary stream))
		    (signal 'empty-completion-error
			    :format-string "Attempting to complete the null string"))

		  (cond ((member completion-mode '(:help :possibilities :apropos-possibilities))
			 (IF (EQL COMPLETION-MODE :HELP)
			     (LET ((GESTURE (FIND CH *HELP-GESTURES* 
						  :TEST #'KEYBOARD-EVENT-MATCHES-GESTURE-NAME-P)))
			       (DISPLAY-ACCEPT-HELP STREAM (OR GESTURE COMPLETION-MODE) ""))
			     ;; SINCE WE'VE ASKED THE INPUT EDITOR NOT TO DO THIS,
			     ;; WE MUST DO IT HERE OURSELVES
			     (DISPLAY-ACCEPT-HELP STREAM COMPLETION-MODE ""))
			 (setq completion-type nil))
			(completion-mode
			 (multiple-value-bind (string success object nmatches)
			     (funcall function stuff-so-far completion-mode)
			   (setq answer-object object)
			   (cond ((= nmatches 0)
				  ;; No valid completion, so no replace input
				  (setq completion-type 'invalid)
				  (when extend
				    (vector-push-extend ch stuff-so-far)))
				 ((= nmatches 1)
				  (setq completion-type (if success 'unique 'ambiguous))
				  ;; Replace contents of stuff-so-far with completion
				  (setf (fill-pointer stuff-so-far) 0)
				  (extend-vector stuff-so-far string)
				  )
				 ((> nmatches 1)
				  (setq completion-type 'ambiguous)
				  ;; Replace contents of stuff-so-far with completion
				  (setf (fill-pointer stuff-so-far) 0)
				  (extend-vector stuff-so-far string)
				  ;; Need-to-add-delimiter test??
				  (when (and extend
					     (not (ends-in-char-p string ch)))
				    (vector-push-extend ch stuff-so-far))))))
			(t (setq answer-object nil)))

		  ;; Check for errors unconditionally, remembering that we may not have
		  ;; called the completer at all (completion-type = NIL)
		  (ecase completion-type
		    ((nil unique left-substring))	;no possible errors to report
		    (invalid
		      (unless allow-any-input
			(when unread
			  (stream-unread-gesture stream ch))
			(signal 'simple-completion-error
				:format-string "Invalid completion: ~A"
				:format-arguments (list (evacuate-temporary-string stuff-so-far)))))
		    (ambiguous
		      ;; Only beep on ambiguous full completions
		      ;;--- This should really signal an AMBIGUOUS-COMPLETION-ERROR,
		      ;;--- letting a higher level condition handler do the beeping
		      (when (eq completion-mode :complete)
			(beep stream))))

		  (when (eq return 'if-completed)
		    (unless (eq completion-type 'unique)
		      (setq return nil)))

		  ;; Decide whether or not to return, remembering that
		  ;; we might have called the completer.
		  (when return
		    (when (or (member completion-type '(nil unique left-substring))
			      allow-any-input)
		      ;; Leave the last delimiter for our caller
		      (when (eq unread t)
			(stream-unread-gesture stream ch))
		      ;; Must REPLACE-INPUT after UNREAD-GESTURE so the delimiter is unread
		      ;; into the input editor's buffer, not the underlying stream's buffer
		      (unless (stream-rescanning-p stream)
			(replace-input stream stuff-so-far :buffer-start location))
		      (return-from complete-input
			(values answer-object t (evacuate-temporary-string stuff-so-far)))))

		  ;; Not returning yet, but update the input editor's buffer anyway
		  (unless (stream-rescanning-p stream)
		    (replace-input stream stuff-so-far :buffer-start location)))))))))))


;========================
#+genera(SCT:BEGIN-PATCH-SECTION)
#+genera(SCT:PATCH-SECTION-SOURCE-FILE "SYS:CLIM;REL-2;CLIM;INPUT-EDITOR-COMMANDS.LISP.147")
#+genera(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-")

#-genera(in-package :clim-internals)
;;; Help commands, handled by special magic

(define-input-editor-command (com-ie-help :rescan nil :type completion)
			     (stream gesture)
  "Display completion help"
  (let ((gesture (find gesture *help-gestures*
		      :test #'keyboard-event-matches-gesture-name-p)))
    (display-accept-help stream (or gesture :help) "")))

