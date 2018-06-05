;;; This makes indenting for forms that contain "ing-" in their name
;;; (e.g. centering-output) indent the same way as those that
;;; begin with "with-"

(defun fi::lisp-get-method-aux (name)
  (or (and (boundp 'fi:lisp-indent-hook-property)
	   fi:lisp-indent-hook-property
	   (get (intern-soft (if fi:indent-methods-case-sensitive
				 name
			       (downcase name)))
		fi:lisp-indent-hook-property))
      (get (intern-soft (if fi:indent-methods-case-sensitive
			    name
			  (downcase name)))
	   'fi:lisp-indent-hook)
      (and (or (string-match "^with-" name)
	       (string-match "ing-" name)
	       (string-match "^do-" name))
	   1)
      (and (string-match "^def" name) 2)))

;;; This makes the arglist triggered by typing a space also do joshua predicates
;;; for some reason this finds the function-name in a different way than the C-c a
;;; method does.  So I had to explicitly make it understand the role of "["

(defun fi::arglist-lisp-space-1 ()
  (let* ((old-point (point))
	 (last-char
	  (progn (ignore-errors (backward-char))
		 (unless (eql (point) old-point)
		   (buffer-substring-no-properties old-point (point)))))
	 (string
	  (buffer-substring-no-properties old-point
					  (progn
					    (goto-char old-point)
					    (ignore-errors
					     (backward-sexp))
					    (point))))
	 (prefix-char 
	  (let ((save (ignore-errors
		       (goto-char old-point)
		       (backward-sexp)
		       (backward-char)
		       (point))))
	    (when save
	      (buffer-substring-no-properties save (1+ save)))))
	 (double-quote-pos (and string (string-match "\"" string)))
	 ;;for joshua match both [ and ( (in string search [ means match any of a set)
	 (paren-pos (and string (string-match "[([]" string)))
	 (symbol-with-package
	  (unless (eql paren-pos 0)
	    (if (and double-quote-pos (eql double-quote-pos 0)
		     string (ignore-errors (elt string 2)))
		(substring string 1 -1)
	      string)))
	 (symbol symbol-with-package))
    (flet ((no-arglist-output-p ()
	     (or (and last-char 
		      (or
		       ;; don't do silly things after comment character
		       (equal last-char ";")
		       ;; do something only if directly after a sexp.
		       (equal last-char " ")))
		 ;; could be something like #+foo, #-foo, or #:foo, any of
		 ;; which is likely to lose.
		 (and string (string-match "^#" string))
		 double-quote-pos ;; there is no output  for strings only.
		 (not (and symbol (stringp symbol) (> (length symbol) 0)))
		 (string-match "^\. " symbol)
		 (string-match "^\\\\" symbol))))
      (goto-char old-point)
      (unless (no-arglist-output-p)
	;; only output for functions within brackets; too much lisp-traffic!
	;; The "[" case if for Joshua
	(when (or (equal prefix-char "(") (equal prefix-char "["))
	  (setq string (fi::normalize-symbol-package string))
	  (fi::make-request (lep::arglist-session :fspec string)
	    ;; Normal continuation
	    (() (what arglist)
	     (let ((fi:pop-up-temp-window-behavior
		    fi:auto-arglist-pop-up-style))
	       (fi:show-some-text nil "%s's arglist: %s" what arglist)))
	    ;; Error continuation
	    ((string) (error)
	     (fi::show-error-text "")))))))
  (self-insert-command (prefix-numeric-value current-prefix-arg)))


