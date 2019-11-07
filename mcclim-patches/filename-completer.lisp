;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2001,2002 by Tim Moore (moore@bricoworks.com)

;;; Definitions for the standard presentation types and generic functions

(in-package :clim-internals)

(defun filename-completer (string action &optional (default *default-pathname-defaults*))
  (flet
      ((deal-with-home (pathname his-directory)
	 ;; SBCL (and maybe others) treat "~/xxx" specially, returning a pathname
	 ;; whose directory is (:ABSOLUTE :HOME xxx)
	 ;; But if you call Directory on that pathname the returned list
	 ;; are all complete pathnames without the :Home part!.
	 ;; So this replaces the :HOME with what it actually means
	 (let* ((home-env-variable (clim-internals::get-environment-variable "HOME"))
		(home (loop for pos = 1 then (1+ next-pos)
			 for next-pos = (position #\/ home-env-variable :start pos)
			 collect (subseq home-env-variable pos next-pos)
			 until (null next-pos)))
		(new-directory (cons
				(first his-directory)
				(append home (rest (rest his-directory))))))
	     (make-pathname :host (pathname-host pathname)
			    :device (pathname-device pathname)
			    :name (pathname-name pathname)
			    :version (pathname-version pathname)
			    :type (pathname-type pathname)
			    :directory new-directory)
	     )))
    ;; Slow but accurate
    (let* ((raw-pathname (pathname string))
	   (raw-directory (pathname-directory raw-pathname))
	   (original-pathname (if (and (listp raw-directory)
				       (eql (first raw-directory) :absolute)
				       (eql (second raw-directory) :Home))
				  (deal-with-home raw-pathname raw-directory)
				  raw-pathname))
	   (original-string (namestring original-pathname))
	   ;; Complete logical pathnames as well as regular pathnames
	   ;; strategy is to keep track of both original string provided and translated string
	   ;; but to return pathname built from original components except for the name.
	   (logical-pathname-p (typep original-pathname 'logical-pathname))
	   (actual-pathname (if logical-pathname-p
				(translate-logical-pathname original-pathname)
				original-pathname))
	   (merged-pathname (merge-pathnames actual-pathname default))
	   ;; (version (pathname-version actual-pathname))
	   completions)
      (let ((search-pathname (make-pathname :host (pathname-host merged-pathname)
					    :device (pathname-device merged-pathname)
					    :directory (pathname-directory merged-pathname)
					    :version :unspecific
					    :type :wild
					    :name :wild)))
	(setq completions (directory search-pathname)))
      ;; Now prune out all completions that don't start with the string
      ;; why is this necessary would complete-from-suggestions do this also?
      (let (;; (name (pathname-name actual-pathname))
       	    (type (pathname-type actual-pathname)))
	;; (loop for pn in completions
	;;    for pn-name = (pathname-name pn)
	;;    for pn-type = (pathname-type pn)
	;;    when (cond
	;; 	    ;; if the query has a type, then the name must be
	;; 	    ;; complete and match
	;; 	    (type
	;; 	     (and
	;; 	      (string-equal pn-name name)
	;; 	      (let ((s (search type pn-type :test #'char-equal)))
	;; 		(and s (zerop s)))))
	;; 	    ;; But if not, then if the query has a name
	;; 	    (name
	;; 	     ;; but at least in SBCL a candidate with neither name nor
	;; 	     ;; type is a directory
	;; 	     (when (and (null pn-name) (null pn-type))
	;; 	       (setq pn-name (first (last (pathname-directory pn)))))
	;; 	     (let ((s (search name pn-name
	;; 			      :test #'char-equal)))
	;; 	       (if (eq action :apropos-possibilities)
	;; 		   (not (null s))
	;; 		   (and s (zerop s))))))
	;;    collect pn into answer
	;;    finally (setq completions answer))
	(when (null type)
	  ;; If the user didn't supply a file type, don't burden him with all
	  ;; sorts of version numbers right now.
	  (let ((new-completions nil))
	    (dolist (pathname completions)
	      (cond
		;; meaning this is actually a directory
		((and (null (pathname-name pathname))
		      (null (pathname-type pathname)))
		 (pushnew (make-pathname :host (pathname-host original-pathname)
					 :device (pathname-device original-pathname)
					 :directory (butlast (pathname-directory pathname))
					 :name (first (last (pathname-directory pathname)))
					 :type nil)
			  new-completions))
		(t
		 (pushnew (make-pathname :host (pathname-host original-pathname)
					 :device (pathname-device original-pathname)
					 :directory (pathname-directory original-pathname)
					 :name (pathname-name pathname)
					 :type (pathname-type original-pathname))
			  new-completions))))
	    (setq completions (nreverse new-completions))))
	(complete-from-possibilities original-string completions '(#\space)
						    :action action
						    :name-key #'namestring
						    :value-key #'identity)))))


(define-presentation-method accept ((type pathname) stream (view textual-view)
                                    &key (default *default-pathname-defaults* defaultp)
                                    ((:default-type accept-default-type) type))
  (multiple-value-bind (pathname success string)
      (complete-input stream
                      #'(lambda (string action)
			  (filename-completer string action default))
                      :allow-any-input t)
    (cond ((and pathname success)
           (values (if merge-default
                       (progn
                         (unless (or (pathname-type pathname)
                                     (null default-type))
                           (setf pathname (make-pathname :defaults pathname
                                                         :type default-type)))
                         (merge-pathnames pathname default default-version))
                       pathname)
                   type))
          ((and (zerop (length string))
                defaultp)
           (values default accept-default-type))
          (t (values string 'string)))))

