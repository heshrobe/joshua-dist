;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-ENV; Base: 10; Lowercase: Yes -*-

(in-package :clim-env)

"Copyright (c) 1994 Harlequin, Inc.  All rights reserved."


;;; Useful presentation types

;; Class name
(define-presentation-type clos:class ()
  :history t)

#+Genera
(define-presentation-method accept ((type clos:class) stream (view textual-view) &key)
  (values
    (completing-from-suggestions (stream :partial-completers '(#\-))
      (map nil #'(lambda (x) 
		   (if (listp x)
		       (suggest (first x) (find-class (second x)))
		       (suggest (symbol-name x) (find-class x))))
	   clos-internals:*all-class-names-aarray*))))

#+LispWorks
(progn
(defvar *class-names-aarray* nil)
(defvar *class-names-aarray-tick* nil)

(define-presentation-method accept ((type clos:class) stream (view textual-view) &key)
  (unless (eql *class-names-aarray-tick* (hash-table-size clos::*class-table*))
    (let ((names nil))
      (maphash #'(lambda (name class) (push (cons name class) names))
               clos::*class-table*)
      (setq *class-names-aarray* (sort names #'string-lessp :key #'car))
      (setq *class-names-aarray-tick* (length *class-names-aarray*))))
    (multiple-value-bind (class success string)
	(completing-from-suggestions (stream :partial-completers '(#\- #\space)
					     :allow-any-input t)
	  (map nil #'(lambda (x) 
		       (suggest (symbol-name (car x)) (cdr x)))
	       *class-names-aarray*))
      (declare (ignore success))
      (unless class
        (ignore-errors
          (setq class (find-class (read-from-string string) :errorp nil)))
        (unless class
          (simple-parse-error "~A is not the name of a class" string)))
      class))
)	;#+LispWorks

#-(or Genera LispWorks)
(define-presentation-method accept ((type clos:class) stream (view textual-view) &key default)
  (let* ((class-name (accept 'symbol :stream stream :view view
				     :default (and default (class-name default))
				     :prompt nil))
	 (class (find-class class-name nil)))
    (unless class
      (input-not-of-required-type class-name type))
    class))

(define-presentation-method present (class (type clos:class) stream (view textual-view) &key)
  (if (typep class 'clos:class)
      (prin1 (class-name class) stream)
      (prin1 class stream)))

#+Genera (pushnew '(clos:class clos:class)
		  clim-internals::*dw-type-to-clim-type-alist* :key #'first)


;; Generic function name
#-LispWorks	;LispWorks doesn't want to do this
(define-presentation-type clos:generic-function ()
  :history t)

#+LispWorks	;we still want a presentation type history for this
(progn
(defvar *generic-function-type-history*
	(make-presentation-type-history 'clos:generic-function 
                                        :history-name "generic function"))

(define-presentation-method presentation-type-history ((type clos:generic-function))
  *generic-function-type-history*)
)	;#+LispWorks

#+Genera
(define-presentation-method accept ((type clos:generic-function) stream (view textual-view) &key)
  (values
    (completing-from-suggestions (stream :partial-completers '(#\-))
      (map nil #'(lambda (x) 
		   (if (listp x)
		       (when (fboundp (second x))
			 (suggest (first x) (fdefinition (second x))))
		       (when (fboundp x)
			 (suggest (symbol-name x) (fdefinition x)))))
	   clos-internals::*all-generic-function-names-aarray*))))

#-Genera
(define-presentation-method accept ((type clos:generic-function) stream (view textual-view) &key default)
  ;;--- Extend CLIM's COMPLETE-SYMBOL-NAME to look in the packages
  ;;--- and then make this use it only on generic functions
  (let* ((gf-name (accept #---ignore 'symbol
			  #+++ignore '((expression) :auto-activate t)
			  :stream stream :view view
			  :default (and default (clos:generic-function-name default))
			  :prompt nil))
	 (gf (and (fboundp gf-name) (fdefinition gf-name))))
    (unless (typep gf 'clos:generic-function)
      (input-not-of-required-type gf-name type))
    gf))

(define-presentation-method present (gf (type clos:generic-function) stream (view textual-view) &key)
  (if (typep gf 'clos:generic-function)
      (prin1 (clos:generic-function-name gf) stream)
      (prin1 gf stream)))

#+Genera (pushnew '(clos:generic-function clos:generic-function)
		  clim-internals::*dw-type-to-clim-type-alist* :key #'first)


;; Method
(define-presentation-type clos:method ())

(define-presentation-method present (method (type clos:method) stream (view textual-view) &key)
  #+Genera (prin1 (clos-internals::function-spec-object method) stream)
  #+LispWorks (prin1 (sys::function-name (clos::method-function method)) stream))


;; Package
(define-presentation-type package ()
  :inherit-from t
  :history t)

(define-presentation-method accept ((type package) stream (view textual-view) &key)
  (values
    (let ((packages (sort (copy-list (list-all-packages)) #'string-lessp
			  :key #'package-name)))
      (completing-from-suggestions (stream :partial-completers '(#\-))
	(map nil #'(lambda (package) 
		     (suggest (package-name package) package))
	     packages)))))

(define-presentation-method present (package (type package) stream (view textual-view) &key)
  (write-string (package-name package) stream))

(define-presentation-method presentation-typep (object (type package))
  (typep object 'clim-lisp::package))

#+Genera (pushnew '(package package)
		  clim-internals::*dw-type-to-clim-type-alist* :key #'first)


;; General function spec
(define-presentation-type function-spec (&key (defined-p nil))
  :history t)

#+Genera
;;--- Use different views to implement support for multiple languages
(define-presentation-method accept ((type function-spec) stream (view textual-view) &key)
  (multiple-value-bind (object success string)
      (completing-from-suggestions (stream :partial-completers '(#\-)
					   :allow-any-input t)
	(map nil #'(lambda (x) 
		     (if (listp x)
			 (suggest (first x) (second x))
			 (suggest (symbol-name x) (fdefinition x))))
	     zwei:*zmacs-completion-aarray*))
    (declare (ignore success))
    (unless object
      (setq object (ignore-errors (read-from-string string))))
    (cond (defined-p
	   (if (fboundp object)
	       object
	       (input-not-of-required-type object type)))
	  (t object))))

#-Genera
(define-presentation-method accept ((type function-spec) stream (view textual-view) &key default)
  (let ((fspec (accept #---ignore 'symbol
		       #+++ignore '((expression) :auto-activate t)
		       :stream stream :view view
		       :default default
		       :prompt nil)))
    ;;--- Extend CLIM's COMPLETE-SYMBOL-NAME to look in the packages
    ;;--- and then make this use it
    (cond (defined-p
	   (if (fboundp fspec)
	       fspec
	       (input-not-of-required-type fspec type)))
	  (t fspec))))

;;--- Use different views to implement support for multiple languages
(define-presentation-method present (fspec (type function-spec) stream (view textual-view) &key)
  (prin1 fspec stream))

(define-presentation-method presentation-typep (object (type function-spec))
  (and (or (symbolp object)
	   (and (listp object)
		(or (eql (car object) 'setf)
		    #+Genera (eql (car object) 'sys:locf))))
       (or (not defined-p)
	   (fboundp object))))
		
#+Genera (pushnew '(sys:function-spec function-spec (:partial-completers :abbreviate-p))
		  clim-internals::*dw-type-to-clim-type-alist* :key #'first)


;; System name
(define-presentation-type system ()
  :inherit-from t
  :history t)

#+Genera
(define-presentation-method accept ((type system) stream (view textual-view) &key)
  (values
    (completing-from-suggestions (stream :partial-completers '(#\- #\space))
      (map nil #'(lambda (x) 
		   (suggest (car x) (cdr x)))
	   sct:*subsystems-aarray*))))

#-Genera
(define-presentation-method accept ((type system) stream (view textual-view) &key)
  (values
    (let ((systems (sort (copy-list clim-defsys::*systems*) #'string-lessp
			 :key #'clim-defsys::system-name)))
      (completing-from-suggestions (stream :partial-completers '(#\- #\space))
	(map nil #'(lambda (system) 
		     (suggest (string (clim-defsys::system-name system)) system))
	     systems)))))

(define-presentation-method present (system (type system) stream (view textual-view) &key)
  #+Genera (princ (sct:system-pretty-name system) stream)
  #-Genera (princ (clim-defsys::system-name system) stream))

(define-presentation-method presentation-typep (object (type system))
  #+Genera (typep object 'sct:basic-system)
  #-Genera (typep object 'clim-defsys::system))

#+Genera (pushnew '(sct:system system)
		  clim-internals::*dw-type-to-clim-type-alist* :key #'first)
#+Genera (pushnew '(sct:subsystem system)
		  clim-internals::*dw-type-to-clim-type-alist* :key #'first)


(define-presentation-type universal-time (&key (pastp t))
  :history t)

(define-presentation-method accept ((type universal-time) stream (view textual-view) &key)
  (handler-bind (#-Genera (time-parser-error
			    #'(lambda (error)
			        (apply #'simple-parse-error
                                       (parse-error-format-string error)
                                       (parse-error-format-arguments error)))))
    (let* ((buffer-start (stream-scan-pointer stream))
	   (time (read-token stream))
	   (utime #+Genera (time:parse-universal-time time :pastp pastp)
		  #-Genera (parse-universal-time time)))
      (unless (stream-rescanning-p stream)
        (presentation-replace-input stream utime type view
				    :buffer-start buffer-start))
      utime)))

(define-presentation-method present (time (type universal-time) stream (view textual-view) &key)
  #+Genera (time:print-universal-time time stream)
  #-Genera (print-universal-time time :stream stream))

(define-presentation-method presentation-typep (object (type universal-time))
  (integerp object))


;; Command table name
(define-presentation-type command-table ()
  :history t)

(define-presentation-method accept ((type command-table) stream (view textual-view) &key)
  (values
    (let ((command-tables 
	    (let ((comtabs nil))
	      (maphash #'(lambda (key comtab)
			   (declare (ignore key))
			   (pushnew comtab comtabs))
		       clim-internals::*all-command-tables*)
	      (sort comtabs #'string-lessp
		    :key #'(lambda (comtab) (string (command-table-name comtab)))))))
      (completing-from-suggestions (stream :partial-completers '(#\-))
	(map nil #'(lambda (comtab) 
		     (suggest (string (command-table-name comtab)) comtab))
	     command-tables)))))

(define-presentation-method present (comtab (type command-table) stream (view textual-view) &key)
  (princ (command-table-name comtab) stream))


;; Process name

;; Command table name
(define-presentation-type process ()
  :history t)

(define-presentation-method accept ((type process) stream (view textual-view) &key)
  (values
    (completing-from-suggestions (stream :partial-completers '(#\- #\space))
      (map nil #'(lambda (process) 
		   (suggest (string (clim-sys:process-name process)) process))
	   (clim-sys:all-processes)))))

(define-presentation-method present (process (type process) stream (view textual-view) &key)
  (princ (clim-sys:process-name process) stream))

(define-presentation-method presentation-typep (object (type process))
  (clim-sys:processp object))


;; Printer

(defparameter *printer-name-alist* ())

(define-presentation-type printer ())

(define-presentation-method accept ((type printer) stream (view textual-view) &key)
  (multiple-value-bind (object success string)
      (completing-from-suggestions (stream :partial-completers '(#\- #\space)
                                           #-Genera :allow-any-input #-Genera t)
        (dolist (printer *printer-name-alist*)
          (suggest (cdr printer) (car printer))))
    (declare (ignore success #+Genera string))
    #-Genera (when (null object)
               (setq object string)
               (pushnew (cons object object) *printer-name-alist*
                        :key #'first :test #'string-equal))
    object))

(define-presentation-method present (printer (type printer) stream (view textual-view)
				     &key acceptably)
  (let ((entry (assoc printer *printer-name-alist*
                      #-Genera :test #-Genera #'string-equal)))
    (when entry
      (write-token (cdr entry) stream :acceptably acceptably))))

(define-presentation-method presentation-typep (object (type printer))
  #+Genera (not (null (assoc object *printer-name-alist*)))
  #-Genera (and (stringp object)
                (not (null (assoc object *printer-name-alist*
				  :test #'string-equal)))))

#+Genera
(scl:add-initialization "Local printers"
   '(let ((site #+++ignore net:*local-site* 
		#---ignore (net:find-object-named :site :scrc)))
      (setq *printer-name-alist*
	    (mapcar #'(lambda (printer)
			(cons printer (scl:send printer :pretty-name)))
		    (net:find-objects-from-property-list ':printer ':site site))))
   '(:now) 'neti:commonly-used-property-lists)

