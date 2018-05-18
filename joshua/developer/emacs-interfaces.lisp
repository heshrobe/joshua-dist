;;; -*- Mode: LISP; Base: 10; Syntax: Common-lisp; Package: lep -*-
;;;

(in-package :lep)

(defmethod definition-undefining-form (fspec (type (eql 'joshua:defrule)))
  `(joshua:undefrule ',fspec))

(defmethod definition-undefining-form (fspec (type (eql ':type)))
  `(setf (find-class ',fspec) nil))

(define-reply undefine-reply (compute-reply-session)
	      (buffer start-point end-point doit)
  (multiple-value-bind (fspec type)
      (buffer-definition-at-point buffer start-point end-point)
    (when (and (eql type :operator) (listp fspec) (eql (first fspec) 'method))
	      (setq type :method))
    (if* (eq :none fspec)
       then (error "can't find buffer definition at point")
       else (let ((uform (definition-undefining-form fspec type)))
	      (when doit (eval uform))
	      (values (format nil "~s" uform))))))

;;; This method should return a form which when evaluated to remove the definition.

(defmethod definition-undefining-form (fspec (type (eql :operator)))
  `(fmakunbound ',fspec))

(defmethod definition-undefining-form (fspec (type (eql :method))) 
  `(fmakunbound ',(fixed-defmethod-fspec fspec)))

(defun fixed-defmethod-fspec (form) 
  (let (qualifiers)
    (do ((x (cddr form) (cdr x)))
	((listp (car x))
	 `(method ,(second form)
		  ,@(nreverse qualifiers)
		  ,(loop for specializer in (car x) collect (fixed-specializer specializer))))
      (push (car x) qualifiers))))

(defun fixed-specializer (specializer)
  (if (listp specializer)
      (destructuring-bind (op value) specializer
	(if (and (eql op 'eql) (listp value))
	    (destructuring-bind (op thing) value
	      (if (eql op 'quote)
		  `(eql ,thing)
		value))
	  specializer))
    specializer))
 