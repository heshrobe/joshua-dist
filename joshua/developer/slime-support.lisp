;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: swank -*-

(in-package :swank)

;;;parsers
(defmethod form-parser ((key (eql 'joshua:define-predicate)) form)
  (values (second form) :joshua-predicate))

(defmethod form-parser ((key (eql 'joshua:defrule)) form)
  (values (second form) :joshua-defrule))

(defmethod form-parser ((key (eql 'joshua:define-object-type)) form)
  (values (second form) :joshua-define-object-type))

(defmethod form-parser ((key (eql 'joshua:define-predicate-method)) form)
  (let ((real-form (macroexpand form)))
    (form-parser 'defmethod (second real-form))))



;;; undefiners

(defmethod definition-undefining-form ((type (eql :joshua-defrule)) fspec do-it?)
  (when do-it? (joshua:undefrule fspec))
  `(joshua:undefrule ',fspec))

(defmethod definition-undefining-form ((type (eql :joshua-predicate)) fspec do-it?)
  (when do-it? (joshua:undefine-predicate fspec))
  `(joshua:undefine-predicate ',fspec))

(defmethod definition-undefining-form ((type (eql :joshua-define-object-type)) fspec do-it?)
  (when do-it? (ji::undefine-object-type fspec))
  `(ji::undefine-object-type ',fspec)
  )
