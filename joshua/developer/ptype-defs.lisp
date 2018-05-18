;;; -*- Mode: LISP; Base: 10; Syntax: Common-lisp; Package: joshua-internals -*-

(in-package :joshua-internals)

;;; Note the serious dwiming of rules that are in multiple packages
(clim:define-presentation-type forward-rule ()
  :history t
  :description "a forward rule")

(clim:define-presentation-type backward-rule ()
  :history t
  :description "a backward rule")

(clim:define-presentation-type rule ()
   :history t
   :description "a Joshua rule")

(clim:define-presentation-type protocol-function ()
  :description "a Joshua protocol function name"
  :history t)

(clim:define-presentation-type database-predication ()
  :inherit-from '()
  :options ((print-truth-value t))
  :description "a predication from the database"
  :history t)

(clim:define-presentation-type predication ()
  :inherit-from ()
   :history t
   :description "a predication")

(clim:define-presentation-type truth-value ()
   :description "a truth value") 


;;; This is here for compilation dependency reasons
(eval-when (:compile-toplevel :load-toplevel :execute)
(defclass tracing-event ()
  ((name :initarg :name :accessor tracing-event-name) 
   (pretty-name :initarg :pretty-name :accessor tracing-event-pretty-name)
   (short-name :initarg :short-name :accessor tracing-event-short-name)
   (active-name :initarg :active-name :accessor tracing-event-active-name)
   (enabled-p :initform nil :initarg :enabled-p :accessor tracing-event-enabled-p)
   (encapsulations :initform nil :initarg :encapsulations :accessor tracing-event-encapsulations)
   (enablers :initform () :initarg :enablers :accessor tracing-event-enablers)
   (documentation :initarg :documentation :accessor tracing-event-documentation)
   (enabling-function :initarg :enabling-function :accessor tracing-event-enabling-function) 
   (disabling-function :initarg :disabling-function :accessor tracing-event-disabling-function))))

(clim:define-presentation-type tracing-event ()
  :options ((name :pretty-name))
  :history t
  :description "a tracing event")

(clim:define-presentation-type trigger-list ()
   :description "a list of rule triggers"
   )

;;; I wish there was a more efficient way to do this - perhaps define-predicate
;;; should maintain a table of predicates.
(clim:define-presentation-type joshua-predicate ()
  :inherit-from '(symbol)
  :description "a Joshua predicate name"
  :history t)

(clim:define-presentation-type tms-predicate ()
  :inherit-from '(joshua-predicate)
  :description "a Joshua TMS predicate")

(clim:define-presentation-type-abbreviation tms-predication-presentation ()
  '(and predication (satisfies nontrivial-tms-p)))

