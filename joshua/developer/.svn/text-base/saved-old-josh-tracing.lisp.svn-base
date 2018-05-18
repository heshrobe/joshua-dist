;;; -*- Mode: LISP; Base: 10; Syntax: Common-lisp; Package: joshua-internals -*-
;;;>
;;;> *****************************************************************************************
;;;> ** (c) Copyright 1990-1988 Symbolics, Inc.  All rights reserved.
;;;> ** Portions of font library Copyright (c) 1984 Bitstream, Inc.  All Rights Reserved.
;;;>
;;;>    The software, data, and information contained herein are proprietary 
;;;> to, and comprise valuable trade secrets of, Symbolics, Inc., which intends 
;;;> to keep such software, data, and information confidential and to preserve 
;;;> them as trade secrets.  They are given in confidence by Symbolics pursuant 
;;;> to a written license agreement, and may be used, copied, transmitted, and 
;;;> stored only in accordance with the terms of such license.
;;;> 
;;;> Symbolics, Symbolics 3600, Symbolics 3670 (R), Symbolics 3675 (R), Symbolics 3630,
;;;> Symbolics 3640, Symbolics 3645 (R), Symbolics 3650 (R), Symbolics 3653, Symbolics
;;;> 3620 (R), Symbolics 3610 (R), Symbolics Common Lisp (R), Symbolics-Lisp (R),
;;;> Zetalisp (R), Genera (R), Wheels (R), Dynamic Windows (R), Showcase, SmartStore (R),
;;;> Semanticue (R), Frame-Up (R), Firewall (R), MACSYMA (R), COMMON LISP MACSYMA (R),
;;;> CL-MACSYMA (R), LISP MACHINE MACSYMA (R), MACSYMA Newsletter (R), PC-MACSYMA, Document
;;;> Examiner (R), Delivery Document Examiner, S-DYNAMICS (R), S-GEOMETRY (R), S-PAINT (R),
;;;> S-RECORD, S-RENDER (R), Displacement Animation, FrameThrower, PaintAmation, "Your Next
;;;> Step in Computing" (R), Ivory, MacIvory, MacIvory model 2, XL400, Symbolics UX400S, 
;;;> Symbolics C, Symbolics Pascal (R), Symbolics Prolog, Symbolics Fortran (R), CLOE (R),
;;;> CLOE Application Generator, CLOE Developer, CLOE Runtime, Common Lisp Developer,
;;;> Symbolics Concordia, Joshua, and Statice (R) are trademarks of Symbolics, Inc.
;;;> 
;;;> RESTRICTED RIGHTS LEGEND
;;;>    Use, duplication, and disclosure by the Government are subject to restrictions 
;;;> as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data and Computer 
;;;> Software Clause at DFAR 52.227-7013.
;;;> 
;;;>      Symbolics, Inc.
;;;>      8 New England Executive Park, East
;;;>      Burlington, Massachusetts  01803
;;;>      United States of America
;;;>      617-221-1000
;;;> *****************************************************************************************
;;;>

(in-package :ji)

;;; The Joshua tracing substrate

;;; This file contains:
;;;  
;;; The defclass and methods for the following classes
;;;
;;; 1.) Joshua-debbugger - keeps track of all of the global state of Joshua
;;; tracing. It makes sure that the right events are enabled and
;;; encapsulated. It provides the top level interface for enabling and
;;; disabling tracing. It knows how to do joshua encapsulations.

;;; 2.) tracing-event - each event at which we might want to trap out
;;; for tracing is defined as one of these. In addition there is a macro
;;; automatically defined for each event that you can insert into your
;;; code. It will make sure that the appropriate tracer is called when
;;; tracing for that event is enabled.
;;;
;;; 3.) Joshua-encapsulations - most events have associated
;;; encapsulations so the system can do the tracing related bookkeeping
;;; only when tracing is on. Encapsulations are cached and managed by
;;; the tracing-events which depend on them. When you enable the
;;; tracing of an event, it will encapsulate the appropriate functions.
;;; See also the file encapsulations.lisp where a number of the
;;; encapsulations are defined.
;;;
;;; 4.) tracers - Each "Type of Tracing" (Forward Rule Tracing, Backward
;;; Rule Tracing...) uses tracer as a component class. A tracer is
;;; defined to have a collection of associated tracing events and
;;; controls the interface to those events. The trace-it method of each
;;; tracer does the actual output for those events. They also keep track
;;; of any filtering the user wants to do on the traced events. For
;;; example the Forward Rule Tracer keeps a list of traced forward rule
;;; triggers.
;;;
;;; 4) forward-rule-tracer, backward-rule-tracer, predication-tracer,
;;; and tms-tracer - These are the classes of the four types of tracers.
;;; 
;;; It also contains all of the definitions that create the instances of
;;; the event and the tracers and register them with joshua-debugger.
;;; See define-tracing-event and define-tracer

;;; This file uses an arglist declaration; this is not defined by common lisp.
;;; this provides it:

#+(or mcl allegro)
(declaim (declaration arglist))

;;; First some defvars 
;;; Where to install joshua commands, for now we just stick them in Global




(defvar *joshua-command-table* (clim:find-command-table 'clim:global-command-table))

;;; This really just keeps a register of all the the strictly Joshua
;;; related commands. Its not currently used by any command loop.
(defvar *joshua-only-command-table*
	(clim:make-command-table 'Joshua))

(defvar *joshua-tracing-command-table*
	(clim:make-command-table 'Joshua-Trace))

;;; Do the debugging tables need to be locked?
(defvar *default-joshua-debugger-table-locking* nil)

;;; Some character styles
(defvar *heading-character-style* '(nil :bold nil))

(defvar *emphasis-character-style* '(nil :bold nil))

(defvar *deemphasis-character-style* '(nil :italic nil))

;;; The prompt used for tracing interaction
(defvar *joshua-tracing-prompt*
  (with-output-to-string (stream)
    (clim:with-text-style (stream '(nil :italic nil))
      (format stream "Joshua trace:" ))))

;;; What charater to use to denote Joshua tracing messages
(defvar *joshua-tracing-character* #\>)

;;; Restrict the width of the menu used by the Joshua trace command
(defvar *trace-options-menu-width* 60)

;;; For now we'll continue to use rule depth to give tracing indentation
;;; I think clim's indenting output will allow a function as the indentation
(defmacro indenting-body ((stream) &body body)
  `(progn (clim:indenting-output (,stream  #'joshua-trace-message-indentor)
	    ,@body)))

;;; By wrapping all of the trace messages with this we make sure that 
;;; any output done by the user between trace outputs is done to a 
;;; new line, but at the same time we get the trace indentation for the 
;;; first line of the message.
(defmacro with-joshua-trace-message-output ((stream) &body body)
  `(progn (fresh-line ,stream)
	  (loop for x from 0 to (* 2 *rule-depth*) do
	        (progn x)                      ; meaning ignore
	        (write-char #\space ,stream))
	  (write-char *joshua-tracing-character* ,stream)
	  (write-char #\space ,stream)
	  ,@body
	  (fresh-line ,stream)
	  ))

;;; A hack to draw lines before and/or after output to the screen
;;; this probably doesn't work because of pane-viewport-region
(defmacro delineating-output ((&optional (stream *standard-output*)
					 &key (fraction-of-screen .8)
					 (location :around) (thickness 2))
			      &body body)
  (let ((right-edge (gensym))
	(draw-line (gentemp "DRAW-LINE")))
    `(flet ((,draw-line (where)
	      (when (member ,location  `(,where :around))
	        (when (eq where :after)
		  (terpri ,stream) (terpri ,stream))
	        (clim:with-bounding-rectangle* (left top ,right-edge bottom) (clim:pane-viewport-region ,stream)
                  (declare (ignore left top bottom))
		  (clim:with-room-for-graphics (,stream :height (1+ ,thickness))
		    (clim:draw-line* ,stream
                                     0 1 
                                     (floor (* ,fraction-of-screen ,right-edge)) 1
                                    :line-thickness ,thickness))))))
       (fresh-line ,stream)
       (,draw-line :before)
       ,@body
       (,draw-line :after))))

;;; A little utility function
(defun print-with-truth-value-as-not (predication truth-value &optional (stream *standard-output*))
  (cond
    ((=  truth-value *true*)
     (print-without-truth-value predication stream))
    ((= truth-value *false*)
     (princ "[not " stream)
     (print-without-truth-value predication stream)
     (princ "]" stream))
    ((= truth-value *unknown*)
     (prin1 predication stream)))) 

;;; these are the internal definitions except it evaluates the function-name
;;; the time and the unique-name

#+genera 
(defun unadvise (function)
  (si:unadvise-1 function nil nil))

#+allegro
(defun advise (function fwrapper-name)
  (fwrap function 'joshua-tracing fwrapper-name))

#+allegro
(defun unadvise (function)
  (funwrap function 'joshua-tracing))

#+mcl
(defun advise (function form)
  (let* ((newsym (gentemp "ADVICE"))
         ; WAS typep advise-thing 'method
         (method-p (or (typep function 'ccl::method) ; can this happen?
                       (and (consp function) (eq (car function) :method)))))
    (ccl::advise-2 (eval (ccl::advise-global-def function newsym :around form method-p))
                   newsym method-p function :around 'joshua-tracing
                   nil))) 

#+mcl
(defun unadvise (function)
  (ccl::unadvise-1 function :around 'joshua-tracing))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The defs for joshua debugger which maintains the global state for joshua
;;; tracing

(defclass joshua-debugger ()
  ((event-to-tracer-table :initarg :event-to-tracer-table :accessor joshua-debugger-event-to-tracer-table 
                          :initform (make-hash-table :test #'eq))
   (event-to-collector-table :initarg :event-to-collector-table :accessor joshua-debugger-event-to-collector-table
                             :initform (make-hash-table :test #'eq))
   (event-to-interactor-table :initarg :event-to-interactor-table :accessor joshua-debugger-event-to-interactor-table
                              :initform (make-hash-table :test #'eq))
   (event-to-symbol-table :initarg :event-to-symbol-table :accessor joshua-debugger-event-to-symbol-table
                          :initform (make-hash-table :test #'eq))
   (tracers :initarg :tracers :accessor joshua-debugger-tracers :initform ())
   (active-tracers :initarg :active-tracers :accessor joshua-debugger-active-tracers :initform ())
   (collectors :initarg :collectors :accessor joshua-debugger-collectors :initform ())
   (active-collectors :initarg :active-collectors :accessor joshua-debugger-active-collectors :initform ())
   (tracing-events :initarg :tracing-events :accessor joshua-debugger-tracing-events :initform ())
   (encapsulations :initarg :encapsulations :accessor joshua-debugger-encapsulations :initform ())
   (global-encapsulations :initarg :global-encapsulations :accessor joshua-debugger-global-encapsulations
                          :initform nil)            ;; these must be active whenever any
   ;; tracing is going on.
   (tracing-filter :initarg :tracing-filter :accessor tracing-filter :initform nil)))

;;; a hack for warning people when I'm going to patch something that
;;; will reset their tracing options
(defmethod warn-and-reset ((self joshua-debugger) &optional (warn-p t))
  (with-slots (active-tracers) self
    (when active-tracers
      (when warn-p
        (clim:beep)
        (let ((stream *standard-output*))
          (delineating-output (stream)
            (clim:with-text-style (stream '(nil :bold nil))
	      (format stream "Warning: Disabling and resetting all Joshua tracing.")))))
      (set-joshua-trace-conditions self :tracer :all :enable-p nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Adding and removing filters for controlling interactive tracing/stepping

;;; The filter should be a two arg function that takes the event and the current depth 
;;; as its args. It returns t when tracing should happen, nil if we want to skip the
;;; event. This is used by the Leap command.
(defmethod add-tracing-filter ((self joshua-debugger) continuation)
  (with-slots (tracing-filter) self
    (setf tracing-filter continuation)))

;;; Tracing filters are reset when we reach a rule depth of 0
(defmethod reset-tracing-state ((self joshua-debugger))
  (with-slots (tracing-filter) self
    (setf tracing-filter nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The code for keeping track of individual tracing events 

;;; create a symbol for each event. This is used by the event macros to determine
;;; whether a tracing event is active.
(defmethod create-symbol-for-event ((self joshua-debugger) event-keyword)
  (intern (concatenate 'string "." (symbol-name event-keyword) ".") 'ji))

(defgeneric handle-event (joshua-debugger event &rest args))

(defmethod handle-event ((self joshua-debugger) event &rest args)
  (with-slots (tracing-filter event-to-tracer-table event-to-interactor-table) self
    (when (or (not tracing-filter)
	      (funcall tracing-filter event *rule-depth*))
      (loop for tracer in (gethash event event-to-tracer-table)
	    do (apply 'trace-it tracer event args))
;  (loop for collector in (gethash event event-to-collector-table) do
;    (apply #'collect collector event args))
    (apply #'interact (gethash event event-to-interactor-table) event args))))

(defmethod add-tracing-event ((self joshua-debugger) event)
  (with-slots (tracing-events event-to-symbol-table event-to-tracer-table event-to-collector-table event-to-interactor-table) self
    (let ((old-one (find event tracing-events :test #'(lambda (obj1 obj2)
						        (eq (tracing-event-name obj1)
							    (tracing-event-name obj2)
							    )))))
      (when old-one (delete-tracing-event self old-one)))
    (let ((event-symbol (create-symbol-for-event self (tracing-event-name event))))
      (proclaim `(special ,event-symbol))
      (set event-symbol nil)
      (setf (gethash (tracing-event-name event) event-to-symbol-table) event-symbol))
    (let ((event-name (tracing-event-name event)))
      (setf (gethash event-name event-to-tracer-table) nil)
      (setf (gethash event-name event-to-collector-table) nil)
      (setf (gethash event-name event-to-interactor-table) nil))
    (push event tracing-events)
    event))

(defmethod delete-tracing-event ((self joshua-debugger) event)
  (with-slots (tracing-events event-to-symbol-table event-to-collector-table event-to-tracer-table event-to-interactor-table) self
    (remhash event event-to-symbol-table)
    (remhash event event-to-collector-table)
    (remhash event event-to-tracer-table)
    (remhash event event-to-interactor-table)
    (setf tracing-events (delete event tracing-events))))

(defmethod symbol-for-event ((self joshua-debugger) event)
  (with-slots (event-to-symbol-table) self
    (gethash event event-to-symbol-table)))

;;; keep track of who enabled the tracing event so we can have one event used 
;;; by multiple tracers/collectors
(defmethod enable-event ((self joshua-debugger) event-name enabler-name)
  (with-slots (tracing-events event-to-symbol-table) self
    (let ((event (find event-name tracing-events
		       :test
		       #'(lambda (name obj)
			   (eq name (tracing-event-name obj))))))
      (unless event (error "Unable to find the event ~s in the list of known tracing-events"
			   event-name))
      (set (gethash event-name event-to-symbol-table) t)
      (enable-tracing-event event enabler-name))))

(defmethod disable-event ((self joshua-debugger) event-name disabler-name)
  (with-slots (tracing-events event-to-symbol-table) self
    (let ((event (find event-name tracing-events
		       :test
		       #'(lambda (name obj)
			   (eq name (tracing-event-name obj))))))
      (unless event (error "Unable to find the event ~s in the list of known tracing-events"
			   event-name))
      (set (gethash event-name event-to-symbol-table) nil)
      (disable-tracing-event event disabler-name))))

(defmethod find-event-by-name ((self joshua-debugger) event-name)
  (with-slots (tracing-events) self
    (let ((event (find event-name tracing-events :test #'(lambda (name obj)
						           (eq name (tracing-event-name obj))))))
      (unless event
        (error "There is no known tracing event named ~s" event-name))
      event)))

(defmethod event-stepped-p ((self joshua-debugger) event-object)
  (with-slots (tracers) self
    (loop for tracer in tracers do
          (when (and (member (tracing-event-name event-object)
		             (tracer-current-interaction-events tracer))
	             (tracer-enabled-p self tracer))
            (return t)))))

(defmethod event-traced-p ((self joshua-debugger) event-object)
  (with-slots (tracers) self
    (loop for tracer in tracers do
          (when (and (member (tracing-event-name event-object)
		             (tracer-current-output-events tracer))
	             (tracer-enabled-p self tracer))
            (return t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code for handling the definition and manipulation of tracers which are 
;;; logical collections of tracing events that use the same functions for 
;;; doing output. Things like the forward-rule-tracer, backward-rule-tracer,
;;; etc.

(defmethod find-tracer-by-class-name ((self joshua-debugger) name &optional error-p)
  (with-slots (tracers) self
    (let (tracer)
      (unless (and (find-class name :errorp nil)
		   (setf tracer (find name tracers :test #'(lambda (name obj1) (typep obj1 name)))))
        (when error-p
	  (error "Unknown Type of Tracing ~s. It must must be one of ~s."
	         name
	         (loop for a-tracer in tracers 
		       collect (tracer-unique-id a-tracer)))))
      tracer)))

(defmethod add-tracer ((self joshua-debugger) tracer)
  (with-slots (event-to-interactor-table tracers event-to-tracer-table) self
    (let ((old-one (find-tracer-by-class-name self (type-of tracer))))
      (when old-one
        (delete-tracer self old-one)))
    (loop for event in (tracer-events tracer) do
          (push tracer (gethash event event-to-tracer-table))
          (setf (gethash event event-to-interactor-table) tracer))
    (push tracer tracers)))

(defmethod delete-tracer ((self joshua-debugger) tracer)
  (with-slots (tracers active-tracers event-to-tracer-table event-to-interactor-table) self
    (when (member tracer active-tracers)
      (disable-tracer self tracer)
      (setf active-tracers (delete tracer active-tracers)))
    (setf tracers (delete tracer tracers))
    (maphash #'(lambda (key value)
	         (when (member tracer value)
		   (setf (gethash key event-to-tracer-table)
		         (delete tracer value))))
	     event-to-tracer-table)
    (maphash #'(lambda (key value)
	         (when (eq tracer value)
		   (setf (gethash key event-to-interactor-table) nil)))
	     event-to-tracer-table)))

(defmethod enable-tracer ((self joshua-debugger) tracer
	                  &key (new-output-events (tracer-current-output-events tracer))
	                  (new-interaction-events (tracer-current-interaction-events tracer)))
  (with-slots (active-tracers) self
    (pushnew tracer active-tracers)
    (enable tracer new-output-events new-interaction-events)
    (ensure-global-encapsulation-state self)))

(defmethod disable-tracer ((self joshua-debugger) tracer)
  (with-slots (active-tracers) self
    (disable tracer)
    (setf active-tracers (delete tracer active-tracers))
    (ensure-global-encapsulation-state self)))
  
(defmethod tracer-enabled-p ((self joshua-debugger) tracer)
  (with-slots (active-tracers) self
    (member tracer active-tracers)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; creating and manipiulating the encapsulations for joshua tracing.

;;; After an encapsulation is defined it should be added to the tracing-events
;;; that it includes

(defmethod add-encapsulation-to-events ((self joshua-debugger) encaps events-list)
  (with-slots (tracing-events) self
    (loop for event in events-list do
          (add-encapsulation
           (find event tracing-events :test #'(lambda (name each-event)
					        (eq (tracing-event-name each-event)
					            name)))
           encaps))))
;;; This depends on the Genera Encapsulation Framework

;;; create a new type of encapsulation for joshua-tracing this should 
;;; go into the system source at the next release

#+genera
(unless (member 'si:joshua-trace si:encapsulation-standard-order)
  (setf si:encapsulation-standard-order
	(append si:encapsulation-standard-order '(si:joshua-trace))))


#+genera
(defun unencapsulate-function-for-joshua-tracing (function-desc)
  (let* ((spec1 (si:unencapsulate-function-spec function-desc 'si:joshua-trace))
	 (spec2 (si:unencapsulate-function-spec spec1 '(si:joshua-trace))))
    (and (scl:neq spec1 spec2)
	 ;; make sure it is unencapsulated
	 (scl:fdefine spec1 (fdefinition spec2)))))

#-genera
(defun unencapsulate-function-for-joshua-tracing (function)
  (unadvise function)
  (fdefinition function))

(defun smash (&rest names)
  (let* ((string-length (loop for name in names sum (length (string name))))
         (total-length (+ string-length (1- (length names))))
         (string (make-array total-length :element-type 'character)))
    (loop with pointer = 0
          for first = t then nil
          for name in names
          for this-string = (string name)
          unless first
          do (setf (aref string pointer) #\-)
             (incf pointer)
          do (replace string this-string :start1 pointer)
          (incf pointer (length this-string)))
    (intern string)))


;;; these are just around for debugging the encapsulation and event 
;;; mechanisms
;
;(defmethod (do-encapsulations joshua-debugger)()
;  (loop for encaps in encapsulations do
;    (when (joshua-encapsulation-enabled-p encaps)
;      (do-encapsulation encaps))))
;
;(defmethod (undo-encapsulations joshua-debugger)()
;  (loop for encaps in encapsulations do
;    (loop for enabler in (joshua-encapsulation-enablers encaps) do 
;      (undo-encapsulation encaps enabler))))
;;
;;;; these are just for debugging 
;(defmethod (clear-encapsulations joshua-debugger)()
;  (setf  encapsulations nil))
;
;(defmethod (enable-all-encapsulations joshua-debugger)()
;  (loop for encaps in encapsulations do
;    (setf (joshua-encapsulation-enabled-p encaps) t))
;  (do-encapsulations self))
;
;(defmethod (enable-all-tracing-events joshua-debugger)()
;  (maphash #'(lambda (ignore value &rest ignore)
;	       (set value t))
;	   event-to-symbol-table)
;  (enable-all-encapsulations self))


;;; Keep track of the current encapsulations. Global encapsulations are those
;;; that must be done whenever any kind of tracing is enabled. For example, 
;;; the encapsulations that maintain the *rule-depth* variable.

(defmethod add-encapsulation ((self joshua-debugger) encapsulation &optional global-p)
  (with-slots (encapsulations global-encapsulations) self
    (let ((old-one (find encapsulation encapsulations
		         :test #'(lambda (obj1 obj2)
				   (eq (joshua-encapsulation-name obj1)
				       (joshua-encapsulation-name obj2))))))
      (when old-one
        (setf encapsulations (delete old-one encapsulations))
        (setf global-encapsulations (delete old-one global-encapsulations))))
    (push encapsulation encapsulations)
    (when global-p
      (push encapsulation global-encapsulations))
    encapsulation))

(defmethod ensure-global-encapsulation-state ((self joshua-debugger))
  (with-slots (global-encapsulations active-tracers) self
    (if active-tracers
      (loop for encaps in global-encapsulations do
            (do-encapsulation encaps :global))
      (loop for encaps in global-encapsulations do
            (undo-encapsulation encaps :global)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; finally some methods used by joshua-debugger to interact with the user

;;; this is called by the tracer after it determines that the event is one 
;;; that the user wants to step at. 


;;; This is still very DW specific

(defgeneric interact (debugger event thing &key &allow-other-keys))

(defmethod interact ((self joshua-debugger) event thing &key &allow-other-keys)
  (declare (ignore event thing ))
  (labels ((read-acc ()
	     (clim:read-command *joshua-tracing-command-table*
	       :stream *terminal-io*
	       )))
    (let ((com-values nil)
	  ;; (clim:*command-table* *joshua-tracing-command-table*)
          )
      ;; any command that returns :done will exit the loop
      (loop until (eq :done (car com-values))
	    do
	    ;; reinitialize com-values so we don't get stuck obeying old ones
	;; (setf cp:*last-command-values* nil)
	(let ((aborted-p
		;; signal abort only when 'aborted is thrown
		(catch 'aborted
                  (with-simple-restart (error "Joshua Trace Command Loop")
		    (with-simple-restart (abort "Joshua Trace Command Loop")
		      (multiple-value-bind (command args flag) (read-acc)
		        (format t "~%~s ~%~s ~%~s" command args flag)
		        (case flag
			  ((:command :accelerator)
			   (apply command args))
			  ((nil) (unless (or (eq args :full-rubout) (null command))
				   (apply command args))))
		        ;; (setf com-values  cp:*last-command-values*)
                        )))
		  nil)))
	  (when aborted-p (abort)))))))

;;; I suppose this should be created statically to avoid consing, but it
;;; makes it easier to patch in new tracers if it isn't
(defmethod create-tracer-alist ((self joshua-debugger) 
                                &key (tracer-list (joshua-debugger-tracers self))
                                (extra-items nil)
                                (First-word-of-doc-strings "Enable"))
  (with-slots () self
    (loop for tracer in tracer-list
	  collect `(,(tracer-name tracer) :value ,tracer
		    :documentation ,(format nil "~a the tracing of ~a"
					    First-word-of-doc-strings
					    (tracer-name tracer)))
          into alist
	  finally (return (append (nreverse alist) extra-items)))))

;;; Set the tracing options, either explicitly or via menu. With enable-p nil
;;; it will disable the tracing options. Here's a good example of a function 
;;; that should really be three or four functions. 
(defmethod set-joshua-trace-conditions ((self joshua-debugger)
	                                &key tracer menu enable-p (trace-events nil trace-supplied-p)
		                        (step-events nil step-supplied-p)
		                        (stream *query-io*) (own-window-p nil))
  (with-slots (tracers active-tracers) self
    (let ((disable-list (when (not enable-p)
			  (if (eq tracer :all) tracers `(,tracer))))
	  (enable-list (when enable-p
		         (if (eq tracer :all) tracers `(,tracer)))))
      (if menu
        ;; Conditions is the list of all tracers that we want to deal with
        (let* ((conditions (if (eq tracer :all) tracers `(,tracer)))
               ;; tracing-list is the list of tracers that should be enabled when the 
               ;;  menu comes up. Here's a little set logic - I don't want to actually 
               ;; enable them until the menu returns normally.
               (tracing-list (set-difference (intersection (union active-tracers enable-list)
                                                           conditions)
                                             disable-list))
               ;; just the alist for accepting conditions
               (tracer-alist (create-tracer-alist self :tracer-list conditions
                                                  :first-word-of-doc-strings "Toggle"))
               ;; These guys will hold the alists of tracers/properties that get munged
               ;; during the accepting-values below
               (output-events-list  (loop for tracer in conditions
                                          ;; these lists will be destructively altered
                                          collect
                                          (cons tracer (tracer-current-output-events tracer)) into alist
                                          finally (return alist)))
               (interaction-events-list (loop for tracer in conditions
                                              ;; these lists will be destructively altered
                                              collect
                                              (cons tracer (tracer-current-interaction-events tracer)) into alist
                                              finally (return alist)))
               (tracer-action-list (loop for tracer in conditions
                                         ;; these lists will be destructively altered
                                         collect (cons tracer (create-tracing-options-alist tracer)) into alist
                                         finally (return alist)))
               ;; Generate some unique-ids for the step and trace event prompts
               (query-identifier-list
                (loop for sub-tracers on tracers
                      do (progn sub-tracers)    ; progn = ignore
                      collect (gensym) into qi-list 
                      collect (gensym) into qi-list
                      finally (return qi-list))))
          (flet ((print-heading (heading stream)
                   (terpri stream) 
                   (clim:with-text-style (stream *deemphasis-character-style*)
                     (princ heading stream))
                   (terpri stream))
                 ;; Get the unique ID
                 (get-query-identifier (tracer &key step)
                   (nth (if step (1+ (* 2 (position tracer tracers)))
                            (* 2 (position tracer tracers)))
                        query-identifier-list)))
            ;; Let success indicate the normal completion of the accepting values (No abort)
            (let ((success-p
                   (catch 'aborted
                     (clim:with-text-style (stream '(nil nil nil))
                       (terpri stream)
                       (restart-bind
                         ((abort #'(lambda (ignore)
                                     (declare (ignore ignore))
                                     (throw 'aborted nil))))
                         (clim:accepting-values (stream :own-window own-window-p 
                                                        :label "Joshua Tracing Options"
                                                        ;; :queries-are-independent nil   ?????
                                                        )
                           (setf tracing-list
                                 (clim:accept `(clim:subset-alist ,tracer-alist)
                                              :prompt nil
                                              :prompt-mode :raw
                                              :default tracing-list
                                              :stream stream))
                           ;; this reverse of tracing list will make the most 
                           ;; recently enabled appear last - minimize redisplay
                           (loop for tracer in (reverse tracing-list)
                                 do
                                 (clim:updating-output (stream :cache-value t :unique-id tracer)
                                   (print-heading (concatenate 'string (tracer-name tracer) " Options") stream))
                                 (terpri stream)
                                 (accept-tracing-options tracer stream (cdr (assoc tracer tracer-action-list)))
                                 (terpri stream)
                                 (setf (cdr (assoc tracer output-events-list))
                                       (accept-events tracer stream
                                                      :prompt "Traced Events "
                                                      :query-identifier (get-query-identifier tracer)
                                                      :default (cdr (assoc tracer output-events-list))))
                                 (terpri stream)
                                 (setf (cdr (assoc tracer interaction-events-list))
                                       (accept-events tracer stream
                                                      :prompt "Stepped Events"
                                                      :query-identifier (get-query-identifier tracer :step t)
                                                      :default (cdr (assoc tracer interaction-events-list))
                                                      ))))))
                     ;;explicitly return t to signal successfull completion
                     t)))
              ;; The user's hit ” so we can do our stuff
              (if success-p
                (loop for tracer in conditions
                      do
		      (cond ((not (member tracer tracing-list))
                             (disable-tracer self tracer))
                            (t
                             (enable-tracer self tracer
                                            :new-output-events (cdr (assoc tracer output-events-list))
                                            :new-interaction-events (cdr (assoc tracer interaction-events-list)))
                             (set-tracing-options tracer (cdr (assoc tracer tracer-action-list))))
                            ))
                (format stream "~&No changes made to the tracing options")))))
        ;;; If the user didn't want the menu just do what they ask
        (cond (enable-p
               (unless (eq tracer :all)
                 (setf step-events
                       (if step-supplied-p
                         (loop for event in step-events
                               collect (tracing-event-name event)
                               into new-events
                               finally (return new-events))
                         (tracer-current-interaction-events tracer)))
                 (setf trace-events
                       (if trace-supplied-p
                         (loop for event in trace-events
                               collect (tracing-event-name event)
                               into new-events
                               finally (return new-events))
                         (tracer-current-output-events tracer))))
               (if (eq tracer :all)
                 (loop for each-tracer in tracers do
		       (enable-tracer self each-tracer))
                 (enable-tracer self tracer
                                :new-interaction-events step-events
                                :new-output-events trace-events)))
              (t (if (eq tracer :all)
                   (loop for each-tracer in tracers do
		         (disable-tracer self each-tracer))
                   (disable-tracer self tracer))))))))

;;; Print information about the global tracing state
(defmethod show-joshua-tracing ((self joshua-debugger) tracer-or-all)
  (with-slots (tracers) self
    (delineating-output (*standard-output* :fraction-of-screen .9)
      (if (eq tracer-or-all :all)
	(loop for (tracer . rest) on tracers
	      while rest
	      do
	      (delineating-output (*standard-output* :fraction-of-screen .6 
						     :location :after :thickness 1)
	        (show-tracing-state tracer))
	      finally (show-tracing-state tracer))
	(show-tracing-state tracer-or-all)))))


(defmethod get-events-alist ((self joshua-debugger) &optional (tracer-or-all :all))
  (with-slots (tracing-events) self
    (if (eq tracer-or-all :all)
      (loop for event in tracing-events
	    collect `(,(tracing-event-pretty-name event)
		      :value ,event
		      :documentation
		      ,(tracing-event-documentation event))
            into alist
	    finally (return alist))
      (loop for event in (tracer-events tracer-or-all)
	    for event-object = (find-event-by-name self event)
	    collect `(,(tracing-event-pretty-name event-object)
                      :value ,event-object :documentation
                      ,(tracing-event-documentation event-object))
            into alist
	    finally (return alist)))))

(defmethod get-current-events ((self joshua-debugger) step-or-trace &optional (tracer-or-all :all))
  (with-slots (tracers) self
    (flet ((get-events-for-tracer (tracer)
             (loop for event in
                   (case step-or-trace
                     (:step (tracer-current-interaction-events tracer))
                     (:trace (tracer-current-output-events tracer)))
                   collect (find-event-by-name self event) into event-list
                   finally (return event-list))))
      (if (eq tracer-or-all :all)
        (loop for tracer in tracers
              append (get-events-for-tracer tracer)
              into all-event-list
              finally (return all-event-list))
        (get-events-for-tracer tracer-or-all)))))

(defmethod collect-options-for-object ((self joshua-debugger) object)
  (with-slots (tracers) self
    (if  (typep object 'tracing-event)
      (loop for tracer in tracers
            nconc (options-for-event tracer object)
            into option-action-alist
            finally (return option-action-alist))
      (loop for tracer in tracers
            nconc (options-for-object tracer object)
            into option-action-alist
            finally (return option-action-alist)))))

(defun abbreviate-string (string length)
  (if (> (length string) length)
      (concatenate 'string (subseq string 0 (1- length)) "...")
      string))

(defmethod get-all-trace-options ((self joshua-debugger) object)
  (let* ((options (collect-options-for-object self object))
	 (choice
	   (clim:menu-choose
	     options
	     ;; :center-p t
	     :label
	     (if (= (length options) 1) "Confirm:"
		 (abbreviate-string
		   (format nil "Joshua Tracing Options for: ~a"
			   (if (typep object 'tracing-event)
			       (tracing-event-pretty-name object)
			       object))
		   *trace-options-menu-width*)))))
    (when choice (apply (car choice) `(,(cadr choice) ,object))
	  (delineating-output (*standard-output*)
	    (show-tracing-state (cadr choice)))
	  )))

(defmacro without-joshua-tracing (&body body)
  (let ((saved-active-tracers (gentemp "TRACERS")))
    `(let ((,saved-active-tracers (joshua-debugger-active-tracers *joshua-debugger*)))
       (unwind-protect
	   (progn (loop for tracer in ,saved-active-tracers do
		    (disable-tracer *joshua-debugger* tracer))
		  ,@body)
	 (loop for tracer in ,saved-active-tracers do
	   (enable-tracer *joshua-debugger* tracer))))))

(defmacro with-joshua-tracing (tracing-types &body body)
  (let ((tracers-coming-in (gentemp "TRACERS-IN"))
	(tracers-going-out (gentemp "TRACERS-OUT")))
    `(let* ((,tracers-coming-in nil)
	    (,tracers-going-out (joshua-debugger-active-tracers *joshua-debugger*)))
       (loop for ttype in ,tracing-types
	     for tracer = (find-tracer-by-class-name *joshua-debugger* ttype t)
	     do (if (member tracer ,tracers-going-out)
		    (setq ,tracers-going-out (delete tracer ,tracers-going-out))
		    (push tracer ,tracers-coming-in)))
       (unwind-protect
	   (progn (loop for tracer in ,tracers-going-out do
		    (disable-tracer *joshua-debugger* tracer))
		  (loop for tracer in ,tracers-coming-in do
		    (enable-tracer *joshua-debugger* tracer))
		  ,@body)
	 (loop for tracer in ,tracers-coming-in do
	   (disable-tracer *joshua-debugger* tracer))
	 (loop for tracer in ,tracers-going-out do
	   (enable-tracer *joshua-debugger* tracer))))))


;;; Return all tracers to their default settings but don't turn them off
;;; altogether
(defmethod reset-tracers ((self joshua-debugger) tracer-or-all &optional events-too-p (verbose-p t))
  (with-slots (tracers) self
    (if (eq tracer-or-all :all)
      (loop for tracer in tracers do
	    (reset-defaults tracer events-too-p))
      (reset-defaults tracer-or-all events-too-p)))
  (when verbose-p (show-joshua-tracing self tracer-or-all)))

;;; This will define a method and a method on joshua-debugger to get to it
(defmacro define-joshua-tracing-method ((name class) argument-list &body body)
  `(progn (defmethod ,name ((self ,class) ,@argument-list)
	    ;; (declare (sys:function-parent ,name define-joshua-tracing-method))
	    ,@body)
	  (defmethod ,name ((self joshua-debugger) ,@argument-list)
	    (declare ;; (sys:function-parent ,name  define-joshua-tracing-method)
		     (arglist ,argument-list))
	    (,name (find-tracer-by-class-name self ',class) ,@argument-list))))


;;; And make an instance of this as it must be around by the time we 
;;; load the defs of encapsulations, tracing-events, tracers, etc.

(defparameter *joshua-debugger* (make-instance 'joshua-debugger))


(warn-and-reset *joshua-debugger*)

;;; Just a kludge to allow people to easily enable and disable tracing
;;; in their code. This should really be a better defined interface.
(defun enable-joshua-tracing (tracing-type &optional (verbose t))
  (let ((tracer (find-tracer-by-class-name *joshua-debugger* tracing-type t)))
    (enable-tracer *joshua-debugger* tracer)
    (when verbose (show-tracing-state tracer))))

(defun disable-joshua-tracing (tracing-type &optional (verbose t))
  (let ((tracer (find-tracer-by-class-name *joshua-debugger* tracing-type t)))
    (disable-tracer *joshua-debugger* tracer)
    (when verbose (show-tracing-state tracer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; forms for defining tracing events
;;; the class for tracing events 

(defmethod add-encapsulation ((self tracing-event) encaps &optional global-p)
  (declare (ignore global-p))
  (with-slots (encapsulations) self
    (let ((old-one (find encaps encapsulations :test #'(lambda (x y)
						         (eq (joshua-encapsulation-name x)
							     (joshua-encapsulation-name y))))))
      (when old-one
        (setf encapsulations (delete old-one encapsulations))))
    (push encaps encapsulations)))

(defmethod enable-tracing-event ((self tracing-event) enabler)
  (with-slots (enablers encapsulations enabling-function enabled-p name) self
    (pushnew enabler enablers)
    (unless enabled-p
      (loop for encaps in encapsulations do
            (do-encapsulation encaps name))
      (when enabling-function (funcall enabling-function ))
      (setf enabled-p t))))

(defmethod disable-tracing-event ((self tracing-event) disabler)
  (with-slots (enablers enabled-p encapsulations name disabling-function enabling-function) self
    (when (member disabler enablers)
      (setf enablers (delete disabler enablers)))
    (when (and enabled-p (null enablers))
      (loop for encaps in encapsulations do
            (undo-encapsulation encaps name))
      (when disabling-function (funcall disabling-function ))    ; ???? said call enabling fn fixed
      (setf enabled-p nil))))

(defmethod enable-alist-item ((self tracing-event) )
  (with-slots (name) self
    `(,name :value self :documentation ,(format nil "Enable tracing of ~a" name))))

;;; create a macro that will filter on whether we are actually interested in the event
(defmacro create-macro-for-event (name arglist event-name)
  `(defmacro ,name (&rest arguments)
     (declare (arglist ,@arglist)
	      ;;(sys:function-parent ,name define-tracing-event)    ????
              )
     (let ((symbol (symbol-for-event *joshua-debugger* ,event-name)))
       `(when ,symbol
	  (handle-event
	    *joshua-debugger* ,',event-name ,@arguments)))))


;;; a macro to define interesting tracing events 
(defmacro define-tracing-event (name pretty-name 
				&key (short-name pretty-name)
				(active-name pretty-name)
				macro-name
				arglist
				documentation
				enabling-function disabling-function)
  (let ((instance (gensym)))
    `(let ((,instance (make-instance 'tracing-event
				     :name ',name
				     :pretty-name ,pretty-name
				     :short-name ,short-name
				     :active-name ,active-name
				     :documentation ,documentation
				     :enabling-function ,enabling-function
				     :disabling-function ,disabling-function)))
       (add-tracing-event *joshua-debugger* ,instance)
       (create-macro-for-event  ,macro-name ,arglist ',name)
      )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Forms for defining joshua-encapsulations
				
;;; the class for keeping track of encapsulations
(defclass joshua-encapsulation ()
  ((name :initarg :name :accessor joshua-encapsulation-name)
   (original-fspec :initarg :original-fspec :accessor joshua-encapsulation-original-fspec) 
   (arglist :initarg :arglist :accessor joshua-encapsulation-arglist)
   (cached-encapsulation :initarg :cached-encapsulation :accessor joshua-encapsulation-cached-encapsulation :initform nil)
   (cached-fdefinition :initarg :cached-fdefinition :accessor joshua-encapsulation-cached-fdefinition :initform nil)
   (enablers :initarg :enablers :accessor joshua-encapsulation-enablers :initform ())
   (wrap-body :initarg :wrap-body :accessor joshua-encapsulation-wrap-body :initform nil)
   (before-body :initarg :before-body :accessor joshua-encapsulation-before-body :initform nil)
   (after-body :initarg :after-body :accessor joshua-encapsulation-after-body :initform nil)
   (call-original-p :initarg :call-original-p :accessor joshua-encapsulation-call-original-p :initform nil)
   (enabled-p :initarg :enabled-p :accessor joshua-encapsulation-enabled-p :initform nil)))

;;; This will force the precompilation of encapsulations at load time. It also 
;;; will make sure that the encapsulation can compile when it is compiled to
;;; core, instead of having to enable it to make sure it compiles.

;;;; in CLOS you do this stuff on initialize-instance not make-instance
;;; the above comment is wrong, you can do it on either.
(defmethod initialize-instance :after ((self joshua-encapsulation) &rest ignore)
  (declare (ignore ignore))
  (do-encapsulation self :self)
  (undo-encapsulation self :self))

;;; We need to look at what event is enabling us. This makes a sloppy
;;; attempt at returning the value from the encapsulated function. The 
;;; general approach to encapsulation probably could use to be rethought.

#| Comments by hes on porting strategy:

The original version assumed access to very low level mechanisms that allowed advising almost anything.
In particular, it assumed that you can advise a specific method.  I believe that certain implementations
make this hard or require access to mechanisms that the prudent implementor would avoid.  In Genera, for example,
I'm not sure that you can, in fact, trace method, as opposed to the generic function.

Looking through the actual uses, I believe that the only uses of method tracing are for ltms specific implementations 
of the basic tms protocol.   While it's possible that we might someday add a variety of other tms styles, we haven't
done it yet (after a number of years).  Therefore, although it's less elegant by far (in fact a kludge) we're just 
going to encapsulate the generic function and typecase the first argument (the origin of all this was flavors after all
where only the first argument was used for dispatch.

If this approach fails we can alway think of other options.

Every implementation has an Advise capability (check allegro) but they vary.  
arglist convention of MCL for the moment.

The MCL arglist is: fspec the-advise &key when name define-if-not 
where name is a unique name for this piece of advise.

Allegro uses the following format: fspec where name position &rest forms
position is for ordering separate pieces of advise.

Allegro also has a facility called fwrapper which is more general, but not available in MCL.



|#

;;; so the assumption based on above is that original fspec is a symbol, naming a generic function

#+mcl
(defun function-encapsulated-p (fspec)
  (ccl::advisedp-1 fspec nil nil))

(defun symbol-macros-from-arglist (arglist)
  (flet ((symbol-macro-from-name (name position)
           `(,name (nth ,position arglist))))
  (loop for name in arglist
        for i from 0
        collect (symbol-macro-from-name name i))))

(defmethod do-encapsulation ((self joshua-encapsulation) enabler)
  (with-slots (#+allegro name enablers enabled-p original-fspec cached-fdefinition cached-encapsulation
	       arglist after-body before-body encapsulated-function
	       call-original-p wrap-body) self
    (unless (member enabler enablers)
      (pushnew enabler enablers)
      (unless enabled-p 
        (unencapsulate-function-for-joshua-tracing original-fspec)
	#+genera
	(scl:if (and (eq (fdefinition original-fspec) cached-fdefinition)
		     (not (si:function-encapsulated-p original-fspec)))
          ;; if so, we can just fdefine the original fspec to the old encapsulation
          (scl:fdefine original-fspec cached-encapsulation nil t)
		(let ((si:inhibit-fdefine-warnings 't))
		  (si:encapsulate-and-compile
		    (si:unencapsulate-function-spec original-fspec 'si:joshua-trace)
		    original-fspec
		    'si:joshua-trace
		    `(symbol-macrolet ,(symbol-macros-from-arglist arglist)
		       ,before-body
		       (let ((.answer
			       ,(when call-original-p
				  `(multiple-value-list
				     (apply ,si:encapsulated-function si:arglist)))))
			 ,after-body
			 (apply #'values .answer)))))
		;; Save the encapsulation so we can reuse it
		(setf cached-encapsulation
		      (fdefinition 
			(si:unencapsulate-function-spec original-fspec 'si:joshua-trace))))
        #+mcl
        (advise
	  original-fspec
	  `(let ((.answer. nil))
	     (symbol-macrolet ,(symbol-macros-from-arglist arglist)
	       ,before-body
	       ,(when call-original-p
		  (if wrap-body
		      `(macrolet ((call-next () `(setq .answer. (multiple-value-list (:do-it)))))
			 ,@wrap-body)
		      `(setq .answer. (multiple-value-list (:do-it)))))
	       ,after-body
	       (apply #'values .answer.))))
        #+allegro
        (fwrap original-fspec 'joshua-tracing name)
        (setf enabled-p t)))))

;;; keep track of which events are enabling this encaps and only 
;;; unencapsulate it when no one's interested
(defmethod undo-encapsulation ((self joshua-encapsulation) disabler)
  (with-slots (enablers original-fspec enabled-p) self
    (setf enablers (delete disabler enablers))
    (unless enablers
      (unencapsulate-function-for-joshua-tracing original-fspec)
      (setf enabled-p nil))))

;;; the macro for defining encapsulations
(defmacro define-joshua-encapsulation ((name original-fspec call-original-p event-list
					     &optional global-p)
				       &key before-body after-body body-wrapper 
					    (arglist (arglist original-fspec)))
  ;; (declare (zwei:indentation 1 1 1 1))
  (let ((instance (gensym)))
    `(progn
       #+allegro
       (def-fwrapper ,name ,arglist
         (let ((.answer. nil))
           ,before-body
           ,(when call-original-p
              (if body-wrapper
                `(macrolet ((call-next () `(setq .answer. (multiple-value-list (call-next-fwrapper)))))
                   ,@body-wrapper)
                `(setq .answer. (multiple-value-list (call-next-fwrapper)))))
           ,after-body
           (apply #'values .answer.)))
       (let ((,instance (make-instance 'joshua-encapsulation
                          :name ',name
                          :arglist ',arglist
                          :original-fspec ',original-fspec
                          :call-original-p ,call-original-p
                          :wrap-body ',body-wrapper
                          :before-body ',before-body
                          :after-body ',after-body)))
         (add-encapsulation *joshua-debugger* ,instance ,global-p)
         (add-encapsulation-to-events *joshua-debugger* ,instance ,event-list)
         ))))

;;; The macro used to tell the debugger about a tracing entity and to 
;;; create the tracing macro.
(defmacro define-tracer (class-name name
			 &key pretty-name events default-trace-events
			 default-step-events 
			 )
  `(add-tracer
     *joshua-debugger*
     (make-instance ',class-name
		    :pretty-name ,pretty-name
		    :events ,events
		    :current-output-events ,default-trace-events
		    :current-interaction-events ,default-step-events
		    :name ,name
;		    :events-alist ,events-alist
		    )))

;;; A hack to unify the predications with explicit truth-values
;;; Note that this must be called from inside a unification-block
(defun unify-predications-with-explicit-t-vs (predication-1 t-v-1
					      predication-2 t-v-2)
  (cond ((eq (predication-predicate  predication-1) 'not)
	 (unify-predications-with-explicit-t-vs
	   (not-predication predication-1)
	   (negate-truth-value t-v-1)
	   predication-2 t-v-2))
	((eq (predication-predicate predication-2) 'not)
	 (unify-predications-with-explicit-t-vs
	   predication-1
	   t-v-1
	   (not-predication predication-2)
	   (negate-truth-value t-v-2)))
	(t (and (= t-v-1 t-v-2)
		(unify predication-1 predication-2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The base class for tracers

(defclass tracer ()
  ((events :initarg :events :accessor tracer-events :initform nil)
   (events-alist :initarg :events-alist :accessor trace-alist :initform nil)
   (unique-id :initarg :unique-id :accessor tracer-unique-id)
   (name :initarg :name :accessor tracer-name)
   (pretty-name :initarg :pretty-name :accessor tracer-pretty-name)
   (current-output-events :initarg :current-output-events :accessor tracer-current-output-events :initform nil)
   (current-interaction-events :initarg :current-interaction-events :accessor tracer-current-interaction-events :initform nil)
   (default-output-events :initarg :default-output-events :accessor tracer-default-output-events :initform nil)
   (default-interaction-events :initarg :default-interaction-events :accessor tracer-default-interaction-events :initform nil)
   ))

;;;; make-instance in CLOS is the wrong method for this, initialize-instance is the right one
(defmethod initialize-instance :after ((self tracer) &rest ignore)
  (declare (ignore ignore))
  (with-slots (events-alist default-output-events current-output-events default-interaction-events current-interaction-events unique-id) self
    (unless events-alist
      (set-events-alist self))
    ;; Store the original event settings so we can reset them
    (setf default-output-events current-output-events
	  default-interaction-events current-interaction-events)
    ;; Ok, so its a hack...
    (setf unique-id (type-of self))))

(defmethod set-events-alist ((self tracer) )
  (with-slots (events-alist events) self
    (setf events-alist
          (loop for event-name in events
                for event = (find-event-by-name *joshua-debugger* event-name)
                collect `(,(tracing-event-short-name event)
                          :value ,event-name
                          :documentation
                          ,(tracing-event-documentation event))))))

(defmethod accept-events ((self tracer) &optional (stream *query-io*)
                          &key (prompt "")  default query-identifier )
  (with-slots (events-alist) self
    (clim:accept `(clim:subset-alist ,events-alist)
	         :stream stream
	         :prompt prompt
                 ;	  #'(lambda (stream ignore)
                 ;			(with-character-style ('(nil :italic nil) stream)
                 ;			  (format stream "~a: ~%" prompt)))
	         :prompt-mode :raw
	         :default default
	         :query-identifier query-identifier
	         )))

;;; the method for enabling a tracer. 
(defmethod enable ((self tracer) new-output-events new-interaction-events)
  (with-slots (unique-id current-output-events current-interaction-events events) self
    (setf current-output-events new-output-events)
    (setf current-interaction-events new-interaction-events)
    (loop for event in events do
          (if (or (member event new-output-events)
	          (member event new-interaction-events))
	    (enable-event *joshua-debugger* event unique-id)
	    (disable-event *joshua-debugger* event unique-id)))))

(defmethod find-event-in-alist ((self tracer) event alist)
  (with-slots () self
    (loop for item in alist
	  when (member event item)
	  do
          (return (car item)))))

(defmethod show-event-state ((self tracer) &optional (stream *standard-output*))
  (with-slots (current-output-events current-interaction-events events-alist) self
    (flet ((present-event (event stream)
             (clim:with-output-as-presentation (stream (find-event-by-name *joshua-debugger* event) 'tracing-event
                                                       :allow-sensitive-inferiors nil)
               (princ (find-event-in-alist self event events-alist)
                      stream))))
      (when  current-output-events
        (format stream "~&Traced event~P: " (length current-output-events))
        (clim:format-textual-list current-output-events #'present-event
			     ;; :filled t   ????
			     :conjunction "and"
                             :stream stream
			     ;; :if-two " and "
                             ))
      (when current-interaction-events
        (format stream "~&Stepped event~P: " (length current-interaction-events))
        (format-textual-list current-interaction-events
			     #'present-event
                             :stream stream
			     ;; :filled t  ????
			     :conjunction "and"
			     ;; :if-two " and "
                             )))))

(defmethod options-for-event ((self tracer) event)
  (with-slots (events current-output-events current-interaction-events name) self
    (let ((event-name (tracing-event-name event))
	  (alist nil))
      (when (member event-name events)
        (if (member event-name current-output-events)
	  (push `("Untrace event"
		  :value (untrace-event ,self)
		  :documentation ,(format nil "Disable the tracing of the ~a event ~a"
					  name (tracing-event-pretty-name event)))
		alist)
	  (push `("Trace event"
		  :value (add-trace-event ,self)
		  :documentation ,(format nil "Enable the tracing of the ~a event ~a"
					  name (tracing-event-pretty-name event)))
		alist))
        (if (member event-name current-interaction-events)
	  (push `("Unstep event"
		  :value (unstep-event ,self)
		  :documentation ,(format nil "Disable stepping of the ~a event ~a"
					  name (tracing-event-pretty-name event)))
		alist)
	  (push `("Step event"
		  :value (add-step-event ,self)
		  :documentation ,(format nil "Enable the stepping of the ~a event ~a"
					  name (tracing-event-pretty-name event)))
		alist)))
      alist)))

(defmethod add-trace-event ((self tracer) event)
  (with-slots (current-output-events unique-id) self
    (let ((event-name (tracing-event-name event))) 
      (pushnew event-name current-output-events)
      (enable-event *joshua-debugger* event-name unique-id)
      (ensure-event-state self t))))

(defmethod untrace-event ((self tracer) event)
  (with-slots (unique-id current-output-events current-interaction-events) self
    (let ((event-name (tracing-event-name event))) 
      (setf current-output-events (delete event-name current-output-events))
      (unless (member event-name current-interaction-events)
        (disable-event *joshua-debugger* event-name unique-id))
      (ensure-event-state self ))))

(defmethod add-step-event ((self tracer) event)
  (with-slots (current-interaction-events unique-id) self
    (let ((event-name (tracing-event-name event)))
      (pushnew event-name current-interaction-events)
      (enable-event *joshua-debugger* event-name unique-id)
      (ensure-event-state self t))))

(defmethod unstep-event ((self tracer)  event)
  (with-slots (current-interaction-events current-output-events unique-id) self
    (let ((event-name (tracing-event-name event)))
      (setf current-interaction-events
	    (delete event-name current-interaction-events))
      (unless (member event-name current-output-events)
        (disable-event *joshua-debugger* event-name unique-id))
      (ensure-event-state self))))

(defmethod ensure-event-state ((self tracer) &optional (must-be-enabled-p))
  (with-slots (current-interaction-events current-output-events) self
    (unless (or current-output-events current-interaction-events)
      (reset-events self)
      (unless must-be-enabled-p
        (disable-tracer *joshua-debugger* self)))
    (when must-be-enabled-p
      (enable-tracer *joshua-debugger* self))))

(defmethod reset-events ((self tracer) )
  (with-slots (current-output-events default-output-events current-interaction-events default-interaction-events) self
    (setf current-output-events default-output-events
	  current-interaction-events default-interaction-events)))

(defmethod disable ((self tracer) )
  (with-slots (self unique-id events) self
    (loop for event in events do
          (disable-event *joshua-debugger* event unique-id))))

;;; A method which modifies the options from a list of instance vars and 
;;; values produced by accepting values.

(defmethod set-tracing-options ((self tracer) option-list &rest ignore)
  (declare (ignore ignore))
  (loop for (name . value) in option-list do
        (setf (slot-value self name ) value)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Defs for forward rule tracing

;;;; ???? he used settable-instance variables
;;; ???? hash-table locking?
(defclass forward-rule-tracer (tracer)
  ((trace-everything-p :accessor frt-trace-everything-p :initform t)
   (traced-forward-rules :accessor frt-traced-forward-rules :initform nil)
   (traced-forward-rule-table :accessor frt-traced-forward-rule-table 
                              :initform (make-hash-table))
   (traced-forward-rule-triggers :accessor frt-traced-forward-rule-triggers :initform nil)		
   (currently-running-forward-rules :accessor frt-currently-running-forward-rules :initform ())
   (forward-rule-firings :accessor frt-forward-rule-firings 
                         :initform (make-hash-table :test #'equal))
   (forward-rule-invocation-count :accessor frt-forward-rule-invocation-count :initform 0)
   ))

(defmacro traced-forward-rule-p (rule-name)
  `(with-slots (traced-forward-rule-table) self
     (gethash ,rule-name traced-forward-rule-table)))

(defmacro store-traced-forward-rule (rule-name)
  `(with-slots (traced-forward-rule-table) self
     (setf (gethash ,rule-name traced-forward-rule-table) ,rule-name)))

(defmacro unstore-traced-forward-rule (rule-name)
  ` (remhash ,rule-name traced-forward-rule-table))

(defmethod filter-forward-rule ((self forward-rule-tracer) rule predications)
  (with-slots (trace-everything-p traced-forward-rule-table traced-forward-rule-triggers) self
    (or trace-everything-p
        (traced-forward-rule-p rule)
        (loop for pred in predications
	      when  (some #'(lambda (p) (with-unification
					  (unify-predications-with-explicit-t-vs
					   p *true* pred (predication-truth-value pred))))
			  traced-forward-rule-triggers)
	      do (return t)))))

;;; These are the events that Forward rules can deal with
(defvar *forward-rule-trace-events* '(:fire-forward-rule :exit-forward-rule
				      :enqueue-forward-rule :dequeue-forward-rule))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim '(inline present-event)))

(defun present-event (event-name &optional (stream *standard-output*) (style :active-name))
  (clim:present (find-event-by-name *joshua-debugger* event-name)
	        `((tracing-event ) :name ,style)
	        :stream stream))

(defun present-triggers-as-list (string triggers &optional (stream *standard-output*))
  (clim:with-output-as-presentation (stream (copy-object-if-necessary triggers) 'trigger-list
				   :allow-sensitive-inferiors nil)
    (princ string stream)))

(defgeneric trace-it (tracer event thing &key &allow-other-keys))

(defmethod trace-it ((self forward-rule-tracer) event rule-name &key triggers importance &allow-other-keys)
  (with-slots () self
    (when (filter-forward-rule self rule-name triggers)
      (with-joshua-trace-message-output (*standard-output*)
        (present-event event *standard-output* :active-name)
        (write-char #\space *standard-output*)
        (clim:with-text-style (*standard-output* *emphasis-character-style*)
	  (clim:present rule-name 'forward-rule :allow-sensitive-inferiors nil))
        (write-char #\space *standard-output*)
        (when importance
	  (format *standard-output* "[Importance: ~d] "
		  importance))
        (when triggers
	  (clim:with-text-style (*standard-output* *deemphasis-character-style*)
	    (present-triggers-as-list
	     (format nil "(~d trigger~:P)" (length triggers)) triggers *standard-output*)))))))


(defmethod interact ((self forward-rule-tracer) event rule-name &key triggers &allow-other-keys)
  (with-slots (current-interaction-events) self
    (when (and (member event current-interaction-events)
	       (filter-forward-rule self rule-name triggers))
      (interact *joshua-debugger* event rule-name))))

;;; This should return an alist of instance-vars and settings that can 
;;; be handled by set-tracing-options

(defgeneric accept-tracing-options (thing &optional stream defaults))

(defmethod accept-tracing-options ((self forward-rule-tracer)  &optional (stream *query-io*) defaults)
  (let ((traced-forward-rules (cdr (assoc 'traced-forward-rules defaults)))
        (traced-forward-rule-triggers (cdr (assoc 'traced-forward-rule-triggers defaults)) )
        (trace-everything-p (cdr (assoc 'trace-everything-p defaults))))
    (setq trace-everything-p (clim:accept '(clim:member-alist (("All" :value t
                                                                :documentation "Trace all forward rules")
                                                               ("Selectively" :value nil
                                                                :documentation "Specify which rules to trace")))
		                          :prompt "Trace forward rules"
		                          :default trace-everything-p
		                          :query-identifier :trace-all-forward-rules-p
		                          :stream stream))
    (cond
     (trace-everything-p 
      (setq traced-forward-rule-triggers nil traced-forward-rules nil))
     (t
      (terpri stream)
      (setq traced-forward-rules (clim:accept '(clim:null-or-type (sequence forward-rule))
			                      :prompt "Trace forward rules"
			                      :default traced-forward-rules
			                      :query-identifier :forward-rules-to-trace
			                      :stream stream
			                      ))
      (terpri stream)
      (setq traced-forward-rule-triggers (clim:accept '(clim:null-or-type (sequence predication))
			                              :prompt "Traced forward rule triggers"
			                              :query-identifier :forward-rule-triggers
			                              :default traced-forward-rule-triggers
			                              :stream stream))))
    (setf (cdr (assoc 'trace-everything-p defaults)) trace-everything-p
          (cdr (assoc 'traced-forward-rules defaults)) traced-forward-rules
          (cdr (assoc 'traced-forward-rule-triggers defaults)) traced-forward-rule-triggers))
  defaults)

(defmethod create-tracing-options-alist ((self forward-rule-tracer))
  (with-slots (traced-forward-rules traced-forward-rule-triggers trace-everything-p) self
    `((trace-everything-p . ,trace-everything-p)
      (traced-forward-rule-triggers . ,traced-forward-rule-triggers)
      (traced-forward-rules . ,traced-forward-rules))))

(defmethod set-tracing-options :after ((self forward-rule-tracer) option-list &rest ignore)
  (declare (ignore option-list ignore))
  (with-slots (traced-forward-rule-table traced-forward-rules traced-forward-rule-triggers) self
    (clrhash traced-forward-rule-table)
    (loop for rule-name in traced-forward-rules do
          (store-traced-forward-rule rule-name))
    (setf traced-forward-rule-triggers
	  (delete-duplicates traced-forward-rule-triggers :test #'variant))
    (ensure-tracing-state self)))

;;; This should return a menu alist whose :value is a list of the method
;;; and the instance to call to do the tracing to do the tracing. If you
;;; change the string that this returns you will have to change the  handler as well

(defmethod options-for-object ((self forward-rule-tracer) object)
  (with-slots (traced-forward-rule-table traced-forward-rule-triggers) self
    (let ((alist nil))
      (when (and (symbolp object)
	         (forward-rule-test-p object))
        (if (traced-forward-rule-p object)
	  (push `( "Untrace Forward Rule"
		   :value (untrace-forward-rule ,self)
		   :documentation
		   ,(format nil "Untrace the forward rule ~a" object))
		alist)
	  (push `("Trace Forward Rule"
		  :value (add-trace-forward-rule ,self)
		  :documentation
		  ,(format nil "Trace the forward rule ~a" object))
		alist)))
      (when (predicationp object)
        (let ((variant (find object traced-forward-rule-triggers :test #'variant)))
	  (if variant 
	    (push `("Untrace Forward Rule Trigger"
		    :value (untrace-forward-rule-trigger ,self)
		    :documentation
		    ,(format nil "Untrace forward rules triggered by patterns matching ~a"
			     object))
	          alist)
	    (push `("Trace Forward Rule Trigger"
		    :value (add-trace-forward-rule-trigger ,self)
		    :documentation
		    ,(format
                      nil
                      "Trace forward rules when they are triggered by patterns matching ~a"
                      object))
		  alist))))
      alist)))

;;; Ok lets try doing it like this
;;; a tester for tracing rules 

(define-joshua-tracing-method (traceable-forward-rule-p forward-rule-tracer) (object)
  (and (symbolp object)
       (forward-rule-test-p object)
       (not (traced-forward-rule-p object))))

;;; And for untracing forward rules
(define-joshua-tracing-method (untraceable-forward-rule-p forward-rule-tracer)(object)
  (and (symbolp object)
       (forward-rule-test-p object)
       (traced-forward-rule-p object)))

;;; For predications 
(define-joshua-tracing-method (traceable-forward-rule-trigger-p forward-rule-tracer) (object)
  (with-slots (traced-forward-rule-triggers) self
    (and (predicationp object)
         (not (find object traced-forward-rule-triggers :test #'variant)))))

(define-joshua-tracing-method (untraceable-forward-rule-trigger-p forward-rule-tracer)(object)
  (with-slots (traced-forward-rule-triggers) self
    (and (predicationp object)
         (find object traced-forward-rule-triggers :test #'variant))))

(defmacro with-output-as-command ((stream command-form) &body body)
  `(clim:with-output-as-presentation (,stream ,command-form 'clim:command)
     ,@body))

(defmethod show-tracing-state ((self forward-rule-tracer) &optional (stream *standard-output*))
  (with-slots (trace-everything-p traced-forward-rules traced-forward-rule-triggers) self
    (cond ((tracer-enabled-p *joshua-debugger* self)
	   (with-output-as-command (stream `(com-disable-joshua-tracing ,self))
	     (format stream "~&Forward Rule tracing is on"))
	   (clim:indenting-output (stream 3)
	     (cond
              (trace-everything-p 
	       (format stream "~%Tracing Allforward rules"))
              (t
	       (when traced-forward-rules
		 (format stream "~%Tracing forward rule~p: " (length traced-forward-rules))
		 (format-textual-list traced-forward-rules
				      #'(lambda (r s) (clim:present r 'forward-rule :stream s))
				      :stream stream
				      ;; :filled t
				      :conjunction "and"
				      ;; :if-two " and "
                                      ))
	       (when traced-forward-rule-triggers
		 (format stream
			 "~&Tracing all forward rules triggered by a predication matching:")
		 (clim:indenting-output (stream 3)
		   (fresh-line stream)
		   (format-textual-list traced-forward-rule-triggers
					#'(lambda (p s) (clim:present p 'predication :stream s))
					:stream stream
					;; :filled t
					:conjunction "or"
					;; :if-two " or "
                                        )))))
	     (show-event-state self stream)))
	  (t
	   (with-output-as-command (stream `(com-enable-joshua-tracing ,self))
	     (format stream "~&Forward Rule tracing is off"))))))

;;; This will turn off rule tracing when there's no way we could be interested 
;;; in anything. 
(defmethod ensure-tracing-state ((self forward-rule-tracer) &optional force-enable-p)
  (with-slots (trace-everything-p traced-forward-rules traced-forward-rule-triggers current-output-events current-interaction-events) self
    (unless (or trace-everything-p
	        traced-forward-rules
	        traced-forward-rule-triggers)
      (reset-defaults self)
      (unless force-enable-p 
        (disable-tracer *joshua-debugger* self)))
    (when (and trace-everything-p (or traced-forward-rules traced-forward-rule-triggers))
      (setf trace-everything-p nil))
    (ensure-event-state self force-enable-p)
    (when (and force-enable-p (not (tracer-enabled-p *joshua-debugger* self)))
      (enable-tracer *joshua-debugger* self
		     :new-output-events current-output-events
		     :new-interaction-events current-interaction-events
		     ))))

(defmethod reset-defaults ((self forward-rule-tracer) &optional events-too-p)
  (with-slots (traced-forward-rule-table traced-forward-rules trace-everything-p traced-forward-rule-triggers) self
    (clrhash traced-forward-rule-table)
    (setf trace-everything-p t
	  traced-forward-rules nil
	  traced-forward-rule-triggers nil)
    (when events-too-p (reset-events self))))
  
(defmethod add-trace-forward-rule ((self forward-rule-tracer) rule-name)
  (with-slots (traced-forward-rule-table trace-everything-p traced-forward-rules) self
    (when trace-everything-p (setf trace-everything-p nil))
    (pushnew rule-name traced-forward-rules)
    (store-traced-forward-rule rule-name)
    (ensure-tracing-state self t)
    ))

(defmethod untrace-forward-rule ((self forward-rule-tracer) rule-name)
  (with-slots (traced-forward-rules traced-forward-rule-table) self
    (setf traced-forward-rules (delete rule-name traced-forward-rules))
    (unstore-traced-forward-rule rule-name)
    (ensure-tracing-state self)))

(defmethod add-trace-forward-rule-trigger ((self forward-rule-tracer) predication)
  (with-slots (trace-everything-p traced-forward-rule-triggers) self
    (when trace-everything-p (setf trace-everything-p nil))
    (pushnew predication traced-forward-rule-triggers :test #'variant)
    (ensure-tracing-state self t)))

(defmethod untrace-forward-rule-trigger ((self forward-rule-tracer)  predication)
  (with-slots (traced-forward-rule-triggers) self
    (setf traced-forward-rule-triggers
	  (delete predication traced-forward-rule-triggers :test #'variant))
    (ensure-tracing-state self)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; backward rule tracer

;;;; ???? setter-methods
(defclass backward-rule-tracer (tracer)
  ((trace-everything-p :accessor brt-trace-everything-p :initform t)
   (traced-backward-rules :accessor brt-traced-backward-rules :initform nil)
   (traced-backward-rule-table :accessor brt-traced-backward-rule-table 
                               :initform (make-hash-table))
   (traced-backward-rule-triggers :accessor brt-traced-backward-rule-triggers :initform nil)))

(defmacro traced-backward-rule-p (rule-name)
  `(with-slots (traced-backward-rule-table) self
     (gethash ,rule-name traced-backward-rule-table)))

(defmacro store-traced-backward-rule (rule-name)
  `(with-slots (traced-backward-rule-table) self
     (setf (gethash ,rule-name traced-backward-rule-table) ,rule-name)))

(defmacro unstore-traced-backward-rule (rule-name)
  `(with-slots (traced-backward-rule-table) self
     (remhash ,rule-name traced-backward-rule-table)))

(defmethod filter-backward-rule ((self backward-rule-tracer) rule-name predication truth-value)
  (with-slots (trace-everything-p traced-backward-rule-triggers) self
    (or trace-everything-p
        (traced-backward-rule-p rule-name)
        (some #'(lambda (p)
		  (with-unification
		    (unify-predications-with-explicit-t-vs
		     p *true* predication truth-value)))
	      traced-backward-rule-triggers))))

(defvar *backward-rule-trace-events* '(:fire-backward-rule :exit-backward-rule 
				       :succeed-backward-rule :retry-backward-rule
 				       :enqueue-backward-rule :dequeue-backward-rule))

(defmethod trace-it ((self backward-rule-tracer) event rule-name &key truth-value predication importance &allow-other-keys)
  (with-slots () self
    (when (filter-backward-rule self rule-name predication truth-value)
      (with-joshua-trace-message-output (*standard-output*)
        (present-event event *standard-output* :active-name)
        (write-char #\space *standard-output*)
        (clim:with-text-style (*standard-output* *emphasis-character-style*)
	  (clim:present rule-name 'backward-rule :allow-sensitive-inferiors nil))
        (write-char #\space *standard-output*)
        (when (and importance (plusp importance))
	  (format *standard-output* " [Importance ~d]" importance))
        (when (member event '(:fire-backward-rule :retry-backward-rule))
	  (clim:with-output-as-presentation (*standard-output* predication 'predication :allow-sensitive-inferiors nil)
	    (clim:with-text-style (*standard-output* *deemphasis-character-style*)
	      (format *standard-output* " (Goal... )")
	      (print-with-truth-value-as-not predication truth-value)
	      )))))))
  
(defmethod interact ((self backward-rule-tracer) event rule &key predication truth-value &allow-other-keys)
  (with-slots (current-interaction-events) self
    (when (and (member event current-interaction-events)
	       (filter-backward-rule self rule predication truth-value))
      (interact *joshua-debugger* event rule predication truth-value))))

(defmethod accept-tracing-options ((self backward-rule-tracer)  &optional (stream *query-io*) defaults)
  (let ((traced-backward-rules (cdr (assoc 'traced-backward-rules defaults)))
        (traced-backward-rule-triggers (cdr (assoc 'traced-backward-rule-triggers defaults)) )
        (trace-everything-p (cdr (assoc 'trace-everything-p defaults))))
    (setq trace-everything-p (clim:accept '(clim:member-alist (("All" :value t
								:documentation "Trace all backward rules")
							       ("Selectively" :value nil
								:documentation "Specify which rules to trace")))
					  :prompt "Trace backward rules"
					  :default trace-everything-p
					  :query-identifier :trace-all-backward-rules-p
					  :stream stream))
    (cond
      (trace-everything-p (setq traced-backward-rules nil
				traced-backward-rule-triggers nil))
      (t
       (terpri stream)
       (setq traced-backward-rules  (clim:accept '(clim:null-or-type (sequence backward-rule))
						:prompt "Trace rules"
						:default traced-backward-rules
						:query-identifier :backward-rules-to-trace
						:stream stream
						))
       (terpri stream)
       (setf traced-backward-rule-triggers (clim:accept '(clim:null-or-type (sequence predication))
						       :prompt "Trace backward rule triggers"
						       :query-identifier :backward-rule-triggers
						       :default traced-backward-rule-triggers
						       :stream stream))))
    (setf (cdr (assoc 'traced-backward-rules defaults)) traced-backward-rules
	  (cdr (assoc 'traced-backward-rule-triggers defaults)) traced-backward-rule-triggers
	  (cdr (assoc 'trace-everything-p defaults)) trace-everything-p))
  defaults)

(defmethod create-tracing-options-alist ((self backward-rule-tracer))
  (with-slots (traced-backward-rules traced-backward-rule-triggers trace-everything-p) self
    `((trace-everything-p . ,trace-everything-p)
      (traced-backward-rule-triggers . ,traced-backward-rule-triggers)
      (traced-backward-rules . ,traced-backward-rules))))

(defmethod set-tracing-options :after ((self backward-rule-tracer) option-list &rest ignore)
  (declare (ignore ignore option-list))
  (with-slots (traced-backward-rule-table traced-backward-rules traced-backward-rule-triggers) self
    (clrhash traced-backward-rule-table)
    (loop for rule-name in traced-backward-rules do
          (store-traced-backward-rule rule-name))
    (setf traced-backward-rule-triggers
	  (delete-duplicates traced-backward-rule-triggers :test #'variant))
    (ensure-tracing-state self)))

;;; This should return a menu alist whose :value is a list of the method
;;; and the instance to call to do the tracing to do the tracing. If you
;;; change the string that this returns you will have to change the  handler as well

(defmethod options-for-object ((self backward-rule-tracer) object)
  (with-slots (traced-backward-rule-triggers) self
    (let ((alist nil))
      (when (and (symbolp object)
	         (backward-rule-test-p object ))
        (if (traced-backward-rule-p object)
	  (push `("Untrace Backward Rule"
		  :value (untrace-backward-rule ,self)
		  :documentation
		  ,(format nil "Untrace the backward rule ~a" object))
		alist)
	  (push `("Trace Backward Rule"
		  :value (add-trace-backward-rule ,self)
		  :documentation
		  ,(format nil "Trace the backward rule ~a" object))
		alist)))
      (when (predicationp object)
        (let ((variant (find object traced-backward-rule-triggers :test #'variant)))
	  (if variant 
	    (push `("Untrace Backward Rule Trigger"
		    :value (untrace-backward-rule-trigger ,self)
		    :documentation
		    ,(format nil
			     "Untrace backward rules when triggered by a pattern matching ~a"
			     object))
		  alist)
	    (push `("Trace Backward Rule Trigger"
		    :value (add-trace-backward-rule-trigger ,self)
		    :documentation
		    ,(format nil "Trace backward rules when triggered by a pattern matching ~a"
			     object))
		  alist))))
      alist)))

(define-joshua-tracing-method (traceable-backward-rule-p backward-rule-tracer) (object)
  (and (symbolp object)
       (backward-rule-test-p object)
       (not (traced-backward-rule-p object))))

;;; And for untracing backward rules
(define-joshua-tracing-method (untraceable-backward-rule-p backward-rule-tracer)(object)
  (and (symbolp object)
       (backward-rule-test-p object)
       (traced-backward-rule-p object)))

;;; For predications 
(define-joshua-tracing-method (traceable-backward-rule-trigger-p backward-rule-tracer)(object)
  (with-slots (traced-backward-rule-triggers) self
    (and (predicationp object)
         (not (find object traced-backward-rule-triggers :test #'variant)))))

(define-joshua-tracing-method (untraceable-backward-rule-trigger-p backward-rule-tracer) (object)
  (with-slots (traced-backward-rule-triggers) self
    (and (predicationp object)
         (find object traced-backward-rule-triggers :test #'variant))))

(defmethod show-tracing-state ((self backward-rule-tracer) &optional (stream *standard-output*))
  (with-slots (trace-everything-p traced-backward-rules traced-backward-rule-triggers) self
    (cond ((tracer-enabled-p *joshua-debugger* self)
	   (with-output-as-command (stream `(com-disable-joshua-tracing ,self))
	     (format stream "~&Backward Rule tracing is on"))
	   (clim:indenting-output (stream 3)
	     (cond
              (trace-everything-p (format stream "~%Tracing All backward rules"))
              (t
	       (format stream "~%Tracing backward rule~p: " (length traced-backward-rules))
	       (format-textual-list traced-backward-rules
				    #'(lambda (r s) (clim:present r 'backward-rule :stream s))
				    :stream stream
				    :conjunction " and "
				    ;; :filled tencap
				    ;; :if-two " and "
                                    )
	       (when traced-backward-rule-triggers
		 (format stream
			 "~&Tracing all backward rules triggered by a predication matching:")
		 (clim:indenting-output (stream 3)
		   (fresh-line stream)
		   (format-textual-list traced-backward-rule-triggers
					#'(lambda (p s) (clim:present p 'predication :stream s))
					:stream stream
					:conjunction "or"
					;; :filled t
					;; :if-two " or "
                                        )))
	       (show-event-state self stream)))))
	  (t
	   (with-output-as-command (stream `(com-enable-joshua-tracing ,self))
	     (format stream "~&Backward Rule tracing is off"))))))

(defmethod ensure-tracing-state ((self backward-rule-tracer) &optional force-enable-p)
  (with-slots (trace-everything-p traced-backward-rules traced-backward-rule-triggers current-output-events current-interaction-events) self
    (unless (or trace-everything-p traced-backward-rules traced-backward-rule-triggers)
      (reset-defaults self)
      (unless force-enable-p
        (disable-tracer *joshua-debugger* self)))
    (when (and trace-everything-p
	       (or traced-backward-rules traced-backward-rule-triggers))
      (setf trace-everything-p nil))
    (ensure-event-state self force-enable-p)
    (when (and force-enable-p (not (tracer-enabled-p *joshua-debugger* self)))
      (enable-tracer *joshua-debugger* self
		     :new-output-events current-output-events
		     :new-interaction-events current-interaction-events
		     ))))

(defmethod reset-defaults ((self backward-rule-tracer)  &optional events-too-p)
  (with-slots (trace-everything-p traced-backward-rules traced-backward-rule-triggers traced-backward-rule-table) self
    (setf trace-everything-p t
	  traced-backward-rules nil
	  traced-backward-rule-triggers nil)
    (clrhash traced-backward-rule-table)
    (when events-too-p (reset-events self))))
    
(defmethod add-trace-backward-rule ((self backward-rule-tracer) rule-name)
  (with-slots (traced-backward-rules) self
    (pushnew rule-name traced-backward-rules)
    (store-traced-backward-rule rule-name)
    (ensure-tracing-state self t)
    ))

(defmethod untrace-backward-rule ((self backward-rule-tracer) rule-name)
  (with-slots (traced-backward-rules) self
    (setf traced-backward-rules (delete rule-name traced-backward-rules))
    (unstore-traced-backward-rule rule-name)
    (ensure-tracing-state self)))

(defmethod add-trace-backward-rule-trigger ((self backward-rule-tracer) predication)
  (with-slots (traced-backward-rule-triggers) self
    (pushnew predication traced-backward-rule-triggers :test #'variant)
    (ensure-tracing-state self t)))

(defmethod untrace-backward-rule-trigger ((self backward-rule-tracer)  predication)
  (with-slots (traced-backward-rule-triggers) self
    (setf traced-backward-rule-triggers
	  (delete predication traced-backward-rule-triggers :test #'variant))
    (ensure-tracing-state self)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Predication tracing 

(defclass predication-tracer (tracer)
  ((trace-everything-p :accessor brt-trace-everything-p :initform t)
   (traced-predicate-classes :accessor brt-traced-predicate-classes :initform nil) 	; a list of class-names
   (interesting-predicate-classes :accessor brt-interesting-predicate-classes :initform nil)	; holds all dependents of
   ;traced-predicate-classes
   (traced-patterns :accessor brt-traced-patterns :initform nil)
   ))

(defmethod traced-predicate-p ((self predication-tracer) predicate)
  (with-slots (interesting-predicate-classes) self
    (member predicate interesting-predicate-classes :test #'eq)))

(defmacro register-traced-predicate ()
  `(with-slots (interesting-predicate-classes traced-predicate-classes) self
     (setf interesting-predicate-classes
	   (loop for pred in traced-predicate-classes
	         nconc `(,pred . ,(find-all-subclasses (find-class pred)))
		 into class-list
	         finally (return (remove-duplicates class-list))))))

(defmethod filter-fact ((self  predication-tracer)  predication truth-value)
  (with-slots (traced-patterns trace-everything-p) self
    (or trace-everything-p
        (traced-predicate-p self (type-of predication))
        (some #'(lambda (p)
		  (with-unification
		    (unify-predications-with-explicit-t-vs
		     p *true* predication truth-value))
		  )
	      traced-patterns))))

(defparameter *predication-trace-events* '(:ask :ask-success
                                     :tell :untell
                                     :notice-truth-value-change :act-on-truth-value-change
                                     :justify :unjustify
                                     ))

(defun print-with-truth-value  (predication truth-value &optional (stream *standard-output*))
  (unless (= truth-value *true*)
    (write-char (cond 
		  ((= truth-value *unknown*) #\?)
 		  ((= truth-value *false*)  #\-)
		  ((= truth-value *contradictory*) #\x))
		stream))
  (print-database-predication-without-truth-value predication stream))

(defmethod trace-it ((self predication-tracer) event predication &key truth-value old-truth-value &allow-other-keys)
  (let ((stream *standard-output*))
    (when (filter-fact self predication truth-value)
      (with-joshua-trace-message-output (stream)
        (present-event event stream :active-name)
        (write-char #\space stream)
        (case event
          (:notice-truth-value-change
           (format stream "of ")
           (clim:with-text-style (stream *emphasis-character-style*)
             (print-with-truth-value predication old-truth-value stream))
           (format stream " from ")
           (clim:present  old-truth-value 'truth-value :stream stream)
           (format stream " to ")
           (clim:present truth-value 'truth-value :stream stream))
          (:act-on-truth-value-change
           (format stream "of ")
           (clim:with-text-style (stream *emphasis-character-style*)
             (print-with-truth-value predication old-truth-value stream))
           (format stream " from ")
           (clim:present  old-truth-value 'truth-value :stream stream)
           (format stream " to ")
           (clim:present truth-value 'truth-value :stream stream))
          ((:ask :tell :ask-success)
           (clim:with-text-style (stream *emphasis-character-style*)
             (print-with-truth-value-as-not predication truth-value stream)))
          #|
          (:ask-success
           (clim:indenting-output (stream `(,(+ 3 (* 2 *rule-depth*)) :character))
               (print-query-results justification :stream stream))) |#       
          (otherwise (princ predication stream)))))))

(defmethod show-tracing-state ((self predication-tracer) &optional (stream *standard-output*))
  (with-slots (trace-everything-p traced-predicate-classes traced-patterns) self
    (cond ((tracer-enabled-p *joshua-debugger* self)
	   (with-output-as-command (stream `(com-disable-joshua-tracing ,self))
	     (format stream "~&Predication tracing is on"))
	   (clim:indenting-output (stream 3)
	     (cond 
              (trace-everything-p (format stream "~%Tracing All predicates"))
              (t
	       (when traced-predicate-classes
		 (format stream "~%Tracing predications of class~p: "
		         (length traced-predicate-classes))
		 (clim:format-textual-list traced-predicate-classes
				      #'(lambda (p s) (clim:present p 'joshua-predicate :stream s))
				      :stream stream
				      :conjunction "and"
				      ;; :filled t
				      ;; :if-two " and "
                                      ))
	       (when traced-patterns
		 (format stream "~&Tracing all predications matching:")
		 (clim:indenting-output (stream 3)
		   (fresh-line stream)
		   (format-textual-list traced-patterns
					#'(lambda (p s) (clim:present p 'predication :stream s))
					:stream stream
					:conjunction "or"
					;; :filled t
					;; :if-two " or "
                                        )))))
	     (show-event-state self stream)))
	  (t
	   (with-output-as-command (stream `(com-enable-joshua-tracing ,self))
	     (format stream "~&Predication tracing is off"))))))

(defmethod interact ((self predication-tracer) event predication &key truth-value &allow-other-keys)
  (with-slots (current-interaction-events) self
    (when (and (member event current-interaction-events)
	       (filter-fact self predication truth-value))
      (interact *joshua-debugger* event predication)))) 

(defmethod accept-tracing-options ((self predication-tracer) &optional (stream *query-io*) defaults)
  (let ((trace-everything (cdr (assoc 'trace-everything-p defaults)))
        (trace-predicate-class (cdr (assoc 'traced-predicate-classes defaults)))
        (trace-predication (cdr (assoc 'traced-patterns defaults))))
    (terpri stream)
    (setf trace-everything (clim:accept 
                            '(clim:member-alist (("All" :value t :documentation "Trace all predications")
                                                 ("Selectively" :value nil :documentation "Specify which predications to trace")))
                            :prompt "Trace Predications"
                            :default trace-everything
                            :stream stream))
    (cond 
     (trace-everything (setq trace-predicate-class nil trace-predication nil))
     (t
      (terpri stream)
      (setf trace-predicate-class (clim:accept '(clim:null-or-type (sequence joshua-predicate))
                                               :prompt "Trace Predicates of class(s)"
                                               :default trace-predicate-class
                                               :stream stream))
      (terpri stream)
      (setf trace-predication (clim:accept '(clim:null-or-type (sequence predication))
                                           :prompt "Trace Facts Matching"
                                           :default trace-predication
                                           :stream stream))))
    (setf (cdr (assoc 'trace-everything-p defaults)) trace-everything
          (cdr (assoc 'traced-predicate-classes defaults)) trace-predicate-class
          (cdr (assoc 'traced-patterns defaults)) trace-predication)
    defaults))

(defmethod create-tracing-options-alist ((self predication-tracer))
  (with-slots (trace-everything-p traced-predicate-classes traced-patterns) self
    `((trace-everything-p . ,trace-everything-p)
      (traced-predicate-classes . ,traced-predicate-classes)
      (traced-patterns . ,traced-patterns))))

;;; Compute the list of interesting predicate classes from the traced predicate classes
(defmethod set-tracing-options :after ((self predication-tracer) option-list &rest ignore)
  (declare (ignore ignore option-list))
  (with-slots (trace-everything-p interesting-predicate-classes) self
    (if trace-everything-p
      (setf interesting-predicate-classes nil)
      (register-traced-predicate))
    (ensure-tracing-state self)))

(defmethod ensure-tracing-state ((self predication-tracer) &optional force-enable-p)
  (with-slots (trace-everything-p traced-predicate-classes traced-patterns current-output-events current-interaction-events) self
    (unless (or trace-everything-p traced-predicate-classes traced-patterns)
      (reset-defaults self)
      (unless force-enable-p
        (disable-tracer *joshua-debugger* self)))
    (when (and trace-everything-p (or traced-predicate-classes traced-patterns))
      (setf trace-everything-p nil))
    (ensure-event-state self force-enable-p)
    (when (and force-enable-p (not (tracer-enabled-p *joshua-debugger* self)))
      (enable-tracer *joshua-debugger* self
		     :new-output-events current-output-events
		     :new-interaction-events current-interaction-events
		     ))))

(defmethod reset-defaults ((self predication-tracer)  &optional events-too-p)
  (with-slots (trace-everything-p traced-predicate-classes traced-patterns interesting-predicate-classes) self
    (setf trace-everything-p t
	  traced-predicate-classes nil
	  traced-patterns nil
	  interesting-predicate-classes nil)
    (when events-too-p (reset-events self))))

;;; Add one predicate class to the list of interesting predicate classes
(defmethod add-trace-predicate ((self predication-tracer) predicate)
  (with-slots (traced-predicate-classes) self
    (pushnew predicate traced-predicate-classes)
    (register-traced-predicate)
    (ensure-tracing-state self t)))

(defmethod untrace-predicate ((self predication-tracer)  predicate)
  (with-slots (traced-predicate-classes) self
    (setf traced-predicate-classes (delete predicate traced-predicate-classes))
    (register-traced-predicate)
    (ensure-tracing-state self)))

(defmethod add-trace-pattern ((self predication-tracer) predication)
  (with-slots (traced-patterns) self
    (setf traced-patterns (pushnew predication traced-patterns :test #'variant))
    (ensure-tracing-state self t)))

(defmethod untrace-pattern ((self predication-tracer)  predication)
  (with-slots (traced-patterns) self
    (setf traced-patterns (delete predication traced-patterns :test #'variant))
    (ensure-tracing-state self)))

;;; This should return a menu alist whose :value is a list of the method
;;; and the instance to call to do the tracing to do the tracing. If you
;;; change the string that this returns you will have to change the  handler as well
(defmethod options-for-object ((self predication-tracer) object)
  (with-slots (traced-predicate-classes traced-patterns) self
    (let ((alist nil))
      (when (and (symbolp object)
	         (joshua-predicate-p object))
        (if (member object traced-predicate-classes)
	  (push `("Untrace Predicate class"
		  :value (untrace-predicate ,self)
		  :documentation
		  ,(format nil "Untrace the predication built on ~a" object))
		alist)
	  (push `("Trace Predicate class"
		  :value (add-trace-predicate ,self)
		  :documentation
		  ,(format nil "Trace predications built on ~a" object))
		alist)))
      (when (predicationp object)
        (let ((variant (find object traced-patterns  :test #'variant)))
	  (if variant 
	    (push `("Untrace Predication Pattern"
		    :value (untrace-pattern ,self)
		    :documentation
		    ,(format nil "Untrace Predications matching ~a" object))
		  alist)
	    (push `("Trace Predications Pattern"
		    :value (add-trace-pattern ,self)
		    :documentation
		    ,(format nil "Trace Predications matching ~a" object))
		  alist))))
      alist)))

(define-joshua-tracing-method (traceable-predicate-p predication-tracer) (object)
  (with-slots (traced-predicate-classes) self
    (and (symbolp object)
         (joshua-predicate-p object)
         (not (member object traced-predicate-classes)))))

(define-joshua-tracing-method (untraceable-predicate-p predication-tracer)(object)
  (with-slots (traced-predicate-classes) self
  (and (symbolp object)
       (joshua-predicate-p object)
       (member object traced-predicate-classes))))

(define-joshua-tracing-method (traceable-predication-p predication-tracer)(object)
  (with-slots (traced-patterns) self
    (and (predicationp object)
         (not (find object traced-patterns  :test #'variant)))))

(define-joshua-tracing-method (untraceable-predication-p predication-tracer)(object)
  (with-slots (traced-patterns) self
    (and (predicationp object)
         (find object traced-patterns  :test #'variant))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rete match and Merge tracing.

;(defclass rete-tracer
;	((trace-everything-p t)
;	 (traced-rete-rules nil)
;	 (traced-rete-patterns nil)
;	 (interesting-rete-nodes
;	  (make-hash-table :test 'eq :number-of-values 0
;			   :locking *default-joshua-debugger-table-locking*))
;	 (local-forward-rule-firings 0)
;	 (local-merges-tried 0)
;	 (local-merges-succeeded 0)
;	 (local-merges-failed 0)
;	 (local-matches-tried 0)
;	 (local-matches-succeeded 0)
;	 (local-matches-failed 0))
;	(tracer)
;  (:conc-name rt)
;  :readable-instance-variables
;  :writable-instance-variables
;  :settable-instance-variables)
;
;(defun-in-class (filter-rete rete-tracer) (rete-node)
;  (or trace-everything-p
;      (gethash rete-node interesting-rete-nodes)))
;
;(defmacro-in-class (register-rule-nodes rete-tracer)()
;  `(progn (clrhash interesting-rete-nodes)
;	 (unless trace-everything-p 
;	   (loop for rule in traced-rete-rules
;		 do
;	     (loop for node in (nodes-leading-to-goals `(,rule))
;		   do
;	       (setf (gethash node interesting-rete-nodes) node))))))
;
;(defvar *rete-trace-events* '(:try-merge :succeed-merge :fail-merge :try-match
;					:succeed-match :fail-match))
;;; sap:>nlm>joshua-metering-interface
;(defmethod (trace-it rete-tracer)
;	   (event node &key merge-left merge-right environment match-predication match-pattern)
;  (ignore environment)
;  (when (filter-rete node)
;	(with-joshua-trace-message-output (*standard-output*)
;	  (let ((*print-structure-contents* nil))
;	    (present-event event *standard-output* :active-name)
;	    (write-char #\space *standard-output*)
;	    (case event
;	      (:try-merge
;		(incf local-merges-tried)
;		(format *standard-output*
;			"of ~S and ~S" merge-left merge-right))
;	      (:succeed-merge
;		(incf local-merges-succeeded)
;		(format *standard-output* "~S and~S"
;			merge-left merge-right))
;	      (:fail-merge
;		(incf local-merges-failed)
;		(format *standard-output* "of ~S and~S"
;			merge-left merge-right))
;	      (:try-match
;		(incf local-matches-tried)
;		(format *standard-output* "~s to pattern ~s"
;			match-predication match-pattern))
;	      (:succeed-match
;		(incf local-matches-succeeded)
;		(format *standard-output* "~s to pattern ~s"
;			match-predication match-pattern))
;	      (:fail-match
;		(incf local-matches-failed)
;		(format *standard-output* "of ~s to pattern ~s "
;			match-predication match-pattern)))))))
;
;(defmethod (interact rete-tracer)(event node &rest ignore)
;  (when (and (member event current-interaction-events)
;	     (filter-rete node))
;    (interact *joshua-debugger*)))
;
;(defmethod (accept-tracing-options rete-tracer)(&optional (stream *query-io*))
;  (let ((te trace-everything-p)
;	(trr traced-rete-rules))
;    (setf te (clim:accept '(clim:member-alist (("All" :value t
;					     :documentation
;					     "Trace Matches and merges for all rules ")
;					    ("Selectively" :value nil
;					     :documentation
;					     "Specify which rules to trace")))
;		     :prompt "Trace Matches and Merges for forward rules"
;		     :default trace-everything-p
;		     :stream stream))
;    (unless te
;      (setf trr (clim:accept '(clim:null-or-type (sequence forward-rule))
;			:prompt "Rules"
;			:default traced-rete-rules
;			:stream stream)))
;    `((trace-everything-p . ,te)
;      (traced-rete-rules . ,trr))))
;
;(defmethod (show-tracing-state rete-tracer)(&optional (stream *standard-output*))
;  (cond ((tracer-enabled-p *joshua-debugger* self)
;	 (format stream "~&~VMatches and Merges~ tracing is ~Von~"
;		 *emphasis-character-style* *emphasis-character-style*)
;	 (clim:indenting-output (stream 3)
;	   (if trace-everything-p
;	       (format stream "~%Tracing matching and merging of ~VAll~ forward rules"
;		       *emphasis-character-style*)
;	       (format stream "~%Tracing matching and merging of forward rule~p: "
;		       (length traced-rete-rules))
;	       (format-textual-list traced-rete-rules
;				    #'(lambda (r s) (clim:present r 'forward-rule :stream s))
;				    :stream stream
;				    :conjunction "and"
;				    :filled t
;				    :if-two " and "))
;;	   (when traced-patterns
;;	     (format stream
;;		     "~&Tracing all predications matching:")
;;	     (clim:indenting-output (stream 3)
;;	       (fresh-line stream)
;;	       (format-textual-list traced-patterns
;;				    #'(lambda (p s) (clim:present p 'predication :stream s))
;;				    :stream stream
;;				    :conjunction "or"
;;				    :filled t
;;				    :if-two " or ")))
;	   (show-event-state self stream)))
;	(t (format stream "~&~VMatches and Merges~ tracing is ~Voff~"
;		 *emphasis-character-style* *emphasis-character-style*))))
;
;(defmethod (set-tracing-options rete-tracer :after)(ignore)
;  (clrhash interesting-rete-nodes)
;  (unless trace-everything-p
;    (register-rule-nodes))
;  (ensure-tracing-state self))
;
;
;(defmethod (options-for-object rete-tracer)(object)
;  (let ((alist nil))
;    (when (and (symbolp object)
;	       (forward-rule-test-p object))
;      (if (member object traced-rete-rules)
;	  (push `(,(abbreviate-string (format nil "Untrace matches and merges of rule ~a"
;					      object)
;				      *trace-options-menu-width*)
;		  :value (untrace-rete-rule ,self)
;		  :documentation
;		  ,(format nil "Untrace the matches and merges of rule ~a" object))
;		alist)
;	  (push `(,(abbreviate-string (format nil "Trace the matches and merges of rule ~a"
;					      object)
;				      *trace-options-menu-width*)
;		  :value (add-trace-rete-rule ,self)
;		  :documentation
;		  ,(format nil "Trace the matches and merges of rule ~a" object))
;		alist)))
;    alist))
;
;(defmethod (add-trace-rete-rule rete-tracer)(rule)
;  (pushnew rule traced-rete-rules)
;  (register-rule-nodes)
;  (ensure-tracing-state self t))
;
;(defmethod (untrace-rete-rule rete-tracer)(rule)
;  (setf traced-rete-rules (delete rule traced-rete-rules))
;  (register-rule-nodes)
;  (ensure-tracing-state self))
;
;(defmethod (ensure-tracing-state rete-tracer)(&optional force-enable-p)
;  (unless (or trace-everything-p traced-rete-rules)
;    (reset-defaults self)
;    (unless force-enable-p
;      (disable-tracer *joshua-debugger* self)))
;  (when (and trace-everything-p traced-rete-rules)
;    (setf trace-everything-p nil))
;  (ensure-event-state self t)
;  (when (and force-enable-p (not (tracer-enabled-p *joshua-debugger* self)))
;    (enable-tracer *joshua-debugger* self
;		   :new-output-events current-output-events
;		   :new-interaction-events current-interaction-events
;		   )))
;
;(defmethod (reset-defaults rete-tracer) (&optional events-too-p)
;  (setf trace-everything-p t
;	traced-rete-rules nil)
;  (register-rule-nodes)
;  (when events-too-p (reset-events self)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TMS tracing.

(defclass TMS-tracer (tracer)
  ((trace-everything-p :accessor pt-trace-everything-p :initform t)
   (traced-predicate-classes :accessor pt-traced-predicate-classes :initform nil)
   (interesting-predicate-classes :accessor pt-interesting-predicate-classes :initform nil)	; holds all dependents of
   ;traced-predicate-classes
   (traced-patterns :accessor pt-traced-patterns :initform nil)
   ))

(defmethod traced-predicate-p ((self TMS-tracer) predicate)
  (with-slots (interesting-predicate-classes) self
    (member predicate interesting-predicate-classes :test #'eq)))

(defmethod find-all-subclasses ((thing standard-class))
  (let ((answers nil))
    (labels ((do-one (sub-class)
	       (let ((his-sub-classes (class-direct-subclasses sub-class)))
		 (loop for sub-class in his-sub-classes
		       unless (member sub-class answers)
                       do (push sub-class answers)
                       (do-one sub-class)))))
      (do-one thing))))

(defmacro register-traced-tms-predicate ()
  `(with-slots (interesting-predicate-classes traced-predicate-classes) self
     (setf interesting-predicate-classes
	   (loop for pred in traced-predicate-classes
	         nconc `(,pred . ,(find-all-subclasses (find-class pred)))
		 into class-list
	         finally (return (remove-duplicates class-list))))))

(defmethod filter-TMS ((self TMS-tracer)  predication truth-value)
  (with-slots (traced-patterns trace-everything-p) self
    (and (nontrivial-tms-p predication)
         (or trace-everything-p
	     (traced-predicate-p self (type-of predication))
	     (some #'(lambda (p)
		       (with-unification
		         (unify-predications-with-explicit-t-vs p *true* predication truth-value)))
		   traced-patterns)))))

(defvar *TMS-trace-events* '(:contradiction :bring-in :retract))

(defmethod trace-it ((self TMS-tracer)  event predication &key justification truth-value &allow-other-keys)
  (when (filter-tms self predication truth-value)
    (with-joshua-trace-message-output (*standard-output*)
      (flet ((print-pred (&optional print-with-t-v-p)
               (clim:with-text-style (*standard-output* *emphasis-character-style*)
                 (if print-with-t-v-p 
                   (clim:present predication 'database-predication :stream *standard-output*)
                   (print-database-predication-without-truth-value predication *standard-output*)))
               (unless (or (= *true* truth-value) print-with-t-v-p)
                 (format *standard-output* " as ")
                 (clim:with-text-style (*standard-output* *emphasis-character-style*)
                   (clim:present truth-value 'truth-value :stream *standard-output*))))
             (print-support ()
               (when justification
                 (let ((name (ltms::clause-name justification)) ; used to be destructure-justification
                       (support justification))
                   (clim:with-text-style (*standard-output* *deemphasis-character-style*)
                     (write-char #\space *standard-output*)
                     (clim:with-output-as-presentation (support *standard-output* 'clim:expression)
                       (format *standard-output* "<-- ")
                       (cond ((joshua-rule-p name)
                              (format *standard-output* "Rule: ")
                              (clim:present name 'rule :stream *standard-output*))
                             (t (format *standard-output* "~a" name)))))))))
        (present-event event *standard-output* :active-name)
        (write-char #\: *standard-output*)
        (write-char #\space *standard-output*)
        (case event
          ((:bring-in :retract)
           (print-pred)
           (print-support))
          (:contradiction
           (format *standard-output* "Establishing ")
           (print-pred)
           (print-support))
          ;; No need to print support for these guys, or to fake the t-v
          )))))

(defmethod show-tracing-state ((self TMS-tracer) &optional (stream *standard-output*))
  (with-slots (traced-patterns trace-everything-p traced-predicate-classes) self
    (cond ((tracer-enabled-p *joshua-debugger* self)
	   (with-output-as-command (stream `(com-disable-joshua-tracing ,self))
	     (format stream "~&TMS tracing is on"))
	   (clim:indenting-output (stream 3)
	     (cond 
              (trace-everything-p (format stream "~%Tracing All TMS predicates"))
              (t (when traced-predicate-classes
		   (format stream "~%Tracing TMS predicatations of class~p: "
			   (length traced-predicate-classes))
		   (format-textual-list traced-predicate-classes
				        #'(lambda (p s) (clim:present p 'joshua-predicate :stream s))
				        :stream stream
				        :conjunction "and"
				        ;; :filled t
				        ;;:if-two " and "
                                        ))
	         (when traced-patterns
		   (format stream
			   "~&Tracing all TMS predications matching:")
		   (clim:indenting-output (stream 3)
		     (fresh-line stream)
		     (format-textual-list traced-patterns
					  #'(lambda (p s) (clim:present p 'predication :stream s))
					  :stream stream
					  :conjunction "or"
					  ;; :filled t
					  ;; :if-two " or "
                                          )))))
	     (show-event-state self stream)))
	  (t
	   (with-output-as-command (stream `(com-enable-joshua-tracing ,self))
	     (format stream "~&TMS tracing is off"))))))

(defmethod interact ((self TMS-tracer) event predication &key truth-value &allow-other-keys)
  (with-slots (current-interaction-events) self
    (when (and (member event current-interaction-events)
	       (filter-TMS self predication truth-value))
      (interact *joshua-debugger* event predication))))

(defmethod accept-tracing-options ((self TMS-tracer) &optional (stream *query-io*) defaults)
  (let ((trace-everything-p (cdr (assoc 'trace-everything-p defaults)))
        (traced-predicate-classes (cdr (assoc 'traced-predicate-classes defaults)))
        (traced-patterns (cdr (assoc 'traced-patterns defaults))))
    (setq trace-everything-p (clim:accept '(clim:member-alist (("All" :value t
                                                                :documentation "Trace all TMS predications")
                                                               ("Selectively" :value nil    
                                                                :documentation "Specify which TMS predications to trace")))
                                          :prompt "Trace TMS operations on which TMS Predications"
                                          :default trace-everything-p
                                          :stream stream))
    (cond 
     (trace-everything-p
      (setq traced-predicate-classes nil traced-patterns nil))
     (t
      (terpri stream)
      (setq traced-predicate-classes (clim:accept '(clim:null-or-type (sequence tms-predicate))
                                                  :prompt "TMS predicate classes"
                                                  :default traced-predicate-classes
                                                  :stream stream))
      (terpri stream)
      (setq traced-patterns (clim:accept '(clim:null-or-type (sequence tms-predication-presentation))
                                         :prompt "Trace TMS predications matching"
                                         :default traced-patterns
                                         :stream stream))))
    (setf (cdr (assoc 'trace-everything-p defaults)) trace-everything-p
          (cdr (assoc 'traced-predicate-classes defaults)) traced-predicate-classes
          (cdr (assoc 'traced-patterns defaults)) traced-patterns))
  defaults)

;;; Compute the list of interesting predicate classes from the traced predicate classes

(defmethod create-tracing-options-alist ((self tms-tracer))
  (with-slots (trace-everything-p traced-predicate-classes traced-patterns) self
    `((trace-everything-p . ,trace-everything-p)
      (traced-predicate-classes . ,traced-predicate-classes)
      (traced-patterns . ,traced-patterns))))

(defmethod set-tracing-options :after ((self TMS-tracer) option-list &rest ignore)
  (declare (ignore option-list ignore))
  (with-slots (trace-everything-p interesting-predicate-classes) self
    (if trace-everything-p
      (setf interesting-predicate-classes nil)
      (register-traced-predicate))
    (ensure-tracing-state self)))

(defmethod ensure-tracing-state ((self TMS-tracer) &optional force-enable-p)
  (with-slots (current-output-events current-interaction-events trace-everything-p traced-predicate-classes traced-patterns) self
    (unless (or trace-everything-p traced-predicate-classes traced-patterns)
      (reset-defaults self)
      (unless force-enable-p
        (disable-tracer *joshua-debugger* self)))
    (when (and trace-everything-p (or traced-predicate-classes traced-patterns))
      (setf trace-everything-p nil))
    (ensure-event-state self force-enable-p)
    (when (and force-enable-p (not (tracer-enabled-p *joshua-debugger* self)))
      (enable-tracer *joshua-debugger* self
		     :new-output-events current-output-events
		     :new-interaction-events current-interaction-events
		     ))))

(defmethod reset-defaults ((self TMS-tracer)  &optional events-too-p)
  (with-slots (trace-everything-p traced-predicate-classes traced-patterns interesting-predicate-classes) self
    (setf trace-everything-p t
	  traced-predicate-classes nil
	  traced-patterns nil
	  interesting-predicate-classes nil)
    (when events-too-p (reset-events self))))

;;; Add one predicate class to the list of interesting predicate classes
(defmethod add-trace-predicate ((self TMS-tracer) predicate)
  (with-slots (traced-predicate-classes) self
    (pushnew predicate traced-predicate-classes)
    (register-traced-predicate)
    (ensure-tracing-state self t)))

(defmethod untrace-predicate ((self TMS-tracer)  predicate)
  (with-slots (traced-predicate-classes) self
    (setf traced-predicate-classes (delete predicate traced-predicate-classes))
    (register-traced-predicate)
    (ensure-tracing-state self)))

(defmethod add-trace-pattern ((self TMS-tracer) predication)
  (with-slots (traced-patterns) self
    (setf traced-patterns (pushnew predication traced-patterns :test #'variant))
    (ensure-tracing-state self t)))

(defmethod untrace-pattern ((self TMS-tracer)  predication)
  (with-slots (traced-patterns) self
    (setf traced-patterns (delete predication traced-patterns :test #'variant))
    (ensure-tracing-state self)))

;;; This should return a menu alist whose :value is a list of the method
;;; and the instance to call to do the tracing to do the tracing. If you
;;; change the string that this returns you will have to change the
;;; handler as well
(defmethod options-for-object ((self TMS-tracer) object)
  (with-slots (traced-predicate-classes traced-patterns) self
    (let ((alist nil))
      (when (and (symbolp object)
	         (tms-predicate-p object))
        (if (member object traced-predicate-classes)
	  (push `("Untrace TMS Predicate"
		  :value (untrace-predicate ,self)
		  :documentation
		  ,(format nil "Turn off the tracing of TMS predications built on ~a" object))
		alist)
	  (push `("Trace TMS Predicate"
		  :value (add-trace-predicate ,self)
		  :documentation
		  ,(format nil "Turn on the tracing of TMS predications built on ~a" object))
		alist)))
      (when (and (predicationp object)(nontrivial-tms-p object))
        (let ((variant (find object traced-patterns  :test #'variant)))
	  (if variant 
	    (push `("Untrace TMS Predication Pattern"
		    :value (untrace-pattern ,self)
		    :documentation
		    ,(format nil "Turn off the tracing of TMS predications matching ~a"
			     object))
		  alist)
	    (push `("Trace TMS Predication Pattern"
		    :value (add-trace-pattern ,self)
		    :documentation
		    ,(format nil "Turn on the tracing of TMS predications matching ~a" object))
		  alist))))
      alist)))


(define-joshua-tracing-method (traceable-tms-predicate-p tms-tracer)(object)
  (with-slots (traced-predicate-classes) self
    (and (symbolp object)
         (joshua-predicate-p object)
         (not (member object traced-predicate-classes)))))

(define-joshua-tracing-method (untraceable-tms-predicate-p tms-tracer) (object)
  (with-slots (traced-predicate-classes) self
    (and (symbolp object)
         (joshua-predicate-p object)
         (member object traced-predicate-classes))))

(define-joshua-tracing-method (traceable-tms-predication-p tms-tracer)(object)
  (with-slots (traced-patterns) self
  (and (predicationp object)
       (not (find object traced-patterns  :test #'variant)))))

(define-joshua-tracing-method (untraceable-tms-predication-p tms-tracer)(object)
  (with-slots (traced-patterns) self
    (and (predicationp object)
         (find object traced-patterns  :test #'variant))))


;;; The end of all the class methods

#|
(compile-class-methods tracing-event joshua-encapsulation
			tracer
			;; rete-tracer
			predication-tracer TMS-tracer
			backward-rule-tracer forward-rule-tracer joshua-debugger)
|#


;;; Here are the actual definitions of the debugging events and tracers
;;; The pretty name is for accepting and presenting
;;; The short-name is for the accept-events in the menu
;;; The active name is for trace messages: "firing forwarard rule ..."

(define-tracing-event :fire-forward-rule "Fire forward rule"
  :short-name "Fire"
  :active-name "Firing forward rule"
  :macro-name trace-fire-forward-rule
  :arglist (rule-name triggers environment importance)
  :documentation "Firing the THEN part of a forward rule" 
  )

(define-tracing-event :exit-forward-rule "Exit forward rule"
  :short-name "Exit"
  :active-name "Exiting forward rule"
  :macro-name trace-exit-forward-rule
  :arglist (rule-name triggers environment importance)
  :documentation "Exiting the THEN part of a forward rule"
  )

(define-tracing-event :enqueue-forward-rule "Queue forward rule"
  :short-name "Queue"
  :active-name "Queuing forward rule"
  :macro-name trace-enqueue-forward-rule
  :arglist (rule-name triggers environment importance)
  :documentation "Putting the rule on the forward importance queue"
  )

(define-tracing-event :dequeue-forward-rule "Dequeue forward rule"
  :short-name "Dequeue"
  :active-name "Dequeuing forward rule"
  :macro-name trace-dequeue-forward-rule
  :arglist (rule-name triggers environment importance)
  :documentation "Taking the forward rule off the importance queue"
  )

(define-tracing-event :fire-backward-rule "Try backward rule"
  :short-name "Try"
  :active-name "Trying backward rule"
  :macro-name trace-fire-backward-rule
  :arglist (rule-name predication truth-value importance)
  :documentation "Trying the IF part of a backward rule")

;;; this event is called exit even though it currently prints as "Failing"
(define-tracing-event :exit-backward-rule "Fail backward rule"
  :short-name "Fail"
  :active-name "Exiting backward rule"
  :macro-name trace-exit-backward-rule
  :arglist (rule-name predication truth-value importance)
  :documentation "Failing the IF part of a backward rule")

(define-tracing-event :succeed-backward-rule "Succeed backward rule"
  :short-name "Succeed"
  :active-name "Succeeding backward rule"
  :macro-name trace-succeed-backward-rule
  :arglist (rule-name predication truth-value importance)
  :documentation "Succeeding from the IF part of a backward rule")

(define-tracing-event :retry-backward-rule "Retry backward rule"
  :short-name "Retry"
  :active-name "Looking for more backward rule matches"
  :macro-name trace-retry-backward-rule
  :arglist (rule-name predication truth-value importance)
  :documentation "Looking for another solution in the IF part of a backward rule")

(define-tracing-event :enqueue-backward-rule "Queue backward rule"
  :short-name "Queue"
  :active-name "Queuing backward rule"
  :macro-name trace-enqueue-backward-rule
  :arglist (rule-name predication truth-value importance)
  :documentation "Putting a backward rule onto the backward importance queue")

(define-tracing-event :dequeue-backward-rule "Dequeue backward rule"
  :short-name "Dequeue"
  :active-name "Dequeuing backward rule"
  :macro-name trace-dequeue-backward-rule
  :arglist (rule-name predication truth-value importance)
  :documentation "Removing a backward rule from the backward importance queue")

(define-tracing-event :ask "Ask predication"
  :short-name "Ask"
  :active-name "Asking predication"
  :macro-name trace-ask
  :arglist (predication &key truth-value old-truth-value justification)
  :documentation "Ask predication")

(define-tracing-event :ask-success "Succeed Ask"
  :short-name "Succeed Ask"
  :active-name "Succeed in Asking Predication"
  :macro-name trace-ask-success
  :arglist (predication &key truth-value old-truth-value justification)
  :documentation "Ask predication succeeds")

(define-tracing-event :tell "Tell predication"
  :short-name "Tell"
  :active-name "Telling predication"
  :macro-name trace-tell
  :arglist (predication &key truth-value old-truth-value justification)
  :documentation "Tell predication to the database")

(define-tracing-event :untell "Untell predication"
  :short-name "Untell"
  :active-name "Untelling predication"
  :macro-name trace-untell
  :arglist (predication &key truth-value old-truth-value justification)
  :documentation "Removing a predication from the database")

(define-tracing-event :notice-truth-value-change "Notice truth value change"
  :short-name "Truth value change"
  :active-name "Noticing truth value change"
  :macro-name trace-notice-truth-value-change
  :arglist (predication &key truth-value old-truth-value justification)
  :documentation "Noticing the Change of the truth value of a predication in the database")

(define-tracing-event :act-on-truth-value-change "Act on truth value change"
  :short-name "Act on truth value change"
  :active-name "Responding to change of truth value"
  :macro-name trace-act-on-truth-value-change
  :arglist (predication &key truth-value old-truth-value justification)
  :documentation "Acting on the change of the truth value of a predication in the database")

(define-tracing-event :try-merge "Try merge"	
  :short-name "Try merge"
  :active-name "Trying to merge"
  :macro-name trace-try-merge
  :arglist (node &key merge-left merge-right environment match-predication match-pattern)
  :documentation "Trying to merge the bindings of matched forward rule triggers ")

(define-tracing-event :succeed-merge "Merge"
  :short-name "Merge"
  :active-name "Merging"
  :macro-name trace-succeed-merge
  :arglist (node &key merge-left merge-right environment match-predication match-pattern)
  :documentation "Successfully merging the bindings of matched forward rule triggers")

(define-tracing-event :fail-merge "Fail merge"
  :short-name "Fail Merge"
  :active-name "Failing to merge"
  :macro-name trace-fail-merge
  :arglist (node &key merge-left merge-right environment match-predication match-pattern)
  :documentation "Failing an attempt to merge the bindings of matched forward rule triggers")

(define-tracing-event :try-match "Try match"
  :short-name "Try match"
  :active-name "Trying to match"
  :macro-name trace-try-match
  :arglist (node &key merge-left merge-right environment match-predication match-pattern)
  :documentation "Trying to match a predication against a forward rule trigger")

(define-tracing-event :succeed-match "Match"
  :short-name "Match"
  :active-name "Matching"
  :macro-name trace-succeed-match
  :arglist (node &key merge-left merge-right environment match-predication match-pattern)
  :documentation "Successfully matching a predication against a forward rule trigger")

(define-tracing-event :fail-match "Fail match"
  :short-name "Fail match"
  :active-name "Failing to match"
  :macro-name trace-fail-match
  :arglist (node &key merge-left merge-right environment match-predication match-pattern)
  :documentation "Failing an attempt to match against a forward rule trigger")

(define-tracing-event :justify "Justify predication"
  :short-name "Justify"
  :active-name "Justifying"
  :macro-name trace-justify
  :arglist (predication &key truth-value old-truth-value justification)
  :documentation "Installing the justification of a predication")

(define-tracing-event :unjustify "Unjustify predication"
  :short-name "Unjustify"
  :active-name "Unjustifying"
  :macro-name trace-unjustify
  :arglist (predication &key truth-value old-truth-value justification)
  :documentation "Removing the justification of a predication")

(define-tracing-event :contradiction "Notice contradiction"
  :short-name "Contradiction"
  :active-name "Noticing contradiction"
  :macro-name trace-contradiction
  :arglist (predication &key truth-value old-truth-value justification)
  :documentation "Noticing a TMS contradiction")

(define-tracing-event :bring-in "Justify TMS predication"
  :short-name "Justify"
  :active-name "Justifying"
  :macro-name trace-bring-in
  :arglist (predication &key truth-value old-truth-value justification)
  :documentation "Bringing a TMS predication into the database")

(define-tracing-event :retract "Unjustify TMS predication"
  :short-name "Unjustify"
  :active-name "Unjustifying"
  :macro-name trace-retract
  :arglist (predication &key truth-value old-truth-value justification)
  :documentation "Retracting a TMS predication from the database")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; here are the defs for the tracers (collections of traced events)
;;; These should be compiled after the events they use.

(define-tracer forward-rule-tracer "Forward Rules"
  :pretty-name "Trace Forward Rules"
  :events *forward-rule-trace-events*
  :default-trace-events '(:Fire-forward-rule :enqueue-forward-rule)
  :default-step-events nil)

(define-tracer backward-rule-tracer "Backward Rules"
  :pretty-name "Trace Backward Rules"
  :events *backward-rule-trace-events*
  :default-trace-events '(:Fire-backward-rule :exit-backward-rule
			  :retry-backward-rule :succeed-backward-rule)
  :default-step-events nil)

(define-tracer predication-tracer "Predications"
  :pretty-name "Trace Predications"
  :events *predication-trace-events*
  :default-trace-events '(:ask :tell :ask-success)
  :default-step-events nil)

;(define-tracer rete-tracer "Matches and Merges"
;  :pretty-name "Trace Matches and Merges"
;  :events *rete-trace-events*
;  :default-trace-events '(:succeed-match)
;  :default-step-events nil)

(define-tracer tms-tracer "TMS Operations"
  :pretty-name "Trace TMS Operations"
  :events *tms-trace-events*
  :default-trace-events '(:contradiction :bring-in :retract)
  :default-step-events nil)
  

