;;; -*- Mode: LISP; Base: 10; Syntax: Common-lisp; Package: joshua-internals -*-


;;;; Notes for changes:
;;; Examine how the rule, forward-rule, backward-rule presentation-types are used
;;; similarly for predication, database-predication, etc.
;;; and for predicate, tms-predicate, etc.
;;; What should be the inheritance structure?
;;; One scheme is to do the obvious hierarchy
;;; always present as the root of the hierarchy (unless you know something more specific)
;;; and make all the translators key off the most general but use testers to refine.
;;; The other scheme would be to present things as the most specific type (which means testing
;;; them at the time they're presented) and avoid the testers in the translators.
;;; Then you have to factor in the commands that are intended to be typed directly and
;;; either provide translators from the more general to the more specific, or always present as the more
;;; specific.
;;; Relevant places to look: show-joshua-rules, show-joshua-predicates, show-joshua-database
;;; other places that present stuff?

(in-package :ji)

;;; Presentation types and supporting functions

;;; is it a rule?
(defun joshua-rule-p (symbol)
  (and (symbolp symbol)
       (get symbol 'rule-debug-info)))

;;; first the p-types for rules
(defun forward-rule-test-p (symbol)
  (let ((debug-info (get symbol 'rule-debug-info)))
    (and debug-info
	 (eq :forward (rule-debug-info-control debug-info)))))

;;; is it a backward rule
(defun backward-rule-test-p (symbol)
  (let ((debug-info (get symbol 'rule-debug-info)))
    (and debug-info
	 (eq :backward (rule-debug-info-control debug-info)))))



(clim:define-presentation-method clim:presentation-typep (object (type forward-rule))
  (and object (forward-rule-test-p object)))

(clim:define-presentation-method clim:accept ((type forward-rule) stream (view clim:textual-view) &key)
  (labels ((same-name-p (x y)
             (equal (symbol-name x)
                    (symbol-name y)))
           (duplicated-p (x)
             (member x (cdr (member x *forward-rules* :test #'same-name-p))
                     :test #'same-name-p)))
    (clim:completing-from-suggestions (stream)
      (loop for rule in *forward-rules*
            do (clim:suggest
                (if (and (duplicated-p rule)
                         (not (eq (symbol-package rule) *package*)))
                  (format nil "~s" rule)
                  (string rule))
                rule)))))

(clim:define-presentation-method clim:present (thing (type forward-rule) stream (view clim:textual-view) &key)
  (princ thing stream))

(clim:define-presentation-method clim:presentation-typep (rule (type backward-rule))
  (and rule (backward-rule-test-p rule)))

(clim:define-presentation-method clim:accept ((type backward-rule) stream (view clim:textual-view) &key)
  (labels ((same-name-p (x y)
             (equal (symbol-name x)
                    (symbol-name y)))
           (duplicated-p (x)
             (member x (cdr (member x *backward-rules* :test #'same-name-p))
                     :test #'same-name-p)))
     (clim:completing-from-suggestions (stream)
       (loop for rule in *backward-rules*
             do (clim:suggest
                 (if (and (duplicated-p rule)
                          (not (eq (symbol-package rule) *package*)))
                   (format nil "~s" rule)
                   (string rule))
		 rule)))))

(clim:define-presentation-method clim:present (rulename (type backward-rule) stream (view clim:textual-view) &key)
  (princ rulename stream))


(clim:define-presentation-method clim:presentation-typep (rule (type rule))
  (and rule (joshua-rule-p rule)))

(clim:define-presentation-method clim:accept ((type rule) stream (view clim:textual-view) &key)
  (clim:accept '(or forward-rule backward-rule)
               :stream stream
               :history 'rule))

(clim:define-presentation-method clim:present (rulename (type rule) stream (view clim:textual-view) &key)
  (princ rulename stream))

(defun joshua-protocol-function-p (symbol)
  (or (protocol-internal-name symbol)
      (protocol-external-name symbol)))

(clim:define-presentation-method clim:presentation-typep (function (type protocol-function))
  (and (symbolp function) (joshua-protocol-function-p function)))

(clim:define-presentation-method clim:accept ((type protocol-function) stream (view clim:textual-view) &key)
  (clim:completing-from-suggestions (stream)
    (loop for name in *joshua-protocol-functions*
          do (clim:suggest name (protocol-internal-name name)))))

(clim:define-presentation-method clim:present (obj (type protocol-function) stream (view clim:textual-view) &key)
  (princ (protocol-external-name obj) stream))


(clim:define-presentation-method clim:presentation-typep (thing (type database-predication))
  (and (predicationp thing) (has-been-in-database-p thing)))

(clim:define-presentation-method clim:accept ((type database-predication) stream (view clim:textual-view) &key default)
  (let ((predication (clim:accept 'predication :stream stream :history 'database-predication :prompt nil :default default)))
    (if (has-been-in-database-p predication)
      (values predication 'database-predication)
      (values (insert predication) 'database-predication))))

(clim:define-presentation-method clim:present (obj (type database-predication) stream (view clim:textual-view) &key)
  (let ((*print-predication-top-level-p* print-truth-value))
    (princ obj stream)))

(defun print-database-predication-without-truth-value (database-predication &optional (stream *standard-output*))
  (clim:present database-predication '((database-predication) :print-truth-value nil)
	   :stream stream))

(defun predication-read-error (error-object stream)
  (declare (ignore error-object))
  (error 'parse-error :stream stream))

#-mcclim(pushnew '(#\[ . #\]) clim-internals::*char-associations* :test #'equal)

(defvar *predication-delimiters* '(#\]))

(clim:define-presentation-method clim:accept ((type predication) stream (view clim:textual-view) &key default)
  (let ((first-char (clim:stream-peek-char stream)))
    (when (member first-char '(#\! #\? #\x) :test #'char-equal)
      (clim:stream-read-char stream))
    (let* ((*readtable* *joshua-readtable*)
           (form (clim:accept '((clim:form) :auto-activate t)
                              :history 'predication
                              :default default
                              ;; :delimiter-gestures nil
                              :prompt nil
                              :stream stream)))
      ;; Is this diseased or what?
      (cond ((and (consp form) (eq (car form) 'predication-maker))
             (values (eval form) 'predication))
            ((predicationp form)
             (values form 'predication))
            (t (error "~s is not a Joshua Predication" form))))))

(clim:define-presentation-method clim:present (obj (type predication) stream (view clim:textual-view) &key)
  (let ((*print-predication-top-level-p* nil))
    (princ obj stream)))

(clim:define-presentation-method clim:presentation-typep (thing (type predication))
  (typep thing 'predication))

(clim:define-presentation-method clim:presentation-typep (thing (type tms-predication-presentation))
  (and (typep thing 'predication) (nontrivial-tms-p thing)))

(clim:define-presentation-translator predication-identity
   (predication predication joshua-trace
    :priority 2)		;beat out expression-identity
   (object)
   (values object 'predication ))

(clim:define-presentation-translator predication-to-expression
    (database-predication #-mcclim clim-env::command-or-form #+mcclim clim:command-or-form
                          #-mcclim clim-env::lisp-listener #+mcclim clim-listener::listener
			  :pointer-documentation ((object stream)
						   (format stream "~a" object)))
   (object)
  (values object 'database-predication))



#+mcl
(ccl:advise print-predication-internal
            (destructuring-bind (statement stream) ccl:arglist
              (clim:with-output-as-presentation (stream statement 'predication)
                (:do-it)))
            :when :around
            :name print-with-presentation
            :define-if-not nil)
#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-fwrapper wrap-print-predication (statement stream)
    (clim:with-output-as-presentation (stream statement 'predication)
      (call-next-fwrapper)))
  (fwrap 'print-predication-internal 'joshua-tracing 'wrap-print-predication))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun wrap-print-predication (function statement stream)
    (clim:with-output-as-presentation (stream statement 'predication)
      (funcall function statement stream)))
  (sb-impl::encapsulate 'print-predication-internal 'joshua-tracing 'wrap-print-predication))


(clim:define-presentation-method clim:accept ((type truth-value) stream (view clim:textual-view) &key)
  (clim:accept `(clim:member-alist (("true" . ,+true+) ("false" . ,+false+)
                                    ("unknown" . ,+unknown+) ("contradictory" . ,+contradictory+)))
          :stream stream
          :prompt nil
          :history nil
          ))

(clim:define-presentation-method clim:present (obj (type truth-value) stream (view clim:textual-view) &key)
  (princ
   (cond
    ((eql obj +true+) "true")
    ((eql obj +false+) "false")
    ((eql obj +unknown+) "unknown")
    ((eql obj +contradictory+) "contradictory"))
   stream))

(clim:define-presentation-method clim:accept ((type tracing-event) stream (view clim:textual-view) &key)
  (clim:accept `(clim:member-alist ,(get-events-alist *joshua-debugger*))
          :stream stream
          :prompt nil
          :history 'tracing-event))

(clim:define-presentation-method clim:present (object (type tracing-event) stream (view clim:textual-view) &key)
  (princ (ecase name
           (:pretty-name (tracing-event-pretty-name object))
           (:name (tracing-event-name object))
           (:short-name (tracing-event-short-name object))
           (:active-name (tracing-event-active-name object)))
         stream))

(clim:define-presentation-method clim:present (obj (type trigger-list) stream (view clim:textual-view) &key)
  (princ obj stream))

(clim:define-presentation-translator ptrans-trigger-list-identity
   (trigger-list clim:form joshua-trace)
   (object)
  `(quote ,object))

;;; P-type for predicates
(defun joshua-predicate-p (predicate-name)
  (and (typep predicate-name '(and symbol (not null)))
       (find-class predicate-name nil)
       (or (gethash predicate-name *all-predicates*)
	   (member predicate-name *models*))))

(clim:define-presentation-method clim:presentation-typep (thing (type joshua-predicate))
  (and (symbolp thing) (joshua-predicate-p thing)))

(clim:define-presentation-method clim:accept ((type joshua-predicate) stream (view clim:textual-view) &key)
  (clim:completing-from-suggestions (stream)
    (loop for predicate being the hash-keys of *all-predicates*
          do (clim:suggest (symbol-name predicate) predicate))
    (loop for model in *models*
          do (clim:suggest (symbol-name model) model))))

(clim:define-presentation-method clim:present (obj (type joshua-predicate) stream (view clim:textual-view) &key)
  (prin1 obj stream))

(defun tms-predicate-p (predicate)
  (and (joshua-predicate-p predicate)
       (subtypep predicate 'basic-tms-mixin)))

(clim:define-presentation-method clim:presentation-typep (thing (type tms-predicate))
  (and (joshua-predicate-p thing) (tms-predicate-p thing)))

(clim:define-presentation-method clim:accept ((type tms-predicate) stream (view clim:textual-view) &key)
  (clim:completing-from-suggestions (stream)
    (clim:suggest "basic-tms-mixin" (class-name (find-class 'basic-tms-mixin)))
    (loop for class in (#+mcl ccl::find-all-subclasses #-mcl find-all-subclasses (find-class 'basic-tms-mixin))
          for class-name = (class-name class)
          do (clim:suggest (string class-name) class-name))))

(clim:define-presentation-method clim:present (obj (type tms-predicate) stream (view clim:textual-view) &key)
  (prin1 obj stream))


(clim:define-presentation-translator ptrans-pred-to-tms-pred
 (joshua-predicate tms-predicate clim:global-command-table
                   :tester-definitive t
                   :tester ((object) (tms-predicate-p object))
                   :gesture t)
 (object)
  (list object))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Joshua Commands and supporting functions

;;; there are a couple of interesting command tables
;;; *joshua-command-table* and *joshua-tracing-command-table*.
;;; *joshua-command-table* is the comtab for joshua listener commands
;;; (currently "Global"), *joshua-tracing-command-table* is for commands
;;; that are available at tracing prompts in the new tracing stuff.
;;; Since the tracing doesn't want to inherit all of the commands from
;;; "Global", these commands are explicitly installed in both comtabs.


;; width of an editor buffer
(defparameter *file-stream-width* 94)

(defun stream-inside-width (stream)
  (clim:window-inside-width stream))

;;; a macro to allow us to define a command in more than one command table,
;;; for when you want a command in another comtab without inheriting all
;;; of the others
(defmacro define-multiple-command ((name . options) command-tables arguments &body body)
  #+genera (declare (zwei:indentation 2 3 3 2))
  `(progn (clim:define-command (,name . ,options) ,arguments ,@body)
	  (loop for comtab in ',command-tables do
	    (clim:add-command-to-command-table ',name (if (symbolp comtab) (symbol-value comtab) comtab)
                                               ,@options
					       :errorp nil))))

;;; a command to list the rules
(define-multiple-command (com-show-joshua-rules :name "Show Joshua Rules")
			 (*joshua-command-table* *joshua-only-command-table* *joshua-tracing-command-table*)
                         (&key (type '(clim:member-alist (("Forward" :value :Forward
				                           :documentation "Show only forward rules")
				                          ("Backward" :value :backward
				                           :documentation "Show only Backward Rules")
				                          ("All" :value :all :documentation "Show rules of any type")))
		                     :default :all
		                     :documentation "Show backward, forward, or all rules")
	                       (matching '(clim:sequence string)
		                         :documentation
		                         "Show only rules with names that contain these substrings")
	                       (Triggered-by '(clim:null-or-type (sequence predication))
				             :default nil
				             :documentation
				             "Show rules with triggers that unify with one or ~\
				 more of these predications"
				             )
	                       #|(packages '((clim:token-or-type (:All)
					                      ((cl:sequence cl:package))))
		                         :prompt "package"
		                         :default `(,*package*)
		                         :documentation "Show rules in which packages")
	                       (search-inherited-symbols 'boolean
				                         :default t
				                         :Mentioned-default nil
				                         :documentation "Include rules inherited by packages")
	                       (system '((or sct:subsystem sct:system))
		                       :prompt nil
		                       :documentation "List only rules in the specified system ")|#
                               )
   (labels ((print-heading (heading stream)
	      (terpri stream)
	      (clim:with-text-style (stream *heading-character-style*)
		(princ heading stream))
	      (terpri stream))
#|	    (thing-in-packages (thing)
	      (declare (dynamic-extent thing))
	      (and (symbolp thing)
		   ;; If we have a symbol, just use its package
		   (or (member (symbol-package thing) packages)
		       (and search-inherited-symbols
			    (loop for pkg in packages
				  when (eq thing (find-symbol (symbol-name thing) pkg))
				    do (return t)))))) |#
#|	    (thing-in-system (thing)
	      (declare (dynamic-extent thing))
	      (let* ((pathname (get-source-file-name-that-works thing))
		     (sys (when pathname (send pathname :get 'sct:system))))
		;; Match if the specified system is EQ to the system we have our
		;; hands on, or if the specified system is not a subsystem, the
		;; current system is a subsystem, and the parent systems are EQ
		(or (eq sys system)
		    (and (not (typep system 'sct:subsystem))
			 (typep sys 'sct:subsystem)
			 (eq system (sct:subsystem-parent-system sys))))))|#
#|	    (get-source-file-name-that-works (symbol)
	      (si:get-source-file-name symbol 'defrule))|#
	    (sort-and-format (list stream heading)
	      (let ((found-array (make-array (min (length list) 50) :fill-pointer 0 :adjustable t)))
                (declare (dynamic-extent found-array))
		(loop for symbol in list do
		  (when (and (loop with symbol-name = (symbol-name symbol)
				   for fragment in matching
				   always (search (string fragment) symbol-name :test #'char-equal))
			     (or (null triggered-by)
				 (loop for trigger in triggered-by
				       when
					 (loop for (trigger-pattern) in
						   (rule-debug-info-triggers
						     (rule-debug-info symbol))
					       when
						 (with-unification
						   ;; truth values?
						   (unify trigger trigger-pattern))
						 do (return t))
					 do (return t)))
                             #|
			     (or (not system)
				 (thing-in-system symbol))
			     (or (eq packages :all)
				 (thing-in-packages symbol))|#
                             )
		    (vector-push-extend symbol found-array)
		    ))
		(sort found-array #'string-lessp :key #'symbol-name)
		(cond ((zerop (fill-pointer found-array))
		       (format *standard-output*"~&No ~a"
			       heading)
		       (when matching
			 (format *standard-output*" matching ")
			 (clim:format-textual-list matching #'prin1
					      :conjunction "and"
					      :stream *standard-output*))
		       #|
		       (when (not (eq packages :all))
			 (format t " in Package~P " (length packages))
			 (format-textual-list packages #'princ
					      :conjunction "and"
					      :stream *standard-output*))
		       (when system
			 (format t " that are in system ~a" system))|#
		       )
		      (t
		       (print-heading heading stream)
		       (clim:formatting-item-list (stream
                                                   ;; fix this.  This keyword arg is in the spec
                                                   ;; and the macro accepts it but its expansion into
                                                   ;; innvoke-form... doesn't
					       #-mcclim :stream-width #-mcclim (clim:window-inside-width *standard-output*))
			 (loop for x from 0 to (1- (fill-pointer found-array))
			       as name = (aref found-array x)
			       do
			   (clim:formatting-cell (stream)
			     (clim:present name 'rule :stream stream)))))))))
     (when (member type '(:all :forward))
       (sort-and-format *forward-rules* *standard-output* "Forward Rules"))
     (when (member type '(:all :backward))
        (sort-and-format *backward-rules* *standard-output* "Backward Rules"))))

;;;
;;; Graphing a Rete network, mostly for debugging, partly for demo-ing.
;;;

;;; I had to hack this a little from the standard Joshua because
;;; here we just have the name of the function as the value in the terminal entry.
;;; I made the child producer return the terminal-entry data structure when it encounters it
;;; and the printer recognize that case.  This replaces the function case.

(defun graph-Rete-network (rule-names &key (orientation :vertical)
				      (follow-extraneous-paths nil)
                                      (stream *standard-output*))
  ;; draw a graph of the rete network of some rules
  (let ((desired-nodes (nodes-leading-to-goals rule-names follow-extraneous-paths)))
    (when rule-names
      (fresh-line)
      (clim:format-graph-from-roots
	rule-names
	#'(lambda (thing stream)
            (clim:surrounding-output-with-border (stream :shape :oval)
	      (etypecase thing
	        (symbol
		 ;; name of a rule that's at the root of a Rete network
		 (clim:with-output-as-presentation (stream (rule-debug-info thing) 'clim:expression
						    :allow-sensitive-inferiors nil)
                   (clim:formatting-item-list (stream :n-columns 1 :row-wise nil)
                     (clim:formatting-cell (stream :align-x :center)
                       (write-string "Rule" stream))
                     (clim:formatting-cell (stream :align-x :center)
		       (format stream "~S" thing)))))
	        (rete-procedure-node
		 (clim:with-output-as-presentation (stream thing 'clim:expression
						           :allow-sensitive-inferiors nil)
		   (format stream "Procedure ~d"
			   (length (basic-rete-node-environments thing)))))
	        (Rete-match-node
		 ;; note that it's a match node and give a typical pattern
                 (clim:with-output-as-presentation (stream thing 'clim:expression
						           :allow-sensitive-inferiors nil)
                   (clim:formatting-item-list (stream :n-columns 1 :row-wise nil)
                     (clim:formatting-cell (stream :align-x :center)
                       (write-string "Match Node" stream))
                     (clim:formatting-cell (stream :align-x :center)
                       (format stream "~s" (match-id-pattern (car (rete-match-node-match-ids thing)))))
                     (clim:formatting-cell (stream :align-x :center)
                       (format stream "~d" (length (basic-rete-node-environments thing)))))))
	        (rete-merge-node
		 ;; these are less printable, so just print the word
		 ;; merge.  Let 'em click if they wanna see
		 (clim:with-output-as-presentation (stream thing 'clim:expression
						           :allow-sensitive-inferiors nil)
		   (format stream "Merge ~d" (length (basic-rete-node-environments thing)))))
	        (rete-or-node
		 (clim:with-output-as-presentation (stream thing 'clim:expression
						           :allow-sensitive-inferiors nil)
		   (format stream "Or ~d" (length (basic-rete-node-environments thing)))))
	        (rete-terminal-entry
		 ;; a rule body is an executable function
		 (clim:with-output-as-presentation (stream thing (type-of thing)
						           :allow-sensitive-inferiors nil)
                   (clim:formatting-item-list (stream :n-columns 1 :row-wise nil)
                     (clim:formatting-cell (stream :align-x :center)
                       (write-string "Rule Body" stream))
                     (clim:formatting-cell (stream :align-x :center)
		       (format stream "~S" (rete-terminal-entry-rule-name thing)))))))))
	#'(lambda (parent)
	    (etypecase parent
	      (symbol
		(loop with debug-info = (rule-debug-info parent)
		      with Rete-nodes = (rule-debug-info-network debug-info)
		      for node in Rete-nodes
		      unless (not (member node desired-nodes))
			collect node))
	      (rete-terminal-entry nil)
	      (basic-rete-node
		(loop for child-entry in (rete-node-children parent)
		      for child-node = (when (basic-rete-child-entry-p child-entry)
                                         ;; Note: SBCL would infer that child-entry
                                         ;; has the above type from this application
                                         ;; and then deterine that it's not a terminal-entry
                                         ;; the test above shuts it up
                                         (rete-child-entry-child child-entry))
		      when (member child-node desired-nodes)
			collect (if (rete-terminal-entry-p child-entry)
				    child-entry
				    child-node)))))
	:merge-duplicates t
	:orientation orientation
        :arc-drawer #'(lambda (stream from-object to-object x1 y1 x2 y2 &rest stuff)
                        (declare (ignore stuff from-object to-object))
                        (clim:draw-arrow* stream x1 y1 x2 y2))
        :stream stream
        ))))

#+allegro
(defun function-name (thing)
  (xref::object-to-function-name thing))

#+sbcl
(defun function-name (thing)
  (sb-impl::%fun-name thing))

;;; Used by graph forward rule triggers
(defun nodes-leading-to-goals (rule-names &optional (follow-extraneous-paths nil))
  (let ((desired-nodes nil))
    (let ((nodes-visited nil))
      (labels ((pursue (parent)
		 (declare (dynamic-extent))
		 ;; (format t "~%Node = ~a Type = ~a" parent (type-of parent))
		 (etypecase parent
		   (rete-terminal-entry
		    (break))
		   (symbol
		     (pushnew parent nodes-visited)
		     (pushnew parent desired-nodes)
		     (loop with debug-info = (rule-debug-info parent)
			   with Rete-nodes = (rule-debug-info-network debug-info)
			   for node in Rete-nodes
			 do (pursue node)))
		   ;; my belief is that this case is dead code
		   ;; because we consider terminal-entries explicitly below
		   (function
		     (when (or follow-extraneous-paths
			       ;; Was sys:function-name in Genera
                               ;; For other ports: in MCL function-name was imported from CCl package
			       (member (function-name parent) rule-names))
		       (pushnew parent nodes-visited)
		       (pushnew parent desired-nodes)
		       t))
		   (basic-rete-node
		    (pushnew parent nodes-visited)
		    ;; (format t "~%Parent ~a ~{~%  Child ~a~^~}" parent (rete-node-children parent))
		     (when
		       (loop with any-success = nil
			   for child-entry in (rete-node-children parent)
			   when (typep child-entry 'rete-terminal-entry)
			   do (let ((the-function (rete-terminal-entry-rule-name child-entry)))
				(when (or follow-extraneous-paths
					  (member the-function rule-names))
				  (pushnew child-entry desired-nodes)
				  (setq any-success t)))
			   else do
				(let ((child-node (rete-child-entry-child child-entry)))
				  ;; return t if any of the children have
				  ;; relevant paths, but pursue all of them just
				  ;; in case.
				  (when (or (member child-node desired-nodes)
					    (and (not (member child-node nodes-visited))
						 (pursue child-node)))
				    (setf any-success t)))
			   finally (return any-success))
		       (push parent desired-nodes)
		       t)))))
	(loop for rule-name in rule-names doing (pursue rule-name))))
    desired-nodes))

(define-multiple-command (com-graph-forward-rule-triggers :name t)
			 (*joshua-command-table* *joshua-only-command-table* *joshua-tracing-command-table*)
                         ((rules `(clim:token-or-type (("All" . :all)) (clim:sequence forward-rule))
	                         :default :all
	                         :documentation "One or more rule names (all is acceptable).")
                          &key
                          (follow-extraneous-paths
                           'clim:boolean
                           :prompt "Show all related rule branches"
                           :default nil
                           :documentation "Only show those nodes that lead to at least one of the selected rules.")
                          (orientation `((member :horizontal :vertical))
		                       :default :vertical
		                       :prompt "Orientation of the graph"
		                       :documentation "Orientation of the graph"))
  (when (eql rules :all)
    (setf rules *forward-rules*))
  (let ((stream *standard-output*))
    (if rules
      (graph-rete-network rules
                          :stream stream
                          :orientation orientation
                          :follow-extraneous-paths  follow-extraneous-paths)
      (clim:with-text-style (stream *deemphasis-character-style*)
        (format stream "No forward rules defined")))))

;;; It could be that this command should be more carefull about its destructive
;;; operations, and query the user first.
(define-multiple-command (com-clear-joshua-database :name "Clear Joshua Database")
			 (*joshua-command-table* *joshua-only-command-table* *joshua-tracing-command-table*)
    ((predications '(clim:token-or-type (("All" :value :all
					  :documentation
					  "Untell all predicates in the Joshua database")
					 ("None" :value nil
					  :documentation
					  "Don't do anything to the Joshua database"))
					(sequence predication))
		   :documentation "Remove specified predications from the database"
		   :default :all)
     &key
     (other-truth-values-too 'clim:boolean
			     :prompt "opposite truth-value too?"
			     :default t
			     :documentation "Delete the negative of the predications ~\
					      as well.")
     (undefine-rules 'clim:boolean
		     :documentation "Undefine all rules")
     (verbose 'clim:boolean
	      :default t
	      :documentation "Print information")
     (query 'clim:boolean
	    :default t
	    :documentation "Query the user before actually untelling or undefining things"))
   (let ((stream *standard-output*))
     (cond ((consp predications)
	    (let ((preds-to-untell nil))
	      (loop for pattern in predications do
		(ask pattern #'(lambda (derivation)
				 (let ((database-predication
				         (ask-database-predication derivation)))
				   (when (predicationp database-predication)
				     (push database-predication preds-to-untell))))
		     :do-backward-rules nil)
		(when other-truth-values-too
		  (ask (predication-maker `(not ,pattern))
		       #'(lambda (derivation)
			   (let ((database-predication
				   (ask-database-predication derivation)))
			     (when (predicationp database-predication)
			       (push database-predication preds-to-untell))))
		       :do-backward-rules nil)))
	      (setf preds-to-untell (nreverse preds-to-untell))
	      (when (or verbose query)
		(clim:with-text-style (stream *heading-character-style*)
		  (clim:surrounding-output-with-border (stream :shape :underline)
		    (format stream "~&Predications being removed:")))
		(if (null preds-to-untell)
		    (format stream "~%  None")
		    (clim:format-items preds-to-untell
				       :stream stream
				       :presentation-type 'predication)))
	      (when (or (not query)
			(null preds-to-untell)
                        (progn (terpri stream)
			       (clim:accept 'clim:boolean
				            :stream stream
				            :prompt "Untell the above predications?"
				            :default t)))
		(clim:noting-progress (stream "Untelling Predications")
		  (let ((length (length preds-to-untell)))
		    (loop for p in preds-to-untell
			  for count = 0 then (1+ count)
			  do
		      (untell p)
		      (clim:note-progress count length)))))
	      (when undefine-rules
		(cond
                 (query
                  (terpri stream)
                  (when (clim:accept 'clim:boolean
                                     :stream stream
                                     :prompt "Undefine all Joshua Rules?"
                                     :default t)
                    (clear nil undefine-rules)))
                 (t
                  (when (and verbose (not query))
                    (format stream "~&Undefining all Joshua Rules"))
                  (clear nil undefine-rules))))))
	   (t (when (eq predications :all)
		(when (and verbose (not query))
		  (format stream "~&Removing all predications from the database"))
		(when query
		  (unless (clim:accept 'clim:boolean
				       :stream stream
				       :prompt "Clear all predications from the database?"
				       :default t)
		    (setf predications nil))))
	      (when  undefine-rules
		(when (and verbose (not query))
		  (format stream "~&Undefining all Joshua Rules"))
		(when query
                  (terpri stream)
		  (unless (clim:accept 'clim:boolean
				       :stream stream
				       :prompt "Undefine all Joshua rules?"
				       :default t)
		    (setf undefine-rules nil))))
	      (clear predications undefine-rules)))))

(define-multiple-command (com-show-joshua-predicates :name "Show Joshua Predicates")
			 (*joshua-command-table* *joshua-only-command-table* *joshua-tracing-command-table*)
                         (&key
                          (matching '(sequence string)
		                    :documentation "Show only predicates containing this substring(s)")
                          (include-models 'clim:boolean
		                          :default nil
		                          :mentioned-default t
		                          :documentation "Show the abstract predicate models also")
                          #|
      (packages '((clim:token-or-type (:All)
				     ((sequence cl:package))))
		:prompt "package"
		:default `(,*package*)
		:documentation "Show predicates in which packages")
      (search-inherited-symbols 'clim:boolean
				:default t
				:Mentioned-default nil
				:documentation "Include predicates inherited by packages")
      (system '((or sct:subsystem sct:system))
	      :prompt nil
	      :documentation "List only predicates in the specified system ")|#
                          )
  (labels (#|(thing-in-packages (thing)
	      (declare (dynamic-extent))
	      (and (symbolp thing)
		   ;; If we have a symbol, just use its package
		   (or (member (symbol-package thing) packages)
		       (and search-inherited-symbols
			    (loop for pkg in packages
				  when (eq thing (find-symbol (symbol-name thing) pkg))
				    do (return t))))))
	    (thing-in-system (thing)
	      (declare (dynamic-extent))
	      (let* ((clim-lisp:pathname (get-source-file-name-that-works thing))
		     (sys (when clim-lisp:pathname (send clim-lisp:pathname :get 'sct:system))))
		;; Match if the specified system is EQ to the system we have our
		;; hands on, or if the specified system is not a subsystem, the
		;; current system is a subsystem, and the parent systems are EQ
		(or (eq sys system)
		    (and (not (typep system 'sct:subsystem))
			 (typep sys 'sct:subsystem)
			 (eq system (sct:subsystem-parent-system sys))))))
	    (get-source-file-name-that-works (symbol)
	      (si:get-source-file-name symbol 'cl:deftype))|#
           (filter-predicate (flavor)
             (and (loop with symbol-name = (symbol-name flavor)
                        for fragment in matching
                        always (search fragment symbol-name :test #'char-equal))
                  #|
		   (or (not system)
		       (thing-in-system flavor))
		   (or (eq packages :all)
		       (thing-in-packages flavor))|#
                  )))
    (let ((found-array (make-array 50 :fill-pointer 0 :adjustable t)))
      (declare (dynamic-extent found-array))
      (maphash #'(lambda (key ignore)
                   (declare (ignore ignore))
                   (when (filter-predicate key)
                     (vector-push-extend key found-array)))
               *all-predicates*)
      (when include-models
        (loop for model in *models*
              do
	      (when (filter-predicate model)
	        (vector-push-extend model found-array))))
      (sort found-array #'string-lessp :key #'symbol-name)
      (let ((stream *standard-output*))
        (cond ((zerop (fill-pointer found-array))
	       (format stream "~&There are no Joshua predicates ")
	       (when matching
		 (format stream "matching ")
		 (clim:format-textual-list matching #'prin1
				           :conjunction "and"
				           :stream stream))
	       #|
	      (when (not (eq packages :all))
		(format t " in Package~P " (length packages))
		(clim:format-textual-list packages #'princ
				     :conjunction "and"
				     :stream *standard-output*))
	      (when system
		(format t " that are in system ~a" system))|#
	       )
	      (t
	       (when (or (<= (length found-array) 100)
			 (clim:accept 'clim:boolean
				      :stream stream
				      :prompt (format nil
						      "There are ~D ~:[possibilities~;possible ~A~].  ~\
				Do you want to see them all? "
						      (length found-array)
						      'joshua-predicate
						      "joshua predicates")))
		 (fresh-line stream)
		 (clim:formatting-item-list (stream :x-spacing '(3 :character))
		   (loop for x from 0 to (1- (fill-pointer found-array))
			 as name = (aref found-array x)
			 do (fresh-line stream)
		         (clim:formatting-cell (stream)
		           (clim:present name 'joshua-predicate :stream stream)
		           (let ((predicate-descriptor (gethash name *all-predicates*)))
			     (when predicate-descriptor
			       (write-char #\space stream)
			       (clim:format-items (predicate-descriptor-arglist predicate-descriptor)
                                                  :n-columns 10
					          :stream stream
					          :printer #'princ)))))))))))))


;;; OK, time to swipe some code from the classs command Show Class Handler
(define-multiple-command (com-show-joshua-protocol)
			 (*joshua-command-table*
			   *joshua-only-command-table* *joshua-tracing-command-table*)
    ((predicate-name 'joshua-predicate
		     :documentation
		     "Describe the protocol implementation for which predicate or model")
     &key
     (protocol-functions '(clim:token-or-type (("All" :value :all
						:documentation
						"Show it for all the Joshua Protocol functions"
						))
					      (sequence protocol-function))
			 :default :all
			 :documentation
			 "Show the predicate's inheritance of which protocol functions")
     )
   (when (eq protocol-functions :all)
     (setf protocol-functions
	   (loop for external-name in *joshua-protocol-functions*
		 collect (protocol-internal-name external-name)
		   into name-list
		 finally (return (nreverse name-list)))))
   (let ((stream *standard-output*))
     (loop for generic in protocol-functions
	   with class = (find-class predicate-name)
	   do
       ;; Get information about the handler from the class system
       (let ((COMBINED-METHOD-LIST (loop for method in (generic-function-methods generic)
					 for specializers = (method-specializers method)
					 when (eql (first specializers) class)
					   collect method)))
	 (when COMBINED-METHOD-LIST
	   (clim:with-text-style (stream *heading-character-style*)
	     (terpri stream)
	     (clim:present generic 'protocol-function :stream stream))
	   (princ #\space stream)
           ;; mcclim implementation causes a note about deleting
           ;; unreachable code which is actually correct given the
           ;; particular arguments to filling output.  But fixing
           ;; filling-output would be a big pain.
           (locally
               #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
             (clim:filling-output (stream)
               (loop for thing in (arglist (protocol-external-name generic))
                     do (print thing stream))))
	   (clim:with-text-style (stream '(nil :italic nil))
	     (format stream "~%Implemented by:"))
	   (loop for method in COMBINED-METHOD-LIST do
	     (format stream "~%  ~s" method))
	   )))))


;;; Show everything in the database, or at least in the dn
(define-multiple-command  (com-show-joshua-database :Name "Show Joshua Database")
			  (*joshua-command-table* *joshua-only-command-table* *joshua-tracing-command-table*)
                          ((pattern '(clim:member-sequence (:all :selective))
	                            :prompt "matching what"
	                            :default :all
	                            :documentation "Show predications in the database matching this pattern.")
                           (other-truth-value-too-p 'clim:boolean
			                            :prompt "opposite truth-value too?"
			                            :default t
			                            :documentation "Show patterns with both truth-values.")
                           &key
                           (show-partially-failing-models 'clim:boolean
				                          :prompt "show models which couldn't completely handle the query"
				                          :mentioned-default t
				                          :default nil
				                          :documentation "Show a list of those models which encountered trouble answering the query"))
  (let ((stream *standard-output*))
    (when (eql pattern :selective)
      (setq pattern (clim:accept 'predication :stream stream :prompt "Matching what pattern?")))
    (with-unbound-logic-variables (predicate arguments)
      (when (eq pattern :all) (setq pattern (predication-maker `(,predicate . ,arguments))))
      (assert (predicationp pattern) nil "Pattern is not a predication: ~S" pattern)
      (let ((true-answers nil)
	    (false-answers nil)
	    (things-i-could-not-handle nil))
        (flet ((print-entries (heading entries)
		 (terpri stream)
		 (clim:surrounding-output-with-border (stream :shape :underline :move-cursor t)
		   (clim:with-text-face (stream :bold)
		     (princ heading stream)))
                 (terpri stream)
		 (if entries
                   (clim:format-items entries
                                      :stream stream
                                      :printer #'(lambda (pred stream)
						   (cond
						    ((symbolp pred)
						     (princ pred stream))
						    (t
						     (print-database-predication-without-truth-value pred stream)
						     (let ((certainty-factor (certainty-factor pred)))
						       (when certainty-factor
							 (format stream " with certainty ~d" (abs certainty-factor))))))))
                   (clim:format-items '("None") :stream stream :printer #'print)))
	       (do-truth-value (truth-value)
		 ;; print out contents of db with this truth-value
		 ;; disable tracing for these asks
		 (catch 'bad-query
		   (without-joshua-tracing
		    (handler-bind ((model-cant-handle-query
				    #'(lambda (condition)
					(pushnew (model-cant-handle-query-model condition)
						 things-i-could-not-handle)
					 (throw 'ask-variable-predicate-escape nil))))
		      (ask-internal pattern
				    truth-value
				    ;; This isn't right.  It works for predications which store and
				    ;; return their canonical predications, but not for other kinds.
				    #'(lambda (backward-support)
					(when (and (consp backward-support)
						   (consp (rest backward-support))
						   (predicationp (third backward-support)))
					  (let ((test-object (second backward-support)))
					    (cond
					     ((eql test-object +true+)
					      (push (third backward-support) true-answers))
					     ((eql test-object +false+)
					      (push (third backward-support) false-answers))))))
				    nil nil))))))
	  (do-truth-value +true+)
	  (when other-truth-value-too-p
	    (do-truth-value +false+))
	  (when (or true-answers other-truth-value-too-p (null false-answers))
	    (print-entries "True things" (nreverse true-answers)))
	  (when (or false-answers other-truth-value-too-p (null true-answers))
	    (print-entries "False things" (nreverse false-answers)))
	  (when (and things-i-could-not-handle show-partially-failing-models)
	    (print-entries "Models which couldn't respond" things-i-could-not-handle)))))))

;;; Replace by Show Predication Support.
(define-multiple-command (com-explain-predication :name t)
                         (*joshua-command-table*)
                         ((p '((database-predication))
	                     :prompt "database predication" :documentation "a database predication to explain")
                          (n '(clim:null-or-type (integer 0))
	                     :prompt "to what depth"
	                     :default nil))
  ;; a real version of this would do it possibly graphically, and with incremental redisplay, etc.
  (explain p n *standard-output*))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commands for manipulating tracing.

;;; I would much prefer to use dw::alist-member here but it doesn't do the right
;;; thing with mentioned defaults, its help is completely messed up on objects with
;;; spaces as well.

(define-multiple-command (com-enable-joshua-tracing :Name "Enable Joshua Tracing")
			 (*joshua-command-table*
			   *joshua-only-command-table* *joshua-tracing-command-table*)
    ((Type-of-tracing `(clim:member-alist
                        ,(create-tracer-alist *joshua-debugger*
                                              :extra-items '(("All" :value :all :documentation "Enable all types of tracing"))
                                              :first-word-of-doc-strings "Enable"))
		      :prompt "Type of tracing"
		      :documentation "What kind of tracing to Enable")
     &key
     (menu 'clim:boolean
	   :default nil
	   :mentioned-default t
	   :documentation "Use the menu to set detailed tracing options")
     (trace-events `((clim:subset-alist ,(get-events-alist *joshua-debugger* Type-of-tracing)))
		   :default (get-current-events *joshua-debugger* :trace Type-of-tracing)
		   :prompt "Trace which events"
		   :when (not (eq Type-of-tracing :all))
		   :documentation "What events to output trace messages for")
     (step-events `((clim:subset-alist ,(get-events-alist *joshua-debugger* Type-of-tracing)))
		  :prompt "Step at which events"
		  :when (not (eq Type-of-tracing :all))
		  :default (get-current-events *joshua-debugger* :step Type-of-tracing)
		  :documentation "What events to stop and interact at"))
   (if (or (eq Type-of-tracing :all) menu)
       (set-joshua-trace-conditions *joshua-debugger* :tracer Type-of-tracing :menu menu
				    :enable-p t)
       (set-joshua-trace-conditions *joshua-debugger* :tracer Type-of-tracing :menu menu
				    :enable-p t :trace-events trace-events
				    :step-events step-events))
   (unless menu
     (show-joshua-tracing *joshua-debugger* Type-of-tracing)))


(define-multiple-command (com-disable-joshua-tracing :Name "Disable Joshua Tracing")
			 (*joshua-command-table*
			   *joshua-only-command-table* *joshua-tracing-command-table*)
    ((Type-of-tracing `(clim:member-alist
			 ,(create-tracer-alist
			    *joshua-debugger*
			    :extra-items
			    '(("All" :value :all
			       :documentation "Disable all types of tracing"))
			    :first-word-of-doc-strings "Disable"))
		      :prompt "Type of tracing"
		      :default :all
		      :documentation "What kind of tracing to disable"))
   (set-joshua-trace-conditions *joshua-debugger* :tracer Type-of-tracing :menu nil
				:enable-p nil)
   (show-joshua-tracing *joshua-debugger* Type-of-tracing))

(define-multiple-command (com-Joshua-trace :name "Joshua Trace")
			 (*joshua-command-table* *joshua-only-command-table* *joshua-tracing-command-table*)
                         ((type '(clim:member-alist (("Rule" :value rule
			                              :documentation "Set the tracing options for a rule" )
			                             ("Predication"  :value predication
			                              :documentation "Set the tracing options for a predication/pattern")
			                             ("Predicate"  :value joshua-predicate
			                              :documentation "Set the tracing options for a predicate flavor")
			                             ("Event" :value tracing-event
			                              :documentation
			                              "Set the tracing options for a particular tracing event")))
	                        :prompt "Type of object to trace (or untrace)"
	                        :default nil
	                        :documentation "What type of object to trace, a rule, a predication, or a Predicate"
	                        )
                          (object type
	                          :prompt "object to trace"
	                          :documentation "What object to trace")
                          &key (tracing-options
	                        `((clim:member-alist ,(append
			                               (collect-options-for-object *joshua-debugger* object)
			                               '(("Menu" :value :menu
				                          :documentation "Bring up a menu of available options"))))
	                          :highlighting-test ,#'equal)
	                        :default :menu
                                :prompt "tracing options"
	                        :documentation "What kind of tracing to do"))
  ;;; Tracing-options will return a list containing the generic and the instance to
  ;;; apply it to to get the desired tracing-options
  (declare (ignore type))
  (if (eq tracing-options :menu)
    ;;; Just let the menu handle everything
    (get-all-trace-options *joshua-debugger* object)
    (when tracing-options
      ;;otherwise they typed in the choice and we can handle it here
      (apply (car tracing-options) `(,(cadr tracing-options) ,object))
      ;;Show them what they did
      (show-joshua-tracing *joshua-debugger* (cadr tracing-options)))))


(define-multiple-command (com-show-joshua-tracing :name "Show Joshua Tracing")
			 (*joshua-command-table*
			   *joshua-only-command-table* *joshua-tracing-command-table*)
    ((Type-of-tracing `(clim:member-alist
			 ,(create-tracer-alist
			    *joshua-debugger*
			    :extra-items
			    '(("All" :value :all
			       :documentation "Disable all types of tracing"))
			    :first-word-of-doc-strings "Show"))
		      :prompt "Type of tracing"
		      :default :all
		      :documentation "What kind of tracing to disable"))
   (show-joshua-tracing *joshua-debugger* Type-of-tracing))

#|
(define-multiple-command (com-show-rule-source :name "Show Rule Definition")
			 (*joshua-command-table*
			   *joshua-only-command-table* *joshua-tracing-command-table*)
    ((rule '(sequence rule)
	   :prompt "rule or rules"
	   :documentation "Show the definitions of which rule or rules")
     &key (load '(alist-member
		   :alist
		   (("Yes" :value t :documentation
		     "Read the file into zmacs if the definition isn't already available")
		    ("No" :value nil :documentation
		     "Don't bother reading the file zmacs if the definition isn't there")
		    ("Query" :value :query :documentation
		     "Ask before reading any files in")))
		:default :query
		:prompt "load the file if necessary?"
		:documentation "Whether to read in the file or query the user if the ~\
				 definition isn't in an editor buffer"))
   (loop for each-rule in rule do
     (show-rule-source each-rule t load)))


;;; This is really completely bogus, the system has no way of knowing where the currently
;;; loaded definition is from.
(defun show-rule-source (rule-name &optional (stream t)
			 (load-p :query)(header-p t))
  (when (eq stream t)(setf stream *standard-output*))
  (let* ((section (si:function-correspondence-info rule-name))
	 (file-name (si:get-source-file-name rule-name 'defrule))
	 (real-file-name (when file-name (zwei:pathname-source-file-pathname file-name))))
    (cond ((or section real-file-name)
	   (when (not section)
	     (case load-p
	       ((t) (zwei:load-file-into-zmacs real-file-name)
		(setf section (si:function-correspondence-info rule-name)))
	       ((nil) (format stream "~&Rule ~s is not in an editor buffer" rule-name))
	       (:query (when
			 (accept 'boolean
				 :prompt (format nil
						 "~&Rule ~s is not in an editor buffer~%~\
					   Load the file into Zmacs and show the definition? "
						 rule-name)
				 :default t)
			 (zwei:load-file-into-zmacs real-file-name)
			 (setf section (si:function-correspondence-info rule-name))))))
	   (when section
	     (when header-p
	       (format stream "~&~VRule ~a:~~%"
		       *deemphasis-character-style* rule-name))
	     (let* ((first-bp (send section :first-bp))
		    (last-bp (send section :last-bp))
		    (interval-stream (zwei:open-interval-stream first-bp last-bp t)))
	       (stream-copy-until-eof interval-stream stream))))
	  (t
	   (format stream
		   "Rule ~s was not loaded from a file or editor buffer. No definition found."
		   rule-name)))))
|#

(define-multiple-command (com-reset-joshua-tracing)
			 (*joshua-command-table*
			   *joshua-only-command-table* *joshua-tracing-command-table*)
 ((Type-of-tracing `(clim:member-alist
		      ,(create-tracer-alist
			 *joshua-debugger*
			 :extra-items
			 '(("All" :value :all
			    :documentation
			    "Restore all types of tracing to their original defaults"
			    ))
			 :first-word-of-doc-strings
			 "Restore the original defaults for"))
		      :prompt "Type of tracing"
		      :default :all
		      :documentation
		      "What kind of tracing to reset back to the original defaults")
  &key (include-events 'clim:boolean
		       :default nil
		       :mentioned-default t
		       :documentation "Reset the traced and stepped events as well"
		       ))
   (reset-tracers *joshua-debugger* Type-of-tracing include-events))



;(define-multiple-command (com-joshua-help)
;			 (*joshua-command-table* *joshua-only-command-table*
; *joshua-tracing-command-table*)
;    (&key
;      (format '((cl:member :brief :detailed)) :default ':brief	;
;	      :documentation "\"Detailed\" shows all commands.  \"Brief\" uses compression."))
;   (scl:format-item-list (mapcar
;			   #'second
;			   (cp::command-table-complete "" *joshua-tracing-command-table*
;						       :possibilities))
;			 :presentation-type `cp:command-name))

#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; commands for the Joshua Tracing prompt - for stepping through the execution
;;; of the rules

;;; By making abort work only as an accelerator (all other aborts are caught), we
;;; get the effect that hitting abort at the command prompt will abort the
;;; execution of the program while hitting abort while typing or executing a
;;; command will bring you back to the command prompt
(cp:define-command-accelerator acc-com-abort "Joshua portable Trace" (#\abort)
			       ()
			       ()
  (throw 'aborted t))

;;; Commands that want to exit the command loop should retun :done, all others
;;; will simply finish and return to the command propmt
(cp:define-command (com-continue :name "Step" :command-table "Joshua portable Trace")
    ()
   (add-tracing-filter *joshua-debugger* nil)
   :done)

(cp:define-command-accelerator acc-com-continue "Joshua portable Trace" (#\return #\c-s)
			       ()()
  `(com-continue))


(cp:define-command (com-leap :name "Leap" :command-table "Joshua portable Trace")
    ((n-levels 'integer
	       :documentation
	       "Leap to the next tracing message at the current depth + n-levels"
	       :prompt "How many levels"
	       :default 0))
   (let ((target-depth (+ *rule-depth* n-levels))
	 (counter 0))
     (add-tracing-filter
       *joshua-debugger*
       #'(lambda (ignore depth)
	   (when
	     (cond ((or (and (not (plusp n-levels))
			     ( depth target-depth))
			(and (plusp n-levels)
			     ( depth target-depth)))
		    (with-character-style
		      (*deemphasis-character-style* *standard-output*)
		      (fresh-line *standard-output*)
		      (joshua-trace-message-indentor *standard-output*)

		      (format *standard-output* " Leaping past ~d traced events"
			      counter))
		    t)
		   (t (incf counter)
		      nil))
	     (reset-tracing-state *joshua-debugger*)
	     t))))
   :done)

(cp:define-command-accelerator acc-com-leap "Joshua portable trace" (#\line #\c-l)
			       (:argument-allowed t)
			       (narg-p narg)
  (unless narg-p (setf narg 0))
  `(com-leap ,narg))

(cp:define-command-accelerator acc-com-leap "Joshua portable trace" (#\) ()
			       ()
  `(com-leap 1))

(cp:define-command-accelerator acc-com-leap "Joshua portable trace" (#\) ()
			       ()
  `(com-leap -1))

(cp:define-command (com-help :command-table "Joshua portable trace")
    (&key
      (format '((cl:member :brief :detailed)) :default ':brief	;
	      :documentation "\"Detailed\" shows all commands.  \"Brief\" uses compression."))
   (cp::show-command-name-help cl:*standard-output* format))

(cp:define-command-accelerator acc-com-simple-help "Joshua portable trace" (#\help) ()
			       ()
  `(com-help))
|#

(defun explain (predication &optional depth (stream *standard-output*))
  ;; explain why a particular predication is believed.
  (flet ((explain-justification (justification stream depth)
           (multiple-value-bind (mnemonic ignore true-support false-support unknown-support certainty)
               (destructure-justification justification)
             (declare (ignore ignore))
             (cond ((and (null true-support) (null false-support) (null unknown-support))
                    (format stream "It is ~/ji::a-an/ ~a" mnemonic mnemonic))
                   (t
                    (format stream "It was derived from rule ~a" mnemonic)
                    (when certainty
                      (format stream " with rule strength ~f" certainty))
                    ;; this seems a bit nicer than the old behavior, which was to follow the tree to its end.
                    (let ((all-literals (append true-support false-support unknown-support)))
                      (loop for literal in all-literals
                          do (explain literal (when depth (1- depth)) stream))))))))
    (when (or (null depth) (not (= depth 0)) )
      (let ((truth-value (predication-truth-value predication)))
        (terpri stream)
        (print-database-predication-without-truth-value predication stream)
        (write-string " is " stream)
        (clim:present truth-value 'truth-value :stream stream)
        (let ((certainty-factor (certainty-factor predication)))
          (when certainty-factor
            (format stream " with certainty ~d" (abs certainty-factor))))
        (when (or (null depth) (not (= depth 1)))
          (unless (= truth-value +unknown+)
            (clim:indenting-output (stream '(2 :character))
              (terpri stream)
              (let ((current-justification (current-justification predication)))
                (if (listp current-justification)
                    (loop for just in current-justification do
                          (terpri stream)
                        (explain-justification just stream depth))
                  (explain-justification current-justification stream depth))))))))))

(defun a-an (stream argument &rest ignore)
  (declare (ignore ignore))
  ;;; a-an hack
  (flet ((vowelp (x)
	   (and (characterp x)
		;; what about semi-vowels, like y?
		(member x '(#\a #\e #\i #\o #\u) :test #'char-equal))))
    (setq argument (aref (string argument) 0)) ;coerce to string & get 1st char
    (princ (if (vowelp argument) "an" "a") stream)))

(defun format-list (stream argument colon-flag atsign-flag &rest parameters)
  ;; print a textual list in a couple of the common cases.
  ;; no modifiers prin1 and "and"
  ;; : modifier   use princ instead
  ;; @ modifier   use "or" instead
  (declare (ignore parameters))
  (clim:format-textual-list argument
		       (if colon-flag #'princ #'prin1)
		       :conjunction (if atsign-flag "or" "and")
		       :stream stream))





;;;; Command Translators

;;; First the translators for rules

;;;tracing and untracing forward rules
(clim:define-presentation-to-command-translator ptrans-forward-to-trace-rule
   (rule com-joshua-trace clim:global-command-table
     :gesture nil
     :menu t
     :documentation "Trace this forward rule"
     :tester ((object)
	      (traceable-forward-rule-p *joshua-debugger* object))
     )
   (object)
  `(rule ,object
         :tracing-options
         (add-trace-forward-rule
          ,(find-tracer-by-class-name *joshua-debugger* 'forward-rule-tracer))))

(clim:define-presentation-to-command-translator ptrans-forward-to-untrace-rule
   (rule com-joshua-trace clim:global-command-table
     :gesture nil
     :menu t
     :documentation "Untrace this forward rule"
     :tester ((object)
	      (untraceable-forward-rule-p *joshua-debugger* object)))
   (object)
  `(rule ,object :tracing-options
         (untrace-forward-rule
          ,(find-tracer-by-class-name *joshua-debugger* 'forward-rule-tracer))))


;;; Tracing and untracing backward rules
(clim:define-presentation-to-command-translator ptrans-backward-to-trace-rule
                                                (rule com-joshua-trace clim:global-command-table
                                                      :gesture nil
                                                      :menu t
                                                      :documentation "Trace this backward rule"
                                                      :tester ((object)
	                                                       (traceable-backward-rule-p *joshua-debugger* object)))
                                                (object)
  `(rule ,object :tracing-options
         (add-trace-backward-rule
          ,(find-tracer-by-class-name *joshua-debugger* 'backward-rule-tracer))))

(clim:define-presentation-to-command-translator ptrans-backward-to-untrace-rule
                                                (rule com-joshua-trace clim:global-command-table
                                                      :gesture nil
                                                      :menu t
                                                      :documentation "Untrace this backward rule"
                                                      :tester ((object)
	                                                       (untraceable-backward-rule-p *joshua-debugger* object)))
                                                (object)
  `(rule ,object
         :tracing-options
         (untrace-backward-rule
          ,(find-tracer-by-class-name *joshua-debugger* 'backward-rule-tracer))))

#|



;;;showing the rule source
(define-presentation-to-command-translator ptrans-rule-to-show-rule-source
   (rule
     :gesture :middle
     :priority 1 ;;needs to override the Show Functions Arguments translator
     :documentation "Show the definition for this rule")
   (rule-name)
  `(com-show-rule-source (,rule-name)))

|#

;;; Translators for predicate flavors
(clim:define-presentation-to-command-translator ptrans-joshua-predicate-to-trace-predicate
   (joshua-predicate com-joshua-trace clim:global-command-table
     :gesture nil
     :menu t
     :documentation "Trace predications of this flavor"
     :tester ((object)
	      (traceable-predicate-p *joshua-debugger* object))
     )
   (object)
  `(joshua-predicate ,object
                     :tracing-options
		     (add-trace-predicate
		       ,(find-tracer-by-class-name *joshua-debugger* 'predication-tracer))))


(clim:define-presentation-to-command-translator ptrans-joshua-predicate-to-untrace-predicate
   (joshua-predicate com-joshua-trace clim:global-command-table
     :gesture nil
     :menu t
     :documentation "Untrace predications of this flavor"
     :tester ((object)
	      (untraceable-predicate-p *joshua-debugger* object))
     )
   (object)
  `(joshua-predicate ,object :tracing-options
		     (untrace-predicate
		       ,(find-tracer-by-class-name *joshua-debugger* 'predication-tracer))))

(clim:define-presentation-to-command-translator ptrans-tms-predicate-to-trace-predicate
   (joshua-predicate com-joshua-trace clim:global-command-table
     :gesture nil
     :menu t
     :documentation "Trace TMS operations on predications of this flavor"
     :tester ((object)
	      (traceable-tms-predicate-p *joshua-debugger* object))
     )
   (object)
  `(joshua-predicate ,object
                     :tracing-options
		     (add-trace-predicate
		       ,(find-tracer-by-class-name *joshua-debugger* 'TMS-tracer))))


(clim:define-presentation-to-command-translator ptrans-tms-predicate-to-untrace-predicate
   (joshua-predicate com-joshua-trace clim:global-command-table
     :gesture nil
     :menu t
     :documentation "Untrace TMS operations on predications of this flavor"
     :tester ((object)
	      (untraceable-tms-predicate-p *joshua-debugger* object)))
   (object)
  `(joshua-predicate ,object
                     :tracing-options
		     (untrace-predicate ,(find-tracer-by-class-name *joshua-debugger* 'TMS-tracer))))

;;; And the translators for predications
(clim:define-presentation-to-command-translator ptrans-predication-to-trace-forward-rule-trigger
                                                (predication com-joshua-trace clim:global-command-table
                                                             :gesture nil
                                                             :menu t
                                                             :documentation "Trace forward rules triggered by predications matching this pattern"
                                                             :tester ((object)
	                                                              (traceable-forward-rule-trigger-p *joshua-debugger* object)))
                                                (object)
  `(predication ,object
                :tracing-options
                (add-trace-forward-rule-trigger
                 ,(find-tracer-by-class-name *joshua-debugger* 'forward-rule-tracer))))


(clim:define-presentation-to-command-translator ptrans-predication-to-untrace-forward-rule-trigger
                                                (predication com-joshua-trace clim:global-command-table
                                                             :gesture nil
                                                             :menu t
                                                             :documentation "Untrace forward rules triggered by predications matching this pattern"
                                                             :tester ((object)
	                                                              (untraceable-forward-rule-trigger-p *joshua-debugger* object)))
                                                (object)
  `(predication ,object
                :tracing-options
                (untrace-forward-rule-trigger
                 ,(find-tracer-by-class-name *joshua-debugger* 'forward-rule-tracer))))

(clim:define-presentation-to-command-translator ptrans-predication-to-trace-backward-rule-trigger
                                                (predication com-joshua-trace clim:global-command-table
                                                             :gesture nil
                                                             :menu t
                                                             :documentation "Trace backward rules triggered by predications matching this pattern"
                                                             :tester ((object)
	                                                              (traceable-backward-rule-trigger-p *joshua-debugger* object))
                                                             )
                                                (object)
  `(predication ,object :tracing-options
                (add-trace-backward-rule-trigger
                 ,(find-tracer-by-class-name *joshua-debugger* 'backward-rule-tracer))))

(clim:define-presentation-to-command-translator ptrans-predication-to-untrace-backward-rule-trigger
                                                (predication com-joshua-trace clim:global-command-table
                                                             :gesture nil
                                                             :menu t
                                                             :documentation "Untrace backward rules triggered by predications matching this pattern"
                                                             :tester ((object)
	                                                              (untraceable-backward-rule-trigger-p *joshua-debugger* object))
                                                             )
                                                (object)
  `(predication ,object :tracing-options
                (untrace-backward-rule-trigger
                 ,(find-tracer-by-class-name *joshua-debugger* 'backward-rule-tracer))))

(clim:define-presentation-to-command-translator ptrans-predication-to-trace-predication
                                                (predication com-joshua-trace clim:global-command-table
                                                             :gesture nil
                                                             :menu t
                                                             :documentation "Trace all predications matching this pattern"
                                                             :tester ((object)
	                                                              (traceable-predication-p *joshua-debugger* object))
                                                             )
                                                (object)
  `(predication ,object :tracing-options
                (add-trace-pattern
                 ,(find-tracer-by-class-name *joshua-debugger* 'predication-tracer))))


(clim:define-presentation-to-command-translator ptrans-predication-to-untrace-predication
                                                (predication com-joshua-trace clim:global-command-table
                                                             :gesture nil
                                                             :menu t
                                                             :documentation "Untrace all predications matching this pattern"
                                                             :tester ((object)
	                                                              (untraceable-predication-p *joshua-debugger* object))
                                                             )
                                                (object)
  `(predication ,object :tracing-options
                (untrace-pattern
                 ,(find-tracer-by-class-name *joshua-debugger* 'predication-tracer))))


(clim:define-presentation-to-command-translator ptrans-tms-predication-to-trace-tms-predication
                                                (tms-predication-presentation com-joshua-trace clim:global-command-table
                                                                 :gesture nil
                                                                 :menu t
                                                                 :documentation "Trace TMS operations on predications matching this pattern"
                                                                 :tester ((object)
	                                                                  (traceable-tms-predication-p *joshua-debugger* object))
                                                                 )
                                                (object)
  `(predication ,object :tracing-options
                (add-trace-pattern
                 ,(find-tracer-by-class-name *joshua-debugger* 'TMS-tracer))))

(clim:define-presentation-to-command-translator ptrans-tms-predication-to-untrace-tms-predication
                                           (tms-predication-presentation com-joshua-trace clim:global-command-table
                                                                         :menu t
                                                                         :gesture nil
                                                                         :documentation "Untrace TMS operations on predications matching this pattern"
                                                                         :tester ((object)
	                                                                          (untraceable-tms-predication-p *joshua-debugger* object)))
                                           (object)
  `(predication ,object :tracing-options
                (untrace-pattern
                 ,(find-tracer-by-class-name *joshua-debugger* 'TMS-tracer))))

(clim:define-presentation-to-command-translator
    pred-to-explain
    (database-predication com-explain-predication
                          #+(and clim (not mcclim)) clim-env::lisp-listener
                          ;; mcclim doesn't export this, foo!
                          #+mcclim clim-listener::listener
                          :gesture :describe)
    (object)
  (list object nil))


(clim:define-presentation-translator
  trans-predication-to-db-predication
    (predication database-predication
                 #+(and clim (not mcclim)) clim-env::lisp-listener
                 ;; mcclim doesn't export this, foo!
                 #+mcclim clim-listener::listener
               :tester ((object) (has-been-in-database-p object)))
  (object)
  object)



#|

(define-presentation-to-command-translator ptrans-tracing-event-to-trace-event
   (tracing-event
     :gesture nil
     :documentation "Trace this event"
     :tester ((event &rest ignore)
	      (not (event-traced-p *joshua-debugger* event)))
     )
   (event)
  `(com-joshua-trace tracing-event ,event :tracing-options
		     (add-trace-event
		       ,(car (gethash (tracing-event-name event)
				      (joshua-debugger-event-to-tracer-table
					*joshua-debugger*))))))

(define-presentation-to-command-translator ptrans-tracing-event-to-untrace-event
   (tracing-event
     :gesture nil
     :documentation "Untrace this event"
     :tester ((event &rest ignore)
	      (event-traced-p *joshua-debugger* event))
     )
   (event)
  `(com-joshua-trace tracing-event ,event :tracing-options
		     (untrace-event
		       ,(car (gethash (tracing-event-name event)
				      (joshua-debugger-event-to-tracer-table
					*joshua-debugger*))))))

(define-presentation-to-command-translator ptrans-tracing-event-to-step-event
   (tracing-event
     :gesture nil
     :documentation "Step this event"
     :tester ((event &rest ignore)
	      (not (event-stepped-p *joshua-debugger* event)))
     )
   (event)
  `(com-joshua-trace tracing-event ,event :tracing-options
		     (add-step-event
		       ,(car (gethash (tracing-event-name event)
				      (joshua-debugger-event-to-tracer-table
					*joshua-debugger*))))))

(define-presentation-to-command-translator ptrans-tracing-event-to-unstep-event
   (tracing-event
     :gesture nil
     :documentation "Unstep this event"
     :tester ((event &rest ignore)
	      (event-stepped-p *joshua-debugger* event))
     )
   (event)
  `(com-joshua-trace tracing-event ,event :tracing-options
		     (unstep-event
		       ,(car (gethash (tracing-event-name event)
				      (joshua-debugger-event-to-tracer-table
					*joshua-debugger*))))))
|#


(define-multiple-command (com-joshua-syntax :name t) (*joshua-command-table*)
                         ((yes-or-no 'clim:boolean
                                     :default t
	                             :prompt "Use Johsua Syntax"))
  ;; a real version of this would do it possibly graphically, and with incremental redisplay, etc.
  (let ((*error-output* *standard-output*))
    (if yes-or-no
      (joshua:enable-joshua)
      (joshua:disable-joshua))))

;;; Need to see whether mcclim's input editor has similar features

#-mcclim
(defvar *joshua-break-chars*
    `(,#\[ ,#\] ,@clim-internals::*word-start-atom-break-chars*))

#-mcclim
(clim-internals::define-input-editor-command (com-ie-show-predicate-arglist :rescan nil)
    (stream input-buffer)
  "Show predicate arglist"
  (multiple-value-bind (symbol package)
      (let ((clim-internals::*word-start-atom-break-chars* *joshua-break-chars*))
	(clim-internals::symbol-at-position Stream input-buffer '(#\[ )))
    (let* ((predicate (find-symbol (symbol-name symbol) package))
	   (predicatep (and predicate (find-class predicate nil))))
      (if predicatep
	  (multiple-value-bind (arglist found-p)
	      (find-predicate-arglist predicate)
	    (when found-p
	      (clim:with-input-editor-typeout (stream)
		#-Cloe-Runtime
		(format stream "~S: (~{~A~^ ~})"
			symbol arglist)
		#+Cloe-Runtime
		(format stream "~S (~A): (~{~:A~^ ~})"
			symbol found-p arglist))))
          (clim:beep stream)))))

#-mcclim
(clim-internals::define-input-editor-gestures
    (:ie-show-predicate-arglist :a :meta)
    )

#-mcclim
(clim-internals::assign-input-editor-key-bindings
 com-ie-show-predicate-arglist :ie-show-predicate-arglist)
