;;; -*- Mode: Lisp; Package: JI; Syntax: Ansi-common-lisp  -*-

#|

The idea is that we have a mixin to hold a certainty factor and justifications
Certainty factors are numbers between -1 and + 1
The certainty factor is in addition to the built-in truth-value (which has 2 bits allocated to it).
Things with negative certainty factor have false truth value
            positive                       true

The :justification argument to tell is extended so that if the mnemonic is a list it's
first element is the mnemonic and it's second is the certaint factor.

Similarly the mnemonic argument to justify is a list of the mnemonic and certainty-factor

Rules can have a :certainty argument in the list after the name.

This is then built into the justification for forward chaining rules.

Backward rules create the answer predication and add a justification (if it's a new justification)

The calculus is that used by mycin

When doing an ask, we need to query both truth values because they both impact on the answer

In contrast to normal joshua behavior we should run all rules and questions
and then add in all the evidence and then return


|#



(in-package :ji)

(define-predicate-model cf-mixin 
                        ((certainty-factor :initform 0 :accessor certainty-factor)
                         (justifications :initform nil :accessor justifications)
                         (out-going-justs :initform nil :accessor outgoing-justs))
                        ())


#|
;;; This guy causes us to ask both truth values and to cache the answers
;;; this is probably a bad idea at this level.  Should be built one more layer of semantics up.
(define-predicate-method (ask cf-mixin :around) (intended-truth-value continuation do-rules do-questions)
  (let ((answers nil))
    (flet ((my-continuation (bs)
             (let* ((answer (ask-query bs))
                    (database-answer (insert (copy-object-if-necessary answer))))
               (pushnew database-answer answers))))
      (call-next-method self *true*  #'my-continuation do-rules do-questions)
      (call-next-method self *false* #'my-continuation do-rules do-questions))
    (loop for answer in answers
          when (eql (predication-truth-value answer) intended-truth-value)
          do (with-stack-list (just self intended-truth-value answer)
               (with-unification 
                 (unify self answer)
                 (funcall continuation just))))))
|#


(define-predicate-method (ask-rules cf-mixin :around) (intended-truth-value continuation do-questions)
  (flet ((my-continuation (bs)
           (let* ((answer (ask-query bs))
                  (database-answer (insert (copy-object-if-necessary answer))))
             (multiple-value-bind (truth-value mnemonic true-support false-support) (parse-bs bs)
               ;; this check is probably redundant, but harmless
               (when (eql truth-value intended-truth-value)
                 (let ((certainty (rule-certainty-factor mnemonic)))
                   (with-stack-list (compound-mnemonic mnemonic certainty)
                     (justify database-answer truth-value compound-mnemonic true-support false-support))))
               (with-stack-list (just self truth-value database-answer)
                 (funcall continuation just))))))
    (call-next-method self intended-truth-value #'my-continuation do-questions)))

(define-predicate-method (ask-questions cf-mixin :around) (intended-truth-value continuation)
  (flet ((my-continuation (bs)
           (let* ((answer (ask-query bs))
                  (database-answer (insert (copy-object-if-necessary answer))))
             (multiple-value-bind (truth-value mnemonic true-support false-support) (parse-bs bs)
               ;; this check is probably redundant, but harmless
               (when (eql truth-value intended-truth-value)
                 (let ((certainty (progn (format *query-io* "What what certainty do you believe ~a"
                                                 database-answer)
                                         (read *query-io*))))
                   (with-stack-list (compound-mnemonic mnemonic certainty)
                     (justify database-answer truth-value compound-mnemonic true-support false-support))))
               (with-stack-list (just self truth-value database-answer)
                 (funcall continuation just))))))
    (call-next-method self intended-truth-value #'my-continuation)))

(defun parse-bs (backward-support)
  (let ((true-support nil)
        (false-support nil)
        (rule-name nil)
        (truth-value nil))
    (labels 
      ((dispatch-this-level (bs top-level?)
         (when bs
	   (destructuring-bind (query bs-truth-value type . rest) bs
             (declare (ignore query))
             (when top-level? 
               (setq truth-value bs-truth-value))
             (cond 
              ((predicationp type)
               (let ((his-truth-value (predication-truth-value type)))
                 (cond ((eql his-truth-value *true*)
                        (push type true-support))
                       ((eql his-truth-value *false*)
                        (push type false-support)))))
              ((and (consp type) (eql (first type) 'rule))
               (when top-level?
                 (setq rule-name (second type)))
               (loop for thing in rest do (dispatch-this-level thing nil)))
              ;; fill these in later
              ((eql type 'and) 
               (loop for thing in rest do (dispatch-this-level thing nil)))
              ((eql type 'or))
              ((eql type 'known) )
              ((eql type 'provable) )))))) 
      (dispatch-this-level backward-support t))
    (values truth-value rule-name true-support false-support)))

(defclass cf-justification ()
  ((true-support :initform nil :initarg :true-support :accessor true-support)
   (false-support :initform nil :initarg :false-support :accessor false-support)
   (mnemonic :initform nil :initarg :mnemonic :accessor mnemonic)
   (conclusion :initform nil :initarg :conclusion :accessor conclusion)
   (certainty-factor :initform nil :initarg :certainty-factor :accessor certainty-factor)
   (truth-value :initform *unknown* :initarg :truth-value :accessor truth-value)
   ))

(defmethod justification-cf ((just cf-justification))
  (macrolet ((minf (place value)
               `(if (or (null ,place) (< (abs ,value) (abs ,place)))
                  (setq ,place ,value))))
    (let ((input-cf 1))
      (loop for supporter in (true-support just)
            for his-cf = (certainty-factor supporter)
            do (minf input-cf his-cf))
      (loop for supporter in (false-support just)
            for his-cf = (certainty-factor supporter)
            do (minf input-cf his-cf))
      (* input-cf (certainty-factor just)))))

(defun set-equal (s1 s2)
  (not (set-exclusive-or s1 s2)))

(defmethod same-justification ((j1 cf-justification) (j2 cf-justification))
  (and (eql (mnemonic j1) (mnemonic j2))
       (eql (certainty-factor j1) (certainty-factor j2) )
       (set-equal (true-support j1) (true-support j2))
       (set-equal (false-support j1) (false-support j2))))

;; this still needs to do the cf calculation and figure out if there's a truth-value flip
(define-predicate-method (justify cf-mixin :around) (truth-value &optional compound-mnemonic
                                                                 true-support false-support unknown-support)
  (let ((mnemonic nil)
        (certainty-factor nil))
    (if (consp compound-mnemonic)
      (destructuring-bind (name cf) compound-mnemonic
        (setq mnemonic name certainty-factor cf))
      (setq mnemonic compound-mnemonic certainty-factor 1))
    (let ((justification (make-instance 'cf-justification
                           :conclusion self
                           :truth-value truth-value
                           :certainty-factor certainty-factor
                           :mnemonic mnemonic
                           :true-support true-support
                           :false-support false-support)))
      (add-justification self justification))
    (let* ((certainty-factor (certainty-factor self))
           (my-current-truth-value (cond ((zerop certainty-factor) *unknown*)
                                         ((plusp certainty-factor) truth-value)
                                         ((minusp certainty-factor) (cond ((eql truth-value *true*) *false*)
                                                                          ((eql truth-value *false*) *true*)
                                                                          (t *unknown*))))))
      (when (or (and (eql my-current-truth-value *false*) (plusp certainty-factor))
                (and (eql my-current-truth-value *true*) (minusp certainty-factor)))
        (setf (certainty-factor self) (- certainty-factor)))
    (call-next-method self my-current-truth-value mnemonic true-support false-support unknown-support))))


;;; notice that below anytime we add a justification we recalculate the cf
;;; and that any time the cf changes we recalculate the cf's of all dependents
;;; so (other than retraction) this is incremental.

(defmethod add-justification ((pred cf-mixin) new-justification)
  (unless (find new-justification (justifications pred) :test #'same-justification)
    (push new-justification (justifications  pred))
    (loop for pred in (true-support new-justification) do (push new-justification (outgoing-justs pred)))
    (loop for pred in (false-support new-justification) do (push new-justification (outgoing-justs pred)))
    (let ((his-cf (justification-cf new-justification))
          (my-cf (certainty-factor pred)))
      (setf (certainty-factor pred)
            (combine-cfs  his-cf my-cf))
      (reevaluate-dependents pred))))

(defmethod reevaluate-cf ((pred cf-mixin))
  (setf (certainty-factor pred) 0)
  (loop for just in (justifications pred)
        for his-cf = (justification-cf just)
        for my-cf = (certainty-factor pred)
        do (setf (certainty-factor pred)
                 (combine-cfs my-cf his-cf)))
  (reevaluate-dependents pred))

(defmethod reevaluate-dependents ((pred cf-mixin))
  (loop for just in (outgoing-justs pred)
        for destination = (conclusion just)
        do (reevaluate-cf destination)))

;; this is the mycin update formula
(defun combine-cfs (my-cf his-cf)
  (cond ((and (> my-cf 0) (> his-cf 0))
         (+ my-cf his-cf (* -1 his-cf my-cf)))
        ((and (< my-cf 0) (< his-cf 0))
         (+ my-cf his-cf (* my-cf his-cf)))
        (t (/ (+ my-cf his-cf)
              (- 1 (min (abs my-cf) (abs his-cf)))))))

