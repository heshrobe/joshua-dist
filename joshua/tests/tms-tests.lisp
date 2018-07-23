;;; -*- Base: 10; Mode: LISP; Syntax: Joshua-ansi; Package: JOSHUA-USER; readtable: joshua -*-

(in-package "JU")

(define-predicate loves (lover lovee) (ltms:ltms-predicate-model)
  :destructure-into-instance-variables
  (:readable-instance-variables lover lovee)
  )

(define-predicate jealous (who) (ltms:ltms-predicate-model))
(define-predicate kills (killer victim) (ltms:ltms-predicate-model))
(define-predicate tragedy (event) (ltms:ltms-predicate-model))

(defrule quality-not-quantity (:forward) ;p 11
  IF [not [loves Demetrius Hermia]]
  THEN [loves Demetrius Helena])

(defrule love-in-idleness (:forward) ;p 12
  IF [not [loves Lysander Hermia]]
  THEN [loves Lysander Helena])

(defvar *women* '(Helena Hermia))
(defrule only-one-love (:forward :importance 1)
  IF [and [loves ?x ?y]
	  (member ?y *women*)]
  THEN (loop for woman in *women*
	       unless (eq woman ?y)
		 ;; make him not love any other woman
		 do (tell `[not [loves ,?x ,woman]])))
 
(defrule unrequited-love (:forward :importance 2) ;p 13
  ;; you don't commit suicide unless you really know the person you love
  ;; doesn't love you back.  If it's just unknown, you can live with hope.
  IF [and [loves ?x ?y]
	  [not [loves ?y ?x]]]
  THEN [kills ?x ?x])

(defrule jealousy (:forward :importance 3) ;p 12
  ;; jealous people tend to try to kill their competition
  IF [and [jealous ?x]
	  [loves ?x ?y]
	  [loves ?z ?y]
	  (different-objects ?x ?z)]
  THEN [kills ?x ?z])

(defrule tragedy-rule (:forward) ;p 13
  ;; if anybody kills anybody, that's a tragedy
  IF [kills ?x ?y]
  THEN [tragedy [kills ?x ?y]]) ;wants to refer to trigger here.

(defrule trying-to-write-a-comedy (:forward)
  ;; no tragedies, please
  IF [tragedy ?fact]
  THEN [ltms:contradiction])

(define-predicate-method (act-on-truth-value-change loves) (old-truth-value &optional old-state)
  (declare (ignore old-state))
  ;; cause a contradiction by justifying true and false of tragedy
  ;; Heaven help us, this actually manages to do a contrapositive inference! -- sgr & HES 1/30/87
  (with-statement-destructured (lover lovee) ()
      (when (eql (predication-truth-value self) *unknown*)
	(when (member lovee *women*)
	  (let ((he-loves-someone nil))
	    (loop named love-check
		  for woman in *women*
		  unless (eq woman lovee)
		    do (ask `[or [loves ,lover ,woman]
				 [not [loves ,lover ,woman]]]
			    #'(lambda (&rest ignore)
                                (declare (ignore ignore))
				(setq he-loves-someone t)
				(return-from love-check (values)))))
	    (unless he-loves-someone
	      (cond
		((eql old-truth-value *true*)
		 (loop for woman in *women*
			      when (not (eql woman lovee))
				do (tell `[loves ,lover ,woman] :justification :assumption)
				   (return (values))))
		((eql old-truth-value *false*)
		 (tell `[loves ,lover ,lovee] :justification :assumption)))))))))

;; "... the amatory preferences of men, who are creatures easily swayed by flowers
;; and dependency-directed backtracking." -- Doyle
(defun midsummer-world (&optional (stream *standard-output*))
  (clear)
  (flet ((do-it ()
	   ;; the root of all evil (and plot, for that matter)
	   (tell [jealous Lysander] :justification :premise)
	   ;; Hermia and Lysander are an item
	   (tell [loves Hermia Lysander] :justification :premise)
	   (tell [loves Lysander Hermia] :justification :assumption)
	   ;; Here' some opportunity for unrequited love and jealousy-killings.
	   (tell [loves Helena Demetrius] :justification :premise)
	   (tell [loves Demetrius Hermia] :justification :assumption)
	   (when stream
	     (format stream "~&This leaves us in  the following condition:")
	     (let ((*standard-output* stream))
	       (ask [or [jealous ?x] [loves ?x ?y] [kills ?x ?y]]  #'print-query)))))
    (do-it))
  ;; return innocuous value
  'done)

(deftest midsummer-test
  ;; test the midsummer TMS example
  (let ((number-of-contradictions 0)
	(number-of-debugger-contradictions 0))
    ;; there should be 2 contradictions, one of which would go into the debugger
    ;; (more than 1 assumption)
    (handler-bind ((tms-contradiction
		     ;; have to handle the contradiction automatically, can't ask user.
		     #'(lambda (condition)
			 ;; make sure it's the contradiction we expect, and then handle it
			 ;; the way we want to.  (Note that 2 contradictions happen, only one
			 ;; of which is interesting.)
			 (incf number-of-contradictions)
			 (let ((non-premises (tms-contradiction-non-premises condition))
			       (premises     (tms-contradiction-premises condition))
			       lysander-loves)
			   (when (> (length non-premises) 1)
			     (incf number-of-debugger-contradictions))
			   (flet ((check-justification-statement (justification statement)
				    (multiple-value-bind (mnemonic predication) (destructure-justification justification)
				      (declare (ignore mnemonic))
				      (equal (predication-statement predication) statement))))
			     (cond ((and (= (length non-premises) 2)
					 (= (length premises)     1)
					 (check-justification-statement
					   (first premises) '(jealous lysander))
					 (setq lysander-loves
					       (find-if #'(lambda (x)
							    (check-justification-statement
							      x '(loves Lysander Hermia)))
							non-premises))
					 (find-if #'(lambda (x)
						      (check-justification-statement
							x '(loves Demetrius Hermia)))
						  non-premises))
				    ;; this is the one we're looking for. (Let the system handle others.)
				    ;; We want to unjustify [loves Lysander Hermia], since that causes the
				    ;; most agitation.
				    (invoke-restart :unjustify-subset (list lysander-loves)))
				   ((= (length non-premises) 1)
				    ;; this is the case the global handler in Genera would have handled
				    (invoke-restart :unjustify-subset non-premises)))
			     )))))
      (midsummer-world nil))
    ;; see if the database is in the state we expect
    (and (check-database [loves Lysander  Hermia])
	 (check-database [loves Hermia    Lysander])
	 (check-database [loves Helena    Demetrius])
	 (check-database [loves Demetrius Helena])
	 (check-database [jealous Lysander])
	 (= number-of-contradictions 2)
	 (= number-of-debugger-contradictions 1))))


;;;
;;; The standard example in default reasoning.  Do (tweety) to run it.
;;;

(define-predicate bird     (boid)          (ltms:ltms-predicate-model))
(define-predicate penguin  (boid)          (ltms:ltms-predicate-model))
(define-predicate fly      (boid)          (ltms:ltms-predicate-model))
(define-predicate abnormal (who for-what) (ltms:ltms-predicate-model))

(define-predicate-method (act-on-truth-value-change abnormal) (old-truth-value &optional old-state)
  (declare (ignore old-state))
  ;; whenever the truth-value of an ABNORMAL predication changes from
  ;; *TRUE* to *UNKNOWN*, force it to be *FALSE* instead.  This is sort of
  ;; like circumscription's minimizing abnormality.
  (when (and (= (predication-truth-value self) *unknown*)
	       (= old-truth-value *true*))
      (justify self *false* :assumption)))
    
(defrule birds-fly-by-default (:forward)
  ;; if ?x is a bird, then it flies unless it's abnormal
  IF [and [bird ?x] :support ?bird]
  THEN (unless (block already-abnormal
		 (map-over-database-predications `[abnormal ,?x [fly ,?x]]
						 #'(lambda (fact)
						     (return-from already-abnormal fact))))
	 ;; if you get here, you don't yet know that ?x is abnormal with respect to fly
	 (let ((the-default (tell `[not [abnormal ,?x [fly ,?x]]]
				  :justification :assumption)))
	   ;; assume this boid is not abnormal
	   ;; *support* is a wart protruding from the implementation substrate
	   (tell `[fly ,?x]
		 :justification `(birds-fly-by-default (,?bird) (,the-default) ())))))

(defrule penguins-are-weird (:forward)
  ;; penguins are birds, but don't fly because they are abnormal with respect to FLY.
  IF [penguin ?x]
  THEN [and [abnormal ?x [fly ?x]]
	    [bird ?x]])

(defrule abnormal-things-dont-fly (:forward)
  ;; things abnormal with respect to FLY don't fly.
  IF [abnormal ?x [fly ?x]]
  THEN [not [fly ?x]])

(defun show-the-database (stream)
  ;; should use the :Provide Output Destination feature
  (ask [or [abnormal ?x ?y]
           [fly ?x]
	   [bird ?x]
	   [penguin ?x]]
       #'(lambda (b-s)
	   (print-query b-s stream)))
  (ask [or [not [abnormal ?x ?y]]
           [not [fly ?x]]
	   [not [bird ?x]]
	   [not [penguin ?x]]]
       #'(lambda (b-s)
	   (print-query b-s stream))))

;;; The apparent silliness of the arguments here is so that the automatic testing functions
;;;  can pass in database probes to look at the results at various points of execution.
(defun tweety (&optional (stream *standard-output*)
			 &key (first-fun #'show-the-database)
			 (second-fun #'show-the-database)
			 (third-fun #'show-the-database)
			 &aux first-value second-value third-value)
  ;; The equivalent of the Global Handler in Genera
  (handler-bind ((tms-contradiction
		   ;; have to handle the contradiction automatically, can't ask user.
		   #'(lambda (condition)
		       ;; make sure it's the contradiction we expect, and then handle it
		       ;; the way we want to.  (Note that 2 contradictions happen, only one
		       ;; of which is interesting.)
		       (let ((non-premises (tms-contradiction-non-premises condition)))
			 (when (= (length non-premises) 1)
			   ;; this is the case the global handler in Genera would have handled
			   (invoke-restart :unjustify-subset non-premises))))))
    (macrolet ((format-maybe (string &rest args)
		 `(when stream (format stream ,string ,@args))))
      ;; how to run this aviary.
      (clear)
      ;;
      (format-maybe "~&First off, Tweety is a bird")
      (tell [bird tweety] :justification :premise)
      (format-maybe "; so we know the following:")
      (setq first-value (funcall first-fun stream))
      ;;
      (format-maybe "~2%However, Tweety is a penguin")
      (let ((penguin-statement (tell [penguin tweety] :justification :premise)))
	(format-maybe "; so we now know that Tweety can't fly:")
	(setq second-value (funcall second-fun stream))
	;;
	(format-maybe "~2%Now I'll give up on the penguin stuff")
	(unjustify penguin-statement))
      (format-maybe "; so we now know that Tweety can fly again: ")
      (setq third-value (funcall third-fun stream))
      (if stream
	  ;; being run interactively
	  (values)
	  ;; being run by the automatic tester
	  (values first-value second-value third-value)))))


(deftest tweety-test
  ;; test the Tweety TMS example
  (multiple-value-bind (first second third)
      (tweety nil
	      :first-fun  #'(lambda (ignore) (declare (ignore ignore)) (check-database [fly tweety]))
	      :second-fun #'(lambda (ignore) (declare (ignore ignore)) (check-database [not [fly tweety]]))
	      :third-fun  #'(lambda (ignore) (declare (ignore ignore)) (check-database [fly tweety])))
    (and first second third)))