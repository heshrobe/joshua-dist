;;; -*- Base: 10; Mode: Lisp; Package: JOSHUA-USER; Syntax: Joshua-ansi -*-
;;; Created 3/07/89 14:26:24 by HES running on MERLIN at SCRC.

(in-package "JU")

(clear)

(define-object-type two-terminal-device
   :slots (v1 v2 voltage current i1 i2)
  )

(defrule 2t-kvl-a (:forward)
  If [and [object-type-of ?device two-terminal-device]
	  [value-of (?device v1) ?v1]
	  [value-of (?device v2) ?v2]]
  Then `[value-of (?device voltage) ,(- ?v2 ?v1)])

(defrule 2t-kvl-b (:forward)
  If [and [object-type-of ?device two-terminal-device]
	  [value-of (?device v1) ?v1]
	  [value-of (?device voltage) ?voltage]]
  Then `[value-of (?device v2) ,(+ ?voltage ?v1)])

(defrule 2t-kvl-c (:forward)
  If [and [object-type-of ?device two-terminal-device]
	  [value-of (?device v2) ?v2]
	  [value-of (?device voltage) ?voltage]]
  Then `[value-of (?device v1) ,(- ?v2 ?voltage)])

(defrule 2t-kcl-a (:forward)
  If [and [object-type-of ?device two-terminal-device]
	  [value-of (?device i1) ?i1]]
  Then `[value-of (?device i2) ,(- ?i1)])

(defrule 2t-kcl-b (:forward)
  If [and [object-type-of ?device two-terminal-device]
	  [value-of (?device i2) ?i2]]
  Then `[value-of (?device i1) ,(- ?i2)])

(defrule 2t-kcl-c (:forward)
  If [and [object-type-of ?device two-terminal-device]
	  [value-of (?device current) ?i]]
  Then [value-of (?device i2) ?i])

(defrule 2t-kcl-d (:forward)
  If [and [object-type-of ?device two-terminal-device]
	  [value-of (?device i2) ?i]]
  Then [value-of (?device current) ?i])

(define-object-type resistor
   :slots (resistance)
   :included-object-types (two-terminal-device))

(defrule ohms-law-a (:forward)
  If [and [object-type-of ?device resistor]
	  [value-of (?device current) ?current]
	  [value-of (?device resistance) ?resistance]]
  Then `[value-of (?device voltage) ,(* ?current ?resistance)])

(defrule ohms-law-b (:forward)
  If [and [object-type-of ?device resistor]
	  [value-of (?device current) ?current]
	  [value-of (?device voltage) ?voltage]]
  Then `[value-of (?device resistance) ,(/ ?voltage ?current)])

(defrule ohms-law-c (:forward)
  If [and [object-type-of ?device resistor]
	  [value-of (?device resistance) ?resistance]
	  [value-of (?device voltage) ?voltage]]
  Then `[value-of (?device current) ,(/ ?voltage ?resistance)])

(defvar *crazy-rule-result* nil)

(defrule crazy-rule (:forward)
  If [and [object-type-of ?device resistor]
	  [value-of (?device v1) ?v1]]
  Then (setq *crazy-rule-result* t))

(deftest resistor-test
  (prog1 t
	 (setq *crazy-rule-result* nil)
	 (make-object 'resistor :name 'r1)
	 (tell [value-of (r1 resistance) 10])
	 (tell [value-of (r1 v1) 0])
	 (tell [value-of (r1 v2) 5]))
  (test-n-successes 1 [value-of (r1 i2) 1/2])
  (test-n-successes 1 [value-of (r1 i1) -1/2])
  (test-n-successes 1 [value-of (r1 current) 1/2])
  (test-n-successes 1 [value-of (r1 voltage) 5])
  (test-n-successes 1 [value-of (r1 v2) 5])
  (test-n-successes 1 [value-of (r1 v1) 0])
  (test-n-successes 1 [value-of (r1 RESISTANCE) 10])
  (eql *crazy-rule-result* t))

;;;[VALUE-OF #<SLOT (R1 I2)> 1/2]
;;;[VALUE-OF #<SLOT (R1 I1)> -1/2]
;;;[VALUE-OF #<SLOT (R1 CURRENT)> 1/2]
;;;[VALUE-OF #<SLOT (R1 VOLTAGE)> 5]
;;;[VALUE-OF #<SLOT (R1 V2)> 5]
;;;[VALUE-OF #<SLOT (R1 V1)> 0]
;;;[VALUE-OF #<SLOT (R1 RESISTANCE)> 10]




(define-object-type electron
   :slots (excitation-state))

(define-object-type proton
   :slots (excitation-state))

(define-object-type hydrogen-atom
   :slots (excitation-state)
   :parts ((e electron)
	   (p proton)))

(define-object-type hydrogen-molecule
   :slots (excitation-state)
   :parts ((atom1 hydrogen-atom)
	   (atom2 hydrogen-atom)))

(defvar *molecule-test-switch-1* nil)
(defvar *molecule-test-switch-2* nil)
(defvar *molecule-test-switch-3* nil)

(defrule test-1 (:forward)
  If [and [object-type-of ?foo proton]
	  [Value-of (?foo excitation-state) ?value]]
  Then (setq *molecule-test-switch-1* t))

(defrule test-2 (:forward)
  If [and [object-type-of ?foo hydrogen-molecule]
	  [Value-of (?foo atom1 p excitation-state) ?value]]
  Then (setq *molecule-test-switch-2* t))

(defrule test-3 (:forward)
  If [and [object-type-of ?foo proton]
	  [Value-of (?foo excitation-state) high]]
  Then (setq *molecule-test-switch-3* t))

(deftest molecule-test
  (let (x y)
    (prog1 t
	   (setq *molecule-test-switch-1* nil
		 *molecule-test-switch-2* nil
		 *molecule-test-switch-3* nil)
	   (make-object 'hydrogen-molecule :name 'x)
	   (setq x (follow-path '(x))
		 y (follow-path `(x atom1 p)))
	   (tell `[value-of (,x atom1 p excitation-state) high]))
    (test-n-successes 1 `[value-of (,x atom1 p excitation-state) high])
    (test-n-successes 1 `[value-of (,y excitation-state) high])
    *molecule-test-switch-1*
    *molecule-test-switch-2*
    *molecule-test-switch-3*))



(define-object-type inverter
   :slots ((input :equalities t)
	   (output :equalities t)))

(defrule inverter-output (:backward)
  If [and [object-type-of ?inverter inverter]
	  [value-of (?inverter input) ?input]
	  (unify ?output (not ?input))
	  ]
  Then [value-of (?inverter output) ?output])

(deftest inverter-test
  (prog1 t
	 (make-object 'inverter :name 'inverter-1)
	 (tell [value-of (inverter-1 input) t]))
  (test-n-successes 1 [value-of (inverter-1 output) nil]))


(define-object-type inverter-pair
  :slots ((input :equalities t)
	  (output :equalities t))
  :parts ((stage-1 inverter)
	  (stage-2 inverter))
  :equalities (((input) (stage-1 input))
	       ((stage-1 output) (stage-2 input))
	       ((output) (stage-2 output))))

(deftest inverter-pair-test
  (prog1 t
	 (make-object 'inverter-pair :name 'ip-1)
	 (tell [value-of (ip-1 input) nil]))
  (test-n-successes 1 [value-of (ip-1 output) ?]))


;(defquestion get-inverter-input (:backward)
;	     [value-of (?inverter input) ?value]
;	     :context [object-type-of ?inverter inverter])
;
;(defun question-test ()
;  (clear)
;  (make-object 'inverter :name 'foo)
;  (ask [value-of (foo output) ?x] #'print-query-results :do-questions t)
;  (ask [value-of (foo input) ?x] #'print-query-results))
;
;(question-test)
;
;(defun advanced-question-test ()
;  (clear)
;  (make-object 'inverter-pair :name 'foo)
;  (ask `[value-of (foo output) ?x] #'print-query-results :do-questions t)
;  (ask [value-of (foo stage-2 output) ?x] #'print-query-results))
;
;(advanced-question-test)