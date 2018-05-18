;;; -*- Base: 10; Mode: LISP; Syntax: Joshua-ansi; Package: JOSHUA-USER -*-

(in-package "JU")

(define-predicate is-married-to (husband wife))
(define-predicate is-father-of (dad kid))
(define-predicate is-mother-of (mom kid))
(define-predicate is-grandfather-of (gramps kid))
(define-predicate is-grandmother-of (gramma kid))

;;; 3 ways to be a grandfather.
(define-predicate is-genetic-grandfather-of (gramps kid))
(define-predicate is-grandfather-thru-step-parent (gramps kid))
(define-predicate is-step-grandfather-of (gramps kid))

;;; 3 ways to be a grandmother.
(define-predicate is-genetic-grandmother-of (gramma kid))
(define-predicate is-grandmother-thru-step-parent (gramma kid))
(define-predicate is-step-grandmother-of (gramma kid))

(defun initial-grampaw ()
  ;; set up the song for singing
  (clear)
  (tell [and [is-married-to father    wife-1]
	     [is-married-to father    daughter] ;at a later time
	     [is-married-to me        widow]
	     [is-married-to husband-1 widow] ;at an earlier time

	     [is-father-of husband-1 daughter]
	     [is-mother-of widow     daughter]

	     [is-father-of father    me]
	     [is-mother-of wife-1    me]

	     [is-father-of me        son-1]
	     [is-mother-of widow     son-1]

	     [is-father-of father    son-2]
	     [is-mother-of daughter  son-2]]))

(defun find-own-grandparent (&optional (gender :male) (stream *standard-output*))
  ;; see if we can get Joshua to sing us this song.
  (initial-grampaw)
  (do-queries-named* silly-tag ((ignore (ecase gender
					  (:male [is-grandfather-of ?who ?who])
					  (:female [is-grandmother-of ?who ?who]))))
		     ;; print a line for each reflexive grandparent of the given gender
		     (declare (ignore ignore))
    (when stream
      (format stream "~&``~@(~A~)'' is ~:[her~;his~] own ~:*~:[grammaw~;grampaw~]!"
	      ?who (eq gender :male)))
    (return-from silly-tag ?who))) ;until the source of the looping is tamed.

;;;
;;; How to find your grandparents without really trying.
;;;

(defrule grandfather-determiner (:backward)
  ;; there are 3 ways to find your grandfather
  IF [or [is-genetic-grandfather-of ?gramps ?kid]
	 [is-grandfather-thru-step-parent ?gramps ?kid]
	 [is-step-grandfather-of ?gramps ?kid]]
  THEN [is-grandfather-of ?gramps ?kid])

(defrule grandmother-determiner (:backward)
  ;; there are 3 ways to find your grandmother
  IF [or [is-genetic-grandmother-of ?gramma ?kid]
	 [is-grandmother-thru-step-parent ?gramma ?kid]
	 [is-step-grandmother-of ?gramma ?kid]]
  THEN [is-grandmother-of ?gramma ?kid])

;;;
;;; Genetic grandparents.  Highest priority.
;;;

(defrule paternal-grandfather (:backward)
  ;; your father's father is your grandfather
  IF [and [is-father-of ?dad ?kid]
	  [is-father-of ?gramps ?dad]]
  THEN [is-genetic-grandfather-of ?gramps ?kid])

(defrule maternal-grandfather (:backward)
  ;; your mother's father is your grandfather
  IF [and [is-mother-of ?mom ?kid]
	  [is-father-of ?gramps ?mom]]
  THEN [is-genetic-grandfather-of ?gramps ?kid])

(defrule paternal-grandmother (:backward)
  ;; your father's mother is your grandmother
  IF [and [is-father-of ?dad ?kid]
	  [is-mother-of ?gramma ?dad]]
  THEN [is-genetic-grandmother-of ?gramma ?kid])

(defrule maternal-grandmother (:backward)
  ;; your mother's mother is your grandmother
  IF [and [is-mother-of ?mom ?kid]
	  [is-mother-of ?gramma ?mom]]
  THEN [is-genetic-grandmother-of ?gramma ?kid])

;;;
;;; One level away from reality.  Middle priority.
;;;

(defrule step-paternal-grandfather (:backward)
  ;; your father's wife's father is your grandfather
  IF [and [is-father-of ?dad ?kid]
	  [is-married-to ?dad ?step-mom]
	  [is-father-of ?step-gramps ?step-mom]]
  THEN [is-grandfather-thru-step-parent ?step-gramps ?kid])

(defrule step-maternal-grandfather (:backward)
  ;; your mother's husband's father is your grandfather
  IF [and [is-mother-of ?mom ?kid]
	  [is-married-to ?step-dad ?mom]
	  [is-father-of ?step-gramps ?step-dad]]
  THEN [is-grandfather-thru-step-parent ?step-gramps ?kid])

(defrule step-paternal-grandmother (:backward)
  ;; your father's wife's mother is your grandmother
  IF [and [is-father-of ?dad ?kid]
	  [is-married-to ?dad ?step-mom]
	  [is-mother-of ?step-gramma ?step-mom]]
  THEN [is-grandmother-thru-step-parent ?step-gramma ?kid])

(defrule step-maternal-grandmother (:backward)
  ;; your mother's husband's mother is your grandmother
  IF [and [is-mother-of ?mom ?kid]
	  [is-married-to ?step-dad ?mom]
	  [is-mother-of ?step-gramma ?step-dad]]
  THEN [is-grandmother-thru-step-parent ?step-gramma ?kid])

;;;
;;; Two levels away from reality.  Lowest priority.
;;;

(defrule grandfather-by-marriage (:backward)
  ;; your grandmother's husband is your grandfather
  IF [and [is-grandmother-of ?gramma ?kid]
	  [is-married-to ?gramps ?gramma]]
  THEN [is-step-grandfather-of ?gramps ?kid])

(defrule grandmother-by-marriage (:backward)
  ;; your grandfather's wife is your grandmother
  IF [and [is-grandfather-of ?gramps ?kid]
	  [is-married-to ?gramps ?gramma]]
  THEN [is-step-grandmother-of ?gramma ?kid])

(deftest grampaw-test
  ;; test the "I'm my own grampaw" example
  (eq (find-own-grandparent :male nil) 'me)
  (eq (find-own-grandparent :female nil) 'daughter))

