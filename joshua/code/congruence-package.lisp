;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: cl-user -*-

(defpackage congruence-closure
  (:nicknames "CC")
  (:use :joshua :joshua-internals :common-lisp)
  ;; This is for the object-modelling Stuff
  (:shadow common-lisp:find common-lisp:union common-lisp:intern)
  (:export 
   "MERGE" "CONGRUENT" "FIND" "EQUATE" "UNION"
   "INIT" "INTERN-TERM" "INTERN" "INTERN-SYMBOL"
   "ANONYMOUS-INDIVIDUAL" "TERM-SURROGATE" "CONSTANT"
   ))