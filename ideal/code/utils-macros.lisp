;; -*- Mode: LISP; Syntax: Common-Lisp; Package: Ideal;  Base: 10 -*-

(in-package :ideal)



;;;;********************************************************
;;;;  Copyright (c) 1989, 1992 Rockwell International -- All rights reserved.
;;;;  Rockwell International Science Center Palo Alto Lab
;;;;********************************************************


;;;;;;;;;;;;;;;;;;;;;;;; Sampath ;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(FIND-NODE FIND-LABEL PROBABILITY< FIND-NODES IDEAL-DEBUG-MSG
            GET-STATE-LABEL IDEAL-DEBUG-ACTIONS MAPAPPEND)))


;***************************** The #% macro **********************************

; Defines dispatch macro character for #\n. The functionality is :  to
; find a node of name xxx in a diagram bound to the symbol yyy you only
; have to type #n(xxx yyy). If the diagram of interest is *diagram* then
; u only have t type #n(xxx).

; Yes, Yes, I know that using symbol-value within code is a very bad
; idea, but remember that this is a top level user interface fn and
; nothing more than that.

(set-dispatch-macro-character #\# #\n
			      #'(lambda (stream subchar args)
				  (declare (ignore subchar args))
				  (let* ((input-list (read stream t nil t ))
					(name (first input-list))
					(diag (if (= (length input-list) 2)
						     (symbol-value (second input-list))
						     *diagram*)))
				    (find-node name diag))))

; Returns node of name <name> in <diagram>.


(defun find-node (name &optional (diagram *diagram*) (error t))
  (or (find name diagram :key #'node-name)
      (and error
           (error "No node of name ~A in diagram ~A" name diagram))))

(defun find-label (label-name node-name &optional (diagram *diagram*))
  (or (find label-name (state-labels (find-node node-name diagram)) :key #'label-name)
      (error "The node ~A has no label of name ~A" node-name label-name)))

(defun find-nodes (name-list diagram)
  (mapcar #'(lambda (name)(find-node name diagram)) name-list))

; Returns label of name <label-name> in the state label list of node
; <node>

(defun get-state-label (node label-name)
  (find label-name (state-labels node) :key #'label-name))

; Like mapcan. only that lists are copied before nconc.

(defun mapappend (fn &rest lists)
  (mapcan #'copy-list (apply #'mapcar (cons fn lists))))

; Type spec for probabilities.The argument is the max probability to be
; allowed.  Used by fn q&s-dist-prob in file id-creation.lisp.The
; type-spec (PROBABILITY< max) includes real numbers in the closed
; interval [0 , max]. The type-spec PROBABILITY is the set of real
; numbers [0 , 1].

(deftype probability<= (&optional (max 1))
  (let ((new-symbol (gensym)) (max-p max))
    (setf (symbol-function new-symbol)
	  #'(lambda (a)(if (not (<= 0 max-p 1))
			   (error "The probability limit ~A is not between 0 and 1." max)
			   (<= 0 a max-p))))
    `(and number (satisfies ,new-symbol))))

; Used to create the debug trace.

(defmacro ideal-debug-msg (&rest format-args)
  `(when *ideal-debug* (format t ,@format-args)))

(defmacro ideal-debug-actions (&rest body)
  `(when *ideal-debug* ,@body))
