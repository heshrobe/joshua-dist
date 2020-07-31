;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: Ideal;  Base: 10 -*-

(in-package :ideal)



;;;;********************************************************
;;;;  Copyright (c) 1989, 1992 Rockwell International -- All rights reserved.
;;;;  Rockwell International Science Center Palo Alto Lab
;;;;********************************************************


;;;;;;;;;;;;;;;;;;;;;;;; Sampath ;;;;;;;;;;;;;;;;;;;;


(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(DISPLAY-DIST
            IDEAL-WARNING
            DIAG-DISPLAY-DIST
            IDEAL-ACTION
            PROB-STRING-OF-COND-CASE
            DIAG-DISPLAY-BEL
            C-CASE-STRING
            DIAG-DISPLAY-LINKS)))

;----------------------------------------

; An input function with type checking

(defun query (type-spec &rest format-args)
  (loop
    (terpri)
    (apply #'format (cons *query-io* format-args)) (format *query-io* "  =>")
    (let ((input (read *query-io*)))
      (cond
	((typep input type-spec) (return input))
	( t (warn "Input ~S is not of type ~S. Try again." input type-spec))))))


(defun ideal-warning (&rest args)
  (format t "~%IDEAL SYSTEM:~%")
  (apply #'warn args))


; Gives error or warning depending on mode argument

(defun ideal-action (mode &rest args)
  (apply (ecase mode (:ERROR #'error)(:WARN #'ideal-warning)) args))

; Some Display Functions

(defun DISPLAY-DIST (node)
  (format t "~%_______________________ Node : ~A _____________________________________"
	  (node-name node))
  (when (noisy-or-node-p node)(display-noisy-or-info node))
  (cond
    ((eq (relation-type node) :prob)		;test for reln type, not node type [mpw]
     (for-all-cond-cases (cc (node-predecessors node))
       (terpri)
       (for-all-cond-cases (nc (list node))
	 (format t (format nil "~~%~~A=> ~~~DF" *display-precision*)        ;;;~6,4,,,F"
		 (prob-string nc cc)
		 (contents-of nc cc)))))
    ((and (decision-node-p node)(null (distribution-repn node)))
     (ideal-warning "The decision node ~A has not been evaluated yet; it has no policy." node))
    (t (format t "~%PREDS: ~{~20A~} "		;take column headings out of loop [mpw]
	       (mapcar #'node-name (node-predecessors node)))
       (for-all-cond-cases (cc (node-predecessors node))
	  (format t "~&       ~{~20A~} =>"
		  (mapcar #'(lambda (a)(label-name (cdr a))) cc))
	  (let ((val (deterministic-state-of node cc)))
	    (cond ((numberp val) (format t "~10,2,,,F ~%" val))
		  ((and (listp val)
			(numberp (car val))
			(typep (cdr val ) 'label))
		   (format t " ~A (EV = ~6,2,,,F)" (label-name (cdr val)) (car val)))
		  ((typep val 'label)		;for deterministic chance nodes [mpw]
		   (format t " ~A" (label-name val)))
		  (T (format t "~A" val)))))))
  (values))

(defun display-noisy-or-info (node)
  (format t "~%This is a ~A noisy or node." (noisy-or-subtype node))
  (format t "~%--- Inhibitor probs ---")
  (cond
    ((null (node-predecessors node))
     (format t "This node has no predecessors."))
    (t (dolist (p (node-predecessors node))
	 (display-predecessors-inhibitor-probs node p))))
  (ecase (noisy-or-subtype node)
	; No need to display the function in the binary case
    ((:BINARY) nil)
    ((:NARY :GENERIC)(display-noisy-or-det-fn node))))

(defun display-predecessors-inhibitor-probs (node pred)
  (format t "~% Predecessor: ~A" (node-name pred))
  (format t "~%---------------------------------")
  (ecase (noisy-or-subtype node)
    ((:NARY :GENERIC)
     (for-all-cond-cases (pred-case pred)
       (format t "~% ~A => ~A" (label-name (state-in pred-case))
               (inhibitor-prob-of node pred-case))))
    ((:BINARY)
     (for-all-cond-cases (pred-case pred)
       (cond
         ((noisy-or-false-case-p pred-case)
          (format t "~% False Label: ~A  Inhibitor Prob: ~A"
                  (label-name (state-in pred-case))
                  (inhibitor-prob-of node pred-case)))
         ((not (zerop (inhibitor-prob-of node pred-case)))
          (format t "~%       Label: ~A  Inhibitor Prob: ~A. * Warning: Non-zero inhibitor ~
                               prob for non-false state of predecessor in binary Noisy or node"
                  (label-name (state-in pred-case))
                  (inhibitor-prob-of node pred-case))))))))

(defun display-noisy-or-det-fn (node)
  (let ((fmt " ~5@A"))
    (labels ((disp (node-list tab-level cond-case)
	;(format t "~%JJJ :~A ~A" (mapcar #'node-name node-list) tab-level)
	       (cond
		 ((null node-list)
		  (format t " => ~A" (label-name (noisy-or-det-fn-of node cond-case))))
		 (t (for-all-cond-cases (pred-case (first node-list))
		      (format t fmt (label-name (state-in pred-case)))
		      (disp (rest node-list) (+ tab-level 1)
			    (combine-cond-cases pred-case cond-case))
		      (format t "~%")

		      (tabs tab-level)))))
	     (tabs (tab-level)
	       (dotimes (x tab-level)
		 (format t fmt ""))))
      (format t "~%----------- Deterministic function -----------------------~%")
      (dolist (p (node-predecessors node))
	(format t fmt (node-name p)))
      (format t "~%")
      ; Underline
      (dolist (p (node-predecessors node))
	(declare (ignore p))
	(format t fmt "-----"))
      (format t "~%")
      (disp (node-predecessors node) 0 nil)
      ; Bottom line
      (dolist (p (node-predecessors node))
	(declare (ignore p))
	(format t fmt "-----")))))

(defun PROB-STRING (nc cc)
  (format nil "P[~A=~A|~{~A~}]"
	  (if (typep nc 'node) (node-name nc) (node-name (node-in nc)))
	  (if (typep nc 'node) ""
	      (if (state-in nc) (label-name (state-in nc)) "") )
	  (mapcar #'(lambda (a)(format nil "~A=~A "
				       (node-name (car a))
				       (label-name (cdr a))))
                  cc)))

(defun PROB-STRING-OF-COND-CASE (cond-case)
  (format nil "~{~A~}" (mapcar #'(lambda (n.s)
				   (format nil " ~5@A=~5@A" (node-name (car n.s))
					   (if  (cdr n.s)(label-name (cdr n.s)))))
			       cond-case)))

(defun VALUE-STRING (node cc)
  (format nil "Value ~A [~{~A~}]"
	  (node-name node)
	  (mapcar #'(lambda (a)(format nil "~A=~A "
				       (node-name (car a))
				       (label-name (cdr a))))
                  cc)))

(defun DIAG-DISPLAY-LINKS (&optional (diagram *diagram*))
  (dolist (n diagram)
    (format t "~%~A Preds ~A" (node-name n) (mapcar #'node-name (node-predecessors n)))))

(defun DIAG-DISPLAY-DIST (&optional (diagram *diagram*))
  (dolist (n diagram)
    (display-dist n))
  (values diagram))

(defun DIAG-DISPLAY-BEL (&optional (diagram *diagram*))
  (dolist (n diagram)
    (display-bel n)))

(defun DISPLAY-BEL (node)
  (format T "~%~A BEL ~{~A ~}" (node-name node)
	  (mapcar #'(lambda(z)
		      (format nil "~A => ~6,4,,,F"
			      (label-name (car z)) (cdr z)))
		  (node-bel node))))
; Added 31 Oct.

(defun node-names (node-list)
  (mapcar #'node-name node-list))


;; Temporary (13 feb)


(defun move-tab (tab-stops &optional (tab-width 12))
  (terpri)
  (dotimes (k (* (- tab-stops 1) tab-width))
    (princ " ")))

(defvar *my-print* nil)

(defmacro my-print (&rest format-args)
  `(when *my-print* (format t ,@format-args)))

(defmacro my-actions (&rest body)
  `(when *my-print* ,@body))
