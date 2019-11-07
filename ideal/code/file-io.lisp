;; -*- Mode: LISP; Syntax: Common-Lisp; Package: Ideal;  Base: 10 -*-

(in-package :ideal)

;;;;********************************************************
;;;;  Copyright (c) 1989, 1992 Rockwell International -- All rights reserved.
;;;;  Rockwell International Science Center Palo Alto Lab
;;;;********************************************************


;;;;;;;;;;;;;;;;;;;;;;;; Sampath ;;;;;;;;;;;;;;;;;;;;


(export '(SAVE-DIAGRAM
	  OLD-FORMAT-DIAGRAM-P
	  LOAD-DIAGRAM
	  PLAIN-PRINT-ENV))

;--------------------------------------------------------

; New save and load diagrams

(defun save-diagram (filename &optional (diagram *diagram*) clique-diagram
		     (default-path-name *default-diag-path-name*))
  (with-open-file (st (merge-pathnames filename default-path-name)
		      :direction :output :if-does-not-exist :create
		      :if-exists #+:LISPM :new-version #-:LISPM :overwrite)
    (print-diag diagram clique-diagram st))
  (values diagram clique-diagram))

(defun print-diag (diagram clique-diagram &optional (st t))
  (multiple-value-bind (new-diag new-clique-diag)
      (copy-for-saving diagram clique-diagram)
    (let ((*print-array* t)
	  (*print-pretty* t)
	  (*print-circle* nil)
	  (*print-length* nil)		;this ensures ALL elements of lists
					;will be printed - rpg
	  (*default-ideal-structure-printing* t)
	  (*package* (find-package cl-user::*ideal-package-name-string*)))
      (dolist (node new-diag)
	(format st "~%;Node==> ~A Labels==>  ~{~%;~A~}~%"
		(list (node-name node) (node-id-number node))
		(mapcar #'(lambda (l)(list (label-name l)(label-id-number l)))
			(state-labels node))))
      (format st "~%;******************* DIAGRAM FORMAT TYPE (Version number)*******")
      (print *current-diagram-format* st)
      (format st "~%; ****************** DIAGRAM FOLLOWS *************************")
      (print new-diag st)
      (format st "~%; ******************* CLIQUE DIAGRAM FOLLOWS ****************")
      (print new-clique-diag st)))
  (values))

(defun load-diagram (filename &optional (default-path-name *default-diag-path-name*))
  (with-open-file (st  (merge-pathnames filename default-path-name) :direction :input)
    (let* ((*package* (find-package cl-user::*ideal-package-name-string*))
	   (format-type (read st))(diagram (read st nil :END))
	   (clique-diagram (read st nil :END)))
      (cond
	((not (current-diagram-format-p format-type))
	 (error "The diagram file is not in the current format. ~
                   Convert it using CONVERT-DIAGRAM-FORMAT before using."))
	(t (copy-after-loading diagram clique-diagram))))))

(defun current-diagram-format-p (format-type)
  (eq format-type *current-diagram-format*))

; Just to check diagrams. To make sure the cross pointers are as they should be
; Seting *print-circle* to t ensures that u can see such stuff on the screen.

(defmacro plain-print-env (form)
  `(let ((*print-circle* t)(*default-ideal-structure-printing* t))
    ,form))

;-------------------------------------------------------------------------------

; Converting old format diagrams into new format.

; In earlier versions of IDEAL distribution arrays and other arrays were
; multidimensional.  Now they are uni-dimensional. This fn converts a
; diagram in the old format to the new format. This stuff will be
; eliminates after some reasonable gap of time.

; Takes each file(s) that match pathname. If the file is in the old
; format the fn converts and resaves the file in the new format. If not
; does nothing.

; The old format diagram files can't be identified in any way except
; that they do not have the version keyword at the head of the file.
; Therefore the (listp diagram) hack.

(defun convert-diagram-format (pathname)
  (dolist (filename (directory (merge-pathnames pathname (make-pathname :version :newest))))
    (when (equal (pathname-type filename) "DIAG")
      (with-open-file (st filename :direction :input)
	(let ((diagram (read st)))
	  (cond
	    ((listp diagram)		
	     (let ((*old-format-diagram* t))
	       (save-diagram filename (convert-diagram (copy-after-loading diagram nil)) nil)
	       (format t "~%;Finished converting file ~A." filename)))
	    (t (warn "The file ~A is already in the new format. Nothing needs to be done"
		     filename)))))))
  (format t "~% ... Done.")
  (values))

(defun convert-diagram (diagram)
  (mapc #'convert-node diagram))

(defun convert-node (node)
  (when (not (decision-node-p node))
    (let ((old-array (distribution-repn node)))
     (create-empty-distribution node)
      (for-all-cond-cases (pred-case (node-predecessors node))
	(for-all-cond-cases (node-case node)
	  (setf (contents-of node-case pred-case)
		(get-contents-from-old-array old-array node-case pred-case )))))))

(defun get-contents-from-old-array (array node-case pred-case)
  (apply #'aref (cons array
		      (old-get-key-for-cond-case
			pred-case
			(relation-type (node-in node-case))
			(state-in node-case)))))

(defun old-get-key-for-cond-case (cond-case relation-type state)
  (let ((first-subscript (case relation-type
			   (:PROB (label-id-number state))
			   ((:DET nil)  *lowest-label-id-number*))))
    (cons first-subscript
	  (mapcar #'(lambda (node.state)(label-id-number (cdr node.state)))
		  (sort (copy-list cond-case)
			#'< :key #'(lambda (n.s)(node-id-number (car n.s))))))))
