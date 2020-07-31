;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Reason: Remove function CLIM-INTERNALS::FRAME-DOCUMENT-HIGHLIGHTED-PRESENTATION-1: change from function to method
;;; Function (CLOS:METHOD CLIM-INTERNALS::FRAME-DOCUMENT-HIGHLIGHTED-PRESENTATION-1 (CLIM:STANDARD-APPLICATION-FRAME T T T T T T)):  changed to method
;;; Written by HES, 10/09/01 09:22:11

(in-package :clim-internals)

(Fmakunbound 'FRAME-DOCUMENT-HIGHLIGHTED-PRESENTATION-1)
;;; apparently this is redone in patch-clim-franz

;;; "-*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-"
#|
(defmethod frame-document-highlighted-presentation-1
       ((frame standard-application-frame) presentation input-context window x y stream)
  (let ((modifier-state (window-modifier-state window)))
    (declare (type fixnum modifier-state))
    (multiple-value-bind (left   left-presentation   left-context
			  middle middle-presentation middle-context
			  right  right-presentation  right-context)
	(find-applicable-translators-for-documentation presentation input-context
						       frame window x y modifier-state)
      (let* ((*print-length* 3)
	     (*print-level* 2)
	     (*print-circle* nil)
	     (*print-array* nil)
	     (*print-readably* nil)
	     (*print-pretty* nil))
	(flet ((document-translator (translator presentation context-type
				     button-name separator)
		 ;; Assumes 5 modifier keys and the reverse ordering of *MODIFIER-KEYS*
		 (let ((bit #o20)
		       (shift-name '("h-" "s-" "m-" "c-" "sh-")))
		   (declare (type fixnum bit))
		   (repeat 5			;length of shift-name
		     (unless (zerop (logand bit modifier-state))
		       (write-string (car shift-name) stream))
		     (pop shift-name)
		     (setq bit (the fixnum (ash bit -1)))))
		 (write-string button-name stream)
		 (document-presentation-translator translator presentation context-type
						   frame nil window x y
						   :stream stream
						   :documentation-type :pointer)
		 (write-string separator stream)))
	  (declare (dynamic-extent #'document-translator))
	  ;;--- The button names should be hard-wired in.  Consider 1-button
	  ;;--- Macs and 2-button PCs...
	  (when left
	    (let ((button-name (cond ((and (eq left middle)
					   (eq left right))
				      (setq middle nil
					    right nil)
				      "L,M,R: ")
				     ((eq left middle) 
				      (setq middle nil)
				      "L,M: ")
				     (t "L: "))))
	      (document-translator left left-presentation left-context
				   button-name (if (or middle right) "; " "."))))
	  (when middle
	    (let ((button-name (cond ((eq middle right)
				      (setq right nil)
				      "M,R: ")
				     (t "M: "))))
	      (document-translator middle middle-presentation middle-context
				   button-name (if right "; " "."))))
	  (when right
	    (document-translator right right-presentation right-context
				 "R: " "."))
	  ;; Return non-NIL if any pointer documentation was produced
	  (or left middle right))))))

|#