;; -*- mode: common-lisp; package: excl -*-
;;
;; case.cl
;; case selection for reading and printing
;;
;; copyright (c) 1985 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2002 Franz Inc, Berkeley, CA  - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;

;; $Id: case.cl,v 1.52.14.3.6.1 2002/06/03 15:49:55 layer Exp $

;; Description:
;;


(in-package :excl)

;; -*- mode: common-lisp; package: excl -*-
;;
;; case.cl
;; case selection for reading and printing
;;
;; copyright (c) 1985 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2002 Franz Inc, Berkeley, CA  - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;

;; $Id: case.cl,v 1.52.14.3.6.1 2002/06/03 15:49:55 layer Exp $

;; Description:
;;


(in-package :excl)


(defun cvt-symbol-case (to-case-cvt pure-case-cvt
			&aux not-converted name-conflict pkg-result)
  ;; convert all of the symbols from one case to another.
  ;; don't convert symbol pnames which have two cases.
  ;; to-case-cvt is a function to convert from the old case to the new case.

  (without-package-locks

   ;; convert packages
   (setq pkg-result (all-packages-case-convert to-case-cvt))

   ;; now symbols
   (do-all-symbols (sym)
     (block symbol-scan
       (let ((name (symbol-name sym))
	     seen-lc seen-uc)
	 (declare (optimize speed))
	 (if* (not *convert-mixed-case-symbols*)
	    then ;; check for a mixed case symbol
		 (dotimes (i (length name))
		   (declare (fixnum i))
		   (let ((ch (schar name i)))
		     (if* (lower-case-p ch)
			then (if* seen-uc
				then (push sym not-converted)
				     (return-from symbol-scan))
			     (setq seen-lc t)
		      elseif (upper-case-p ch)
			then (if* seen-lc
				then (push sym not-converted)
				     (return-from symbol-scan))
			     (setq seen-uc t)))))
	 ;; name is all one case or we don't care if it is mixed case
	 (let ((purep (excl::.primcall 'sys::purep name)))
	   (let ((newstring
		  (funcall (the known-function
			     (if purep pure-case-cvt to-case-cvt))
			   name)))
	     (when (and purep (eql newstring name))
	       (setq newstring (funcall to-case-cvt name)))
	     (multiple-value-bind (newsym flag)
		 (find-symbol newstring (symbol-package sym))
	       (if* (and flag (franz::memq flag '(:external :internal))
			 (not (eq sym newsym)))
		  then (push sym name-conflict)
		  else ;; do the conversion
		       (if* purep
			  then (.inv-symbol-name sym newstring)
			  else (replace name newstring))
		       (if* (eq flag :inherited)
			  then (shadow sym (symbol-package sym)))))))))
     (reset-symbol-print-flags sym))

   (dolist (fcn *symbol-case-convert-hook*) (funcall fcn))

   (list pkg-result not-converted name-conflict)))
