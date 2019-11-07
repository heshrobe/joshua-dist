;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

"Copyright (c) 1994-2003 Scott McKay, Howard Shrobe, John C. Mallery.  All rights reserved."

(in-package :cl-user)


;;;------------------------------------------------------------------- 
;;;
;;; PATCHES TO MCL TO MAKE CLIM-ENV WORK
;;;

(in-package :clim-utils) 

;; Old definition writes everything to a string, which breaks presentation format commands
;(defun format (stream format-control &rest format-args)
;  (when (null stream)
;    (return-from format
;      (apply #'common-lisp:format nil format-control format-args)))
;  (when (eq stream 't)
;    (setq stream *standard-output*))
;  (cond ((streamp stream)
;	 ;; this isn't going to quite work for ~&,
;	 ;; but it's better than nothing.
;	 (write-string (apply #'common-lisp:format nil format-control format-args) stream)
;	 nil)
;	(t (apply #'common-lisp:format stream format-control format-args))))

(let ((ccl:*warn-if-redefine* nil))
;; clim:utils;cl-stream-functions.lisp
(defun format (stream format-control &rest format-args)
  (declare (dynamic-extent format-arguments))
  (apply #'common-lisp:format stream format-control format-args))
)