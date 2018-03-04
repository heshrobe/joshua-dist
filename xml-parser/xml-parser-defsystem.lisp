;;; -*- Mode: Common-Lisp; Package: COMMON-LISP-USER -*-

(in-package :common-lisp-user)

(eval-when (:execute :load-toplevel)
  (let* ((loading-file *load-truename*)
	 (host (pathname-host loading-file))
	 (device (pathname-device loading-file))
	 (home-dir (pathname-directory loading-file))
	 (wild-dir (append home-dir (list :wild-inferiors))))
    (let ((home-directory (make-pathname :directory home-dir 
				   :host host 
				   :device device))
	  (wild-directory (make-pathname :directory wild-dir
					 :host host 
					 :device device
					 :type :wild
					 :name :wild
					 :version :unspecific)))
      (setf (logical-pathname-translations "xml-parser")
	`(("home;*.*"	,home-directory)
	  ("**;*.*" ,wild-directory
		    ))))))

(defsystem xml-parser
    (:default-pathname "xml-parser:home;"
	:default-module-class separate-destination-module)
  (:serial
   ("nox-package")
   ("rdf-constants")
   ("xml-util")
   ("xml-parser")))