;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: Cl-user -*-

(in-package :cl-user)

(eval-when (:execute :load-toplevel)
  (let* ((loading-file *load-truename*)
         (host (pathname-host loading-file))
         (device (pathname-device loading-file))
         (home-dir (butlast (pathname-directory loading-file)))
         (wildcard-dir (append home-dir (list :wild-inferiors))))
    (let ((home (make-pathname :directory home-dir :host host :device device))
	  (wildcard (make-pathname :directory wildcard-dir :host host :device device)))
      (setf (logical-pathname-translations "joshua")
	    `(("home;*.*" ,home)
	      ("**;*.*" ,wildcard)
	       )))))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun find-joshua-readtable ()
    (symbol-value (intern (string-upcase "*joshua-readtable*") (find-package :joshua-internals)))
    )

  (defclass joshua-module-mixin ()
    ())

  (defmethod defsys:compile-module :around ((module joshua-module-mixin) &key)
	     (let ((*readtable* (find-joshua-readtable)))
	       (call-next-method)))
  
  (defmethod defsys:load-module :around ((module joshua-module-mixin) &key)
	     (let ((*readtable* (find-joshua-readtable)))
	       (call-next-method)))

  (defclass joshua-module (joshua-module-mixin defsys:lisp-module)
    ())

  (defclass separate-destination-joshua-module 
      (joshua-module-mixin separate-destination-module)
    ()))

(defsystem joshua
    (:pretty-name "Joshua"
     :default-module-class separate-destination-module
     :default-pathname "joshua:code;")
  (:serial
    "packaged"
    "borrowin"
    "mapfvars"
    "mapforms"
    "readnnwr"
    "preddefs"
    "predicat"
    "unificat"
    "predprot"
    "clos-heaps"
    "predimpl" 
    "rete" 
    "matcher"
    "discrimi"
    "supplied"
    "rules"
    "ltms"
    "congruence-closure"
    "objectmo"
    "cfs"
    "better-grapher"
    ))
