;;; -*- Mode: Common-lisp; Package: common-lisp-user -*-

(in-package :common-lisp-user)

;;;(eval-when (:compile-toplevel :execute :load-toplevel)
;;;  (proclaim '(optimize (debug 3) (safety 3))))

#+mcl(require :appleevent-toolkit)

#+(and allegro macosx) (require :climxm)
#+(and allegro linux (not smp)) (require :climxm)
#+allegro (require :asdf)
#+(and allegro microsoft) (require :climnt)

(load "~/quicklisp/setup.lisp")

(ql:quickload :cl-json)

(defparameter *missing-systems* nil)
(defparameter *existing-systems* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro load-if-there (name pathname-spec)
  `(let ((pathname ,pathname-spec))
     (cond
      ((probe-file pathname)
       (load pathname)
       (pushnew ',name *existing-systems*)
       (format t "~%~a loaded" ',name))
      (t
       (format t "~%~a Not Found" ',name)
       (pushnew ',name *missing-systems*)))))

(defmacro if-there (name &body forms)
  `(unless (member ',name *missing-systems*)
    ,@forms)))


(let* ((loading-file *load-truename*)
       (directory (cdr (pathname-directory loading-file))))
  (print loading-file)
  ;;; note that this one doesn't need the mcl :external-format because it's for allegro
  ;;; only
  #+allegro
  ;; This provides a class for the Allegro version of defsystem that allows me to control where the binaries
  ;; go
  (load (make-pathname :directory `(:absolute ,@directory)
			:device (pathname-device loading-file)
			:name "system-class" :type "lisp"))
  ;; XML Parser
  (load-if-there Xml-parser
		 (make-pathname :directory `(:absolute ,@directory "xml-parser")
				:device (pathname-device loading-file)
				:name "xml-parser-defsystem" :type "lisp"))
  ;; XML Server for XML-RPC
  (load-if-there Xml-Server
		 (make-pathname :directory `(:absolute ,@directory "sample-xml-rpc-server")
							 :device (pathname-device loading-file)
							 :name "defsystem" :type "lisp"))
  ;; Clim Fixes
   (load (make-pathname :directory `(:absolute ,@directory "clim-fixes")
			:device (pathname-device loading-file)
			:name "clim-fixes-defsystem" :type "lisp"))
   ;; Clim Env
   (load (make-pathname :directory `(:absolute ,@directory "clim-env" "portable-lisp-environment")
			:device (pathname-device loading-file)
			:name "load-env" :type "lisp"))
   ;; Joshua
   (load (make-pathname :directory `(:absolute ,@directory "joshua" "code")
			:device (pathname-device loading-file)
			:name "joshua-defsystem" :type "lisp"))
   ;; Joshua Developer Tools
   (load (make-pathname :directory `(:absolute ,@directory "joshua" "developer")
			:device (pathname-device loading-file)
			:name "jd-defsystem" :type "lisp"))
   ;; Ideal Bayesian Inference Sysetm
   (load-if-there ideal
		  (make-pathname :directory `(:absolute ,@directory "ideal")
				 :device (pathname-device loading-file)
				 :name "load-ideal" :type "lisp"))
   ;; load the defsystems for the all the current apps
   ;; Programmer's Apprentice
   (load-if-there Natsoft  "~/Research-Projects/natural-software/code/defsystem.lisp")
  ;; AWDRAT
   (load-if-there AWDRAT  "~/Research-Projects/awdrat/code/defsystem.lisp")
  ;; Control System Demo
   (load-if-there Control-System "~/Research-Projects/control-system/defsystem.lisp")
  ;; Attack Planner
   (load-if-there Attack-Planner "~/Research-Projects/attack-planning/code/defsystem.lisp")
   )


#+Allegro
(eval-when (:compile-toplevel :load-toplevel)
  (setq excl:*additional-logical-pathname-name-chars* '(#\_ #\*)))


(defun build-it (&key compile recompile xml-server apps)
  ;; clim-fixes
  (when compile
    (compile-system 'clim-fixes :recompile recompile))
  (load-system 'clim-fixes)
  ;; clim-environment
  (load-clim-env :compile compile :recompile recompile)
  ;; joshua
  (when compile
    (compile-system 'joshua :recompile recompile))
  (load-system 'joshua)
  ;; joshua-developer
  (when compile
    (compile-system 'joshua-developer :recompile recompile))
  (load-system 'joshua-developer)
  ;; ideal
  (when compile
    (if-there ideal
     (compile-system 'ideal :recompile recompile)))
  (load-system 'ideal)
  ;; xml-parser
  (if-there xml-parser
	      (when compile
		(compile-system 'xml-parser :recompile recompile))
	      (load-system 'xml-parser))
  ;; xml-server
  (when xml-server
    (if-there xml-server
	      (when compile
		(compile-system 'sample-xml-rpc-server :recompile recompile))
	      (load-system 'sample-xml-rpc-server)))
  (when apps
    (if-there natsoft
	      (when compile
		(compile-system 'natsoft :recompile recompile))
	      (load-system 'natsoft))
    (if-there awdrat
	      (when compile
		(compile-system 'awdrat :recompile recompile))
	      (load-system 'awdrat))
    (if-there control-system
	      (when compile
		(compile-system 'controls :recompile recompile))
	      (load-system 'controls))
    (if-there attack-planner
	      (when compile
		(compile-system 'aplan :recompile recompile))
	      (load-system 'aplan)))
  )
