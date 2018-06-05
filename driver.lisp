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

(let* ((loading-file *load-truename*)
       (directory (cdr (pathname-directory loading-file))))
  (print loading-file)
  ;;; note that this one doesn't need the mcl :external-format because it's for allegro
  ;;; only
  #+allegro
   (load (make-pathname :directory `(:absolute ,@directory)
			:device (pathname-device loading-file)
			:name "system-class" :type "lisp"))
   (load (make-pathname :directory `(:absolute ,@directory "xml-parser")
			:device (pathname-device loading-file)
			:name "xml-parser-defsystem" :type "lisp"))
   (load (make-pathname :directory `(:absolute ,@directory "sample-xml-rpc-server")
			:device (pathname-device loading-file)
			:name "defsystem" :type "lisp"))
   (load (make-pathname :directory `(:absolute ,@directory "clim-fixes") 
			:device (pathname-device loading-file)
			:name "clim-fixes-defsystem" :type "lisp")
	 #+mcl :external-format #+mcl :unix)
   (load (make-pathname :directory `(:absolute ,@directory "clim-env" "portable-lisp-environment") 
			:device (pathname-device loading-file)
			:name "load-env" :type "lisp")
	 #+mcl :external-format #+mcl :unix)
   (load (make-pathname :directory `(:absolute ,@directory "joshua" "code")
			:device (pathname-device loading-file)
			:name "joshua-defsystem" :type "lisp")
	 #+mcl :external-format #+mcl :unix)
   (load (make-pathname :directory `(:absolute ,@directory "joshua" "developer")
			:device (pathname-device loading-file)
			:name "jd-defsystem" :type "lisp")
	 #+mcl :external-format #+mcl :unix)
   (load (make-pathname :directory `(:absolute ,@directory "ideal")
			:device (pathname-device loading-file)
			:name "load-ideal" :type "lisp"))
   ;; load the defsystems for the all the current apps
   (load "~/Research-Projects/natural-software/code/defsystem.lisp")
  
   (load "~/Research-Projects/new-awdrat/code/defsystem.lisp")
  
   (load "~/Research-Projects/control-system/defsystem.lisp")
  
   (load "~/Research-Projects/attack-planning/defsystem.lisp")
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
    (compile-system 'ideal :recompile recompile))
  (load-system 'ideal)
  ;; xml-parser
  (when xml-server
    (when compile
      (compile-system 'xml-parser :recompile recompile))
    (load-system 'xml-parser)
    ;; xml-rpc-server
    (when compile
      (compile-system 'sample-xml-rpc-server :recompile recompile))
    (load-system 'sample-xml-rpc-server))
  (when apps
    (when compile
      (compile-system 'natsoft :recompile recompile))
    (load-system 'natsoft)
    (when compile
      (compile-system 'awdrat :recompile recompile))
    (load-system 'awdrat)
    (when compile
      (compile-system 'controls :recompile recompile))
    (load-system 'controls)
    (when compile
      (compile-system 'aplan :recompile recompile))
    (load-system 'aplan))    
  )
