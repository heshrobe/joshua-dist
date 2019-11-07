;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

;;; Copyright (c) 1994-2000, Scott McKay.
;;; Copyright (c) 2001-2003, Scott McKay and Howard Shrobe.
;;; All rights reserved.  No warranty is expressed or implied.
;;; See COPYRIGHT for full copyright and terms of use.

(in-package #-ansi-90 :user #+ansi-90 :common-lisp-user)

;;; CLIM Environment system declaration

(eval-when (:load-toplevel :execute)
   (let* ((loading-file *load-truename*)
             (host (pathname-host loading-file))
             (device (pathname-device loading-file))
             (loading-dir (pathname-directory loading-file))
	     (wild-sub-dir (append loading-dir (list :wild-inferiors))))
      (let ((home (make-pathname :directory loading-dir :host host :device device))
	      (wild (make-pathname :directory wild-sub-dir :host host :device device)))
         (setf (logical-pathname-translations "portable-lisp-environment")
                  `(("source;*.*" ,home)
	             ("**;*.*"     ,wild)
                     )))))

#+(or MCL Lispworks)
(clim-defsys:defsystem 
  clim-environment
  (:default-pathname "PORTABLE-LISP-ENVIRONMENT:source;"
   :default-binary-pathname (format NIL "PORTABLE-LISP-ENVIRONMENT:~A-binaries;"
				    #+MCL "MCL"
				    #+Allegro "ALLEGRO"
				    #+Lispworks "LISPWORKS"))
  ("pkgdcl" #+mcl :language #+mcl :lisp-unix)
  ;; Patches to the various Lisp systems...
  #+Lispworks ("lispworks-patches")
  #+Allegro ("allegro-patches")
  ;; Various utilities
  ("defs"               :load-before-compile ("pkgdcl") #+mcl :language #+mcl :lisp-unix)
  ("macros"             :load-before-compile ("defs") #+mcl :language #+mcl :lisp-unix)
  ("utilities"          :load-before-compile ("pkgdcl" "defs" "macros") #+mcl :language #+mcl :lisp-unix)
  ("heap"               :load-before-compile ("macros") #+mcl :language #+mcl :lisp-unix)
  ("source-compare" :load-before-compile ("macros") #+mcl :language #+mcl :lisp-unix)
  ("text-scroll"        :load-before-compile ("macros") #+mcl :language #+mcl :lisp-unix)
  ("time"      :load-before-compile ("macros") #+mcl :language #+mcl :lisp-unix)
  ("types"              :load-before-compile ("macros" #-Genera "time") #+mcl :language #+mcl :lisp-unix)
  ("selections"		:load-before-compile ("macros") #+mcl :language #+mcl :lisp-unix)
  ("tool-bar"		:load-before-compile ("macros") #+mcl :language #+mcl :lisp-unix)
  ;; The various "free-standing" commands
  ("comtabs"            :load-before-compile ("macros" "selections") #+mcl :language #+mcl :lisp-unix)
  ("activity-commands"  :load-before-compile ("macros" "types" "comtabs") #+mcl :language #+mcl :lisp-unix)
  ("communication-commands" :load-before-compile ("macros" "types" "comtabs") #+mcl :language #+mcl :lisp-unix)
  ("documentation-commands" :load-before-compile ("macros" "types" "comtabs") #+mcl :language #+mcl :lisp-unix)
  ("editing-commands"   :load-before-compile ("macros" "types" "comtabs") #+mcl :language #+mcl :lisp-unix)
  ("file-commands"      :load-before-compile ("macros" "types" "comtabs") #+mcl :language #+mcl :lisp-unix)
  ("printer-commands"   :load-before-compile ("macros" "types" "comtabs") #+mcl :language #+mcl :lisp-unix)
  ("debugging-commands" :load-before-compile ("macros" "types" "comtabs") #+mcl :language #+mcl :lisp-unix)
  ("lisp-commands"      :load-before-compile ("macros" "types" "comtabs") #+mcl :language #+mcl :lisp-unix)
  ("system-commands"    :load-before-compile ("macros" "types" "comtabs") #+mcl :language #+mcl :lisp-unix)
  ("comtab-commands"    :load-before-compile ("macros" "types" "comtabs") #+mcl :language #+mcl :lisp-unix)
  ("session-commands"   :load-before-compile ("macros" "types" "comtabs") #+mcl :language #+mcl :lisp-unix)
  ("tape-commands"      :load-before-compile ("macros" "types" "comtabs") #+mcl :language #+mcl :lisp-unix)
  ("utility-commands"   :load-before-compile ("macros" "types" "comtabs") #+mcl :language #+mcl :lisp-unix)
  ;; The various tools
  ("listener" :load-before-compile ("selections"
                                                      "activity-commands"
                                                      "communication-commands"
                                                      "documentation-commands"
                                                      "editing-commands"
                                                      "file-commands"
                                                      "printer-commands" 
                                                      "debugging-commands"
                                                      "lisp-commands"
                                                      "system-commands"
                                                      "session-commands"
                                                      "tape-commands"
                                                      "utility-commands") #+mcl :language #+mcl :lisp-unix)
  ("debugger"           :load-before-compile ("listener" "text-scroll") #+mcl :language #+mcl :lisp-unix)
  ("inspector"          :load-before-compile ("text-scroll") #+mcl :language #+mcl :lisp-unix)
  ("browser"            :load-before-compile ("text-scroll") #+mcl :language #+mcl :lisp-unix)
  ("clos-browser"       :load-before-compile ("browser") #+mcl :language #+mcl :lisp-unix)
  ("function-browser"   :load-before-compile ("browser") #+mcl :language #+mcl :lisp-unix)
  ("package-browser"    :load-before-compile ("browser") #+mcl :language #+mcl :lisp-unix)
  ("fsedit"             :load-before-compile ("listener" "text-scroll") #+mcl :language #+mcl :lisp-unix)
  ("peek"               :load-before-compile ("listener" "text-scroll") #+mcl :language #+mcl :lisp-unix)
  #+Lispworks ("unix-tools")
  ("navigator"	        :load-before-compile ("listener"
                                                                  "debugger"
                                                                  "inspector"
                                                                  "clos-browser"
                                                                  "function-browser"
                                                                  "package-browser"
                                                                  "fsedit"
                                                                  "peek") #+mcl :language #+mcl :lisp-unix))

#+Lispworks
(clim-defsys:defsystem clim-dylan-environment
    (:default-pathname (frob-pathname "env")
     :default-binary-pathname (frob-pathname "env"))
  ("dylan-patches")
  ("dylan-types")
  ("dylan-libs")
  ("dylan-listener")
  ("dylan-debugger")
  ("dylan-browsers")
  ("dylan-env")
  #+++ignore ("dylan-editor"))



#+(and Genera ignore)
(clim-defsys:import-into-sct 'clim-environment
  :pretty-name "CLIM Environment"
  :default-pathname "local:>swm>clim>env>"
  :required-systems '(clim))

#+Genera
(sct:defsystem clim-environment
    (:pretty-name "CLIM Environment"
     :default-pathname "portable-lisp-environment:source;"
     :default-destination-pathname "portable-lisp-environment:genera;"
     :journal-directory "portable-lisp-environment:genera;patch;"
     :required-systems (clim))
  (:serial
    "pkgdcl"
    ;; Various utilities
    (:parallel "defs" "macros")
    (:parallel "utilities" "heap")
    "text-scroll"
    "types"
    "selections"
    ;; The various "free-standing" commands
    "tool-bar"
    "comtabs"
    (:parallel
      "activity-commands"
      "communication-commands"
      "documentation-commands"
      "editing-commands"
      "file-commands"
      "printer-commands"
      "debugging-commands"
      "lisp-commands"
      "system-commands"
      "comtab-commands"
      "session-commands"
      "tape-commands"
      "utility-commands")
    ;; The various tools
    "listener"
    (:parallel
      "debugger"
      "inspector"
      "browser"
      "clos-browser"
      "function-browser" 
      "package-browser")
    (:parallel
      "fsedit" 
      "peek")
    #+ignore "unix-tools"
    "navigator"))

;;; this assumes you've loaded the aisl-clos sysdcl which defines the md module-type 

#+allegro
(defsystem clim-environment
    (:pretty-name "CLIM Environment"
     :default-module-class separate-destination-module 
     :default-pathname "portable-lisp-environment:source;")
  (:serial
    "pkgdcl"
    "allegro-patches"
    ;; Various utilities
    (:parallel "defs" "macros")
    (:parallel "utilities" "heap" "source-compare")
    "text-scroll"
    "time"
    "types"
    "selections"
    ;; The various "free-standing" commands
    "tool-bar"
    "comtabs"
    (:parallel
      "activity-commands"
      "communication-commands"
      "documentation-commands"
      "editing-commands"
      "file-commands"
      "printer-commands"
      "debugging-commands"
      "lisp-commands"
      "system-commands"
      "comtab-commands"
      "session-commands"
      "tape-commands"
      "utility-commands")
    ;; The various tools
    "listener"
    (:parallel
      "debugger"
      "inspector"
      "browser"
      "clos-browser"
      "function-browser" 
      "package-browser")
    (:parallel
      "fsedit" 
      "peek")
    "unix-tools"
    "navigator"))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (pushnew :clim-env *features*))