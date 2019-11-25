;;; -*- Syntax: Ansi-common-lisp; Package: cl-USER; Base: 10; Mode: LISP -*- 

(in-package :cl-user)

(eval-when (:load-toplevel :execute)
  (let* ((loading-file *load-truename*)
         (host (pathname-host loading-file))
         (device (pathname-device loading-file))
         (loading-dir (pathname-directory loading-file))
         (wildcard-dir (append loading-dir (list :wild-inferiors))))
    (let ((home (make-pathname :directory loading-dir :host host :device device))
	  (wildcard (make-pathname :directory wildcard-dir :host host :device device)))
      (setf (logical-pathname-translations "clim-fixes")
	    `(("source;*.*" ,home)
	      ("**;*.*" ,wildcard))))))
 
#+mcl
(loop for name in '("load-system" "compile-system" "defsystem")
      for upcased = (string-upcase name)
      for clim-ds-sym = (intern upcased 'clim-ds)
      do (import clim-ds-sym 'clim)
      do (import clim-ds-sym 'cl-user)
      do (export clim-ds-sym 'clim))

#+mcl
(let ((good-string (string-upcase "boolean")))
  (unless (find-symbol good-string 'clim)
    (import (find-symbol good-string) 'clim)
    (export (find-symbol good-string) 'clim)))

#+mcl
(clim:defsystem clim-fixes
    (:default-pathname "clim-fixes:source;" :default-package :clim-internals)
    ("erase-output-record" :language :lisp-unix)
    ("extended-help" :language :lisp-unix)
    ("fix-redisplay-bug" :language :lisp-unix)
    ;; use the Allegro formatter and not this fix 
    ;; ("graph-formatting-fix")
    ("newer-cad-grapher" :language :lisp-unix)
    ;; ("dialog-boolean-bug")
    ("mcl-ie-arglist" :language :lisp-unix)
    ("input-editing-fix" :language :lisp-unix)
    ("fix-postscript" :language :lisp-unix)
    ("yank-pretty-print" :language :lisp-unix)
    ("ruled-tables" :language :lisp-unix)
    ("sequence-element-replace-input" :language :lisp-unix)
    ("patch-draw-pixmap" :language :lisp-unix)
    )

#+allegro
(defsystem clim-fixes
    (:default-pathname "clim-fixes:source;" 
	:default-package :clim-internals
	:default-module-class separate-destination-module)
  (:serial
   ;; this is a new capability akin to indenting-output
   "centering-output"
   "accept-provide-default-sequence-element"
   #+mswindows
   "allegro-background"
   "allegro-gestures"
   "erase-output-record"
   "extended-help"
   "fix-redisplay-bug"
   "layout-specs"
   ;; this fixes something that isn't broken in the allegro
   ;; sources but the patch (based on MCL sources) breaks
   ;; the grapher
   ;; "graph-formatting-fix"
   "highlighted-presentation-1"
   "newer-cad-grapher"
   "input-editing-fix"
   "patch-clim-franz"
   ;; The ACL implementation of clim:postscript; is different from the MCL/Genera
   ;; versions and doesn't seem to be as buggy.
   ;; "fix-postscript"
   "patch-read-token-franz"
   "yank-print-pretty"
   "ruled-tables"
   "sequence-element-replace-input"
   "complete-logical-pathnames"
   #+(or macosx unix)
   "patch-draw-pixmap"
   #+(or macosx unix)
   "patch-tk-silica-image"
   #+(or macosx unix)
   "pixmap-from-file"
   "drag-and-drop-expand-ptype"
   "fix-presentation-highlighting-and-documentation"
   "fix-options-on-translators"
   "fix-filling-output"
   "fix-tab-as-complete"
   ))

#+genera
(sct:defsystem clim-fixes
    (:default-pathname "clim-fixes:code;" :default-package :clim-internals) 
  (:serial
    "erase-output-record"
    "extended-help"
    "fix-redisplay-bug"
    "graph-formatting-fix"
    "newer-cad-grapher"
    "fix-postscript"
    "ruled-tables"
    "sequence-element-replace-input"
    "patch-draw-pixmap"
    ))
