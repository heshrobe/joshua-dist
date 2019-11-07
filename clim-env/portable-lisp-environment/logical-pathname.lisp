;;; -*- Syntax: Ansi-Common-Lisp; Package: FUTURE-COMMON-LISP-USER; Base: 10; Mode: LISP -*-

#+MCL
(setf (logical-pathname-translations "portable-lisp-environment")
      `(("sources;*.*.*" "quincy-adams:common lisp:extensions:clim-env:portable-lisp-environment:*.*.*")
	("**;*.*.*"	"quincy-adams:common lisp:extensions:clim-env:portable-lisp-environment:**:*.*.*")))

#+(and MIT-site Genera)
(setf (logical-pathname-translations "portable-lisp-environment")
      `(("sources;*.*.*" "w:>hes>clim-env>portable-lisp-environment>*.*.*")
	("**;*.*.*" "w:>clim-env>portable-lisp-environment>**>*.*.*")))
