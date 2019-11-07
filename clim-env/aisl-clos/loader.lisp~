(in-package :cl-user)
;;; ======================================================================================

#-defsystem
(require "AISL-DEFSYSTEM" (make-pathname :directory 
					 (append
					   (butlast (pathname-directory *load-truename*))
					   '("aisl-defsystem"))
					 :name "loader"
					 :type "lisp"))

(eval-when (eval load)
  (let* ((loading-file *load-truename*)
	 (host (pathname-host loading-file))
	 (device (pathname-device loading-file))
	 (loading-dir (pathname-directory loading-file)))
    (setf (logical-pathname-translations "aisl-clos")
          `(("**;*.*" ,(make-pathname :directory loading-dir :host host :device device))
	    ))))

;; (define-logical-host-relative-to-current-path "AISL-CLOS")

;; (load "AISL-CLOS:aisl-clos-pkg")
(load "AISL-CLOS:sysdcl")

(compile-system 'AISL-CLOS)
(load-system 'AISL-CLOS)

(provide "AISL-CLOS")
