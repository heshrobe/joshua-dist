(in-package :cl-user)

(eval-when (eval load)
  (let* ((loading-file *load-truename*)
	 (host (pathname-host loading-file))
	 (device (pathname-device loading-file))
	 (loading-dir (pathname-directory loading-file))
	 (parent-directory (butlast loading-dir))
	 (clos-directory (append parent-directory (list "aisl-clos"))))
    (setf (logical-pathname-translations "portable-lisp-environment")
          `(("aisl-clos;*.*" ,(make-pathname :directory clos-directory :host host :device device))
	    ("**;*.*" ,(make-pathname :directory loading-dir :host host :device device))))))

(load "portable-lisp-environment:load-end.lisp")

;; Start the environment now
(defun start-clim-environment ()
  (when clim-sys:*multiprocessing-p*
    (clim-sys:make-process #'clim-env:start-clim-environment
			   :name "Navigator")))

(clim-env:start-clim-environment)
