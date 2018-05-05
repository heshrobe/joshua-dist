;;; -*- package common-lisp-user: -*-
(in-package :common-lisp-user)
#+allegro

(eval-when (:compile-toplevel :load-toplevel :execute)
  (without-package-locks 

   (defclass separate-destination-module (defsys:lisp-module)
     ())
   
   (defmethod defsys:product-pathname ((module separate-destination-module))
     (let ((source-pathname (translate-logical-pathname (ds:source-pathname module))))
       (let* ((my-directory (append (pathname-directory source-pathname) 
				    (list (string-upcase
					   (format nil "~a-binaries"
						   #+(and unix solaris2) 'solaris
						   #+(and unix macosx) 'macosx
						   #+(and unix linux smp) 'linux-smp
						   #+(and unix linux (not smp)) 'linux-non-smp
						   #+MSWindows 'windows)))))
	      (full-pathname (make-pathname :directory my-directory
					    :host (pathname-host source-pathname)
					    :device (pathname-device source-pathname)
					    :name (pathname-name source-pathname)
					    :type "fasl"
					    )))
	 (ensure-directories-exist full-pathname)
	 full-pathname)))

   (defclass data-module (defsys:lisp-module)
     ())
   
   (defmethod defsys:load-module-action ((module data-module) &rest ignore)
     nil)
   
   (defmethod defsys:compile-module-action ((module data-module) &rest ignore)
     nil)
   
   ))