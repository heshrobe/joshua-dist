(in-package :CL-USER)
;;; ======================================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; the preferred way
  (defun define-logical-host-relative-to-current-path (logical-hostname)
    (setf (logical-pathname-translations logical-hostname)
      `(#+ALLEGRO
	("*.*"
	 ,(make-pathname :directory (pathname-directory 
				     *load-truename*)
			 :name "*"
			 :type "*"))
	#+ALLEGRO
	("*;*.*"
	 ,(format NIL "~A"
		  (make-pathname :directory (append (pathname-directory 
						     *load-truename*)
						    '("*"))
				 :name "*"
				 :type "*")))
	#+ALLEGRO
	("*;*;*.*"
	 ,(format NIL "~A"
		  (make-pathname :directory (append (pathname-directory
						     *load-truename*)
						    '("*/*"))
				 :name "*"
				 :type "*")))
	#+ALLEGRO
	("*;*;*;*.*"
	 ,(format NIL "~A"
		  (make-pathname :directory (append (pathname-directory
						     *load-truename*)
						    '("*/*/*"))
				 :name "*"
				 :type "*")))
	#+ALLEGRO
	("*;*;*;*;*.*"
	 ,(format NIL "~A"
		  (make-pathname :directory (append (pathname-directory
						     *load-truename*)
						    '("*/*/*/*"))
				 :name "*"
				 :type "*")))
	#+ALLEGRO
	("*;*;*;*;*;*.*"
	 ,(format NIL "~A"
		  (make-pathname :directory (append (pathname-directory
						     *load-truename*)
						    '("*/*/*/*/*"))
				 :name "*"
				 :type "*")))
	#-ALLEGRO
	("**;*.*"
	 ,(make-pathname :directory (append
				     (pathname-directory *load-truename*)
				     '("**"))
			 :name "*"
			 :type "*")))))
  
  ;; the less than preferred way
  (defun pathname-relative-to-loading-file (&optional (subdir NIL) (dir *load-truename*))
    (namestring
     (make-pathname
      :name nil
      :type nil
      :defaults dir
      :directory (if subdir
                     (append (pathname-directory dir) (list subdir))
                   (pathname-directory dir)))))
) ;; eval-when

;;; ======================================================================================

;;(load (make-pathname :directory
;;					 (pathname-directory
;;					  (pathname-relative-to-loading-file NIL))
;;                                         :name "defsystem"
;;                                         :type "lisp"))
