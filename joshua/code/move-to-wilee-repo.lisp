;;; -*- Mode: Common-lisp; Package: joshua-user -*=

(in-package :joshua-user)

(defparameter *big-files* (list ))

(defparameter *repo-pathname* #p"~/Research-Projects/CHASE/wilee/threat-planner/threat_planner/joshua/code/")

(defparameter *local-pathname* #p"~/josh-dist/joshua/code/*.lisp")

(defparameter *excluded-files* (list #p"~/josh-dist/joshua/code/move-to-wilee-repo.lisp"
				     #p"~/josh-dist/joshua/code/cloehacks.lisp"
				     #p"~/josh-dist/joshua/code/joshua-defsystem.lisp"
				     #p"~/josh-dist/joshua/code/original-mapforms.lisp"
				     ))


(defun move-to-wilee-repo ()
  (let ((all-files (append (cl:directory *local-pathname*) (cl:directory (merge-pathnames "*.asd" *local-pathname*)))))
    (loop for file in all-files
	unless (or (member file *excluded-files* :test #'pathname-match-p) (find #\# (pathname-name file) :test #'char-equal))
	do (let ((destination (make-pathname :name (pathname-name file)
						   :type (pathname-type file) 
						   :directory (pathname-directory *repo-pathname*))))
		  (if (or (null (file-write-date destination))
			  (> (file-write-date file) (file-write-date destination)))
		      (sys:copy-file file destination :overwrite t :verbose t)
		    (format t "~%File ~a is up to date" file))))
    ))

