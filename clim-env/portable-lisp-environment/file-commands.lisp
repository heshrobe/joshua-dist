;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-ENV; Base: 10; Lowercase: Yes -*-

;;; Copyright (c) 1994-2000, Scott McKay.
;;; Copyright (c) 2001-2003, Scott McKay and Howard Shrobe.
;;; All rights reserved.  No warranty is expressed or implied.
;;; See COPYRIGHT for full copyright and terms of use.

(in-package :clim-env)

;;; General pathname utilities

(defun directory-pathname-p (pathname)
  #+Genera (and (pathnamep pathname)
		(string-equal (pathname-type pathname) "directory")
		(eql (pathname-version pathname) 1))
  #+Lispworks (sys::check-file-type pathname)
  #+MCL (if (or (ccl::wild-pathname-p pathname :name)
		(ccl::wild-pathname-p pathname :type))
	  t
	  (ccl:directoryp (make-pathname :version nil :defaults pathname)))
  #+Allegro (and (null (pathname-name pathname))
		 (null (pathname-type pathname))))

(defun expand-wildcard-pathname (pathname)
  (directory pathname))

(defun merge-wildcard-pathnames (source-pathname wildcard-input wildcard-output)
  (assert (not (wild-pathname-p source-pathname)) () "The source pathname ~A must not have any wildcards" source-pathname)
  #-Genera (translate-pathname source-pathname wildcard-input wildcard-output)
  #+Genera (scl:send wildcard-input :translate-wild-pathname
		     wildcard-output source-pathname))

#+Genera
(defun pathname-as-directory (pathname)
  (make-pathname :name nil :type nil :version nil
		 :directory (if (pathname-name pathname)
			      (append (pathname-directory pathname)
				      (list (pathname-name pathname)))
			      (pathname-directory pathname))
		 :defaults pathname))

#+(and Allegro (or Unix MSWindows))
(defun pathname-as-directory (pathname)
  (make-pathname :name nil :type nil :version nil
		 :directory (if (pathname-name pathname)
			      (append (pathname-directory pathname)
				      (list (pathname-name pathname)))
			      (pathname-directory pathname))
		 :defaults pathname))

#-(or Genera Allegro)
(defun pathname-as-directory (pathname)
  (make-pathname :name nil :type nil :version nil :defaults pathname))

#+Genera
(defun directory-list (directory-pathname &rest options)
  (declare (dynamic-extent options))
  (cdr (apply #'fs:directory-list directory-pathname options)))

#-(or Genera MCL)
(defun directory-list (directory-pathname &rest options)
  (declare (dynamic-extent options)
	   (ignore options))
  (flet ((make-pathname-plist (pathname)
	   (let ((directory-p (directory-pathname-p pathname)))
	     (multiple-value-bind (size creation-date author)
		 #-Lispworks
	         (handler-case
		     (with-open-file (file-stream pathname :direction :input) 
		       (values (unless directory-p (file-length file-stream))
			       (file-write-date file-stream)
			       (file-author file-stream)))
		   (file-error () (values nil 0 nil)))
		 #+Lispworks
		 (system::with-file-stats (author length write-date)
		   pathname
		   (values length write-date author)
		   (values nil 0 nil))
	       #+(and Allegro (or MSWindows Unix))
	       (when (eql creation-date 0)
		 (setq pathname (pathname-as-directory pathname)
		       directory-p t))
	       `(,pathname
		 ,.(when directory-p (list :directory t))
		 ,.(when size `(:length-in-bytes ,size))
		 ,.(when creation-date `(:modification-date ,creation-date))
		 ,.(when author `(:author ,author)))))))
    (mapcar #'make-pathname-plist (directory directory-pathname))))

#+MCL
(defun directory-list (pathname &rest options)
  (declare (dynamic-extent options))
  "Returns a Lisp Machine style directory listing."
  (let ((pathnames (directory (merge-pathnames pathname "*.*")
			      :files t
			      :resolve-aliases t
			      :directories t)))
    (when (member :sorted options)
      (setq pathnames (sort pathnames #'(lambda (x y)
					  (and (string< (pathname-name x) (pathname-name y))
					       (string< (pathname-type x) (pathname-type y)))))))
    (loop for path in pathnames
	  for directory-p = (ccl:directoryp path)
	  for size = (if directory-p 0 (ccl::file-data-size path))
	  for creation-date = (ccl::file-write-date path)
	  collect `(,path
		    ,.(when directory-p (list :directory t)) 
		    ,.(when size `(:length-in-bytes ,size))
		    ,.(when creation-date `(:modification-date ,creation-date))))))

(defun sort-directory-list (directory-list sort-by)
  (labels ((name-sort-key (pathname)
	     (or (if (directory-pathname-p pathname)
		   (car (last (pathname-directory pathname)))
		   (pathname-name pathname))
		 ""))
	   (type-lessp (p1 p2)
	     (let ((type1 (pathname-type p1))
		   (type2 (pathname-type p2)))
	       (and type1 type2 (string-lessp type1 type2))))
	   (pathname-lessp (p1 p2)
	     (let ((name1 (name-sort-key p1))
		   (name2 (name-sort-key p2)))
	       (or (string-lessp name1 name2)
		   (and (string-equal name1 name2)
			(type-lessp p1 p2)))))
	   (file-size (plist)
	     (getf (cdr plist) :length-in-bytes 0))
	   (file-modification-date (plist)
	     (getf (cdr plist) :modification-date 0)))
    (declare (dynamic-extent #'name-sort-key #'type-lessp #'pathname-lessp
			     #'file-size #'file-modification-date))
    (multiple-value-bind (predicate key)
	(ecase sort-by
	  (:name (values #'pathname-lessp #'first))
	  (:type (values #'type-lessp #'first))
	  (:size (values #'< #'file-size))
	  (:date (values #'< #'file-modification-date)))
      (sort directory-list predicate :key key))))

(defun sorted-directory-list (directory-pathname sort-by)
  (sort-directory-list (directory-list directory-pathname) sort-by))


;;; Directory commands 

(defun show-directory (directory-pathname
		       &key before since (sort-by :name) &aux (total-bytes 0))
  (flet ((format-entry (stream path-plist before since)
	   (destructuring-bind (pathname &key directory length-in-bytes modification-date author &allow-other-keys) path-plist
	     (when modification-date
	       (when (and before (>= modification-date before))
		 (return-from format-entry))
	       (when (and since (< modification-date since))
		 (return-from format-entry)))
	     (when length-in-bytes
	       (incf total-bytes length-in-bytes))
	     (with-output-as-presentation (stream pathname 'pathname :single-box t)
	       (formatting-row (stream)
		 (formatting-cell (stream)
		   (format stream "  ~A"
			   (if directory (car (last (pathname-directory pathname))) (file-namestring pathname))))
		 (formatting-cell (stream :align-x :right)
		   (if directory
		     (write-string "Directory" stream)
		     (format stream "~D" (or length-in-bytes ""))))
		 (formatting-cell (stream :align-x :right)
		   (when modification-date
		     (print-universal-time modification-date :stream stream)))
		 (formatting-cell (stream)
		   (write-string (or author "") stream) ))))))
    (declare (inline format-entry))
    (let ((directory-list (sorted-directory-list directory-pathname sort-by)))
      (with-frame-standard-output (stream)
	(fresh-line stream)
	(present directory-pathname 'pathname :stream stream)
	(typecase directory-pathname
	  (logical-pathname 
	   (fresh-line stream)
	   (present (typecase directory-pathname
		      (logical-pathname (translate-logical-pathname directory-pathname))
		      (t directory-pathname)) 'pathname
		    :stream stream)))
	(fresh-line stream)
	(formatting-table (stream :x-spacing "   ")
	  (dolist (path-plist directory-list)
	    (format-entry stream path-plist before since)))
	(unless (zerop total-bytes)
	  (format stream "~&Total of ~A bytes in the files listed"
	    (cond ((> total-bytes 1e6)
		   (format nil "~1$M" (/ total-bytes 1e6)))
		  ((> total-bytes 1e4)
		   (format nil "~1$K" (/ total-bytes 1e3)))
		  (t (format nil "~D" total-bytes)))))))))

(define-command (com-show-directory :command-table directories :name t)
    ((directory '((pathname) 
		  :default-type #+(or Unix MCL) nil #-(or Unix MCL) :wild
		  :default-version #+Genera :newest #-Genera nil)
		:provide-default t :prompt "pathname")
     &key
     (before '(null-or-type (universal-time :pastp t))
	     :default nil
	     :prompt "date"
	     :documentation "Show files created before this date")
     (since '(null-or-type (universal-time :pastp t))
	    :default nil
	    :prompt "date"
	    :documentation "Show files created on or after this date")
     (sort-by '(member :name :type :size :date)
	      :default :name
	      :prompt "sort by"
	      :documentation "What to sort the directory by"))
  (show-directory directory :before before :since since :sort-by sort-by))

(define-presentation-to-command-translator com-show-directory
    (pathname com-show-directory directories
     :priority 2
     :tester ((object)
	      (directory-pathname-p object))
     :gesture :select)
    (object)
  (list (pathname-as-directory object)))

;; Show the contents of the current (home) directory
(define-command (com-show-home-directory :command-table directories :name t) ()
  (show-directory (make-pathname :defaults #-Allegro (user-homedir-pathname)
				           #+Allegro (excl:current-directory)
				 :name :wild
				 :type #+Unix nil #-Unix :wild
				 :version #+Unix nil #-Unix :newest)))

;; Show the current directory ('pwd')
(define-command (com-show-current-directory :command-table directories :name t) ()
  (with-frame-standard-output (stream)
    (present (make-pathname :defaults #-Allegro (user-homedir-pathname)
				      #+Allegro (excl:current-directory)
			    :name :wild
			    :type #+Unix nil #-Unix :wild
			    :version #+Unix nil #-Unix :newest)
	     'pathname
	     :stream stream)))

;; Set the current directory ('cd')
(define-command (com-set-current-directory :command-table directories :name t)
    ((directory '((pathname) 
		  :default-type #+(or Unix MCL) nil #-(or Unix MCL) :wild
		  :default-version #+Genera :newest #-Genera nil)
		:provide-default t :prompt "pathname"))
  #-Allegro nil
  #+Allegro
  (let ((directory (make-pathname :name nil :type nil :version nil
				  :directory (pathname-directory directory)
				  :defaults directory)))
    (setf (car excl::*directory-stack*) (excl:chdir (namestring directory)))
    (excl::clear-directory-cache)
    (setq *default-pathname-defaults* (pathname (car excl::*directory-stack*)))))

(defun create-directories-recursively (pathname &optional (stream *standard-output*))
  "Recursively create directories according to the directories present in PATHNAME."
  (let* ((path (etypecase pathname
		 (logical-pathname (translate-logical-pathname pathname))
		 (pathname pathname)))
	 (directory (make-pathname :host (pathname-host path)
				   :device (pathname-device path)
				   :directory (pathname-directory path))))
    #+Genera
    (handler-case 
	(let  ((*standard-output* nil))
	  (fs:create-directories-recursively pathname))
      (condition (condition)
	(fresh-line stream)
	(dbg:report condition stream)
	(return-from create-directories-recursively nil)))
    #+MCL
    (handler-case
	(ccl:create-directory directory :if-exists :error)
      (condition (condition) 
	(fresh-line stream)
	(ccl::report-condition condition stream)
	(return-from create-directories-recursively nil)))
    #+Allegro 
    (when (not (probe-file directory))
      (excl::ensure-directories-exist directory))
    (when stream
      (fresh-line stream)
      (present directory 'pathname :stream stream)
      (write-string " created."))
    directory))

(define-command (com-create-directory :command-table directories :name t) 
    ((directory '((pathname) :default-type nil :default-version nil)
		:provide-default t :prompt "directory"
		:documentation "Create a directory, possibly including any missing superior directories."))
  (with-frame-standard-output (stream)
    (create-directories-recursively directory stream))) 

(defun delete-directory (pathname &optional (confirm-p t) (stream *standard-output*))
  (let* ((path (etypecase pathname
		 (logical-pathname (translate-logical-pathname pathname))
		 (pathname pathname)))
	 (directory (make-pathname :host (pathname-host path)
				   :device (pathname-device path)
				   :directory (pathname-directory path)))
	 deleted-dir)
    (cond ((or (not confirm-p)
	       (yes-or-no-p "Permanently delete ~A and its contents? " directory))
	   #+Genera
	   (handler-case 
	       (let ((*standard-output* nil))
		 (fs:delete-directory directory :confirm nil))
	     (condition (condition) 
	       (fresh-line stream)
	       (dbg:report condition stream)
	       (return-from delete-directory nil)))
	   #+Allegro
	   (handler-case 
	       (excl:delete-directory-and-files directory :quiet t)
	     (condition (condition) 
	       (format stream "~&Error: ~A" condition)
	       (return-from delete-directory nil)))
	   #+MCL 
	   (handler-case
	       (setq deleted-dir (ccl::delete-file  directory :if-does-not-exist nil))
	     (condition (condition) 
	       (fresh-line stream)
	       (ccl::report-condition condition stream)
	       (return-from delete-directory nil)))
	   (when stream
	     (fresh-line stream)
	     (cond (deleted-dir
		    (present deleted-dir 'pathname :stream stream)
		    (write-string " deleted."))
		   (t (present directory 'pathname :stream stream)
		      (write-string " not found."))))
	   deleted-dir)
	  (t (fresh-line stream)
	     (present directory 'pathname :stream stream)
	     (write-string " not deleted")
	     nil))))

(define-command (com-delete-directory :command-table directories :name t) 
    ((directory '((pathname) :default-type nil :default-version nil)
		:provide-default t :prompt "directory"
		:documentation "Delete a directory, including any files and subdirectories it contains.")
     &key
     (confirm 'boolean :default t 
	      :documentation "Ask for confirmation before permanently deleting the directory."))
  (with-frame-standard-output (stream)
    (delete-directory directory confirm stream)))

#+Genera
(define-command (com-expunge-directory :command-table directories :name t)
    ((directory '((pathname) :default-type #+Unix nil #-Unix :wild
		  :default-version #+Unix nil #-Unix :wild)
		:prompt "directory"))
  (let ((freed (fs:expunge-directory directory)))
    (with-frame-standard-output (stream)
      (fresh-line stream)
      (format stream "~D block~:P freed" freed))))

#-Genera
(define-command (com-empty-trashcan :command-table directories :name t) ()
  ;;--- This assumes we have implemented a TrashCan in Delete File
  )


;;--- Clean Directory
;;--- Compare Directories
;;--- Show Disk Usage 


;;; File commands 

(defun show-file (pathname stream &key verbose)
  (when verbose
    (with-text-face (stream :italic)
      (fresh-line stream)
      (format stream "File ~A~%" pathname)))
  (clim-utils:with-temporary-string (line-buffer :length 1000)
    (with-open-file (fs pathname :if-does-not-exist nil)
      (when fs
	#+Genera
	(loop
	  (multiple-value-bind (nil eof-p)
	      (scl:send fs :string-line-in nil line-buffer)
	    (when eof-p 
	      (return-from show-file))
	    (write-string line-buffer stream)
	    (write-char #\Newline stream)))
	#-Genera
	;;--- Why there isn't a non-consing version of READ-LINE is beyond me
	(let ((eof-marker '#:eof))
	  (loop
	    (let ((ch (read-char fs nil eof-marker)))
	      (cond ((eql ch eof-marker)
		     (unless (zerop  (fill-pointer line-buffer))
		       (write-string line-buffer stream)
		       (write-char #\newline stream))
		     (return-from show-file))
		    ((member ch '(#\Newline #-Genera #\Return))
		     (write-string line-buffer stream)
		     (write-char #\Newline stream)
		     (setf (fill-pointer line-buffer) 0))
		    (t
		     (vector-push-extend ch line-buffer))))))))))

(define-command (com-show-file :command-table files :name t)
    ((pathnames '(sequence pathname)
		:provide-default t
		:prompt "file(s)"))
  (with-frame-standard-output (stream)
    (dolist (pathname pathnames)
      (if (wild-pathname-p pathname)
	(dolist (p (expand-wildcard-pathname pathname))
	  (show-file p stream :verbose t))
	(show-file pathname stream :verbose (not (null (cdr pathnames))))))))

(define-presentation-to-command-translator com-show-file
    (pathname com-show-file files
     :gesture :select
     :tester ((object)
              (not (directory-pathname-p object))))
    (object)
  (list (list object))) 

(define-command (com-delete-file :command-table files :name t)
    ((pathnames '(sequence pathname) :provide-default nil :prompt "file(s)")
     &key
     (query 'boolean :default #+Genera nil #-Genera t :mentioned-default t
	    :documentation "Ask before deleting each file"))
  (flet ((deletef (pathname)
	   (when (or (not query)
		     (yes-or-no-p "Delete the file ~A? " pathname))
	     ;;--- On Unix systems, this should put file into a trashcan
	     (delete-file pathname)
	     (with-frame-standard-output (stream)
	       (fresh-line stream)
	       (format stream "File ~A deleted." pathname)))))
    (declare (dynamic-extent #'deletef))
    (dolist (pathname pathnames)
      (if (wild-pathname-p pathname)
	(dolist (p (expand-wildcard-pathname pathname))
	  (deletef p))
	(deletef pathname)))))

(define-presentation-to-command-translator delete-file
    (pathname com-delete-file files
     :gesture nil)
    (object)
  (list (list object)))

(define-drag-and-drop-translator d&d-delete-file
    (pathname command trashcan files)
    (object)
  `(com-delete-file ,(list object)))

#-MCL
(define-command (com-undelete-file :command-table files :name t)
    ((pathnames '(sequence pathname)
		:provide-default nil
		:prompt "file(s)")
     &key
     (query 'boolean
	    :default nil :mentioned-default t
	    :documentation "Ask before undeleting each file"))
  (flet ((undeletef (pathname)
	   (when (or (not query)
		     (yes-or-no-p "Undelete the file ~A? " pathname))
	     ;;--- On Unix systems, this should remove the file from the trashcan
	     #+Genera (zl:undeletef pathname)
	     (with-frame-standard-output (stream)
	       (fresh-line stream)
	       (format stream "File ~A undeleted." pathname)))))
    (declare (dynamic-extent #'undeletef))
    (dolist (pathname pathnames)
      (if (wild-pathname-p pathname)
	(dolist (p (expand-wildcard-pathname pathname))
	  (undeletef p))
	(undeletef pathname)))))

#-MCL
(define-presentation-to-command-translator undelete-file
    (pathname com-undelete-file files
     :gesture nil)
    (object)
  (list (list object)))

;;--- Clean File -- Genera only

;; Why Common Lisp doesn't define this is beyond me...
(defun copy-file (from-file to-file &rest keys 
		  &key element-type create-directories if-exists &allow-other-keys)
  (declare (ignore element-type create-directories #+(or Genera Lispworks) if-exists
		   #-Genera keys)
	   (dynamic-extent keys))
  ;;--- Do the portable version sometime
  #+Genera    (apply #'zl:copyf from-file to-file :copy-author t :copy-creation-date t keys)
  #+MCL       (prog1 to-file (ccl::copy-file from-file to-file :if-exists if-exists))
  #+Lispworks (prog1 to-file (sys::copy-file from-file to-file))
  #+Allegro   (prog1 to-file (sys::copy-file from-file to-file
					     :overwrite (member if-exists '(:overwrite :supersede)))))

(define-command (com-copy-file :command-table files :name t)
    ((from-files '(sequence pathname) :provide-default nil :prompt "from file(s)")
     (to-file 'pathname :default (first from-files) :prompt "to file")
     &key
     (byte-size '((integer 1))
		:default 8 :prompt "byte size in bits"
		:documentation "Byte size in which to do copy operation")
     (mode '((member-alist
	      (("Binary" :value :binary
			 :documentation "Binary bytes of a size given by Byte Size")
	       ("Character" :value :character
			    :documentation "Ordinary characters")
	       ("Default" :value :default
		 :documentation "Determined automatically per file")))
	     :description "a copy mode")
	   :default :default
	   :documentation "Mode in which to perform the copy")
     (create-directories '((member-alist
			    (("Yes" :value :yes
				    :documentation "Automatically create it")
			     ("Error" :value :error
				      :documentation "Signal an error")
			     ("Query" :value :query
				      :documentation "Ask what to do then"))))
			 :default :query
			 :documentation "What to do if a destination directory does not exist")
     (if-exists '((member-alist
		   (("New Version" :value :new-version
				   :documentation "Create the next version -- default if target is .NEWEST")
		    ("Overwrite" :value :overwrite
				 :documentation "Destructively modify target")
		    ("Supersede" :value :supersede
				 :documentation "Replace the target -- default if target is not .NEWEST")
		    ("Nothing" :value nil
			       :documentation "Do nothing if the target already exists"))))
		:default nil
		:documentation "What to do if asked to copy to an existing file")
     (query 'boolean
	    :default nil :mentioned-default t
	    :documentation "Ask before copying each file"))
  (with-frame-standard-output (stream)
    (let ((element-type (ecase mode
			  (:binary `(unsigned-byte ,(or byte-size '*)))
			  (:character `character)
			  (:default (if (null byte-size) `:default `(unsigned-byte ,byte-size)))))
	  (create-directories (ecase create-directories
				(:yes t)
				(:error nil)
				(:query :query))))
      (flet ((do-copy (from-file to-file)
	       (when (or (not query) (y-or-n-p "Copy file ~A to ~A? " from-file to-file))
		 (handler-case
		     (let ((truename (apply #'copy-file from-file to-file 
					    :element-type element-type :create-directories create-directories 
					    (and if-exists `(:if-exists ,if-exists)))))
		       (format stream "~&Copied ~A to ~A." from-file truename))
		   (file-error (error)
		     (format stream "~&Error copying ~A:~%~A" from-file error))))))
	(declare (dynamic-extent #'do-copy))
	(dolist (from-file from-files)
	  (if (wild-pathname-p from-file)
	    (let ((files (expand-wildcard-pathname from-file)))
	      (let ((merged-to (merge-pathnames to-file from-file #+Unix nil #-Unix :wild)))
		(dolist (file files)
		  (let ((truename (truename file)))
		    (unless (directory-pathname-p truename)
		      (do-copy truename (merge-wildcard-pathnames file from-file merged-to)))))))
	    (do-copy from-file to-file))))))) 

(define-command (com-rename-file :command-table files :name t)
    ((from-files '(sequence pathname)
		 :provide-default nil
		 :prompt "from file(s)")
     (to-file 'pathname
	      :default (first from-files)
	      :prompt "to file")
     &key
     (query 'boolean
	    :default nil :mentioned-default t
	    :documentation "Ask before renaming each file"))
  (with-frame-standard-output (stream)
    (flet ((do-rename (from-file to-file)
	     (when (or (not query)
		       (y-or-n-p "Rename file ~A to ~A? " from-file to-file))
	       (handler-case
		   (multiple-value-bind (new-name old-truename new-truename)
		       (rename-file from-file to-file)
		     (declare (ignore new-name))
		     (fresh-line stream)
		     (format stream "Renamed ~A to ~A." old-truename new-truename))
		 (file-error (error)
		   (fresh-line stream)
		   (format stream "Error renaming ~A:~%~A" from-file error))))))
      (declare (dynamic-extent #'do-rename))
      (dolist (from-file from-files)
	(if (wild-pathname-p from-file)
	  (let ((files (expand-wildcard-pathname from-file)))
	    (let ((merged-to (merge-pathnames to-file from-file
					      #+Unix nil #-Unix :wild)))
	      (dolist (file files)
		(let ((truename (truename file)))
		  (unless (directory-pathname-p truename)
		    (do-rename truename 
		      (merge-wildcard-pathnames file from-file merged-to)))))))
	  (do-rename from-file to-file))))))

;;--- Append Files
;;--- Create File
;;--- Create Link
;;--- Set File Properties -- Genera only

;;--- Compress File
;;--- Uncompress File

(defun search-file (pathname stream strings conjunction)
  (clim-utils:with-temporary-string 
      (line-buffer :length 1000)
    (with-open-file (fs pathname :if-does-not-exist nil)
      (when fs
	(let ((line-number 0)
	      (printed nil))
	  (flet ((search-line (line)
		   (when (if (eql conjunction 'and)
			   (every #'(lambda (string) (search string line :test #'char-equal))
				  strings)
			   (some #'(lambda (string) (search string line :test #'char-equal))
				 strings))
		     (unless printed
		       (with-output-as-presentation (stream pathname 'pathname)
			 (format stream "~%In file ~A~%" pathname))
		       (setq printed t))
		     (format stream "#~3D: " line-number)
		     (write-string line stream)
		     (write-char #\Newline stream)
		     (force-output stream))
		   (incf line-number)))
	    (declare (dynamic-extent #'search-line))
	    #+Genera
	    (loop (multiple-value-bind (nil eof-p)
		      (scl:send fs :string-line-in nil line-buffer)
		    (when eof-p 
		      (return-from search-file))
		    (search-line line-buffer)))
	    #-Genera
	    ;;--- Why there isn't a non-consing version of READ-LINE is beyond me
	    (let ((eof-marker '#:eof))
	      (loop (let ((ch (read-char fs nil eof-marker)))
		      (cond ((eql ch eof-marker)
			     (return-from search-file))
			    ((member ch '(#\Newline #-Genera #\Return #+mcl #\linefeed))
			     (search-line line-buffer)
			     (setf (fill-pointer line-buffer) 0))
			    (t (vector-push-extend ch line-buffer))))))))))))

(define-command (com-find-string :command-table files :name t)
    ((strings '(sequence string)
	      :prompt "substring(s)"
	      :documentation "Substrings to search for")
     &key
     (files '(sequence pathname)
	    :default nil
	    :prompt "file(s)"
	    :documentation "Files to search through")
     (systems '(sequence (type-or-string system))
	      :default nil
	      :prompt "systems(s)"
	      :documentation "Systems to search through")
     (conjunction '(member and or)
		  :default 'and
		  :documentation "AND or OR of the strings")
     (include-components 'boolean
			 :default t
			 :documentation "Yes or No"
			 ))
  (with-frame-standard-output (stream)
    (when files
      (dolist (pathname files)
	(if (wild-pathname-p pathname)
	  (dolist (p (expand-wildcard-pathname pathname))
	    (search-file p stream strings conjunction))
	  (search-file pathname stream strings conjunction))))
    (when systems
      (flet ((search-system-file (pathname)
	       (search-file (typecase pathname
			      (logical-pathname (translate-logical-pathname pathname))
			      (t pathname))
			    stream strings conjunction)))
	(declare (dynamic-extent #'search-system-file))
	(dolist (system systems)
	  #+Allegro
	  (loop for pathname in (all-files-in-system system include-components)
	      do (search-system-file pathname))
	  #-(or Genera Allegro)
	  (clim-defsystem::system-map-files system #'search-system-file
					    :file-type :source
					    :include-components nil)))))) 



(define-command (com-show-differences :command-table files :name t)
    ((file1 'pathname :provide-default t :prompt "file")
     (file2 'pathname :provide-default t :prompt "file")
     &key
     (case-sensitive 'boolean :default nil :mentioned-default t
		     :documentation "Use case sensitive comparison.")
     (whitespace-sensitive 'boolean :default nil :mentioned-default t
			   :documentation "Do not ignore whitespace differences at the start and end of lines.")
     #-Genera
     (ignore-comments 'boolean :default t :mentioned-default nil
		      :documentation "Attempt to ignore semi-colon comments.")
     #-Genera
     (blank-line-sensitive 'boolean :default nil :mentioned-default t
			   :documentation "Do not ignore differences based on blank lines.")
     (show-context 'boolean :default t :mentioned-default nil
		   :documentation "Separate the differences with marking lines.")
     #-Genera
     (detailed-header 'boolean :default nil :mentioned-default t
		      :documentation "Print a detailed header."))
  (with-frame-standard-output (stream)
    #+Genera
    (srccom:source-compare file1 file2 stream show-context
			   :ignore-case-and-style (not case-sensitive)
			   :ignore-whitespace (not whitespace-sensitive))
    #-Genera
    (srccom:source-compare file1 file2 :output-stream stream
			   :ignore-case (not case-sensitive)
			   :ignore-whitespace (not whitespace-sensitive)
			   :ignore-comments ignore-comments
			   :ignore-blank-lines (not blank-line-sensitive)
			   :print-context show-context
			   :print-fancy-header detailed-header)))
