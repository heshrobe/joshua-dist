;;; -*- Mode: Common-lisp; Package: tk-silica -*-

;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2002 Franz Inc, Berkeley, CA  - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: image.lisp,v 1.27.10.1 2002/02/08 19:11:25 layer Exp $


;;;; Patches from file acl/src/clim/tk-silica/image.lisp
;;; Use pbm filters for everything else

;;; Patched compute filter to return two different parts of a command line
;;; Write-image-file needs to be patched to match, I've included it but not patched it
;;; yet.


(in-package :tk-silica)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (fmakunbound 'write-image-file)
  (defgeneric write-image-file (format pathname array designs pallette)
  ))

(defparameter *bitmap-file-types*
    '((:bitmap nil "xbm")
      (:pixmap "xpm")
      (:pixmap-3 "xpm")
      (:jpg "jpg")
      (:gif  "gif")
      (:tiff "tiff" "tif")))

(defmethod read-image-file ((format t) pathname palette)
  (multiple-value-bind (format first-filter second-filter)
      (compute-filter-for-bitmap-format format)
    (let* ((tempname (system:make-temp-file-name))
	   (truename (truename pathname))
	   (command (format nil "~A ~A | ~A" first-filter tempname second-filter)))
      (unwind-protect
	  (progn
	    (system:copy-file truename tempname
			      :link t)
	    (with-open-stream (fstream (excl:run-shell-command
					command
					:wait nil
					:output :stream))
	      (handler-case (read-image-file format fstream palette)
		(error (c)
		  (error "Unable to read image file: ~s (from ~s), \"~a\" ~
			while executing ~s." tempname pathname c command)))))
	(sys:os-wait)
	(delete-file tempname)))))

(defun write-bitmap-file (pathname array &key designs (format :bitmap)
					      (port (find-port)))
  (let ((palette (and port (port-default-palette port))))
    (write-image-file format pathname array designs palette)))

(defmethod write-image-file ((format t) pathname array designs palette)
  (multiple-value-bind (format read-filter filter)
      (compute-filter-for-bitmap-format format)
    (declare (ignore read-filter))
    ;; copied from code/streamc.cl - basically a truename but without
    ;; the probe-file
    (let* ((tempname (system:make-temp-file-name))
	   (truename (translate-logical-pathname
		      (merge-pathnames (pathname pathname))))
	   (command (format nil "~A > ~A" filter tempname)))
      (unwind-protect
	  (progn
	    (with-open-stream (fstream (excl:run-shell-command
					command
					:wait nil
					:input :stream))
	      (handler-case (write-image-file format fstream array designs palette)
		(error (c)
		  (error "Unable to write image file: ~s (from ~s), \"~a\" ~
			while executing ~s." tempname pathname c command))))
	    (system:copy-file tempname truename))
	(sys:os-wait)
	(delete-file tempname)))))

;;; This needs to be fixed to match the one below
(defmethod compute-filter-for-bitmap-format ((format (eql :gif)))
  (values :pixmap-3 "giftopnm | ppmtoxpm" "xpmtoppm | ppmtogif"))

(defmethod compute-filter-for-bitmap-format ((format (eql :tiff)))
  (values :pixmap-3 
	  "tifftopnm" "ppmtoxpm"
	  "xpmtoppm | pnmtotiff"))

(defmethod compute-filter-for-bitmap-format ((format (eql :png)))
  (values :pixmap-3 
	  "pngtopnm" "ppmtoxpm"
	  "xpmtoppm | pnmtotiff"))

(defmethod compute-filter-for-bitmap-format ((format (eql :jpg)))
  (values :pixmap-3 
	  "jpegtopnm" "ppmtoxpm"
	  "xpmtoppm | pnmtotiff"))

;;;



;; Support for the xpm version 3 format

(defmethod write-image-file ((format (eql :pixmap-3)) pathname array designs palette)
  (if (streamp pathname)
      (write-xpm-v3-file pathname array designs palette)
    (with-open-file (fstream pathname :direction :output
		     :if-exists :overwrite :if-does-not-exist :create)
      (write-xpm-v3-file fstream array designs palette))))

(defun read-xpm-v3-file (stream palette)
  (labels ((ensure-next-char (c)
	     (assert (eql c (skip-whitespace)) () "Expected ~S" c)
	     (read-char stream))
	   (read-a-token (predicate &optional (stream stream))
	     (skip-whitespace)
	     (let ((chars (make-array 0 :fill-pointer 0
				      :adjustable t
				      :element-type 'character)))
	       (loop
		 (let ((c (peek-char nil stream nil nil)))
		   (unless (and c (funcall predicate c))
		     (return (coerce chars 'simple-string)))
		   (vector-push-extend c chars))
		 (read-char stream))))
	   (skip-comment ()
	     (when (eql #\/ (skip-whitespace))
	       (read-char stream)	; /
	       (read-char stream)	; *
	       (loop
		 (peek-char #\* stream)
		 (read-char stream)
		 (when (eql #\/ (read-char stream))
		   (return)))))
	   (skip-trailing-crap ()
	     (loop
	       (case (skip-whitespace)
		 (#\, (read-char stream))
		 (#\/ (skip-comment))
		 (t (return)))))
	   (read-a-string ()
	     (read stream))
	   (skip-whitespace ()
	     (let (c)
	       (loop
		 (unless  (eql (setq c (peek-char t stream)) #\newline)
		   (return c))))))

    (let (width height ncolors pixels colors cpp)

      (assert (eql #\/ (skip-whitespace)) () "File must begin with a comment")

      (skip-comment)
      (assert (string= (read-a-token #'alpha-char-p) "static")
	  () "Expected static keyword")
      (assert (string= (read-a-token #'alpha-char-p) "char")
	  () "Expected char keyword" )
      (ensure-next-char #\*)

      (read-a-token #'(lambda (c) (or (alphanumericp c) (eql c #\_))))

      (ensure-next-char #\[)
      (ensure-next-char #\])
      (ensure-next-char #\=)
      (ensure-next-char #\{)

      (skip-comment)

      (let ((values (read-a-string)))
	(with-input-from-string (s values)
	  (setq width (read s)
		height (read s)
		ncolors (read s)
		cpp (read s))))

      (skip-trailing-crap)

      (let ((array (make-array (list height width))))

	(dotimes (i ncolors)
	  (let* ((string (prog1 (read-a-string) (skip-trailing-crap)))
		 (chars (subseq string 0 cpp))
		 (values nil))
	    (with-input-from-string (s string :start cpp)
	      (loop
		(let ((key (read s nil nil)))
		  (when (eq key nil) (return))
		  (assert (member key '(m s g4 g c) :test #'string-equal)
		      () "Expected either m, s, g4, g or . Got ~S" key)
		  (push (cons key
			      (case (peek-char t s)
				(#\#	; rgb
				 (read-char s)
				 (let* ((number (read-a-token #'(lambda (c) (digit-char-p c 16)) s))
					(color-string-length (length number)))
				   (assert (or (= color-string-length 12)
					       (= color-string-length 6)) ()
				     "Expected 6 or 12 character hex string. Got ~S" number)
				   (let ((comp-length (/ color-string-length 3)))
				     (flet ((get-integer (i)
					      (/ (parse-integer number
								:start (* i comp-length)
								:end (* (1+ i) comp-length)
								:radix 16)
						 (if (= comp-length 2) 255 65535))))
				       (make-rgb-color
					(get-integer 0)
					(get-integer 1)
					(get-integer 2))))))
				(#\%	; hsv
				 (read-char s)
				 (error "HSV color spec not implemented")
				 )
				(t	; color-name
				 ;; Color names can be multi-word containing spaces
				 ;; This used to be (read s) but that screws up
				 ;; under these circumstances.
				 (intern (read-line s)))))
			values)
		  )))
	    (assert values () "Expected  key,color for ~S" chars)
	    (push (cons chars values) colors)))


	(setq pixels (nreverse pixels))

	(dotimes (i height)
	  (let ((string (read-a-string)))
	    (skip-trailing-crap)
	    (dotimes (j width)
	      (setf (aref array i j)
		(position
		 (let ((index (* cpp j)))
		   (subseq string index (+ index cpp)))
		 colors
		 :key #'car
		 :test #'string=)))))

	(values array
		(mapcar #'(lambda (name-and-versions)
			    (let ((color (or (cdr (assoc "c" (cdr name-and-versions)
							 :test #'string-equal))
					     (cdr (car (cdr name-and-versions))))))
			      (etypecase color
				(color color)
				(symbol (cond ((string-equal color "none")
					       +background-ink+)
					      (palette
					       (find-named-color (string color) palette))
					      (t color))))))
			colors))))))

(defparameter printing-chars
    (loop for i below 256 
	for char = (code-char i)
	when (and (standard-char-p char)
		  (not (eql #\" char))
		  (not (eql #\\ char))
		  (not (eql #\newline char))
		  (not (eql #\return char)))
	collect char))

(defun number-of-chars-to-encode (size)
  (ceiling (log size) (log (length printing-chars))))

(defun write-xpm-v3-file (stream array designs &optional palette)
  (unless palette
    (setq palette (clim:port-default-palette (clim:find-port))))
  (let ((color-name-table (make-hash-table))
	(color-code-table nil)
	(height (array-dimension array 0))
	(width (array-dimension array 1)))
    (declare (ignore color-code-table))
    (maphash #'(lambda (key value)
		 (setf (gethash value color-name-table) key))
	     (palette-named-color-cache palette))
    (write-string "/* XPM */" stream)
    (terpri stream)
    (write-string "static char *noname[] = {" stream)
    (terpri stream)
    (write-string "/* width height ncolors chars_per_pixel */" stream)
    (format stream "~%\"~d ~d ~d ~d\",~%"
	    width height
	    (length designs)
	    (ceiling (log (length designs) 2) 4))
    (write-string "/* colors */" stream)
    (flet ((number-to-chars (number)
	     (multiple-value-bind (q r) (floor number (length printing-chars))
	       (when (null r) (setq r 0))
	       (format nil "~c~c" (nth q printing-chars) (nth r printing-chars))))
	   (convert-color-to-hex (color)
	     (multiple-value-bind (r g b) (clim:color-rgb color)
	       (format nil "#~2,'0x~2,'0x~2,'0x"
		       (round (* r 255))
		       (round (* g 255))
		       (round (* b 255))))))
      (loop for d across designs
	  for i from 0
	  for chars = (number-to-chars i)
	  do (format stream "~%\"~2a c ~a\","
		     chars
		     (cond
		       ((typep d 'clim-utils:rgb-color) (convert-color-to-hex d))
		       ((typep d 'clim-utils:gray-color)
			(or (gethash d color-name-table)
			    (convert-color-to-hex d)))
		       ((eql d clim:+background-ink+) "none")
		       (t (error "Bad design ~a" d)))))
      (terpri stream)
      (write-string "/* pixels */" stream)
      (loop for j below height
	  unless (= j 0) do (write-char #\, stream)
	  do (format stream "~%\"")
	     (loop for i below width
		 for cell-value = (aref array j i)
		 for chars = (number-to-chars cell-value)
		 do (format stream "~2a" chars))
	     (format stream "\""))
      (format stream "~%};")
      )))

(defmethod scale-and-quantize-image-file ((format t) pathname
					  &key (width nil)
					       (height nil)
					       (reduce nil)
					       (color-quantization 256))
  (multiple-value-bind (output-format first-filter second-filter)
      (compute-filter-for-bitmap-format format)
    (let* ((truename (truename pathname))
	   (output-pathname (make-pathname :device (pathname-device truename)
					   :directory (pathname-directory truename)
					   :name (pathname-name truename)
					   :type (second (assoc output-format *bitmap-file-types*))))
	   (command (format nil "~A ~A | pnmcrop ~:[~; | pamscale~]~@[ -width=~a~]~@[ -height=~a~]~@[ -reduce=~a~]~@[ | pnmquant ~a~] | ~a > ~a"
			    first-filter truename
			    (or width height reduce) 
			    width height reduce
			    color-quantization
			    second-filter
			    output-pathname)))
      (print command)
      (excl:run-shell-command command :wait t)
      )))

(defmethod crop-xpm-file (input-pathname output-name)
  (let* ((truename (truename input-pathname))
	 (output-pathname (make-pathname :device (pathname-device truename)
					 :directory (pathname-directory truename)
					 :name output-name
					 :type "xpm"))
	 (command (format nil "xpmtoppm ~A | pnmcrop | pnmquant 256 | ppmtoxpm > ~a"
			  truename
			  output-pathname)))
    (print command)
    (excl:run-shell-command command :wait t)
    ))


