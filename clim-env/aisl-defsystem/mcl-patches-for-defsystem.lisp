;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; PATCHES FOR MCL ... ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :CCL)

(let ((*warn-if-redefine-kernel* NIL))

;;; Without this modification, MCL doesn't permit:
;;;
;;;    (make-pathname :name :unspecific)
;;;
;;; According to the HyperSpec version of the ANSI Spec:
;;;   "valid pathname name n. a string, nil, :wild, :unspecific, or some other object
;;;    defined by the implementation to be a valid pathname name."
;;; so this definition is changed to be identical to %std-type-component.

#-CCL-4.3
(defun %std-name-component (name)
  (cond ((or (null name) (eq name :unspecific)) name)
        ((eq name :wild) "*")
        (t (%path-std-quotes name ".:;*" ".:;"))))

#-CCL-4.3
;;; You probably want to fix your internal function to match ...
(defun directory-pathname-p (path)
  (let ((name (pathname-name path))(type (pathname-type path)))
    (and  (or (null name) (eq name :unspecific) (zerop (length name)))
          (or (null type) (eq type :unspecific)))))

#-CCL-4.3
;;; ... and it might be nice to print these things.
(defun file-namestring (path)
  (let* ((name (pathname-name path))
         (type (pathname-type path))
         (version (pathname-version path)))
    (case version
      (:newest (setq version ".newest"))
      (:wild (setq version ".*"))
      ((nil :unspecific) (setq version nil))
      (t (setq version (if (fixnump version)
                         (%str-cat "." (%integer-to-string version))
                         (%str-cat "." version)))))
    (if (and type (neq type :unspecific))
      (if (null name)
        (%str-cat "." type (or version ""))
        (%str-cat name "." type (or version "")))
      (if version ; version no type
        (if (null name)
          (%str-cat "." version)
          (%str-cat name "." version))
        (or (when (eq name :unspecific) "")
            name
            "")))))


;;; It doesn't appear that you support ensure-directories-exist.

;;; probe-directory is also broken (see following discussion):
;;;     (let ((*default-pathname-defaults* (make-pathname :name "foo" :type "lisp")))
;;;        (probe-file "MyHardDrive:Top Directory:"))
;;; fails even when the directory exists because merge-pathnames gets called in a
;;; call to %path-to-iopb in probe-file.  I'm not aware that generally pathname args
;;; to functions get merged with *default-pathname-defaults*, and probe-file's
;;; spec entry doesn't lead me to think this should happen either.

;;; It seems generally that because calls to merge-pathnames are sprinkled through
;;; the sources in ccl:I1;I1-files.lisp that if *default-pathname-defaults* is
;;; bound to a pathname which includes a type component, then directory
;;; operations fail to do-the-right-thing because you get diretory names with
;;; an extension - like foo.lisp instead of foo.  The following hacks gets around
;;; this problem for create-directory and probe-file, but you might want to fix it
;;; in a more general way.

;;; Useful for the following hacks.
(defun directory-ize-defaults ()
  (merge-pathnames
   (make-pathname :name :unspecific
                  :type :unspecific)
   *default-pathname-defaults*))

;;; Modified so that *default-pathname-defaults* does NOT include :name or
;;; :type components before the call to full-pathname. Without this
;;; modification, full-pathname will add the :type default to the result of
;;; (dirpath-to-filepath path) ... potentially resulting in directories named
;;; "...:somename.ext:"
(defun create-directory (path &key (if-exists :error) &aux errno full-path)
  (when (directory-pathname-p path) ; and translate-logical?
    (setq path (dirpath-to-filepath path)))
  (let ((*default-pathname-defaults* (directory-ize-defaults)))
    (setq full-path (full-pathname path :no-error nil)))
  (%stack-iopb (pb np)
    (setq errno (%path-to-iopb full-path pb nil nil T))
    (when (or (eq errno $fnfErr) (eq errno $nsvErr)
              (eq errno $dirNFErr)(eq errno $paramERR)) ; Aufs again
      (create-directory (make-pathname :directory (pathname-directory full-path) :defaults nil)
                        :if-exists if-exists)
      (setq errno (%path-to-iopb full-path pb :errchk nil T)))
    (unless (%izerop errno)
      (signal-file-error errno path))
    (setq errno (#_PBDirCreateSync pb))
    (when (eq errno $dupFNErr)
      (or (setq path (if-exists if-exists (%path-from-iopb pb) "Createç"))
          (return-from create-directory nil))
      (%path-to-iopb path pb :errchk)
      (file-errchk (#_PBHDeleteSync pb) path)
      (setq errno (#_PBDirCreateSync pb)))
    (%dir-path-from-iopb pb)))

(defun probe-file (path &aux (dirp (directory-pathname-p path)))
  (%stack-iopb (pb np)
    (multiple-value-bind (errno aliasp)
        (let ((*default-pathname-defaults*
               (if dirp
                 (directory-ize-defaults)
                 *default-pathname-defaults*)))
          (%path-to-iopb path pb))
      (when (%izerop errno)
        (let ((dir-result (%ilogbitp $ioDirFlg (%get-byte pb $ioFlAttrib))))
          ;(print-pb pb)
          (when (or (eq dir-result dirp)
                    (and dir-result aliasp))
            (if dir-result 
              (%dir-path-from-iopb pb)
              (%path-from-iopb pb))))))))

) ; LET
