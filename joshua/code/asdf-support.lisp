;;; -*- Mode: Common-lisp; Package: asdf/interface -*-

(in-package :asdf/interface)

;;; being paranoidly safe
(export (intern (string-upcase "joshua-file") 'asdf/interface) 'asdf/interface)

(defun find-joshua-readtable ()
  (symbol-value (intern (string-upcase "*joshua-readtable*") (find-package :joshua-internals)))
  )

(defclass joshua-file (cl-source-file)
  ()
  )

(defmethod asdf:perform :around ((operation compile-op) (c joshua-file))
  ;; We bind *readtable* so that we're reading in joshua syntax
  ;; We also bind all the variables associated with compiling and caching matcher code
  ;; In other versions, we wrapped compile-file, but in asdf 3.3.2 this seems not work
  ;; because compile-file doesn't get called (for reasons I don't understand at all).
  ;; However, this is a better solution if you're using asdf anyhow.
  ;; I still do wrap compile-file so that if it's called directly things work
  ;; Note that wrapping it twice with this
  ;; will have the same effect as once.
  (let ((*readtable* (find-joshua-readtable))
	(ji::*file-matcher-cache* nil)
	(ji::*file-semi-matcher-cache* nil)
	(ji::*file-merger-cache* nil)
	(ji::*file-semi-merger-cache* nil)
	(ji::*compiling-joshua-file* t))
    (declare (special ji::*file-matcher-cache* ji::*file-semi-matcher-cache*
		      ji::*file-merger-cache* ji::*file-semi-merger-cache*
		      ji::*compiling-joshua-file*))
    (call-next-method)))

(defmethod asdf:perform :around ((operation load-op) (c joshua-file))
  ;; here we just need to make sure it's using the right readtable
  (let ((*readtable* (find-joshua-readtable)))
    (call-next-method)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; I'm not sure where to put this really
;;; It's really an extension to asdf that's independent of joshua
;;; But for the moment the only place I really care about it is in
;;; Aplan which I broke down into subsystems
;;; The problem is that my patches to clim-listener uses
;;; this to determine all the files in a system and joshua
;;; need not be loaded before clim-listener
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export (intern (string-upcase "subsystem") 'asdf/system) 'asdf/system))

(defclass asdf/system:subsystem (asdf:system) ())
