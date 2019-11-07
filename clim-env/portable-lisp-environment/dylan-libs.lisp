;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-ENV; Base: 10; Lowercase: Yes -*-

"Copyright (c) 1994 Harlequin, Inc.  All rights reserved."

(in-package :clim-env)


;;; Dylan library commands

(define-command-table libraries)

(define-command (com-compile-library :command-table libraries :name t)
    ((library 'dylan-library
              :prompt "library name")
     &key
     (condition '(member :always :changed-sources)
		:default :changed-sources
		:documentation "Which files to compile")
     (load '(member :everything :newly-compiled :only-for-dependencies :nothing)
	   :default :newly-compiled
	   :documentation "Which files to load.")
     (query '(member :yes :no :confirm)
	    :default :no :mentioned-default :yes
	    :documentation "Query about loading files") 
     (silent 'boolean
	     :default nil :mentioned-default t
	     :documentation "Suppress all terminal output"))
  (dylan::load-library library))

(define-presentation-to-command-translator compile-library
    (dylan-library com-compile-library libraries
     :gesture nil)
    (object)
  (list object))

(define-drag-and-drop-translator d&d-compile-library
    (dylan-library command compiler libraries)
    (object)
  `(com-compile-library ,object))


(define-command (com-load-library :command-table libraries :name t)
    ((library 'dylan-library
              :prompt "library name")
     &key
     (condition '(member :always :changed-sources)
		:default :changed-sources
		:documentation "Which files to load")
     (query '(member :yes :no :confirm)
	    :default :no :mentioned-default :yes
	    :documentation "Query about loading files") 
     (silent 'boolean
	     :default nil :mentioned-default t
	     :documentation "Suppress all terminal output"))
  ;;--- Implement :QUERY
  (ecase condition
    (:changed-sources
     (dylan::ensure-library library))
    (:always
     (dylan::load-library library))))

(define-presentation-to-command-translator load-library
    (dylan-library com-load-library libraries
     :gesture nil)
    (object)
  (list object))

(define-drag-and-drop-translator d&d-load-library
    (dylan-library command compiler libraries)
    (object)
  `(com-load-library ,object))


(define-command (com-show-available-libraries :command-table libraries :name t) ()
  (with-frame-standard-output (stream)
    (let ((names (dylan::available-library-names)))
      (fresh-line stream)
      (filling-output (stream :fill-width '(72 :character))
        (write-string "Available libraries are " stream)
        (format-textual-list names #'(lambda (lib stream)
                                       (present lib 'dylan-library :stream stream))
                             :stream stream :conjunction "and")
	(write-string "." stream)))))

(define-command (com-show-loaded-libraries :command-table libraries :name t) ()
  (with-frame-standard-output (stream)
    (let ((names (dylan::loaded-library-names)))
      (fresh-line stream)
      (filling-output (stream :fill-width '(72 :character))
        (write-string "Loaded libraries are " stream)
        (format-textual-list names #'(lambda (lib stream)
                                       (present lib 'dylan-library :stream stream))
                             :stream stream :conjunction "and")
        (write-string "." stream)))))

