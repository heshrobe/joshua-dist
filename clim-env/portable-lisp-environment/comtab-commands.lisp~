;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-ENV; Base: 10; Lowercase: Yes -*-

;;; Copyright (c) 1994-2000, Scott McKay.
;;; Copyright (c) 2001-2003, Scott McKay and Howard Shrobe.
;;; All rights reserved.  No warranty is expressed or implied.
;;; See COPYRIGHT for full copyright and terms of use.

(in-package :clim-env)

;;; Commands for examining command tables!

(define-presentation-type presentation-translator-presentation 
                          (&optional command-table-name))

(define-presentation-type menu-item-presentation ())

(define-presentation-type command-menu-item-presentation ()
   :inherit-from 'menu-item-presentation)

(define-presentation-type function-menu-item-presentation ()
   :inherit-from 'menu-item-presentation)

(define-presentation-type menu-menu-item-presentation ()
   :inherit-from 'menu-item-presentation)

(define-presentation-type divider-menu-item-presentation ()
   :inherit-from 'menu-item-presentation)

(defvar *menu-item-type-presentation-alist*
       '((:command  . command-menu-item-presentation)
	 (:function . function-menu-item-presentation)
	 (:menu     . menu-menu-item-presentation)
	 (:divider  . divider-menu-item-presentation)))

(declaim (inline menu-type-to-ptype))
(defun menu-type-to-ptype (menu-type)
  (cdr (assoc menu-type *menu-item-type-presentation-alist*)))



(define-command (com-show-comtab-commands :command-table comtabs
                                          :name "Show Command Table Commands")
    ((comtab 'command-table :prompt "command table")
     &key
     (show-args 'boolean :default nil :mentioned-default t
                         :documentation "Show arguments to each command")
     (show-name 'boolean :default nil :mentioned-default t
                         :documentation "Show command-line name of each command")
     (inherited 'boolean :default nil :mentioned-default t
                         :documentation "Include commands inherited from other command tables"))
   (when show-args
     (cerror "Go ahead without args" "Sorry, don't know how to get args yet."))
   (let ((stream *standard-output*)
         the-comtab-name
         (found-some nil)
	 (tables-so-far ()))
     (formatting-table (stream)
        (flet ((printer (com)
                 (setq found-some t)
                 (with-output-as-presentation
                     (stream com `(command-name :command-table ,the-comtab-name)
                             :single-box t)
		   (formatting-row (stream)
                     (formatting-cell (stream)
                       (princ com stream))
		     (when show-name
		       (formatting-cell (stream)
	                 (let ((com-name (command-line-name-for-command com comtab)))
			   (when com-name
			     (write-string com-name stream)))))))))
          (declare (dynamic-extent #'printer))
          (if inherited
	    (do-command-table-inheritance (ct comtab)
	      (unless (member ct tables-so-far)
		(push ct tables-so-far)
		(formatting-row (stream)
		  (formatting-cell (stream :align-x :center)
		    (with-output-as-presentation (stream ct 'command-table)
		      (with-text-style (stream '(nil :bold :smaller))
			(write-string (symbol-name (command-table-name ct))
				      stream)))))
		(setq the-comtab-name (command-table-name ct))
		(map-over-command-table-commands #'printer ct :inherited nil)))
	    (progn
	      (setq the-comtab-name (command-table-name comtab))
	      (map-over-command-table-commands #'printer comtab :inherited nil)))))
     (unless found-some
       (format stream "~%No commands found."))))

(define-presentation-to-command-translator comtab-to-show-commands
    (command-table com-show-comtab-commands comtabs
                   :gesture nil)
    (object)
  `(,object))

(define-command (com-show-comtab-translators :command-table comtabs
                                             :name "Show Command Table Translators")
    ((comtab 'command-table :prompt "command table")
     &key
     (show-name 'boolean :default nil :mentioned-default t
		:documentation "Show name of each translator")
     (inherited 'boolean :default nil :mentioned-default t
		:documentation "Include translators inherited from other command tables"))
   (let ((stream *standard-output*)
         the-comtab-name
         (found-some nil)
	 (tables-so-far ()))
     (formatting-table (stream)
       (with-text-face (stream :italic)
         (formatting-row (stream)
           (when show-name
             (formatting-cell (stream)
	       (write-string "Name" stream)))
           (formatting-cell (stream)
	     (write-string "Gesture" stream))
           (formatting-cell (stream)
	     (declare (ignore stream)))
           (formatting-cell (stream)
	     (write-string "From" stream))
           (formatting-cell (stream)
	     (declare (ignore stream)))
           (formatting-cell (stream)
	     (write-string "To" stream))))
       (flet ((printer (transl)
                (setq found-some t)
                (with-output-as-presentation
		    (stream transl `(presentation-translator-presentation ,the-comtab-name)
                            :single-box t)
		  (let ((gesture (clim-internals::presentation-translator-gesture-name transl)))
                    (formatting-row (stream)
                      (when show-name
                        (formatting-cell (stream)
                          (princ (clim-internals::presentation-translator-name transl)
                                 stream)))
                      (formatting-cell (stream :align-x :right)
                        (when gesture (princ gesture stream)))
                      (formatting-cell (stream)
                        (when gesture (write-string "x" stream)))
                      (formatting-cell (stream :align-x :left)
                        (princ (clim-internals::presentation-translator-from-type transl)
                               stream))
                      (formatting-cell (stream)
                        (write-string "->" stream))
                      (formatting-cell (stream)
                        (princ (clim-internals::presentation-translator-to-type transl)
                               stream)))))))
	 (declare (dynamic-extent #'printer))
         (if inherited
	   (do-command-table-inheritance (ct comtab)
	     (unless (member ct tables-so-far)
	       (push ct tables-so-far)
	       (setq the-comtab-name (command-table-name ct))
	       (formatting-row (stream)
		 (formatting-cell (stream :align-x :center)
		   (with-output-as-presentation (stream ct 'command-table)
		     (with-text-style (stream '(nil :bold :smaller))
		       (write-string (symbol-name the-comtab-name) stream)))))
	       (map-over-command-table-translators #'printer ct :inherited nil)))
	   (progn
	     (setq the-comtab-name (command-table-name comtab))
	     (map-over-command-table-translators #'printer comtab :inherited nil)))))
     (unless found-some
       (format stream "~%No translators found."))))

(define-presentation-to-command-translator comtab-to-show-translators
    (command-table com-show-comtab-translators comtabs
                   :gesture nil)
    (object)
  `(,object))

(define-command (com-show-comtab-inheritance :command-table comtabs
                                             :name "Show Command Table Inheritance")
    ((comtab 'command-table :prompt "command table"))
   (let ((stream *standard-output*))
     (labels ((print-it (comtab indents)
                (fresh-line stream)
                (dotimes (i indents) (write-char #\space stream))
                (with-output-as-presentation (stream comtab 'command-table)
                  (write-string (symbol-name (command-table-name comtab)) stream))
                (let ((new-indents (+ indents 2)))
                  (dolist (ct (command-table-inherit-from comtab))
                    (print-it (find-command-table ct) new-indents)))))
       (declare (dynamic-extent #'print-it))
       (print-it comtab 0))))

(define-presentation-to-command-translator comtab-to-show-inheritance
    (command-table com-show-comtab-inheritance comtabs
                   :gesture nil)
    (object)
  `(,object))

;;; Some shared printers for show-comtab-menu and show-comtab-keystrokes

(defun print-keystroke (key stream)
  (if key
    (clim-internals::describe-gesture-spec key :stream stream :brief nil)
    (write-string "no key" stream)))

(define-command (com-show-comtab-menu :command-table comtabs
                                      :name "Show Command Table Menu")
    ((comtab 'command-table :prompt "command table")
     &key
     (show-submenus 'boolean :default t
                    :documentation "Show contents of sub-menus")
     (show-keystrokes 'boolean :default nil :mentioned-default t
		      :documentation "Show keystroke accelerator for each menu-item")
     (show-type 'boolean :default nil :mentioned-default t
		:documentation "Show item-type for each menu-item")
     (show-value 'boolean :default nil :mentioned-default t
		 :documentation "Show value for each menu-item"))
   (let ((stream *standard-output*)
         (indent-level 0))
     (formatting-table (stream)
       (labels ((printer (name key item)
                  (let* ((item-type (command-menu-item-type item))
                         (item-value (command-menu-item-value item))
                         (ptype (menu-type-to-ptype item-type)))
                    (with-output-as-presentation (stream (list name comtab) ptype
                                                         :single-box t)
                      (formatting-row (stream)
                        (formatting-cell (stream)
                          (dotimes (i indent-level) (write-char #\space stream))
                          (write-string (if (symbolp name) (symbol-name name) name) stream))
                        (when show-keystrokes
                          (formatting-cell (stream)
                            (print-keystroke key stream)))
                        (when show-type
                          (formatting-cell (stream)
                            (write-string (symbol-name item-type) stream)))
                        (when show-value
                          (formatting-cell (stream)
                            (princ item-value stream)))))
                    (when (and show-submenus (eql item-type :menu))
                      (incf indent-level 2)
                      (map-over-command-table-menu-items #'printer item-value)
                      (decf indent-level 2)))))
         (declare (dynamic-extent #'printer))
         (map-over-command-table-menu-items #'printer comtab)))))

(define-presentation-to-command-translator comtab-to-show-menu
    (command-table com-show-comtab-menu comtabs
                   :gesture nil)
    (object)
  `(,object))

(define-command (com-show-comtab-keystrokes :command-table comtabs
                                            :name "Show Command Table Keystrokes")
    ((comtab 'command-table :prompt "command table")
     &key
     (inherited 'boolean :default nil :mentioned-default t
		:documentation "Include keystrokes inherited from other command tables"))
   (let ((stream *standard-output*)
         the-comtab-name
         (found-some-keys nil)
	 (tables-so-far ()))
     (formatting-table (stream)
       (flet ((printer (name key item)
                (setq found-some-keys t)
                (let* ((item-type (command-menu-item-type item))
                       (item-value (command-menu-item-value item))
                       (ptype (menu-type-to-ptype item-type)))
                  (with-output-as-presentation (stream (list key comtab) ptype
                                                       :single-box t)
                    (formatting-row (stream)
                      (formatting-cell (stream)
                        (print-keystroke key stream))
                      (formatting-cell (stream)
                        (write-string name stream))
                      (formatting-cell (stream)
                        (format stream "~S" item-value)))))))
	 (declare (dynamic-extent #'printer))
         (if inherited
	   (do-command-table-inheritance (ct comtab)
	     (unless (member ct tables-so-far)
	       (push ct tables-so-far)
	       (setq the-comtab-name (command-table-name ct))
	       (formatting-row (stream)
		 (formatting-cell (stream :align-x :center)
		   (with-output-as-presentation (stream ct 'command-table)
		     (with-text-style (stream '(nil :bold :smaller))
		       (write-string (symbol-name the-comtab-name) stream)))))
	       (map-over-command-table-keystrokes #'printer ct)))
	   (progn
	     (setq the-comtab-name (command-table-name comtab))
	     (map-over-command-table-keystrokes #'printer comtab)))))
     (unless found-some-keys
       (format stream "~%No keystrokes found."))))

(define-presentation-to-command-translator comtab-to-show-keystrokes
    (command-table com-show-comtab-keystrokes comtabs
                   :gesture nil)
    (object)
  `(,object))



;;; Undefining / removing

(define-command (com-remove-command-table :command-table comtabs 
                                          :name "Remove Command Table")
    ((command-table 'command-table))
  (clim-internals::remove-command-table
    (if (symbolp command-table)
      command-table
      (command-table-name command-table))))

(define-presentation-to-command-translator remove-this-command-table
    (command-table com-remove-command-table comtabs
                   :gesture :delete)
    (object)
  `(,object))


(define-command (com-remove-command :command-table comtabs 
                                    :name "Remove Command Name")
    ((command-name 'command-name)
     (command-table 'command-table))
  (remove-command-from-command-table
    command-name
    (if (symbolp command-table)
      (find-command-table command-table)
      command-table)))

(define-presentation-to-command-translator remove-this-command
    (command-name com-remove-command comtabs
                  :gesture :delete)
    (object presentation)
  (destructuring-bind (translator &key command-table) (presentation-type presentation)
    (declare (ignore translator))
    `(,object ,(find-command-table command-table))))


(define-command (com-remove-presentation-translator :command-table comtabs
		                        :name "Remove Presentation Translator")
    ((translator 'presentation-translator-presentation)
     (command-table 'command-table))
  (remove-presentation-translator-from-command-table
    (if (symbolp command-table)
      (find-command-table command-table)
      command-table)
    (clim-internals::presentation-translator-name translator)
    :errorp nil))

(define-presentation-to-command-translator remove-this-translator
    (presentation-translator-presentation
     com-remove-presentation-translator
     comtabs
     :gesture :delete
     :tester ((presentation) (consp (presentation-type presentation))))
    (object presentation)
  (destructuring-bind (translator &optional comtab-name) (presentation-type presentation)
    (declare (ignore translator))
    `(,object ,(find-command-table comtab-name))))


(define-command (com-remove-menu-item :command-table comtabs
		                      :name "Remove Menu Item")
    ((menu-item 'menu-item-presentation)
     (command-table 'command-table))
  (remove-menu-item-from-command-table
    (if (symbolp command-table)
      (find-command-table command-table)
      command-table)
    menu-item
    :errorp nil))

(define-presentation-to-command-translator remove-this-menu-item
    (menu-item-presentation com-remove-menu-item comtabs
                            :gesture :delete)
    (object)
  (destructuring-bind (item-name comtab-name) object
    `(,item-name ,comtab-name)))


(define-command (com-remove-keystroke-accelerator
                   :command-table comtabs
		   :name "Remove Keystroke Accelerator")
    ((keystroke-item 'menu-item-presentation)
     (command-table 'command-table))
  (remove-keystroke-from-command-table
    (if (symbolp command-table)
      (find-command-table command-table)
      command-table)
    keystroke-item
    :errorp t))

(define-presentation-to-command-translator remove-this-keystroke-accelerator
    (menu-item-presentation com-remove-keystroke-accelerator comtabs
                            :gesture :delete)
    (object)
  (destructuring-bind (item-name comtab-name) object
    `(,item-name ,comtab-name)))


#||
;;; testing

(unless (find-command-table 'test-table :errorp nil)
  (make-command-table 'test-table :inherit-from '(lisp-listener)))

(define-command (com-whatsit :command-table test-table 
                             :name "Whatsit" :menu "WHAT"
                             :keystroke (:r :control))
    ()
  ())

||#
