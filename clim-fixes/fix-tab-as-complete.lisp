;;; -*- Model: Common-lisp; package: clim-utils; -*-

(in-package :clim-utils)

;;; from /utils/lisp-utilities.lisp

;;; Characters that are ordinary text rather than potential input editor commands.
;;; Note that GRAPHIC-CHAR-P is true of #\Space
(defun ordinary-char-p (char)
  (and (eql char (code-char (char-code char))) ; false for #\control-c
       (or (graphic-char-p char)
	   ;; For characters, CHAR= and EQL are the same.  Not true of EQ!
	   (eql char #\Newline)
	   (eql char #\Return)
	   ;; (eql char #\Tab) I want tab to be a gesture
	   )))

;;; from input-editor-commands.lisp
(in-package :clim-internals)
(defparameter *completion-gestures* '(:complete :tab))

;;; from completer.lisp
(defparameter *magic-completion-gestures*
        (append *completion-gestures* *help-gestures*
                *possibilities-gestures* *apropos-possibilities-gestures*))