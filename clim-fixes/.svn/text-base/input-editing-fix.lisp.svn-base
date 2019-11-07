;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: interactive-protocol.lisp,v 1.23 92/12/16 16:46:40 cer Exp $

(in-package :clim-internals)

#+mcl
(defclass input-editing-stream-mixin
	  (standard-encapsulating-stream input-editing-stream #+mcl fundamental-input-stream)
      ;; The fill-pointer of INPUT-BUFFER is the "high water" mark,
      ;; that is, it points past the last thing in the buffer.
     ((input-buffer :initform (make-array 100 :fill-pointer 0 :adjustable t)
		    :accessor input-editor-buffer)
      ;; STREAM-SCAN-POINTER  is the point at which parsing takes place.
      (scan-pointer :initform 0 :accessor stream-scan-pointer)
      ;; STREAM-INSERTION-POINTER is where the input-editing cursor is,
      ;; that is, input editing commands and insertions take place
      ;; at the insertion pointer.
      (insertion-pointer :initform 0 :accessor stream-insertion-pointer)
      ;; STREAM-RESCANNING-P is now part of the input editing stream protocol.
      ;; If T, it means that the input editor is "rescanning", that is, re-processing
      ;; input that was already typed in response to some editing command.
      (rescanning-p :accessor stream-rescanning-p :initform nil)
      (rescan-queued :initform nil)
      ;; A buffer for an activation gesture to process.  Conceptually,
      ;; the activation gesture lives at the end of the input buffer.
      (activation-gesture :initform nil)
      ;; State for prefixed commands, holds a command aarray
      (command-state :initform nil)
      ;; Absolute value of numeric argument for input editing commands
      (numeric-argument :initform nil)
      ;; Whether there's a numeric argument, and if so what sign {nil, +1, -1}
      (numeric-argument-p :initform nil)
      ;; State for passing information from c-Y/c-m-Y to m-Y
      (previous-history :initform nil)
      (previous-insertion-pointer :initform nil)
      (match-string :initform nil)
      ;; For deciding whether to do kill-ring merging, etc.
      (last-command-type :initform nil)
      ;; A mark that we can set
      (mark :initform nil)
      (scroll-after-motion-commands :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; for better control of arglist gestures

(in-package :clim-internals)

(defvar *word-start-atom-break-chars* '(#\space #\( #\) #\" ))

(defun word-start-and-end (string start-chars &optional (start 0))
  (declare (values word-start word-end colon))
  (flet ((open-paren-p (thing)
           (or (not (characterp thing))                ;noise strings and blips are delimiters
               (member thing start-chars)))
         (atom-break-char-p (thing)
           (or (not (characterp thing))                ;ditto
               (not (graphic-char-p thing))
               (multiple-value-bind (mac nt)
                   (get-macro-character thing)
                 (and mac (not nt)))
               (member thing *word-start-atom-break-chars*))))
    (declare (dynamic-extent #'open-paren-p #'atom-break-char-p))
    (let* ((word-start
             (forward-or-backward string start t #'open-paren-p))
           (word-end
             (and word-start
                  (or (forward-or-backward string (1+ word-start) nil
                                           #'atom-break-char-p)
                      (let ((fp (fill-pointer string)))
                        (and (> fp (1+ word-start)) fp)))))
           (colon
             (and word-start word-end
                  (position #\: string
                            :start (1+ word-start) :end (1- word-end)))))
      (values (and word-start
                   (if (atom-break-char-p (aref string word-start))
                       (1+ word-start)
                       word-start))
              (and word-end (1- word-end))
              colon))))

;;;;;;;;;;;;;

;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-

;; $fiHeader: ccl-streams.lisp,v 1.3 92/02/24 13:05:06 cer Exp $

(in-package :clim-utils)

#+mcl
(defmethod ccl::stream-tyi ((stream fundamental-input-stream))
  (with-input-editing (stream)
    (let ((char (stream-read-char stream)))
      (if (eql char ':eof) nil char))))