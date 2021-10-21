;;; -*- Mode: Lisp; Package: DREI-LISP-SYNTAX -*-

;;;  (c) copyright 2005, 2014 by
;;;           Robert Strandh (robert.strandh@gmail.com)
;;;  (c) copyright 2006-2007 by
;;;           Troels Henriksen (athas@sigkill.dk)
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

;;; A syntax module for analysing Common Lisp using an LR based
;;; parser.




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Joshua syntax in DREI
;;; I wish that none of this were necessary but it is
;;;
;;; Joshua users 3 macro characters to define its syntax
;;; in the original CLIM that was all that was necessary
;;; its input editor was fine with that.
;;;
;;; But in McClim input editing is handled with DREI
;;; which has some virtues, but also some costs:
;;;
;;; For these three pieces of syntax with have to define
;;; lexer and parer rules.  And form to object rules.
;;; The lexer and parser stuff is relatively straightforward
;;; The form-to-object rules simply get the string corresponding
;;; to the form (i.e. the parse-tree) and then
;;; calls the joshua reader on the string.
;;; I originally tried walking the parse tree
;;; but the way that drei handles backquote is quite
;;; different from the way joshua does.
;;;
;;;
;;; The "right" way to do this would be to:
;;; 1) Define a joshua syntax as a direct sub-class of lisp-syntax
;;; 2) Make everything below use joshua-syntax not lisp syntax
;;; 3) change the accept :around methods in Libraries/DREI/input-editor.lisp to
;;;    selectively use lisp or joshua syntax (global variable in DREI for syntax to use
;;;    set by enable joshua in joshua)
;;; 4) Encapsulate constituentp to check this varible and return either the
;;;    original or the joshua version
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(in-package :drei-base)

;;; This is necessary so that [ ] ? are treated as special characters
;;; that terminate a token.  When not in joshua syntax this should use
;;; the original list.

;; In ~/quicklisp/local-projects/McCLIM/Libraries/Drei/base.lisp:

(defparameter *joshua-constituent-characters*
  '(#\! #\$ #\% #\& #\* #\+ #\- #\. #\/
    #\: #\< #\= #\> #\@ #\^ #\~ #\_
    #\{ #\} #\#))

(defparameter *lisp-constituent-characters*
 '(#\! #\$ #\% #\& #\* #\+ #\- #\. #\/
                         #\: #\< #\= #\> #\? #\@ #\^ #\~ #\_
                         #\{ #\} #\[ #\] #\#))

(let ((sb-ext:*muffled-warnings* 'sb-kernel:redefinition-warning))
  (defun constituentp (obj)
    "A predicate to ensure that an object is a constituent character."
    (and (characterp obj)
         ;; #+sbcl (sb-impl::constituentp obj)
         (or (alphanumericp obj)
             (member obj
                     (if joshua:*in-joshua-mode* *joshua-constituent-characters* *lisp-constituent-characters*)))))
  )

;;; in ~Libraries/Drei/input-editor.lisp`

(in-package :drei)



(defun appropriate-lisp-syntax ()
  (If (eql joshua:*in-joshua-mode* t)
      "Joshua"
      "Lisp"))

(let ((sb-ext:*muffled-warnings* 'sb-kernel:redefinition-warning))
(define-presentation-method accept :around
  ((type command-or-form)
   (stream drei-input-editing-mixin)
   view &key)
  (with-drei-options ((drei-instance stream)
                      :syntax (appropriate-lisp-syntax)
                      :keep-syntax t)
    (call-next-method)))

(define-presentation-method accept :around
  ((type command)
   (stream drei-input-editing-mixin)
   view &key)
  (with-drei-options ((drei-instance stream)
                      :syntax "Fundamental"
                      :keep-syntax nil)
    (call-next-method)))

(define-presentation-method accept :around
  ((type expression)
   (stream drei-input-editing-mixin)
   view
   &key)
  (with-drei-options ((drei-instance stream)
                      :syntax (appropriate-lisp-syntax)
                      :keep-syntax t)
    (redraw-input-buffer stream)
    (call-next-method))))



(in-package :drei-lisp-syntax)

(define-syntax joshua-syntax (lisp-syntax)
  ()
  (:name "Joshua")
  (:pathname-types "lisp" "lsp" "cl")
  (:command-table lisp-table)
  (:default-initargs :initial-state |initial-state |))

;;In ~/quicklisp/local-projects/McCLIM/Libraries/Drei/lisp-syntax.lisp


;; Add lexeme type for #\[ and #\]

(defclass left-bracket-lexeme (parenthesis-lexeme) ())
(defclass right-bracket-lexeme (parenthesis-lexeme) ())
(defclass unmatched-right-bracket-lexeme (lisp-lexeme) ())

(defclass logic-variable-lexeme (parenthesis-lexeme) ())
(defclass logic-variable-termination-lexeme (parenthesis-lexeme) ())

;; modify top-level lex method with a case for #\[ that produces left-bracket-lexeme

;; lex method for lexr-list-state

;; when get #\] add right-bracket-lexeme

(defmethod lex ((syntax joshua-syntax) (state lexer-toplevel-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (let ((object (object-after scan)))
      (case object
        (#\( (fo) (make-instance 'left-parenthesis-lexeme))
        (#\) (fo) (make-instance 'unmatched-right-parenthesis-lexeme))
        (#\[ (fo) (make-instance 'left-bracket-lexeme)) ;this is for joshua
        (#\] (fo) (make-instance 'unmatched-right-bracket-lexeme)) ;this is for joshua
        (#\' (fo) (make-instance 'quote-lexeme))
        (#\? (fo) (make-instance 'logic-variable-lexeme)) ;this is for joshua
        (#\; (fo)
             (loop until (or (end-of-buffer-p scan)
                             (end-of-line-p scan)
                             (not (eql (object-after scan) #\;)))
                   do (fo))
             (make-instance 'line-comment-start-lexeme))
        (#\" (fo) (make-instance 'string-start-lexeme))
        (#\` (fo) (make-instance 'backquote-lexeme))
        (#\, (fo)
             (cond ((end-of-buffer-p scan)
                    (make-instance 'incomplete-lexeme))
                   (t
                    (case (object-after scan)
                      (#\@ (fo) (make-instance 'comma-at-lexeme))
                      (#\. (fo) (make-instance 'comma-dot-lexeme))
                      (t (make-instance 'comma-lexeme))))))
        (#\# (fo)
             (cond ((end-of-buffer-p scan)
                    (make-instance 'incomplete-lexeme))
                   (t
                    (let ((prefix 0))
                      (loop until (end-of-buffer-p scan)
                            while (and (characterp (object-after scan))
                                       (digit-char-p (object-after scan)))
                            do (setf prefix
                                     (+ (* 10 prefix)
                                        (digit-char-p (object-after scan))))
                               (fo))
                    (if (or (end-of-buffer-p scan)
                            (not (characterp (object-after scan))))
                        (make-instance 'incomplete-lexeme)
                        (case (object-after scan)
                          ((#\Backspace #\Tab #\Newline #\Linefeed
                                        #\Page #\Return #\Space #\) #\]) ;this is for joshua
                           (fo)
                           (make-instance 'error-lexeme))
                          (#\\ (fo)
                               (cond ((or (end-of-buffer-p scan)
                                          (not (characterp (object-after scan))))
                                      (make-instance 'incomplete-character-lexeme))
                                     ((not (constituentp (object-after scan)))
                                      (fo) (make-instance 'complete-character-lexeme))
                                     (t (loop until (end-of-buffer-p scan)
                                           while (constituentp (object-after scan))
                                           do (fo))
                                        (make-instance 'complete-character-lexeme))))
                          (#\' (fo)
                               (make-instance 'function-lexeme))
                          (#\( (fo)
                               (make-instance 'simple-vector-start-lexeme))
                          (#\* (fo)
                               (loop until (end-of-buffer-p scan)
                                  while (or (eql (object-after scan) #\1)
                                            (eql (object-after scan) #\0))
                                  do (fo))
                               (if (and (not (end-of-buffer-p scan))
                                        (constituentp (object-after scan)))
                                   (make-instance 'error-lexeme)
                                   (make-instance 'bit-vector-form)))
                          (#\: (fo)
                               (make-instance 'uninterned-symbol-lexeme))
                          (#\. (fo)
                               (make-instance 'readtime-evaluation-lexeme))
                          ((#\B #\b #\O #\o #\X #\x)
                           (let ((radix
                                  (ecase (object-after scan)
                                    ((#\B #\b) 2)
                                    ((#\O #\o) 8)
                                    ((#\X #\x) 16))))
                             (fo)
                             (when (and (not (end-of-buffer-p scan))
                                        (char= (object-after scan)
                                               #\-))
                               (fo))
                             (loop until (end-of-buffer-p scan)
                                while (digit-char-p (object-after scan) radix)
                                do (fo)))
                           (if (and (not (end-of-buffer-p scan))
                                    (constituentp (object-after scan)))
                               (make-instance 'error-lexeme)
                               (make-instance 'number-lexeme)))
                          ((#\R #\r)
                           (fo)
                           (cond
                             ((<= 2 prefix 36)
                              (loop until (end-of-buffer-p scan)
                                 while (and (characterp (object-after scan))
                                            (digit-char-p (object-after scan) prefix))
                                 do (fo))
                              (if (and (not (end-of-buffer-p scan))
                                       (constituentp (object-after scan)))
                                  (make-instance 'error-lexeme)
                                  (make-instance 'number-lexeme)))
                             (t (make-instance 'error-lexeme))))
                                        ;((#\C #\c) )
                          ((#\A #\a) (fo)
                           (make-instance 'array-start-lexeme))
                          ((#\S #\s) (fo)
                           (cond ((and (not (end-of-buffer-p scan))
                                       (eql (object-after scan) #\())
                                  (fo)
                                  (make-instance 'structure-start-lexeme))
                                 ((end-of-buffer-p scan)
                                  (make-instance 'incomplete-lexeme))
                                 (t (make-instance 'error-lexeme))))
                          ((#\P #\p) (fo)
                           (make-instance 'pathname-start-lexeme))
                          (#\= (fo)
                               (make-instance 'sharpsign-equals-lexeme))
                          (#\# (fo)
                               (make-instance 'sharpsign-sharpsign-form))
                          (#\+ (fo)
                               (make-instance 'reader-conditional-positive-lexeme))
                          (#\- (fo)
                               (make-instance 'reader-conditional-negative-lexeme))
                          (#\| (fo)
                               (make-instance 'long-comment-start-lexeme))
                          (#\< (fo)
                               (make-instance 'error-lexeme))
                          (t (fo) (make-instance 'undefined-reader-macro-lexeme))))))))
        (#\| (fo) (make-instance 'multiple-escape-start-lexeme))
        (t (cond ((or (constituentp object)
                      (eql object #\\))
                  (lex-token syntax scan))
                 (t (fo) (make-instance 'literal-object-form))))))))

;;; for Joshua
(defmethod lex ((syntax joshua-syntax) (state lexer-list-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (let ((object (object-after scan)))
      (case object
        (#\) (fo) (make-instance 'right-parenthesis-lexeme))
        (#\] (fo) (make-instance 'right-bracket-lexeme))
        (t (call-next-method))))))



;;; predication forms, pretty much like lists except with brackets

(defclass pred-form (form) ())
(defclass complete-pred-form (pred-form complete-form-mixin) ())
(defclass incomplete-pred-form (pred-form incomplete-form-mixin) ())

(define-parser-state open-pred-state (lexer-list-state form-may-follow) ())
(define-parser-state closed-pred-state (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow left-bracket-lexeme)  open-pred-state)
;;; Note: this has to be FORM for the second term (i.e. once you're accepting a pred
;;; and form advances, not just pred forms
(define-new-lisp-state (open-pred-state form) open-pred-state)
(define-new-lisp-state (open-pred-state comment) open-pred-state)
(define-new-lisp-state (open-pred-state right-bracket-lexeme) closed-pred-state)

;;; reduce according to the rule pred -> ( pred* )
(define-lisp-action (closed-pred-state t)
  (reduce-until-type complete-pred-form left-bracket-lexeme))

;;; reduce at the end of the buffer
(define-lisp-action (open-pred-state (eql nil))
  (reduce-until-type incomplete-pred-form left-bracket-lexeme t))




;;; logic variable
(defclass logic-variable-form (form) ())
(defclass complete-logic-variable-form (logic-variable-form complete-form-mixin) ())
(defclass incomplete-logic-variable-form (logic-variable-form incomplete-form-mixin) ())

(define-parser-state open-lv-state (form-may-follow) ())
(define-parser-state open-lv-with-complete-form (lexer-toplevel-state parser-state) ())
(define-parser-state open-lv-with-incomplete-form (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow logic-variable-lexeme) open-lv-state)
(define-new-lisp-state (open-lv-state complete-form-mixin) open-lv-with-complete-form)
(define-new-lisp-state (open-lv-state incomplete-form-mixin) open-lv-with-incomplete-form)
(define-new-lisp-state (open-lv-state comment) open-lv-state)
(define-new-lisp-state (open-lv-state unmatched-right-parenthesis-lexeme) |( form* ) |)

;;; reduce according to the rule form -> ? form
(define-lisp-action (open-lv-with-complete-form t)
  (reduce-until-type complete-logic-variable-form logic-variable-lexeme))

(define-lisp-action (open-lv-with-incomplete-form t)
  (reduce-until-type incomplete-logic-variable-form logic-variable-lexeme))

(define-lisp-action (open-lv-state right-parenthesis-lexeme)
  (reduce-until-type incomplete-logic-variable-form logic-variable-lexeme))

(define-lisp-action (open-lv-state unmatched-right-parenthesis-lexeme)
  (reduce-until-type incomplete-logic-variable-form logic-variable-lexeme))

(define-lisp-action (open-lv-state (eql nil))
  (reduce-until-type incomplete-logic-variable-form logic-variable-lexeme t))

(define-lisp-action (open-lv-state logic-variable-termination-lexeme)
  (reduce-until-type complete-logic-variable-form logic-variable-lexeme))

;;; This Is here so that we don't ignore whitespace but treate
;;; it as a delimiter if seen right after a ?
(defmethod skip-inter ((syntax joshua-syntax) (state open-lv-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (if (end-of-buffer-p scan)
        nil
        t)))

(defmethod lex ((syntax joshua-syntax) (state open-lv-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (let ((object (object-after scan)))
      (if (or (member object '(#\] #\)))
              (whitespacep syntax object))
          (make-instance 'logic-variable-termination-lexeme)
          (call-next-method)))))


;;; The easiest way to deal with things defined by reader macros
;;; is to just get the string corresponding to the form (e.g. backquote, predication, logic-variable)

(defmethod form-to-joshua-object ((syntax joshua-syntax) (form form)
                                  &key &allow-other-keys)
  (let* ((string (form-string syntax form))
         (joshua-form (let* ((*readtable* ji::*joshua-readtable*))
                        (read-from-string string))))
    joshua-form))


(defmethod form-to-object :around ((syntax joshua-syntax) (form backquote-form)
                                   &key &allow-other-keys)
  (destructuring-bind (backquote-lexeme internal-form)  (children form)
    (declare (ignore backquote-lexeme internal-form))
    ;; actually joshua does this for all backquotes
    (form-to-joshua-object syntax form)))


(defmethod form-to-object ((syntax joshua-syntax) (form pred-form)
                           &key &allow-other-keys)
  (form-to-joshua-object syntax form))


(defmethod form-to-object ((syntax joshua-syntax) (form logic-variable-form)
                           &key &allow-other-keys)
  (form-to-joshua-object syntax form))
