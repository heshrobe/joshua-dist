;;; -*- package: nox; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  xml-parser.lisp
;;;
;;;
;;; ----------------------------------------------------------------------------
;;;
;;;   The contents of this file are subject to the NOKOS License Version 1.0a
;;;   (the "License"); you may not use this file except in compliance with
;;;   the License. 
;;;
;;;   Software distributed under the License is distributed on an "AS IS"
;;;   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
;;;   License for the specific language governing rights and limitations under
;;;   the License. 
;;;
;;;   The Original Software is 
;;;     WILBUR: Nokia RDF/XML Processor for CLOS
;;;
;;;   Copyright (c) 2001 Nokia and others. All Rights Reserved.
;;;   Portions Copyright (c) 1989-1992 Ora Lassila. All Rights Reserved.
;;;
;;;   Contributor(s): Ora Lassila (mailto:ora.lassila@nokia.com)
;;;                   Louis Theran <theran@pobox.com>
;;;
;;; ----------------------------------------------------------------------------
;;;
;;;
;;;   Version: $Id: xml-parser.lisp,v 1.4 2001/07/17 22:45:19 theran Exp $
;;;
;;;   Purpose: This file contains an implementation of an XML parser. This
;;;   parser was motivated by RDF, and consequently does not implement all the
;;;   features of XML 1.0. In fact, it needs a lot of work. Tough...
;;;


(in-package :nox)


;;; ----------------------------------------------------------------------------
;;;
;;;   NAME READTABLE
;;;

(defparameter *name-reader* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant -name-start-characters-
    (let ((s (concatenate 'string
                          (loop for i from (char-code #\a) to (char-code #\z)
                                collect (code-char i)))))
      (concatenate 'string s (string-upcase s) "_:")))
  (defconstant -name-characters-
    (let ((v (make-array 256)))
      (dotimes (i 256)
        (setf (svref v i) nil))
      (dolist (c (concatenate 'list
                              -name-start-characters-
                              (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                                    #\. #\-)))
        (setf (svref v (char-code c)) t))
      v)))

(defvar *nr-buffer* (make-base-string 256
                                      :adjustable t
                                      :fill-pointer 0))

(defun name-reader (stream char)
  (setf (fill-pointer *nr-buffer*) 0)
  (vector-push char *nr-buffer*)
  (with-loop&read-char (c stream)
    (cond ((svref -name-characters- (char-code c))
           (vector-push-extend c *nr-buffer*))
          (t
           (unread-char c stream)
           (return (concatenate 'string *nr-buffer*))))))

(defun single-character-reader (stream char)
  (declare (ignore stream))
  char)

(defun not-allowed-reader (stream char)
  (declare (ignore stream))
  (error 'syntax-error :thing char))

(eval-when (:load-toplevel :execute)
  (setf *name-reader* (copy-readtable nil))
  (dotimes (i 256)
    (let ((c (code-char i)))
      (unless (whitespace-char-p c)
        (set-macro-character c #'not-allowed-reader nil *name-reader*))))
  (set-macro-character #\/ #'single-character-reader nil *name-reader*)
  (set-macro-character #\! #'name-reader nil *name-reader*)
  (set-macro-character #\? #'name-reader nil *name-reader*)
  (map nil #'(lambda (c)
               (set-macro-character c #'name-reader nil *name-reader*))
       -name-start-characters-))


;;; ----------------------------------------------------------------------------
;;;
;;;   XML READTABLE
;;;

(defparameter *xml-reader* nil)

(defun read-declaration (stream name)
  (declare (special *dtd-reader*))      ; forward ref
  (cond ((string= name "!DOCTYPE")
         (let ((name (read-using *name-reader* stream t))
               (next (skip-whitespace stream)))
           (cond ((not (eql next #\[))
                  (make-instance 'dtd-start
                    :string name :externalp t
                    :stuff (read-delimited-list #\> stream t)))
                 (t
                  (setf (parser-in-dtd-p *current-parser*) t
                        (parser-readtable *current-parser*) *dtd-reader*)
                  (skip-whitespace stream t) ; skip [
                  (make-instance 'dtd-start :string name)))))
        ((string= name "!")
         (let ((char (read-char stream t nil t)))
           (cond ((char= char #\[) ; CDATA, INCLUDE, IGNORE
                  (let ((name (read-until-char stream #\[)))
                    (cond ((string= name "CDATA")
                           (read-until-%%> stream #\]))
                          ((find name '("INCLUDE" "IGNORE") :test #'string=)
                           (error 'feature-not-supported :thing name))
                          (t
                           (error 'syntax-error :thing "!["))))))))
        ((string= name "!--")
         (make-instance 'comment :string (read-until-%%> stream #\-)))
        (t
         (error 'unknown-declaration :thing name))))

(defun open-anglebracket-reader (stream char)
  (declare (ignore char))
  (let ((name (read-using *name-reader* stream t)))
    (cond ((eql name #\/)
           (make-instance 'close-tag
             :string (first (read-delimited-list #\> stream t))))
          ((char= (char name 0) #\!)
           (read-declaration stream name))
          ((char= (char name 0) #\?)
           (let* ((stuff (read-delimited-list #\> stream t)))
             (if (eql (first (last stuff)) #\?)
               (make-instance 'proc-instruction :string name) ; ignore attrs
               (error 'pi-termination-problem :thing name))))
          (t
           (let ((stuff (read-delimited-list #\> stream t))
                 (tag (make-instance 'open-tag :string name))
                 (attr nil))
             (loop (cond ((null stuff)
                          (return tag))
                         ((eql (setf attr (pop stuff)) #\/)
                          (setf (tag-empty-p tag) t)
                          (return tag))
                         ((eql (pop stuff) #\=)
                          (setf (tag-attribute tag attr) (pop stuff)))
                         (t
                          (error 'syntax-error :thing "missing =")))))))))

(defun quoted-string-reader (stream char)
  (read-until-char-expanding-entities stream char nil))

(defun read-xml-token (stream &aux (char (peek-char t stream nil nil)))
  (when char
    (if (or (char= char #\<)
            (and (char= char #\])
                 (parser-in-dtd-p *current-parser*)))
      (read-using (parser-readtable *current-parser*) stream)
      (read-until-char-expanding-entities stream #\< t))))

(eval-when (:load-toplevel :execute)
  (setf *xml-reader* (copy-readtable nil))
  (dotimes (i 256)
    (let ((c (code-char i)))
      (unless (whitespace-char-p c)
        (set-macro-character c #'not-allowed-reader nil *xml-reader*))))
  (set-macro-character #\< #'open-anglebracket-reader nil *xml-reader*)
  (set-macro-character #\> (get-macro-character #\)) nil *xml-reader*)
  (set-macro-character #\= #'single-character-reader nil *xml-reader*)
  (set-macro-character #\/ #'single-character-reader nil *xml-reader*)
  (set-macro-character #\? #'single-character-reader nil *xml-reader*)
  (set-macro-character #\' #'quoted-string-reader nil *xml-reader*)
  (set-macro-character #\" #'quoted-string-reader nil *xml-reader*)
  (map nil #'(lambda (c)
               (set-macro-character c #'name-reader nil *xml-reader*))
       -name-start-characters-))


;;; ----------------------------------------------------------------------------
;;;
;;;   DTD READTABLE
;;;

(defparameter *dtd-reader*  nil)

(defun dtd-open-anglebracket-reader (stream char)
  (declare (ignore char))
  (let ((name (read-using *name-reader* stream t))
        (stuff (read-delimited-list #\> stream t)))
    (cond ((string= name "!ENTITY")
           (make-instance 'entity-declaration
             :name (pop stuff) :string (pop stuff)))
          ((string= name "!ELEMENT")
           (make-instance 'element-declaration
             :name (pop stuff) :contentspec (pop stuff)))
          ((string= name "!ATTLIST")
           (make-instance 'attlist-declaration
             :name (pop stuff)))
          ((string= name "!NOTATION")
           (error 'feature-not-supported :thing name))
          (t
           (error 'unknown-declaration :thing name)))))

(defun dtd-parenthesis-reader (stream char)
  (declare (ignore char))
  (read-delimited-list #\) stream t))

(defun close-bracket-reader (stream char)
  (declare (ignore char))
  (cond ((not (parser-in-dtd-p *current-parser*))
         (error 'syntax-error :thing "]"))
        ((not (char= (skip-whitespace stream t) #\>))
         (error 'dtd-termination-problem))
        (t
         (setf (parser-readtable *current-parser*) *xml-reader*)
         (make-instance 'dtd-end))))

(eval-when (:load-toplevel :execute)
  (setf *dtd-reader* (copy-readtable *xml-reader*))
  (set-macro-character #\< #'dtd-open-anglebracket-reader nil *dtd-reader*)
  (set-macro-character #\# (get-macro-character #\A) nil *dtd-reader*)
  (set-macro-character #\] #'close-bracket-reader nil *dtd-reader*)
  (set-macro-character #\( #'dtd-parenthesis-reader nil *dtd-reader*)
  (set-macro-character #\) (get-macro-character #\)) nil *dtd-reader*))


;;; ----------------------------------------------------------------------------
;;;
;;;   CLASS XML-PARSER
;;;

(defclass xml-parser (sax-producer)
  ((expand-namespaces-p
    :initarg :expand-namespaces-p
    :initform t
    :reader parser-expand-namespaces-p)
   (entities
    :initform (make-hash-table :test #'equal)
    :reader parser-entities)
   (in-dtd-p
    :initform nil
    :accessor parser-in-dtd-p)
   (canonical-uris
    :initform (make-hash-table :test #'equal)
    :reader parser-canonical-uris)
   (readtable
    :initform nil
    :accessor parser-readtable)))

(defconstant -standard-entities- '(("gt"   . ">")
                                   ("lt"   . "<")
                                   ("amp"  . "&")
                                   ("quot" . "\"")
                                   ("apos" . "'")))

(defmethod get-entity ((self xml-parser) name)
  (gethash name (parser-entities self)))

(defmethod (setf get-entity) (definition (self xml-parser) name)
  (setf (gethash name (parser-entities self)) definition))

(defmethod get-canonical-uri ((self xml-parser) uri)
  (gethash uri (parser-canonical-uris self) uri))

(defmethod (setf get-canonical-uri) (new-uri (self xml-parser) uri)
  (setf (gethash uri (parser-canonical-uris self)) new-uri))

(defmethod initialize-instance :after ((self xml-parser) &rest args)
  (declare (ignore args))
  (dolist (pair -standard-entities-)
    (destructuring-bind (n . e) pair (setf (get-entity self n) e)))
  (setf (get-canonical-uri self -alternate-rdf-uri-) -rdf-uri-
        (get-canonical-uri self -alternate-rdfs-uri-) -rdfs-uri-
        (get-canonical-uri self (subseq -rdfs-uri- 0 (1- (length -rdfs-uri-))))
         -rdfs-uri-))

(defmethod parse ((self xml-parser) stream locator)
  (let ((*current-parser* self)
        (consumer (sax-producer-consumer self)))
    (setf (parser-readtable self) *xml-reader*)
    (handler-bind ((end-of-file #'(lambda (c)
                                    (declare (ignore c))
                                    (error 'syntax-error :thing "eof"))))
      (start-document consumer locator)
      (parse-start self stream nil nil)
      (end-document consumer (sax-consumer-mode consumer)))))

(defun parse-start (parser stream end namespaces &aux continuep)
  (loop
    (multiple-value-setq (continuep namespaces)
      (parse-token parser stream (read-xml-token stream) end namespaces))
    (unless continuep
      (return-from parse-start nil))))

(defmethod parse-token ((self xml-parser)
                        stream (token string) ; char-content
                        end namespaces)
  (declare (ignore stream))
  (char-content (sax-producer-consumer self) (collapse-whitespace token)
                (sax-consumer-mode (sax-producer-consumer self)))
  (values end namespaces))

(defmethod parse-token ((self xml-parser)
                        stream (token open-tag) end namespaces)
  (flet ((expand (name)
           (or (expand-name-with-namespace name namespaces)
               (progn (cerror "Do not expand"
                              'missing-namespace-definition :thing name)
                      name))))
    (declare (dynamic-extent #'expand))
    (let ((consumer (sax-producer-consumer self)))
      (when (parser-expand-namespaces-p self)
        (setf namespaces (add-namespaces self token namespaces))
        (shiftf (tag-original-name token)
                (token-string token)
                (expand (token-string token)))
        (dolist (k&v (tag-attributes token))
          (setf (car k&v) (expand (car k&v)))))
      (setf (tag-namespaces token) namespaces)
      (start-element consumer token (sax-consumer-mode consumer))
      (if (tag-empty-p token)
        (end-element consumer token (sax-consumer-mode consumer))
        (parse-start self stream token namespaces))
      (values end namespaces))))

(defun add-namespaces (parser tag namespaces)
  (do-string-dict (key value (tag-attributes tag))
    (multiple-value-bind (n p) (name&prefix key)
      (cond ((string= p "xmlns")
             (setf (tag-attributes tag)
                   (string-dict-del (tag-attributes tag) key))
             (setf namespaces
                   (string-dict-add namespaces
                                    n (get-canonical-uri parser value))))
            ((and (null p) (string= n "xmlns"))
             (setf (tag-attributes tag)
                   (string-dict-del (tag-attributes tag) key))
             (setf namespaces
                   (string-dict-add namespaces
                                    nil (get-canonical-uri parser value)))))))
  namespaces)

(defun ends-in-hash-p (string)
  (declare (type string string))
  (let ((c (char string (1- (length string)))))
    (or (char= c #\#)
        (char= c #\/))))

(defun expand-name-with-namespace (string namespaces)
  (multiple-value-bind (n p) (name&prefix string)
    (or (and (null p)
             (hack-rdf-attribute-name n namespaces))
        (let ((uri (string-dict-get namespaces p)))
          (cond (uri
                 (values
                  (concatenate 'string
                               uri (and (not (ends-in-hash-p uri)) "#") n)
                  n p))
                ((or (null p) (string-equal p "xml"))
                 (values string nil nil))
                (t
                 (values nil n p)))))))

(defun hack-rdf-attribute-name (name namespaces)
  (and (car (rassoc -rdf-uri- namespaces :test #'string=))
       (cdr (assoc name -rdf-attr-map- :test #'string=))))

(defmethod parse-token ((self xml-parser)
                        stream (token close-tag) end namespaces)
  (declare (ignore stream))
  (cond ((null end)
         (error 'unexpected-end-tag :thing (token-string end)))
        ((string= (tag-original-name end) (token-string token))
         (setf (tag-counterpart token) end
               (tag-counterpart end) token)
         (end-element (sax-producer-consumer self) end
                      (sax-consumer-mode (sax-producer-consumer self)))
         (values nil namespaces))
        (t
         (error 'unexpected-end-tag
                :expectation (tag-original-name end)
                :thing (token-string token)))))

(defmethod parse-token ((self xml-parser)
                        stream (token proc-instruction) end namespaces)
  (declare (ignore stream end))
  (let ((consumer (sax-producer-consumer self)))
    (proc-instruction consumer token (sax-consumer-mode consumer))
    (values t namespaces)))

(defmethod parse-token ((self xml-parser)
                        stream (token entity-declaration) end namespaces)
  (declare (ignore stream end))
  (setf (get-entity self (entity-name token)) (token-string token))
  (values t namespaces))

(defmethod parse-token ((self xml-parser)
                        stream (token comment) end namespaces)
  (declare (ignore stream end))
  (values t namespaces))

(defmethod parse-token ((self xml-parser)
                        stream (token dtd-start) end namespaces)
  (declare (ignore stream end))
  (when (dtd-external-p token)
    (xml-warning "External DTD ignored:~{ ~S~}" (dtd-stuff token)))
  (values t namespaces))

(defmethod parse-token ((self xml-parser)
                        stream (token dtd-end) end namespaces)
  (declare (ignore stream end))
  (values t namespaces))

(defmethod parse-token ((self xml-parser)
                        stream (token dtd-declaration) end namespaces)
  (declare (ignore stream end))
  (xml-warning "~S ignored" (class-name (class-of token)))
  (values t namespaces))

(defmethod parse-token ((self xml-parser) stream token end namespaces)
  (declare (ignore stream end namespaces))
  (error 'syntax-error :thing token))

(defun parse-from-stream (stream locator parser-class &rest options)
  (declare (dynamic-extent options))
  (let ((parser (apply #'make-instance parser-class options)))
    (values parser
            (parse parser stream locator))))

(defun parse-from-file (file parser-class &rest options)
  (declare (dynamic-extent options))
  (let ((pathname (translate-logical-pathname file)))
    (with-open-file (stream pathname)
      (apply #'parse-from-stream stream (make-file-url pathname) parser-class
             options))))


;;; ----------------------------------------------------------------------------
;;;
;;;    CLASS XML-FORMATTER
;;;

(defclass xml-formatter (sax-consumer)
  ((stream
    :initarg :stream
    :initform nil
    :reader formatter-stream)
   (level
    :initform 0
    :accessor formatter-level)
   (indent-delta
    :initarg :indent-delta
    :initform 2
    :reader formatter-indent-delta)))

(defmethod replay ((formatter xml-formatter) events)
  (dolist (event events)
    (let ((mode (sax-consumer-mode formatter)))
      (etypecase event
        (open-tag
         (start-element formatter event mode))
        (close-tag
         (end-element formatter (tag-counterpart event) mode))
        (string
         (char-content formatter event mode))))))

(defun reverse-expand-name (name namespaces &aux (nn (length name)))
  (do-string-dict (prefix uri namespaces)
    (let ((un (length uri)))
      (when (and (>= nn un)
                 (string= name uri :end1 un))
        (return-from reverse-expand-name
          (values (if (= nn un)
                    (format nil "~@[~A~]:" prefix)
                    (format nil "~@[~A:~]~A"
                            prefix
                            (let ((n (subseq name un)))
                              (if (char= (char n 0) #\#)
                                (subseq n 1) n))))
                  t)))))
  (values name nil))

(defmethod start-element ((self xml-formatter) (tag open-tag) mode)
  (declare (ignore mode))
  (let ((stream (formatter-stream self)))
    (format stream "~&~V@T<~A"
            (formatter-level self)
            (reverse-expand-name (token-string tag) (tag-namespaces tag)))
    (do-string-dict (attribute value (tag-attributes tag))
      (format stream " ~A=\"~A\""
              (reverse-expand-name attribute (tag-namespaces tag))
              value))
    (princ (if (tag-empty-p tag) "/>" #\>) stream)
    (incf (formatter-level self) (formatter-indent-delta self))))

(defmethod end-element ((self xml-formatter) tag mode)
  (declare (ignore mode))
  (decf (formatter-level self) (formatter-indent-delta self))
  (unless (tag-empty-p tag)
    (format (formatter-stream self) "~&~V@T</~A>"
            (formatter-level self)
            (reverse-expand-name (token-string tag) (tag-namespaces tag)))))

(defmethod char-content ((self xml-formatter) char-content mode)
  (declare (ignore mode))
  (princ (string-trim '(#\Space #\Tab #\Newline) char-content)
         (formatter-stream self)))

(defmethod start-document ((self xml-formatter) locator)
  (declare (ignore locator))
  (format (formatter-stream self) "~&<?xml version=\"1.0\"?>"))

;;;; A simple consumer that builds nested plists
;;; 
;;; this is actually quite extensible
;;; The process sexp method can be customized using eql methods 
;;; to dispatch on the tag.
;;; the default of this method does nothing
;;; But you can build arbitary structure if you'd like.

(defclass xml-sexp-builder (sax-consumer)
  ((current-sexp :initform nil :accessor current-sexp)
   (current-tag :initform nil :accessor current-tag)
   (stack :initform nil :accessor stack)
   (tags-seen :initform nil :accessor tags-seen))) 
  
(defmethod process-sexp ((self xml-sexp-builder) tag expression)
  (pushnew tag (tags-seen self))
  (cons tag (cdr expression))
  )

(defmethod start-element ((self xml-sexp-builder) (tag open-tag) mode)
  (declare (ignore mode))
  (with-slots (stack current-sexp current-tag) self
    (push current-sexp stack)
    (setq current-tag tag)
    (setq current-sexp (list tag))))

(defmethod end-element ((self xml-sexp-builder) tag mode)
  (declare (ignore mode tag))
  (with-slots (current-sexp current-tag stack) self
    (setq current-sexp (process-sexp self (intern (string-upcase (token-string current-tag))) current-sexp))
    (let ((this-exp current-sexp))
      (setq current-sexp (pop stack))
      (setq current-tag (first current-sexp))
      (setq current-sexp (nconc current-sexp (list this-exp))))
	))

(defmethod char-content ((self xml-sexp-builder) char-content mode)
  (declare (ignore mode))
  (with-slots (current-sexp) self
    (setq current-sexp (nconc current-sexp (list (string-trim '(#\Space #\Tab #\Newline) char-content))))))

(defmethod start-document ((self xml-sexp-builder) locator)
  (declare (ignore locator))
  (format t "~&<?xml version=\"1.0\"?>")
  (with-slots (current-sexp current-tag stack) self 
    (let ((tag (make-instance 'token :string "document")))
      (setq current-sexp (list tag)	
            current-tag tag
            stack nil))))

(defmethod end-document ((self xml-sexp-builder) locator)
  (declare (ignore locator))
  (end-element self 'document nil)
  self
  )
