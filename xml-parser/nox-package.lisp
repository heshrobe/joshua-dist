;;; -*- package: COMMON-LISP-USER; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  nox-package.lisp
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
;;;
;;; ----------------------------------------------------------------------------
;;;
;;;
;;;   Version: $Id: nox-package.lisp,v 1.2 2001/07/17 22:36:24 theran Exp $
;;;
;;;   Purpose: <explanation of the file contents>
;;;


(in-package :common-lisp-user)


(defpackage nox
  (:nicknames "NOKIA-XML-CL"
              "WILBUR-XML")
  (:use :common-lisp
        #+:mcl "CCL")
  (:export "XML-SEXP-BUILDER"           ; the consumer that builds sexps of objects from xml
           "CURRENT-SEXP"               ; only thing a client of his would want to see
           "PROCESS-SEXP"               ; the method to control what structure to build for an xml element
           "XML-ERROR"                  ; from xml-util.lisp
           "ERROR-THING"
           "SYNTAX-ERROR"
           "PI-TERMINATION-PROBLEM"
           "DTD-TERMINATION-PROBLEM"
           "UNEXPECTED-END-TAG"
           "ERROR-EXPECTATION"
           "UNKNOWN-DECLARATION"
           "UNKNOWN-CHARACTER-REFERENCE"
           "MALFORMED-URL"
           "FEATURE-NOT-SUPPORTED"
           "MISSING-DEFINITION"
           "ERROR-DEFINITION-TYPE"
           "MISSING-ENTITY-DEFINITION"
           "MISSING-NAMESPACE-DEFINITION"
           "XML-WARNING"
           "*CURRENT-PARSER*"
           "READ-USING"
           "STRING-DICT-GET"
           "STRING-DICT-GET-BY-VALUE"
           "STRING-DICT-ADD"
           "STRING-DICT-DEL"
           "DO-STRING-DICT"
           "MAKE-FILE-URL"
           "MAKE-HTTP-URL"
           "PARSE-URL"
           "TOKEN"
           "TOKEN-STRING"
           "OPEN-TAG"
           "CLOSE-TAG"
           "ENTITY-DECLARATION"
           "ENTITY-NAME"
           "COMMENT"
           "CHAR-CONTENT"
           "TAG-COUNTERPART"
           "TAG-ATTRIBUTE"
           "TAG-ATTRIBUTES"
           "TAG-EMPTY-P"
           "TAG-NAMESPACES"
           "START-ELEMENT"
           "END-ELEMENT"
           "CHAR-CONTENT"
           "PROC-INSTRUCTION"
           "START-DOCUMENT"
           "END-DOCUMENT"
           "SAX-CONSUMER"
           "SAX-CONSUMER-PRODUCER"
           "SAX-CONSUMER-MODE"
           "SAX-PRODUCER"
           "SAX-PRODUCER-CONSUMER"
           "SAX-FILTER"
           "FIND-FIRST-PRODUCER"
           "-WHITESPACE-CHARS-"
           "*NAME-READER*"              ; from xml-parser.lisp
           "XML-PARSER"
           "GET-ENTITY"
           "GET-CANONICAL-URI"
           "PARSE"
           "EXPAND-NAME-WITH-NAMESPACE"
           "PARSE-FROM-STREAM"
           "PARSE-FROM-FILE"
           "XML-FORMATTER"
           "REPLAY"
           "REVERSE-EXPAND-NAME"
           "-RDF-URI-"                  ; from rdf-constants.lisp
           "-RDFS-URI-"
           "RDF-URI"
           "RDFS-URI"
           "-RDF-ATTRS-"
           "-RDF-ATTR-MAP-"
           "-RDF-ID-URI-"
           "-RDF-RESOURCE-URI-"
           "-RDF-ABOUT-URI-"
           "-RDF-ABOUTEACH-URI-"
           "-RDF-ABOUTEACHPREFIX-URI-"
           "-RDF-BAGID-URI-"
           "-RDF-PARSETYPE-URI-"
           "-RDF-DESCRIPTION-URI-"
           "-RDF-TYPE-URI-"
           "-RDF-RDF-URI-"
           "-RDF-LI-URI-"
           "-RDF-STATEMENT-URI-"
           "-RDF-SUBJECT-URI-"
           "-RDF-PREDICATE-URI-"
           "-RDF-OBJECT-URI-"
           "-RDF-BAG-URI-"
           "-RDF-SEQ-URI-"
           "-RDF-ALT-URI-"
           "-RDFS-RESOURCE-URI-"
           "-RDFS-CLASS-URI-"
           "-RDFS-SUBCLASSOF-URI-"
           "-RDFS-SUBPROPERTYOF-URI-"
           "-RDFS-SEEALSO-URI-"
           "-RDFS-ISDEFINEDBY-URI-"
           "-RDFS-CONSTRAINTRESOURCE-URI-"
           "-RDFS-CONSTRAINTPROPERTY-URI-"
           "-RDFS-RANGE-URI-"
           "-RDFS-DOMAIN-URI-"
           "-RDFS-COMMENT-URI-"
           "-RDFS-LABEL-URI-"
           "-RDFS-LITERAL-URI-"
           "-RDFS-CONTAINER-URI-"))
