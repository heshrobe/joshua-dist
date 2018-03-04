;;; -*- package: nox; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  rdf-constants.lisp
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
;;;   Version: $Id: rdf-constants.lisp,v 1.3 2001/07/17 22:34:54 theran Exp $
;;;
;;;   Purpose: This file contains definitions for various constants used by the
;;;   RDF parser (mostly URIs). Given that the XML parser has to deal with the
;;;   issue of RDF M+S vagueness on the namespaces of RDF attributes (such as
;;;   "about"), the definitions in this file are in the NOX package.
;;;


(in-package :nox)


;;; ----------------------------------------------------------------------------
;;;
;;;   HELPERS
;;;

(eval-when (:compile-toplevel :load-toplevel)
  (defconstant -rdf-uri- "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  (defconstant -rdfs-uri- "http://www.w3.org/2000/01/rdf-schema#")
  (defun rdf-uri (string)
    (concatenate 'string -rdf-uri- string))
  (defun rdfs-uri (string)
    (concatenate 'string -rdfs-uri- string))  

(defconstant -alternate-rdf-uri-
  "http://www.w3.org/TR/REC-rdf-syntax/")
(defconstant -alternate-rdfs-uri-
  "http://www.w3.org/TR/1999/PR-rdf-schema-19990303#")


;;; ----------------------------------------------------------------------------
;;;
;;;   RDF M+S ATTRIBUTE URIS
;;;

(defconstant -rdf-id-uri-              (rdf-uri "ID"))
(defconstant -rdf-resource-uri-        (rdf-uri "resource"))
(defconstant -rdf-about-uri-           (rdf-uri "about"))
(defconstant -rdf-abouteach-uri-       (rdf-uri "aboutEach"))
(defconstant -rdf-abouteachprefix-uri- (rdf-uri "aboutEachPrefix"))
(defconstant -rdf-bagid-uri-           (rdf-uri "bagID"))
(defconstant -rdf-parsetype-uri-       (rdf-uri "parseType"))

(defconstant -rdf-attrs-     `(,-rdf-id-uri-
                               ,-rdf-resource-uri-
                               ,-rdf-about-uri-
                               ,-rdf-abouteach-uri-
                               ,-rdf-abouteachprefix-uri-
                               ,-rdf-bagid-uri-
                               ,-rdf-parsetype-uri-))

(defconstant -rdf-attr-map- `((,"ID"              . ,-rdf-id-uri-)
                              (,"resource"        . ,-rdf-resource-uri-)
                              (,"about"           . ,-rdf-about-uri-)
                              (,"aboutEach"       . ,-rdf-abouteach-uri-)
                              (,"aboutEachPrefix" . ,-rdf-abouteachprefix-uri-)
                              (,"bagID"           . ,-rdf-bagid-uri-)
                              (,"parseType"       . ,-rdf-parsetype-uri-)))


;;; ----------------------------------------------------------------------------
;;;
;;;   RDF M+S RESOURCE, PROPERTY, ETC. URIS
;;;

(defconstant -rdf-description-uri- (rdf-uri "Description"))
(defconstant -rdf-type-uri-        (rdf-uri "type"))
(defconstant -rdf-rdf-uri-         (rdf-uri "RDF"))
(defconstant -rdf-li-uri-          (rdf-uri "li"))
(defconstant -rdf-statement-uri-   (rdf-uri "Statement"))
(defconstant -rdf-subject-uri-     (rdf-uri "subject"))
(defconstant -rdf-predicate-uri-   (rdf-uri "predicate"))
(defconstant -rdf-object-uri-      (rdf-uri "object"))
(defconstant -rdf-bag-uri-         (rdf-uri "Bag"))
(defconstant -rdf-seq-uri-         (rdf-uri "Seq"))
(defconstant -rdf-alt-uri-         (rdf-uri "Alt"))


;;; ----------------------------------------------------------------------------
;;;
;;;   RDF SCHEMA URIS
;;;

(defconstant -rdfs-resource-uri-           (rdfs-uri "Resource"))
(defconstant -rdfs-class-uri-              (rdfs-uri "Class"))
(defconstant -rdfs-subclassof-uri-         (rdfs-uri "subClassOf"))
(defconstant -rdfs-subpropertyof-uri-      (rdfs-uri "subPropertyOf"))
(defconstant -rdfs-seealso-uri-            (rdfs-uri "seeAlso"))
(defconstant -rdfs-isdefinedby-uri-        (rdfs-uri "isDefinedBy"))
(defconstant -rdfs-constraintresource-uri- (rdfs-uri "ConstraintResource"))
(defconstant -rdfs-constraintproperty-uri- (rdfs-uri "ConstraintProperty"))
(defconstant -rdfs-range-uri-              (rdfs-uri "range"))
(defconstant -rdfs-domain-uri-             (rdfs-uri "domain"))
(defconstant -rdfs-comment-uri-            (rdfs-uri "comment"))
(defconstant -rdfs-label-uri-              (rdfs-uri "label"))
(defconstant -rdfs-literal-uri-            (rdfs-uri "Literal"))
(defconstant -rdfs-container-uri-          (rdfs-uri "Container "))

) ; eval-when