;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: Cl-user -*-

(in-package :cl-user)

#+allegro
(defsystem sample-xml-rpc-server
    (:default-module-class separate-destination-module
     :default-pathname "~/josh-dist/sample-xml-rpc-server/")
  (:serial
   "sample-xml-rpc-server.lisp"))