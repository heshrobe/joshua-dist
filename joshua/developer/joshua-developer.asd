;;; -*- Mode: LISP; Syntax: Common-lisp; Package: cl-User; -*-

(in-package :cl-user)


#+asdf
(asdf:defsystem joshua-developer
    :pathname "."
     :name "Joshua Developer"
    :description "Development tools for Joshua"
    :serial t
    :depends-on (joshua)
    :components
    (#+sbcl
     (:file "missing-stuff")
     (:file "ptype-defs")
     (:file "tracing")
     (:file "encapsulations")
     (:file "ptypes-and-commands")
     #+mcclim
     (:file drei-interface)
     #+mcclim
     (:file slime-support)
     #+allegro
     (:file "emacs-interfaces")
     ))
