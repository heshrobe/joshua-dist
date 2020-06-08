;;; -*- Mode: LISP; Syntax: Common-lisp; Package: cl-User; -*-

(in-package :cl-user)

#+Genera
(scl:defsystem joshua-developer
    (:pretty-name "Joshua Developer"
     :short-name "JDev"
     :default-pathname "joshua:portable;developer;"
     :journal-directory "joshua:portable;developer;patch;"
     :bug-reports t
     :advertised-in (:disk-label :herald :finger)
     :source-category (:basic)
     :distribute-sources t
     :distribute-binaries t
     :required-systems ("joshua-portable" "clim-environment")
     :maintaining-sites (:scrc :jamaica-plain :mit))
  (:serial-definitions
    "editor"
    "ptype-defs"
    "tracing"
    "encapsulations"
    "ptypes-and-commands"))


#+mcl
(clim-defsys:defsystem joshua-developer
  ( :default-pathname "joshua:developer;")
  ("ptype-defs" :language :lisp-unix)
  ("tracing" :language :lisp-unix :load-before-compile ("ptype-defs"))
  ("encapsulations" :load-before-compile ("tracing") :language :lisp-unix)
  ("ptypes-and-commands" :load-before-compile ("encapsulations") :language :lisp-unix))

#+allegro
(defsystem joshua-developer
    ( :default-pathname "joshua:developer;"
     :default-module-class separate-destination-module
     :pretty-name "Joshua Developer")
  (:serial
    ("ptype-defs")
    ("tracing")
    ("encapsulations")
    ("ptypes-and-commands")
    ("emacs-interfaces")
    ))


#+asdf
(asdf:defsystem joshua-developer
    :pathname "."
     :name "Joshua Developer"
    :description "Development tools for Joshua"
    :serial t
    :components
    ((:file "mcclim-additions")
     (:file "ptype-defs")
     (:file "tracing")
     (:file "encapsulations")
     (:file "ptypes-and-commands")
     #+allegro
     (:file "emacs-interfaces")
     ))
