;;; -*- Mode: LISP; Base: 10; Syntax: Common-lisp; Package: joshua-internals -*-


(in-package :ji)


#+sbcl
(defun ji::function-name (function)
  (typecase function
    (symbol (when (fdefinition function) function))
    (function (sb-impl::%fun-name function))))

#+sbcl
(defun ji::arglist (function)
  (sb-introspect:function-lambda-list function))
