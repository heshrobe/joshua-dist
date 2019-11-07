;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-

"Copyright (c) 1994 Harlequin, Inc.  All rights reserved."

(in-package :dylan)


;;; Patches to Dylan emulator

;;--- from ~dylan/emulator/run-time/support.lisp
(defmethod dylan-print :around ((o t)
                                &key 
                                (stream *standard-output*)
                                (verbose? t))
  (when (and *print-readably* (typep (class-of o) 'clos::dylan-class))
    (signal 'print-not-readable :object o))
  (call-next-method))

