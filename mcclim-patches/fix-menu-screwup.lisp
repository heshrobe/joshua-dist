;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by
;;;           Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;  (c) copyright 2000, 2014 by
;;;           Robert Strandh (robert.strandh@gmail.com)
;;;  (c) copyright 2004 by
;;;           Gilbert Baumann <unk6@rz.uni-karlsruhe.de>

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

(in-package :clim-internals)


;;; from panes
(defun make-clim-command-menu-pane (&rest options)
  (apply #'make-clim-stream-pane :type 'command-menu-pane options))

;;; from frames
(defun do-pane-creation-form (name form)
  (cond
    ;; Single form which is a function call
    ((and (= (length form) 1)
	  (listp (first form)))
     `(coerce-pane-name ,(first form) ',name))
    ;; Standard pane denoted by a keyword (i.e `:application')
    ((keywordp (first form))
     (case (first form)
       (:application `(make-clim-application-pane
                       :name ',name
                       ,@(cdr form)))
       (:interactor `(make-clim-interactor-pane
                      :name ',name ,@(cdr form)
                      ,@(cdr form)))
       (:pointer-documentation `(make-clim-pointer-documentation-pane
                                 :name ',name
                                 ,@(cdr form)))
       (:command-menu `(make-clim-command-menu-pane
       			:name ',name
       			,@(cdr form)))
       (otherwise `(make-pane ,(first form) :name ',name ,@(cdr form)))))
    ;; Non-standard pane designator fed to the `make-pane'
    (t `(make-pane ',(first form) :name ',name ,@(cdr form)))))
