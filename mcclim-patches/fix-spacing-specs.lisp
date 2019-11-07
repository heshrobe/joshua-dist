;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000,2001 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by
;;;           Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;  (c) copyright 2000, 2001,2014 by
;;;           Robert Strandh (robert.strandh@gmail.com)
;;;  (c) copyright 2001 by
;;;           Lionel Salabartan (salabart@emi.u-bordeaux.fr)
;;;           Arnaud Rouanet (rouanet@emi.u-bordeaux.fr)
;;;  (c) copyright 2002, 2003 by
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

;;; $Id: panes.lisp,v 1.197 2009/08/01 22:11:06 gbaumann Exp $

(in-package :clim-internals)


;;; in panes.lisp

(defmethod spacing-value-to-device-units (pane x)
  (cond ((realp x) x)
        ((consp x)
         (ecase (cadr x)
           (:pixels (car x))
           (:point  (* (car x) (graft-pixels-per-inch (graft pane)) 1/72))
           (:mm     (* (car x) (graft-pixels-per-millimeter (graft pane))))
           (:character (device-units-in-character-expression pane x))
           (:line (device-units-in-line-expression pane x))))))

(defgeneric device-units-in-character-expression (pane character-expression))
(defmethod device-units-in-character-expression (pane character-expression) 0)
(defmethod device-units-in-character-expression ((pane single-child-composite-pane) character-expression)
  (device-units-in-character-expression (sheet-child pane) character-expression))
(defmethod device-units-in-character-expression ((pane composite-pane) character-expression)
  (loop for child in (sheet-children pane) maximize (device-units-in-character-expression child character-expression)))
(defmethod device-units-in-character-expression ((pane clim-stream-pane) character-expression)
  (* (first character-expression) (text-style-character-width (pane-text-style pane)
						      (sheet-medium pane)
						      #\m)))

(defgeneric device-units-in-line-expression (pane line-expression))
(defmethod device-units-in-line-expression (pane line-expression) 0)
(defmethod device-units-in-line-expression ((pane single-child-composite-pane) line-expression)
  (device-units-in-line-expression (sheet-child pane) line-expression))
(defmethod device-units-in-line-expression ((pane composite-pane) line-expression)
  (loop for child in (sheet-children pane) maximize (device-units-in-line-expression child line-expression)))
(defmethod device-units-in-line-expression ((pane clim-stream-pane) line-expression)
  (* (first line-expression) (stream-line-height pane)))
