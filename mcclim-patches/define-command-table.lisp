;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by Robert Strandh (strandh@labri.u-bordeaux.fr)
;;;  (c) copyright 2002 by Tim Moore (moore@bricoworks.com)

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




(defmacro define-command-table (name &key (inherit-from nil inherit-supplied-p)
                                          (menu nil menu-supplied-p)
                                          inherit-menu)
  `(let ((old-table (gethash ',name *command-tables* nil))
	 (inherit-from-arg (or ',inherit-from '(global-command-table))))
     (if old-table
         (with-slots (inherit-from menu) old-table
           ,(if inherit-supplied-p
                `(setq inherit-from ',inherit-from)
                `(setq inherit-from '(global-command-table)))
           ,(when menu-supplied-p
              `(setq menu (menu-items-from-list ',menu)))
           old-table)
         (make-command-table ',name
			     :inherit-from ,(if inherit-supplied-p
                                                `',inherit-from
                                                `'(global-command-table))
                             :inherit-menu ,inherit-menu
			     :menu ',menu
			     :errorp nil))))

