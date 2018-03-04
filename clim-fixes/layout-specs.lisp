;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SILICA; Base: 10; Lowercase: Yes -*-

(in-package :silica)


;;; This fixes a problem in clim:silica;db-layout.lisp

;;; Problem is that 
;;; (defmethod compose-space :around ((pane client-overridability-mixin) &key width height)
;;; calls (space-requirement-components (normalize-space-requirement pane space-requirement))
;;;  as well as           (space-requirement-components (call-next-method))
;;; and then does arithmetic on the resulting heights, max-heights, widths, max widths etc.
;;; but these can be any of the allowable compound expressions such as '(50 :character)
;;; and need to be converted before the arithmetic.
;;; normalize space requirements does nothing for this class for some reason.  All the motif
;;; sub-classes seem to do something except for slider, but I'm not sure what since I don't have the source.

(defmethod normalize-space-requirement ((pane client-overridability-mixin) (sr general-space-requirement))
    (with-slots (width min-width max-width height min-height max-height) sr
      (macrolet ((fix-it (slot-name)
		   `(when (clim-internals::unit-space-requirement-p ,slot-name)
		      (setq ,slot-name (clim-internals::process-unit-space-requirement pane ,slot-name)))))
	(fix-it width)
	(fix-it min-width)
	(fix-it max-width)
	(fix-it height)
	(fix-it min-height)
	(fix-it max-height)))	
  sr)