;;; -*- Mode: Common-lisp: Package: clim-internals -*-

(in-package :clim-internals)

(define-graphics-recording draw-pixmap (ink clipping-region)
  :bounding-rectangle
  (if (typep clipping-region 'clim-utils:everywhere)
      (values x y (+ x (pixmap-width pixmap)) (+ y (pixmap-height pixmap)))
    (with-bounding-rectangle* (left top right bottom) clipping-region
      (values (max x left)
	      (max y top)
	      (min (+ x (pixmap-width pixmap)) right)
	      (min (+ y (pixmap-height pixmap)) bottom)))))