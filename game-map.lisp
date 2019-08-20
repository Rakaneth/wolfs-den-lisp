(in-package #:wolfs-den-lisp)

(defstruct tile 
  (glyph #\x)
  (color "transparent")
  (block-sight t)
  (block-path t))


(defclass game-map ()
  (width
   :initarg width))
