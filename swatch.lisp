(in-package :wolfs-den-lisp)

(defparameter *swatch* (make-hash-table :test 'equal))
(defmacro define-color (color-name color-value)
  `(setf (gethash ,color-name *swatch*) ,color-value))

(defparameter *sepia* (blt:rgba 191 171 143))
(defparameter *dark-sepia* (blt:rgba 120 101 97))

(define-color "yellow" (blt:yellow))
(define-color "blue" (blt:blue))
(define-color "sepia" *sepia*)
(define-color "transparent" (blt:rgba 0 0 0 0))
(define-color "white" (blt:white))
(define-color "stairs" (blt:yellow))
(define-color "stone" (blt:rgba 120 120 120))
(define-color "dark-stone" (blt:rgba 120 120 120 128))
(define-color "wood" (blt:rgba 191 171 143))
(define-color "undead" (blt:rgba 220 115 255))
(define-color "timberwolf" *sepia*)

(defun color-from-name (color-string)
  (or (gethash color-string *swatch*) 
      (error (format nil "No color ~a in swatch" color-string))))
