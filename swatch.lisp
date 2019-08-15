(in-package :wolfs-den-lisp)

(defparameter *swatch* 
  (list (cons "yellow" (blt:yellow))
        (cons "blue" (blt:blue))
        (cons "sepia" (blt:rgba 127 101 91))))

(defun color-from-name (color-string)
  (let ((result (assoc color-string *swatch* :test #'equalp)))
    (if result
        (cdr result)
        (blt:white))))
