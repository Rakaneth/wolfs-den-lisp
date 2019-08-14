(in-package :wolfs-den-lisp)

(defun clamp (val low high)
  (cond
    ((< val low) low)
    ((> val high) high)
    (t val)))

(defun between-p (val low high)
  (eql (clamp val low high) val))

(defun decorate (text color-string)
  (format nil "[color=~A]~A[/color]" color-string text))
