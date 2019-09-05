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

(defstruct queue
  (vec (make-array 10 :adjustable t :fill-pointer 0)))

(defun enqueue (q item)
  (vector-push-extend item (queue-vec q)))

(defun dequeue (q)
  (let ((fst (elt (queue-vec q) 0)))
    (delete fst (queue-vec q) :count 1 :test #'equal)
    fst))

(defun peek (q)
  (elt (queue-vec q) 0))

(defmethod print-object ((q queue) stream)
  (print-unreadable-object (q stream :type t)
    (format stream "~A" (queue-vec q))))

