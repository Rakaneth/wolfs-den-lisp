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

(defun create-queue (&rest args)
  (loop :with q = (make-queue) 
        :for item in args
        :do (enqueue item q)
        :finally (return q)))

(defun enqueue (item q)
  (vector-push-extend item (queue-vec q)))

(defun dequeue (q)
  (let ((fst (elt (queue-vec q) 0)))
    (delete fst (queue-vec q) :count 1 :test #'equal)
    fst))

(defun peek (q)
  (elt (queue-vec q) 0))

(defun queue-empty-p (q)
  (zerop (queue-length q)))

(defun queue-length (q)
  (length (queue-vec q)))

(defmethod print-object ((q queue) stream)
  (print-unreadable-object (q stream :type t)
    (format stream "~d ~A" (queue-length q) (queue-vec q))))

(defstruct (priority-queue (:include queue))
  (sort-pred #'<))

(defun create-priority-queue (args &key sort-pred)
  (loop :with q = (make-priority-queue :sort-pred (or sort-pred #'<))
        :for (weight . item) in args
        :do (priority-enqueue item weight q)
        :finally (return q)))

(defun priority-enqueue (item weight q)
  (vector-push-extend `(,weight . ,item) (queue-vec q))
  (sort (queue-vec q) (priority-queue-sort-pred q) :key #'car))

