(in-package #:wolfs-den-lisp)

(defparameter *screen-width* 100)
(defparameter *screen-height* 40)
(defvar *maps* (make-hash-table))

(defun add-map (map)
  (setf (gethash (game-map/id map) *maps*) map))

(defun remove-map (map-id)
  (remhash map-id *maps*))
