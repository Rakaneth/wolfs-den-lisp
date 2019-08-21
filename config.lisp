(in-package #:wolfs-den-lisp)

(defparameter *screen-width* 100)
(defparameter *screen-height* 40)
(defvar *maps* (make-hash-table))
(defvar *screens* (make-array 10 :fill-pointer 0 :adjustable t))

(defmacro add-map (map))
(defmacro remove-map (map))
