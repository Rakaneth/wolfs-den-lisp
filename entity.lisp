(in-package :wolfs-den-lisp)

(defclass entity ()
  ((x 
    :initarg :x 
    :accessor entity/x)
  (y 
    :initarg :y 
    :accessor entity/y)
  (char 
    :initarg :char 
    :accessor entity/char)
  (color
    :initarg :color 
    :accessor entity/color
    :initform (blt:yellow))))

(defmethod move ((e entity) dx dy)
  (incf (entity/x e) dx)
  (incf (entity/y e) dy))

(defmethod move-coord ((e entity) coord)
  (move e (car coord) (cdr coord)))

(defmethod move-to ((e entity) x y)
  (setf (entity/x e) x)
  (setf (entity/y e) y))

(defmethod draw ((e entity))
  (with-slots (x y char color) e
    (setf (blt:color) color
          (blt:cell-char x y) char)))
