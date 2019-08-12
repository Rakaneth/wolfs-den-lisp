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
    :accessor entity/color)))

(defmethod move ((e entity) dx dy)
  (incf (entity/x e) dx)
  (incf (entity/y e) dy))

(defmethod draw ((e entity))
  (with-slots (x y char color) e
    (setf (blt:color) color
          (blt:cell-char x y) char)))