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
    :initform (blt:yellow))
   (name
    :initarg :name
    :accessor entity/name
    :initform "No name")
   (desc
    :initarg :desc
    :accessor entity/desc
    :initform "No desc")))

(defmethod pos ((e entity))
  (cons (entity/x e) (entity/y e)))

(defmethod move-to ((e entity) x y)
  (setf (entity/x e) x)
  (setf (entity/y e) y)
  (pos e))

(defmethod move-by ((e entity) delta)
  (let* ((cur-pos (pos e))
         (new-pos (translate-coord cur-pos delta)))
    (move-to e (car new-pos) (cdr new-pos))))


(defmethod draw ((e entity))
  (with-slots (x y char color) e
    (setf (blt:color) color
          (blt:cell-char x y) char))
  (setf (blt:color) (blt:white)))
