(in-package #:wolfs-den-lisp)

(defclass rect ()
  ((x1 :initarg :x1 :accessor rect/x1)
   (x2 :initarg :x2 :accessor rect/x2)
   (y1 :initarg :y1 :accessor rect/y1)
   (y2 :initarg :y2 :accessor rect/y2)
   (width :reader rect/w)
   (height :reader rect/h)))

(defmethod initialize-instance :after ((r rect) &key x y w h)
  (with-slots (x1 x2 y1 y2 width height) r
    (setf x1 x
          y1 y
          x2 (1- (+ x w))
          y2 (1- (+ y h))
          width w
          height h)))

(defmethod intersect-p ((r1 rect) (r2 rect))
  (cond
    ((< (rect/x2 r1) (rect/x1 r2)) nil)
    ((< (rect/x2 r2) (rect/x1 r1)) nil)
    ((< (rect/y2 r1) (rect/y1 r2)) nil)
    ((< (rect/y2 r2) (rect/y1 r1)) nil)
    (t t)))

(defmethod points ((r rect))
  (points-list (rect/x1 r) (rect/x2 r) (rect/y1 r) (rect/y2 r)))

(defmethod interior ((r rect))
  (points-list (1+ (rect/x1 r))
               (1- (rect/x2 r))
               (1+ (rect/y1 r))
               (1- (rect/y2 r))))

(defmethod perimeter ((r rect))
  (set-difference (points r) (interior r) :test #'equal))

(defmethod print-text ((r rect) x y text &rest args)
  (with-accessors ((x1 rect/x1)
                   (y1 rect/y1)
                   (w rect/w)
                   (h rect/h)) r
    (blt:print (+ x x1) (+ y y1) (apply #'format (cons nil (cons text args))) :width (- w 2) :height (- h 2))))

(defmethod draw ((r rect) &key caption)
  (with-accessors ((x1 rect/x1)
                   (y1 rect/y1)
                   (w rect/w)
                   (h rect/h)) r
    (blt:draw-box x1 y1 w h)
    (when caption
      (print-text r 1 0 caption))))
