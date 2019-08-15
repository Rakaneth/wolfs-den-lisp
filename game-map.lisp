(in-package #:wolfs-den-lisp)

(defstruct tile 
  (glyph #\x)
  (color "transparent")
  (block-sight t)
  (block-path t)
  (explored nil))

(defparameter *null-tile* (make-tile))

(defun create-tile (&key tile-type (color "white") (explored nil))
  (case tile-type
    (:floor (make-tile :glyph #\. :color color :block-sight nil :block-path nil :explored explored ))
    (:wall (make-tile :glyph #\# :color color :explored explored))
    (:stairs-up (make-tile :glyph #\< :color "yellow" :explored explored))
    (:stairs-down (make-tile :glyph #\> :color "yellow" :explored explored))
    (t *null-tile*)))

(defclass rect ()
  ((x1 :initarg :x1 :accessor rect/x1)
   (x2 :initarg :x2 :accessor rect/x2)
   (y1 :initarg :y1 :accessor rect/y1)
   (y2 :initarg :y2 :accessor rect/y2)))

(defmethod initialize-instance :after ((r rect) &key x y w h)
  (with-slots (x1 x2 y1 y2) r
    (setf x1 x
          y1 y
          x2 (1- (+ x w))
          y2 (1- (+ y h)))))

(defmethod intersect-p ((r1 rect) (r2 rect))
  (cond
    ((< (rect/x2 r1) (rect/x1 r2)) nil)
    ((< (rect/x2 r2) (rect/x1 r1)) nil)
    ((< (rect/y2 r1) (rect/y1 r2)) nil)
    ((< (rect/y2 r2) (rect/y1 r1)) nil)
    (t t)))

(defmethod points ((r rect))
  (loop :with x-start = (rect/x1 r)
        :with x-end = (rect/x2 r)
        :for x from x-start to x-end
        :nconc (loop :with y-start = (rect/y1 r)
                     :with y-end = (rect/y2 r)
                     :for y from y-start to y-end
                     :collect (cons x y))))

(defmethod interior ((r rect))
  (loop :with x-start = (1+ (rect/x1 r))
        :with x-end = (rect/x2 r)
        :for x from x-start below x-end
        :nconc (loop :with y-start = (1+ (rect/y1 r))
                     :with y-end = (rect/y2 r)
                     :for y from y-start below y-end
                     :collect (cons x y))))

(defmethod perimeter ((r rect))
  (set-difference (points r) (interior r) :test #'equal))


(defclass game-map ()
  ((width
    :initarg :width
    :reader game-map/w)
   (height
    :initarg :height
    :reader game-map/h)
   (tiles
    :accessor game-map/tiles)
   (name
    :initarg :name
    :accessor game-map/name)
   (id
    :initarg :id
    :accessor game-map/id)))


(defmethod initialize-instance :after ((map game-map) &rest initargs)
  (declare (ignore initargs))
  (setf (game-map/tiles map) 
        (make-array (list (game-map/w map) (game-map/h map)))))

(defmethod initialize-tiles )

(defmethod in-bounds-p ((m game-map) coord)
  (and (between-p (car coord) 0 (1- (game-map/w m)))
       (between-p (cdr coord) 0 (1- (game-map/h m)))))

(defmethod get-tile ((m game-map) coord)
  (if (in-bounds-p m coord)
      (aref (game-map/tiles m) (car coord) (cdr coord))
      *null-tile*))

(defmethod set-tile ((m game-map) coord tile-key)
  (let* ((-tile (get-tile m coord))
         (explored (tile-explored -tile)))
    (setf (aref (game-map/tiles m) (car coord) (cdr coord)) 
          (create-tile :tile-type tile-key :explored explored))))

(defmethod blocked-p ((m game-map) coord)
  (tile-block-path (get-tile m coord)))

(defmethod opaque-p ((m game-map) coord) 
  (tile-block-sight (get-tile m coord)))

(defmethod explored-p ((m game-map) coord)
  (tile-explored (get-tile m coord)))


