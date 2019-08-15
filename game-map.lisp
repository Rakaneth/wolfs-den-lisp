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
  (setf (game-map/tiles map) (make-array (list (game-map/w map) (game-map/h map)) :initial-element *null-tile*)))

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
    (setf (aref (game-map/tiles m) (car coord) (cdr coord)) (create-tile :tile-type tile-key :explored explored))))
