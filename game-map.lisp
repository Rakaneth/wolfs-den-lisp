(in-package #:wolfs-den-lisp)

(defvar *maps* (make-hash-table))
(defparameter *viewport-width* 50)
(defparameter *viewport-height* 30)

(defun add-map (map)
  (setf (gethash (game-map/id map) *maps*) map))

(defun remove-map (map-id)
  (remhash map-id *maps*))

(defstruct tile 
  (glyph #\x)
  (color "transparent")
  (block-sight t)
  (block-path t)
  (explored nil))

(defun points-list (x-start x-end y-start y-end)
  (loop :for x from x-start to x-end
        :nconc (loop :for y from y-start to y-end
                     :collect (cons x y))))

(defparameter *null-tile* (make-tile))

(defun create-tile (&key tile-type (color "white") (explored nil))
  (case tile-type
    (:floor (make-tile :glyph #\. :color color :block-sight nil :block-path nil :explored explored ))
    (:wall (make-tile :glyph #\# :color color :explored explored))
    (:stairs-up (make-tile :glyph #\< :color "yellow" :explored explored))
    (:stairs-down (make-tile :glyph #\> :color "yellow" :explored explored))
    (t (make-tile))))

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
  (points-list (rect/x1 r) (rect/x2 r) (rect/y1 r) (rect/y2 r)))

(defmethod interior ((r rect))
  (points-list (1+ (rect/x1 r))
               (1- (rect/x2 r))
               (1+ (rect/y1 r))
               (1- (rect/y2 r))))

(defmethod perimeter ((r rect))
  (set-difference (points r) (interior r) :test #'equal))

(defclass game-map ()
  ((width :initarg :width :reader game-map/w)
   (height :initarg :height :reader game-map/h)
   (tiles :accessor game-map/tiles)
   (name :initarg :name :accessor game-map/name)
   (id :initarg :id :accessor game-map/id)
   (x-edge :reader game-map/x-edge)
   (y-edge :reader game-map/y-edge)
   (focus :initform (cons 0 0) :accessor game-map/focus)
   (entities :initform `() :accessor game-map/entities)))

(defun cam (m center-point)
  (let ((x (car center-point))
        (y (cdr center-point))
        (w (game-map/w m))
        (h (game-map/h m))
        (calc (lambda (p md s) (clamp (- p (/ s 2))
                                      0
                                      (max 0 (- md s))))))
    (values (funcall calc x w *viewport-width*)
            (funcall calc y h *viewport-height*))))

(defmethod initialize-instance :after ((map game-map) &rest initargs)
  (declare (ignore initargs))
  (with-slots (tiles x-edge y-edge width height) map
    (setf tiles (make-array (list width height) 
                            :element-type 'tile 
                            :initial-element (copy-tile *null-tile*))
          x-edge (1- width)
          y-edge (1- height))))

(defmethod points ((m game-map))
  (with-slots (x-edge y-edge) m
    (points-list 0 x-edge 0 y-edge)))

(defun all-walls (m)
  (loop :for pos in (points m)
        :do (set-tile m pos :wall)))

(defun random-walls (m &optional (chance 50))
  (loop :for pos in (points m)
        :do (if (getf  (roll 100 :target (- 100 chance)) :success)
                (set-tile m pos :wall)
                (set-tile m pos :floor))))

(defmethod in-bounds-p ((m game-map) coord)
  (and (between-p (car coord) 0 (game-map/x-edge m))
       (between-p (cdr coord) 0 (game-map/y-edge m))))

(defmethod get-tile ((m game-map) coord)
  (if (in-bounds-p m coord)
      (aref (game-map/tiles m) (car coord) (cdr coord))
      *null-tile*))

(defmethod set-tile ((m game-map) coord tile-key)
  (let* ((-tile (get-tile m coord))
         (explored (tile-explored -tile)))
    (setf (aref (game-map/tiles m) (car coord) (cdr coord)) 
          (create-tile :tile-type tile-key :explored explored))))

(defmethod adj ((m game-map) coord &key (include-walls nil))
  (let* ((x (car coord))
         (y (cdr coord))
         (xe (game-map/x-edge m))
         (ye (game-map/y-edge m))
         (x1 (clamp (1- x) 0 xe))
         (x2 (clamp (1+ x) 0 xe))
         (y1 (clamp (1- y) 0 ye))
         (y2 (clamp (1+ y) 0 ye))
         (cands (points-list x1 x2 y1 y2))
         (test (lambda (p) (and (not (equal `(,x . ,y) p))
                                (or include-walls (not (blocked-p m p)))))))
    (remove-if-not test cands)))

(defmethod blocked-p ((m game-map) coord)
  (tile-block-path (get-tile m coord)))

(defmethod opaque-p ((m game-map) coord) 
  (tile-block-sight (get-tile m coord)))

(defmethod explored-p ((m game-map) coord)
  (tile-explored (get-tile m coord)))


