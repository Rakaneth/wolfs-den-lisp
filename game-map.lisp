(in-package #:wolfs-den-lisp)

(defvar *maps* (make-hash-table :test 'equal))
(defparameter *viewport-width* 60)
(defparameter *viewport-height* 30)

(defun add-map (map)
  (setf (gethash (game-map/id map) *maps*) map))

(defun remove-map (map-id)
  (remhash map-id *maps*))

(defun get-map (map-id)
  (gethash map-id *maps*))

(defstruct tile 
  (glyph #\Nul)
  (color "transparent")
  (block-sight t)
  (block-path t)
  (explored nil))

(defun points-list (x-start x-end y-start y-end)
  (loop :for y from y-start to y-end
        :nconc (loop :for x from x-start to x-end
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

(defmethod print-text ((r rect) x y text)
  (with-accessors ((x1 rect/x1)
                   (y1 rect/y1)
                   (w rect/w)
                   (h rect/h)) r
    (blt:print (+ x x1) (+ y y1) text :width (- w 2) :height (- h 2))))

(defmethod draw ((r rect) &key caption)
  (with-accessors ((x1 rect/x1)
                   (y1 rect/y1)
                   (w rect/w)
                   (h rect/h)) r
    (blt:draw-box x1 y1 w h)
    (when caption
      (print-text r 1 0 caption))))

(defclass game-map ()
  ((width :initarg :width :reader game-map/w)
   (height :initarg :height :reader game-map/h)
   (tiles :accessor game-map/tiles)
   (name :initarg :name :accessor game-map/name)
   (id :initarg :id :accessor game-map/id)
   (x-edge :reader game-map/x-edge)
   (y-edge :reader game-map/y-edge)
   (focus :initform (cons 0 0) :accessor game-map/focus)
   (entities :initform `() :accessor game-map/entities)
   (floors :accessor game-map/floors)
   (regions :initform nil :accessor game-map/regions)))

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
  (with-slots (tiles x-edge y-edge width height floors) map
    (setf tiles (make-array (list width height) 
                            :element-type 'tile 
                            :initial-element (copy-tile *null-tile*))
          x-edge (1- width)
          y-edge (1- height))
    (setf floors (make-array (* width height)
                             :fill-pointer 0
                             :adjustable t))))

(defmethod points ((m game-map))
  (with-slots (x-edge y-edge) m
    (points-list 0 x-edge 0 y-edge)))

(defun all-walls (m)
  (loop :for pos in (points m)
        :do (set-tile m pos :wall)
        :finally (return m)))

(defun random-walls (m &optional (chance 50))
  (loop :for pos in (points m)
        :do (if (getf  (roll 100 :target (- 100 chance)) :success)
                (set-tile m pos :wall)
                (set-tile m pos :floor))
        :finally (return m)))

(defun wall-border (m)
  (let ((map-rect (make-instance 'rect 
                                 :x 0 
                                 :y 0 
                                 :w (game-map/w m) 
                                 :h (game-map/h m))))
    (dolist (pt (perimeter map-rect) map-rect)
      (set-tile m pt :wall))))

(defmethod in-bounds-p ((m game-map) coord)
  (and (between-p (car coord) 0 (game-map/x-edge m))
       (between-p (cdr coord) 0 (game-map/y-edge m))))

(defmethod get-tile ((m game-map) coord)
  (if (in-bounds-p m coord)
      (aref (game-map/tiles m) (car coord) (cdr coord))
      *null-tile*))

(defmethod set-tile ((m game-map) coord tile-key)
  (with-accessors ((floors game-map/floors)
                   (tiles game-map/tiles)
                   (id game-map/id)) m
    (let* ((-tile (get-tile m coord))
           (explored (tile-explored -tile)))
      (setf (aref tiles (car coord) (cdr coord)) 
            (create-tile :tile-type tile-key :explored explored))
      (if (eql tile-key :floor)
          (unless (find coord floors :test #'equal)
            (vector-push coord floors))
          (delete coord floors :test #'equal)))))

(defmethod adj ((m game-map) coord &key (include-walls nil) &allow-other-keys)
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

(defmethod random-floor ((m game-map) &key (radius 1) center-point)
  (if center-point
    (let ((cands (remove-if-not #'(lambda (c) 
                                    (<= (distance c center-point) radius)) 
                                (game-map/floors m))))
      (get-random-element cands))
    (get-random-element (game-map/floors m))))

(defmethod blocked-p ((m game-map) coord)
  (tile-block-path (get-tile m coord)))

(defmethod opaque-p ((m game-map) coord) 
  (tile-block-sight (get-tile m coord)))

(defmethod explored-p ((m game-map) coord)
  (tile-explored (get-tile m coord)))

(defmethod explore ((m game-map) coord)
  (setf (tile-explored (get-tile m coord)) t))

(defun viewport (m)
  (multiple-value-bind (x y) (cam m (focus-coord m))
    (points (make-instance 'rect 
                           :x x
                           :y y
                           :w *viewport-width*
                           :h *viewport-height*))))

(defun index->coord (width idx)
  (cons (mod idx width)
        (floor idx width)))

(defun coord->index (width coord)
  (destructuring-bind (x . y) coord
    (+ x (* y width))))

(defun focus-coord (m)
  (let ((focus (game-map/focus m)))
    (etypecase focus
      (cons focus)
      (entity (pos focus)))))

(defun map->screen (m coord)
  (let* ((center-point (focus-coord m))
         (x (car coord))
         (y (cdr coord)))
    (multiple-value-bind (left top) (cam m center-point)
      (cons (- x left) (- y top)))))

(defmethod draw ((m game-map) &key)
  (loop :for i below (* *viewport-width* *viewport-height*)
        :for screen-coord = (index->coord *viewport-width* i)
        :with vw = (viewport m)
        :for tl = (get-tile m (nth i vw))
        :for (screen-x . screen-y) = screen-coord
        :for glyph = (tile-glyph tl)
        :for color = (tile-color tl)
        :unless (zerop (char-code glyph))
          :do (setf (blt:color) (color-from-name color)
                    (blt:cell-char screen-x screen-y) glyph)
        :end))

(defun on-screen (coord)
  (destructuring-bind (x . y) coord
    (and (between-p x 0 (1- *viewport-width*)) 
         (between-p y 0 (1- *viewport-height*)))))

(defmethod draw-entities ((m game-map))
  (loop :for e in (game-map/entities m)
        :for screen-coord = (map->screen m (pos e))
        :for glyph = (entity/char e)
        :for color = (entity/color e)
        :for (x . y) = screen-coord
        :if (on-screen screen-coord)
          :do (setf (blt:color) (color-from-name color)
                    (blt:cell-char x y) glyph)
        :end
        :finally (setf (blt:color) (blt:white))))

(defmethod add-entity ((m game-map) (e entity))
  (pushnew e (game-map/entities m)))

(defmethod remove-entity ((m game-map) (e entity))
  (setf (game-map/entities m) (remove e (game-map/entities m))))

(defmethod iterate-dungeon ((m game-map))
  (loop :for c in (points m)
        :for neis = (adj m c :include-walls t)
        :for walls = (count-if #'(lambda (n) (blocked-p m n)) neis)
        :if (>= walls 5)
          :collect c into make-walls
        :end
        :if (< walls 4)
          :collect c into make-floors
        :end
        :finally (dolist (w make-walls) (set-tile m w :wall))
        :finally (dolist (f make-floors) (set-tile m f :floor))))

(defmethod add-to-region ((m game-map) coord region)
  (push (getf (game-map/regions m) region) coord))

(defmethod get-region ((m game-map) region)
  (getf (game-map/regions m) region))

(defmethod flood-fill ((m game-map) coord region)
  (loop :with bucket = (make-array)
        :while bucket
        :for next = (pop bucket)
        :for n = (translate-coord next +north+)
        :for s = (translate-coord next +south+)
        :for e = (translate-coord next +east+)
        :for w = (translate-coord next +west+)
        :collecting next into checked 
        :unless (or (find next checked :test #'equal) (blocked-p m next))
          :do (push n (cdr (last bucket))) 
          :and :do (push s (cdr (last bucket))) 
          :and :do (push w (cdr (last bucket))) 
          :and :do (push e (cdr (last bucket))) 
          :and :do (add-to-region m next region)
        :end
        :do (debug-print "MAP" "Flood fill: bucket ~A, checked ~A" bucket checked)
        :finally (return (get-region m region))))
