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
  (if (show-region m region)
      (push coord (getf (game-map/regions m) region))
      (setf (getf (game-map/regions m) region) (list coord))))

(defmethod show-region ((m game-map) region)
  (getf (game-map/regions m) region))

(defmethod get-region ((m game-map) coord)
  (loop :for (region pts) on (game-map/regions m) by #'cddr
        :when (find coord pts :test #'equal)
          :do (return region)
        :end))

(defmethod flood-fill ((m game-map) coord region)
  (let ((bucket (create-queue coord)))
    (loop :while (not (queue-empty-p bucket))
          :for next = (dequeue bucket)
          :for n = (translate-coord next +north+)
          :for s = (translate-coord next +south+)
          :for e = (translate-coord next +east+)
          :for w = (translate-coord next +west+)
          :unless (or (find next checked :test #'equal) 
                      (blocked-p m next))
            :collecting next into checked
            :and :do (enqueue e bucket) 
            :and :do (enqueue s bucket) 
            :and :do (enqueue n bucket) 
            :and :do (enqueue w bucket) 
            :and :do (add-to-region m next region)
          :end
          :finally (return (show-region m region)))))

(defmethod clear-regions ((m game-map))
  (setf (game-map/regions m) nil))

(defmethod find-regions ((m game-map))
  (clear-regions m)
  (loop :for pt in (points m)
        :with idx = 0
        :unless (or (get-region m pt)
                    (blocked-p m pt))
          :do (debug-print "MAP" "Getting regions on ~A" pt)
          :and :do (flood-fill m pt idx)
          :and :do (incf idx)
        :end
        :finally (return (game-map/regions m))))

;;Dijkstra's
(defmethod find-path (start dest (m game-map) &key cost-fn)
  (loop :with q = (create-pri-queue :initial-items (list `(0 . ,start)))
        :with w = (game-map/w m)
        :with came-from = (list (coord->index w start) :start)
        :with cost-so-far = (make-hash-table :test #'equal)
        :initially (setf (gethash start cost-so-far) 0)
        :until (queue-empty-p q)
        :for current = (dequeue q)
        :for pt = (pri-node/data current)
        :for dist = (pri-node/weight current)
        :for current-idx = (coord->index w pt)
        :when (equal pt dest)
          :do (return (path-from came-from dest w))
        :end
        :do (loop :for nei in (adj m pt :include-walls t)
                  :for nei-idx = (coord->index w nei)
                  :for cur-cost = (if cost-fn
                                      (funcall cost-fn pt nei)
                                      1)
                  :for new-cost = (+ (gethash pt cost-so-far) cur-cost)
                  :if (or (not (gethash nei cost-so-far)) 
                          (< new-cost (gethash nei cost-so-far)))
                    :do (setf (gethash nei cost-so-far) new-cost)
                    :and :do (priority-enqueue new-cost nei q)
                    :and :do (setf (getf came-from nei-idx) pt)
                  :end)))

(defun path-from (path-table dest w)
  (loop :with next = dest
        :with path = (list dest)      
        :until (eq :start next)
        :for n = (getf path-table (coord->index w next))
        :do (push n path)
        :do (setf next n)
        :finally (return (cdr path))))

;; debugging paths
(defmethod draw-path (path (m game-map))
  (dolist (step path)
    (set-tile m step :marked)))


