(in-package #:wolfs-den-lisp)

(defvar *maps* (make-hash-table :test 'equal))
(defparameter *viewport-width* 60)
(defparameter *viewport-height* 30)
(defparameter *region-min* 30)

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
   (regions :initform nil :accessor game-map/regions)
   (marked :initform nil :accessor game-map/marked)
   (wall-color :initarg :wall-color :accessor game-map/wall-color)
   (floor-color :initarg :floor-color :accessor game-map/floor-color)))

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
            (create-tile :t-type tile-key :explored explored))
      (if (eql tile-key :floor)
          (unless (find coord floors :test #'equal)
            (vector-push coord floors))
          (delete coord floors :test #'equal)))))

(defmethod adj ((m game-map) coord &key include-walls four-way)
  (let* ((x (car coord))
         (y (cdr coord))
         (xe (game-map/x-edge m))
         (ye (game-map/y-edge m))
         (x1 (clamp (1- x) 0 xe))
         (x2 (clamp (1+ x) 0 xe))
         (y1 (clamp (1- y) 0 ye))
         (y2 (clamp (1+ y) 0 ye))
         (cands (points-list x1 x2 y1 y2))
         (test (lambda (p) (and (not (equal coord p))
                                (or include-walls (not (blocked-p m p)))
                                (or (not four-way) (member p `((,x . ,y1)
                                                               (,x . ,y2)
                                                               (,x1 . ,y)
                                                               (,x2 . ,y))
                                                           :test #'equal))))))
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
          :do (put-char screen-coord glyph color)
        :end))

(defun on-screen (coord)
  (destructuring-bind (x . y) coord
    (and (between-p x 0 (1- *viewport-width*)) 
         (between-p y 0 (1- *viewport-height*)))))

(defmethod draw-entities ((m game-map))
  (loop :for e in (sort (copy-seq (game-map/entities m)) #'< :key #'entity/layer)
        :for glyph = (entity/char e)
        :for color = (entity/color e)
        :for pt = (pos e)
        :do (draw-on-map m pt glyph color)))

(defmethod draw-marked ((m game-map))
  ;; (debug-print "MAP-DRAWING: MARKED" "~A" (game-map/marked m))
  (dolist (mk (game-map/marked m))
    (draw-on-map m mk #\* "red")))

(defmethod draw-on-map ((m game-map) coord glyph color)
  (let ((screen-coord  (map->screen m coord)))
    (when (on-screen screen-coord)
      (put-char screen-coord glyph color))))

(defmethod add-entity ((m game-map) (e entity))
  (pushnew e (game-map/entities m)))

(defmacro by-id (fn seq id-sym)
  `(,fn #'(lambda (e) (search ,id-sym (entity/id e)))
        ,seq))

(defmethod remove-entity ((m game-map) eid)
  (setf (game-map/entities m) (by-id remove-if (game-map/entities m) eid)))

(defmethod get-entity ((m game-map) eid)
  (by-id find-if (game-map/entities m) eid))

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

;; A* pathfinding
(defmethod find-path (start dest (m game-map) &key cost-fn four-way)
  (loop :with q = (create-pri-queue)
        :with came-from = (make-hash-table :test #'equal)
        :with cost-so-far = (make-hash-table :test #'equal)
        :with debug-topic = (format nil "PATHFINDING: ~a to ~a" start dest)
        :with closed-list
          :initially (setf (gethash start came-from) :start)
          :initially (setf (gethash start cost-so-far) 0)
          :initially (priority-enqueue 0 start q)
          :initially (debug-print debug-topic "START")
        :until (queue-empty-p q)
        :for current-q-node = (dequeue q)
        :for current = (pri-node/data current-q-node)
        :for current-f = (pri-node/weight current-q-node)
        :do (push current closed-list)
        :do (debug-print debug-topic "Examining current: ~a (~d)" 
                         current current-f)
        :do (debug-print debug-topic "F-values: ~a" (get-keys q))
        :when (equal current dest)
          :do (return (path-from came-from dest))
        :end
        :do (loop :with neis = (adj m current :include-walls t 
                                              :four-way four-way)
                  :for nei in neis
                  :for cost = (if cost-fn (funcall cost-fn nei) 1)
                  :for g = (and cost (+ cost (gethash current cost-so-far)))
                  :for h = (and g (manhattan-distance nei dest))
                  :for f = (and h (+ g h))
                  :for nei-idx = (get-idx nei q)
                  :if (and f
                           (not (member nei closed-list :test #'equal))
                           (or (not (nth-value 1 (gethash nei cost-so-far)))
                               (< g (gethash nei cost-so-far))))
                    :do (setf (gethash nei cost-so-far) g
                              (gethash nei came-from) current)
                    :and :do (if nei-idx
                                 (debug-print debug-topic 
                                              "better path found for ~a: ~d -> ~d"
                                              nei (get-key nei-idx q) f)
                                 (debug-print debug-topic
                                              "Node ~a (score ~d)  is new" nei f))
                    :and :do (if nei-idx
                                 (decrease-key nei-idx f q)
                                 (priority-enqueue f nei q))
                  :end)))

(defmethod frontier ((m game-map) region)
  (remove-if-not #'(lambda (pt) (near-wall-p m pt)) region))

(defun path-from (path-table dest)
  (loop :with next = dest
        :with path = (list dest)      
        :until (eq :start next)
        :for n = (gethash next path-table)
        :do (push n path)
        :do (setf next n)
        :finally (return (cddr path))))

;; debugging paths
(defmethod draw-path (path (m game-map))
  (dolist (step path)
    (mark m step)))

(defmethod get-player-entity ((m game-map))
  (find-if #'(lambda (e) (entity/player-p e)) (game-map/entities m)))

(defmethod near-wall-p ((m game-map) coord)
  (and (some #'(lambda (pt) (blocked-p m pt)) 
             (adj m coord :include-walls t :four-way t))
       (floor-p m coord)))

(defmethod door-support-p ((m game-map) coord)
  (or ()))

(defmethod perimeter-p ((m game-map) coord)
  (destructuring-bind (x . y) coord
    (or (zerop x)
        (zerop y)
        (= (game-map/x-edge m) x)
        (= (game-map/y-edge m) y))))

(defmacro is-tile (mp coord type)
  `(eq (tile-type (get-tile ,mp ,coord)) ,type))

(defmethod wall-p ((m game-map) coord)
  (is-tile m coord :wall))

(defmethod floor-p ((m game-map) coord)
  (is-tile m coord :floor))

(defmethod fill-caves ((m game-map))
  (with-accessors ((regions game-map/regions)) m
    (loop :for (item region) :on regions :by #'cddr
          :if (<= (length region) *region-min*)
            :do (dolist (pt region)
                  (set-tile m pt :wall))
            :and :do (remf regions item)
          :end
          :finally (return regions))))

(defmethod mark ((m game-map) coord)
  (pushnew coord (game-map/marked m)))

(defmethod clear-marks ((m game-map))
  (setf (game-map/marked m) nil))

(defmethod %connect-fn ((m game-map))
  (lambda (pt)
    (cond
      ((perimeter-p m pt) (debug-print "PATHFINDING-CAVES" "Excluding ~a, on perimeter" pt) nil)
      ((wall-p m pt) 1)
      ((near-wall-p m pt) 10)
      (t (debug-print "PATHFINDING-CAVES" "Excluding ~a, not a wall or near a wall" pt) nil))))

(defmethod connect-regions ((m game-map))
  (loop :for (reg-id region) :on (game-map/regions m) :by #'cddr
        :collect (frontier m region) :into regions
        :finally (reduce #'(lambda (ra rb)
                             (let* ((pa (get-random-element ra))
                                    (pb (get-random-element rb))
                                    (fn (%connect-fn m))
                                    (path (find-path pa pb m
                                                     :four-way t
                                                     :cost-fn fn)))
                               (dolist (p path rb)
                                 (set-tile m p :floor)))) 
                         regions)
        :finally (return m)))

(defmethod radius ((m game-map) r center-point &key rad-type include-walls)
  (let ((fn (case rad-type
              (:euclid #'euclid-distance)
              (:manhattan #'manhattan-distance)
              (t #'distance))))
    (remove-if-not #'(lambda (pt)
                       (and (<= (funcall fn center-point pt) r)
                            (or include-walls (not (blocked-p m pt)))))
                   (points m))))
