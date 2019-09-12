(in-package #:wolfs-den-lisp)

(defclass main-screen (screen) 
  ((cur-map :initarg :cur-map :accessor main-screen/cur-map)
   (msgs 
    :initform (make-instance 'rect :x 0 :y 30 :w 30 :h 10)
    :accessor main-screen/msgs)
   (skls 
    :initform (make-instance 'rect :x 30 :y 30 :w 30 :h 10)
    :accessor main-screen/skls)
   (info 
    :initform (make-instance 'rect :x 60 :y 30 :w 40 :h 10)
    :accessor main-screen/info)
   (stats
    :initform (make-instance 'rect :x 60 :y 0 :w 40 :h 30)
    :accessor main-screen/stats)
   (target-mode
    :initform nil
    :accessor main-screen/target-mode)
   (target
    :initform nil
    :accessor main-screen/target)
   (target-radius
    :initform nil
    :accessor main-screen/target-radius)
   (target-rad-type
    :initform nil
    :accessor main-screen/target-rad-type)))

(defmethod draw-path-to-target ((ms main-screen))
  (let* ((m (main-screen/cur-map ms))
         (player (game-map/focus m))
         (path (find-path (pos player)
                          (main-screen/target ms)
                          m
                          :cost-fn #'(lambda (pt)
                                       (cond
                                         ((wall-p m pt) 1)
                                         ((near-wall-p m pt) 10)
                                         (t nil)))
                          :four-way t)))
    (draw-path path m)
    (or path t)))

(defmethod initialize-instance :after ((ms main-screen) &key)
  (setf (slot-value ms 'id) "main"))

(defmethod handle ((ms main-screen))
  (let* ((m (main-screen/cur-map ms))
         (player (game-map/focus m))
         (target (main-screen/target ms)))
    (blt:key-case (blt:read)
                  (:numpad-8 (move-by player +north+))
                  (:numpad-9 (move-by player +northeast+))
                  (:numpad-6 (move-by player +east+))
                  (:numpad-3 (move-by player +southeast+))
                  (:numpad-2 (move-by player +south+))
                  (:numpad-1 (move-by player +southwest+))
                  (:numpad-4 (move-by player +west+))
                  (:numpad-7 (move-by player +northwest+))
                  (:space (toggle-target-mode ms :r 7) t)
                  (:p (if target (draw-path-to-target ms) t))
                  (:c (clear-marks m) (clear-targeting ms) t)
                  (:close nil)
                  (t (debug-print "SCREEN" "Key pressed") t))))

(defmethod draw-stats ((ms main-screen))
  (with-accessors ((stats main-screen/stats)
                   (m main-screen/cur-map)) 
      ms
    (let ((player (game-map/focus m)))
      (draw stats :caption "Stats")
      (print-text stats 1 1 (display-string player))
      (print-text stats 1 2 (format nil "~A (~d, ~d)" 
                                    (game-map/name m) 
                                    (entity/x player) 
                                    (entity/y player)))
      (print-text stats 1 3 (format nil "Target: ~@[~a~]" (main-screen/target ms))))))

(defmethod draw-ui ((ms main-screen) &key)
  (with-accessors ((msgs main-screen/msgs)
                   (skls main-screen/skls)
                   (info main-screen/info)) 
      ms
    (draw msgs :caption "Messages")
    (draw skls :caption "Skills")
    (draw info :caption "Info")
    (draw-stats ms)))


(defmethod draw ((ms main-screen) &key)
  (let ((m (main-screen/cur-map ms)))
    (draw m)
    (draw-targeting ms)
    (draw-entities m)
    (draw-ui ms)
    (draw-marked m)))

(defmethod toggle-target-mode ((ms main-screen) &key r rad-type)
  (with-accessors ((m main-screen/cur-map)
                   (targeting main-screen/target-mode)
                   (target main-screen/target)
                   (r-type main-screen/target-rad-type)
                   (radius main-screen/target-radius))
      ms
    (if targeting
        (progn
          (setf targeting nil
                (game-map/focus m) (get-player-entity m)
                target (pos (get-entity m "cursor")))
          (remove-entity m "cursor"))
        (with-accessors ((x entity/x)
                         (y entity/y))
            (game-map/focus m)
          (let ((cursor  (make-instance 'entity 
                                        :e-type :cursor
                                        :id "cursor"
                                        :char #\X
                                        :x x
                                        :y y
                                        :color "red"
                                        :name "Targeting")))
            (add-entity m cursor)
            (setf (game-map/focus m) cursor
                  targeting t
                  r-type rad-type
                  radius r))))))

(defmethod clear-targeting ((ms main-screen))
  (with-accessors ((target main-screen/target)
                   (t-type main-screen/target-rad-type)
                   (t-rad main-screen/target-radius)) 
      ms
    (setf target nil
          t-type nil
          t-rad nil)))

(defmethod draw-targeting ((ms main-screen))
  (with-accessors ((r-type main-screen/target-rad-type)
                   (r main-screen/target-radius)
                   (m main-screen/cur-map)
                   (targeting main-screen/target-mode))
      ms
    (when (and r targeting)
      (dolist (pt (radius m r (focus-coord m) :rad-type r-type))
        (draw-on-map m pt #\* "yellow")))))

