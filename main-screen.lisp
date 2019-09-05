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
    :accessor main-screen/stats)))

(defmethod initialize-instance :after ((ms main-screen) &key)
  (setf (slot-value ms 'id) "main"))

(defmethod handle ((ms main-screen))
  (let ((player (game-map/focus (main-screen/cur-map ms))))
    (blt:key-case (blt:read)
                  (:numpad-8 (move-by player +north+))
                  (:numpad-9 (move-by player +northeast+))
                  (:numpad-6 (move-by player +east+))
                  (:numpad-3 (move-by player +southeast+))
                  (:numpad-2 (move-by player +south+))
                  (:numpad-1 (move-by player +southwest+))
                  (:numpad-4 (move-by player +west+))
                  (:numpad-7 (move-by player +northwest+))
                  (:space (iterate-dungeon (main-screen/cur-map ms)) t)
                  (:close nil)
                  (t (format t "Key pressed~%") t))))

(defmethod draw-ui ((ms main-screen) &key)
  (with-accessors ((msgs main-screen/msgs)
                   (skls main-screen/skls)
                   (info main-screen/info)
                   (stats main-screen/stats)) ms
    (draw msgs :caption "Messages")
    (draw skls :caption "Skills")
    (draw info :caption "Info")
    (draw stats :caption "Stats")))


(defmethod draw ((ms main-screen) &key)
  (let ((m (main-screen/cur-map ms)))
    (draw m)
    (draw-entities m)
    (draw-ui ms)))

