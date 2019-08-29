(in-package #:wolfs-den-lisp)

(defclass main-screen (screen) 
  ((cur-map :initarg :cur-map :accessor main-screen/cur-map)))

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
                  (:close nil)
                  (t (format t "Key pressed~%") t))))


(defmethod draw ((ms main-screen))
  (draw (main-screen/cur-map ms)))
