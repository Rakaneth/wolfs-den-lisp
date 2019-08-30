(in-package #:wolfs-den-lisp)

(defclass new-game-menu (menu) ())

(defmethod handle ((ngm new-game-menu))
  (blt:key-case (blt:read)
                (:escape (pop-screen) t)
                (:return (if (zerop (menu/selected ngm))
                             (let ((start-map (create-map :mines-upper))
                                   (player (create-creature :keldun :player t))
                                   (npc (create-creature :wolf :pos '(1 . 2))))
                               (random-walls start-map)
                               (add-map start-map)
                               (add-entity start-map player)
                               (add-entity start-map npc)
                               (push-screen (make-instance 'main-screen
                                                           :cur-map start-map))
                               t)))))
