(in-package #:wolfs-den-lisp)

(defclass new-game-menu (menu) ())

(defun new-game! ()
  (let ((start-map (create-map :mines-upper))
        (player (create-creature :keldun :player t :pos '(15 . 27)))
        (npc (create-creature :wolf :pos '(1 . 2))))
    (add-map start-map)
    (add-entity start-map player)
    (add-entity start-map npc)
    (setf (game-map/focus start-map) player)))

(defmethod handle ((ngm new-game-menu))
  (blt:key-case (blt:read)
                (:escape (pop-screen) t)
                (:return (progn 
                           (clear-screens)
                           (new-game!)
                           (push-screen (make-instance 'main-screen
                                                       :cur-map (get-map "mines-upper")))
                           t))))
