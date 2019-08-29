(in-package #:wolfs-den-lisp)

(defclass new-game-menu (menu) ())

(defmethod handle ((ngm new-game-menu))
  (blt:key-case (blt:read)
                (:escape (pop-screen) t)
                (:return (if (zerop (menu/selected ngm))
                             (progn
                               (clear-screens)
                               (add-map (make-instance 'game-map 
                                                        :id "mines-upper"
                                                        :name "Upper Mines"
                                                        :width 100
                                                        :height 30))
                               (push-screen (make-instance 'main-screen 
                                                           :cur-map (get-map "mines-upper")))
                               (random-walls (get-map "mines-upper"))
                               t)))))
