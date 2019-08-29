(in-package #:wolfs-den-lisp)

(defclass title-screen (screen) ())

(defmethod initialize-instance :after ((ts title-screen) &key)
  (setf (slot-value ts 'id) "title"))

(defmethod draw ((ts title-screen))
  (print-center  "Wolf's Den II: Common Lisp Edition" 20 *screen-width*)
  (print-center "by Rakaneth" 21 *screen-width*))

(defmethod handle ((ts title-screen))
  (blt:key-case (blt:read)
                (:escape nil)
                (:close nil)
                (t (push-screen (make-instance 'new-game-menu 
                                               :name "new-game"
                                               :menu-items `("New Game" "Continue"))))))
