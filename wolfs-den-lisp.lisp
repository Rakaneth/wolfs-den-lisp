;;;; wolfs-den-lisp.lisp

(in-package #:wolfs-den-lisp)



(defun render (entities)
  (blt:clear)
  (mapc #'draw entities)
  (blt:print 50 0 (entity/name (first entities)))
  (blt:refresh))

(defun config ()
  (blt:set "window.resizeable = true")
  (blt:set "window.size = ~Ax~A" *screen-width* *screen-height*)
  (blt:set "window.title = Wolf's Den II: Common Lisp Edition"))

(defun main()
  (blt:with-terminal
    (config)
    (loop :with player = (make-instance 'entity
                          :x 20
                          :y 20
                          :char #\@
                          :color "sepia")
          :with entities = (list player)
          :do
            (render entities)
            (blt:key-case (blt:read)
                          (:numpad-8 (move-by player +north+))
                          (:numpad-9 (move-by player +northeast+))
                          (:numpad-6 (move-by player +east+))
                          (:numpad-3 (move-by player +southeast+))
                          (:numpad-2 (move-by player +south+))
                          (:numpad-1 (move-by player +southwest+))
                          (:numpad-4 (move-by player +west+))
                          (:numpad-7 (move-by player +northwest+))
                          (:escape (return))
                          (:close (return))))))
