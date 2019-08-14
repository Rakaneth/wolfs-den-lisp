;;;; wolfs-den-lisp.lisp

(in-package #:wolfs-den-lisp)

(defparameter *screen-width* 100)
(defparameter *screen-height* 40)

(defun render (entities)
  (blt:clear)
  (mapc #'draw entities)
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
                          :color (blt:blue))
          :with entities = (list player)
          :do
            (render entities)
            (blt:key-case (blt:read)
                          (:numpad-8 (move-coord player +north+))
                          (:numpad-9 (move-coord player +northeast+))
                          (:numpad-6 (move-coord player +east+))
                          (:numpad-3 (move-coord player +southeast+))
                          (:numpad-2 (move-coord player +south+))
                          (:numpad-1 (move-coord player +southwest+))
                          (:numpad-4 (move-coord player +west+))
                          (:numpad-7 (move-coord player +northwest+))
                          (:escape (return))
                          (:close (return))))))
