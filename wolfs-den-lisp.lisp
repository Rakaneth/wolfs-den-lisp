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
                          :color (blt:white))
          :with entities = (list player)
          :do
            (render entities)
            (blt:key-case (blt:read)
                          (:escape (return))
                          (:close (return))))))