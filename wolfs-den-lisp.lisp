;;;; wolfs-den-lisp.lisp

(in-package #:wolfs-den-lisp)

(defparameter *screen-width* 100)
(defparameter *screen-height* 40)

(defun draw ()
  (blt:clear)
  (blt:refresh))

(defun config ()
  (blt:set "window.resizeable = true")
  (blt:set "window.size = ~Ax~A" *screen-width* *screen-height*)
  (blt:set "window.title = Wolf's Den II: Common Lisp Edition"))

(defun main()
  (blt:with-terminal
    (config)
    (loop :do
      (draw)
      (blt:key-case (blt:read)
                    (:escape (return))
                    (:close (return))))))