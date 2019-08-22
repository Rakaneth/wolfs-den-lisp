;;;; wolfs-den-lisp.lisp

(in-package #:wolfs-den-lisp)

(defun draw-screens () 
  (loop :for s across *screens*
        :do (draw s)))

(defun render ()
  (blt:clear)
  (draw-screens)
  (blt:refresh))

(defun config ()
  (blt:set "window.resizeable = true")
  (blt:set "window.size = ~Ax~A" *screen-width* *screen-height*)
  (blt:set "window.title = Wolf's Den II: Common Lisp Edition"))

(defun new-game ()
  (reset-screens)
  (push-screen (make-instance 'title-screen)))

(defun main()
  (blt:with-terminal
    (config)
    (new-game)
    (loop :for top = (cur-screen)
          :do (render)
              (unless (handle top) (return)))
    (clear-screens)))
