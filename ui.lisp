(in-package #:wolfs-den-lisp)

(defvar *screens* (make-array 10 :fill-pointer 0 :adjustable t))

(defun reset-screens ()
  (setq *screens* (make-array 10 :fill-pointer 0 :adjustable t)))

(defun clear-screens ()
  (loop :until (zerop (length *screens*))
        :do (pop-screen)))

(defun print-center (text y width)
  (let* ((x (floor (- width (length text)) 2)))
    (blt:print x y text)))

(defgeneric draw (drawable)
  (:documentation "Draw an object to the screen."))

(defgeneric handle (screen)
  (:documentation "Handle inputs on the screen. Should return NIL to exit the game."))

(defclass screen ()
  ((id :initarg :id :reader screen/id)))

(defmethod enter ((s screen))
  (format t "Entered ~a screen.~%" (screen/id s)))

(defmethod exit ((s screen))
  (format t "Exited ~a screen.~%" (screen/id s)))

(defmethod handle ((s screen))
  (blt:key-case (blt:read)
                (:escape nil)
                (:close nil)
                (t (progn
                     (format t "pressed key~%")
                     t))))

(defun push-screen (s)
  (enter s)
  (vector-push-extend s *screens*))

(defun pop-screen ()
  (exit (vector-pop *screens*)))

(defun cur-screen () 
  (elt *screens* (1- (length *screens*))))

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

(defclass menu (screen) 
  ((items :initarg :menu-items :accessor menu/items)
   (selected :initform 0 :accessor menu/selected)
   (name :initarg :name :accessor menu/name)))

(defmethod initialize-instance :after ((m menu) &key)
  (setf (slot-value m 'id) 
        (format nil "~A-menu" (menu/name m))))

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

(defmethod draw ((m menu))
  (let* ((lst (menu/items m))
         (max-string (reduce (lambda (a b)
                               (max (length a) (length b)))
                             lst))
         (box-width (+ 2 max-string))
         (box-height (+ 2 (length lst)))
         (x (/ (- *screen-width* box-width) 2))
         (y (/ (- *screen-height* box-height) 2)))
    (blt:draw-box x y box-width box-height)
    (loop :for item in lst
          :for idx = (position item lst)
          :for color = (if (= idx (menu/selected m))
                           (blt:cyan)
                           (blt:white))
          :do (setf (blt:color) color) 
              (blt:print (1+ x) (+ 1 idx y) item)
          :finally (setf (blt:color) (blt:white)))))



