(in-package #:wolfs-den-lisp)

(defvar *screens* (make-array 10 :fill-pointer 0 :adjustable t))

(defun reset-screens ()
  (setq *screens* (make-array 10 :fill-pointer 0 :adjustable t)))

(defun clear-screens ()
  (loop :until (zerop (length *screens*))
        :do (pop-screen)))

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
                     (format t "pressed key")
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
  (blt:print 35 20 "Wolf's Den II: Common Lisp Edition")
  (blt:print 35 21 "by Rakaneth"))

(defmethod handle ((ts title-screen))
  (blt:key-case (blt:read)
                (:escape nil)
                (:close nil)
                (t (push-screen (make-instance 'new-game-menu)))))

(defclass main-screen (screen) 
  ((cur-map :initarg :cur-map :accessor main-screen/cur-map)))

(defmethod initialize-instance :after ((ms main-screen) &key)
  (setf (slot-value ms 'id) "main"))

(defmethod draw ((ms main-screen)))

(defclass menu (screen) 
  ((items :initarg :menu-items :accessor menu/items)
   (selected :initform 0 :accessor menu/selected)
   (name :initarg :name :accessor menu/name)))

(defmethod initialize-instance :after ((m menu) &key)
  (setf (slot-value m 'id) 
        (format nil "~A-menu" (menu/name m))))

(defclass new-game-menu (menu) ())

(defmethod initialize-instance :after ((ngm new-game-menu) &key)
  (declare (ignore initargs))
  (setf (menu/items ngm) (list "New Game" "Continue")
        (menu/name ngm) "new-game")
  (call-next-method))

(defmethod handle ((ngm new-game-menu))
  (blt:key-case (blt:read)
                (:escape (pop-screen))
                (:return (if (zerop (menu/selected ngm))
                             (progn
                               (clear-screens)
                               (add-map (make-instance 'game-map 
                                                        :id "mines-upper"
                                                        :name "Upper Mines"
                                                        :width 100
                                                        :height 30))
                               (push-screen (make-instance 'main-screen 
                                                           :cur-map (get-map "mines=upper"))))))))

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



