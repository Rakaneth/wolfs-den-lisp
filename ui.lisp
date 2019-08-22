(in-package #:wolfs-den-lisp)

(defgeneric draw (drawable)
  (:documentation "Draw an object to the screen."))

(defgeneric handle (screen)
  (:documentation "Handle inputs on the screen. Should return NIL to exit the game."))

(defclass screen ()
  ((screen/id :initarg :id :reader screen/id)))

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
  (setf (slot-value ts 'screen/id) "title"))

(defmethod draw ((ts title-screen))
  (blt:print 35 20 "Wolf's Den II: Common Lisp Edition")
  (blt:print 35 21 "by Rakaneth"))

(defclass main-screen (screen) ())

(defmethod initialize-instance :after ((ms main-screen) &key)
  (setf (slot-value ms 'screen/id) "main"))



