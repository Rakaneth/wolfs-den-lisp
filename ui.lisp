(in-package #:wolfs-den-lisp)

(defgeneric draw (drawable)
  (:documentation "Draw an object to the screen."))

(defgeneric handle (screen key-code shift-p)
  (:documentation "Handle inputs on the screen."))

(defclass screen ()
  ((screen/id :initarg :id :reader screen/id)))

(defmethod enter ((s screen))
  (format t "Entered ~a screen.~%" (screen/id s)))

(defmethod exit ((s screen))
  (format t "Exited ~a screen.~%" (screen/id s)))

(defmethod handle ((s screen) key-code shift-p)
  (format t "Pressed ~a key.~@[Shift is down.~]~%" key-code shift-p))

(defun push-screen (s)
  (enter s)
  (vector-push-extend s *screens*))

(defun pop-screen ()
  (exit (vector-pop *screens*)))

