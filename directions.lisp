(in-package :wolfs-den-lisp)

(defparameter +north+ (cons 0 -1))
(defparameter +northeast+ (cons 1 -1))
(defparameter +east+ (cons 1 0))
(defparameter +southeast+ (cons 1 1))
(defparameter +south+ (cons 0 1))
(defparameter +southwest+ (cons -1 1))
(defparameter +west+ (cons -1 0))
(defparameter +northwest+ (cons -1 -1))

(defun translate-coord (coord delta)
  (cons (+ (car coord) (car delta)) 
        (+ (cdr coord) (cdr delta))))

(defun distance (a b)
  (destructuring-bind (x1 . y1) a
    (destructuring-bind (x2 . y2) b
      (max (abs (- y2 y1)) 
           (abs (- x2 x1))))))

(defun euclid-distance (a b)
  (let* ((x1 (car a))
         (y1 (cdr a))
         (x2 (car b))
         (y2 (cdr b))
         (dx (- x2 x1))
         (dy (- y2 y1)))
    (sqrt (+ (* dy dy) (* dx dx)))))

(defun manhattan-distance (a b)
  (destructuring-bind (x1 . y1) a
    (destructuring-bind (x2 . y2) b
      (+ (abs (- x2 x1))
         (abs (- y2 y1))))))

(defun adj-p (a b)
  (= (distance a b) 1))

