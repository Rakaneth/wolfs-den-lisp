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

