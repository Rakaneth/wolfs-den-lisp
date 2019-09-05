(in-package #:wolfs-den-lisp)

;;; set up screen widths/heights

(defparameter *screen-width* 100)
(defparameter *screen-height* 40)

;;; set up logging

(defvar *log-path* "wolfs-den-log.log")
(defvar *debug-mode* t)

(defvar *game-turn* 0)

(defun debug-print (topic &rest args)
  (when *debug-mode*
      (with-open-file (f *log-path* 
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)
        (format f "~a: [~a] ~a~%" *game-turn* topic 
                (apply #'format (cons nil args))))))

