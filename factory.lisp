(in-package #:wolfs-den-lisp)

(defvar *creature-templates* (make-hash-table))
(defvar *map-templates* (make-hash-table))
(defvar *item-templates* (make-hash-table))

(defmacro define-template (repo-sym item-key plist-template)
  (let ((repo (ecase repo-sym 
                (:creature '*creature-templates*)
                (:map '*map-templates*)
                (:item '*item-templates*))))
    `(setf (gethash ,item-key ,repo) ,plist-template)))

(defmacro with-template (repo-sym template-id template-sym &body body)
  (let ((repo (ecase repo-sym
                (:map '*map-templates*)
                (:creature '*creature-templates*)
                (:item '*item-templates*))))
    `(let ((,template-sym (gethash ,template-id ,repo)))
       (unless ,template-sym
         (error (format nil "No template ~a in ~a templates" ,template-id ,repo-sym)))
       ,@body)))

(defun create-map (template-id)
  (with-template :map template-id template
    (make-instance 'game-map
                   :id (getf template :id)
                   :name (getf template :name)
                   :width (getf template :width)
                   :height (getf template :height))))

(defun create-creature (template-id &key (pos '(0 . 0)) player name)
  (with-template :creature template-id template
    (make-instance 'entity
                   :id (getf template :id)
                   :name (or name (getf template :name))
                   :color (getf template :color)
                   :char (getf template :glyph)
                   :x (car pos)
                   :y (cdr pos)
                   :player player)))


