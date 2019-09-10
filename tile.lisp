(in-package #:wolfs-den-lisp)

(defstruct tile 
  (glyph #\Nul)
  (color "transparent")
  (block-sight t)
  (block-path t)
  (explored nil)
  (type :null-tile))

(defun points-list (x-start x-end y-start y-end)
  (loop :for y from y-start to y-end
        :nconc (loop :for x from x-start to x-end
                     :collect (cons x y))))

(defparameter *null-tile* (make-tile))

(defun create-tile (&key t-type (color "white") (explored nil))
  (case t-type
    (:floor (make-tile :glyph #\. 
                       :color color 
                       :block-sight nil 
                       :block-path nil
                       :type t-type
                       :explored explored))
    (:wall (make-tile :glyph #\# 
                      :color color 
                      :explored explored 
                      :type t-type))
    (:stairs-up (make-tile :glyph #\< 
                           :color "yellow" 
                           :block-sight nil
                           :block-path nil
                           :type t-type
                           :explored explored))
    (:stairs-down (make-tile :glyph #\> 
                             :color "yellow" 
                             :block-sight nil
                             :block-path nil
                             :type t-type
                             :explored explored))
    (t (make-tile))))
