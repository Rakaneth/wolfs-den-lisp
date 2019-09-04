(in-package #:wolfs-den-lisp)

(defclass entity ()
  ((x 
    :initarg :x 
    :accessor entity/x)
   (y 
    :initarg :y 
    :accessor entity/y)
   (char 
    :initarg :char
    :initform #\@
    :accessor entity/char)
   (color
    :initarg :color 
    :accessor entity/color
    :initform "white")
   (name
    :initarg :name
    :accessor entity/name
    :initform "No name")
   (desc
    :initarg :desc
    :accessor entity/desc
    :initform "No desc")
   (id
    :initarg :id
    :reader entity/id)
   (player-p
    :initform nil
    :accessor entity/player-p
    :initarg :player)
   (tags
    :initform nil
    :accessor entity/tags
    :initarg :tags)
   (stats
    :initform (list :str 0
                    :stam 0
                    :spd 0
                    :sag 0
                    :skl 0
                    :smt 0
                    :atp 0
                    :dfp 0
                    :res 0
                    :tou 0
                    :wil 0
                    :pwr 0
                    :vis 0
                    :dmg 0
                    :hardness 0)
    :accessor entity/stats)
   (inventory
    :initform nil
    :accessor entity/inventory)
   (money
    :initform 0
    :accessor entity/money)))


(defmethod initialize-instance :after ((e entity) &key e-type)
  (with-slots ((id id)
               (tags tags)) e
    (setf id (format nil "~a-~a" id (get-uuid)))
    (push e-type tags)))
    
(defmethod pos ((e entity))
  (cons (entity/x e) (entity/y e)))

(defmethod move-to ((e entity) x y)
  (setf (entity/x e) x)
  (setf (entity/y e) y)
  (pos e))

(defmethod move-by ((e entity) delta)
  (let* ((cur-pos (pos e))
         (new-pos (translate-coord cur-pos delta)))
    (move-to e (car new-pos) (cdr new-pos))))

(defmethod move-by ((c cons) delta)
  (translate-coord c delta))

(defmethod display-string ((e entity))
  (decorate (entity/name e) (entity/color e)))

(defmethod print-object ((e entity) stream)
  (print-unreadable-object (e stream :type t)
    (with-accessors ((name entity/name)
                     (id entity/id)
                     (glyph entity/char)) 
        e
      (format stream "~a ~a (~a)" glyph name id))))

(defmethod has-tag ((e entity) tag)
  (member tag (entity/tags e)))

(defmethod add-tag ((e entity) tag)
  (with-accessors ((tags entity/tags)) e 
    (setf tags (adjoin tag tags))))

(defmethod remove-tag ((e entity) tag)
  (remove tag (entity/tags e)))

(defmethod typeof-entity ((e entity))
  (cond ((has-tag e :creature) :creature)
        ((has-tag e :item) :item)
        (t :unknown)))

(defmethod creature-p ((e entity))
  (has-tag e :creature))

(defmethod item-p ((e entity))
  (has-tag e :item))

(defmacro with-stat-list (e stat-list stats-sym key-sym val-sym &rest body)
  `(loop :for (,key-sym ,val-sym) on ,stat-list by #'cddr 
         :while v
         :with ,stats-sym = (entity/stats ,e)
         :do ,@body
         :finally (return ,stats-sym)))

(defmethod get-stat ((e entity) stat-symbol)
  (getf (entity/stats e) stat-symbol))

(defmethod set-stat! ((e entity) stat-symbol val)
  (setf (getf (entity/stats e) stat-symbol) val))

(defmethod set-stats! ((e entity) stat-list)
  (with-stat-list e stat-list _ k v
    (set-stat! e k v)))

(defmethod mod-stats! ((e entity) stat-list)
  (with-stat-list e stat-list _ k v
    (set-stat! e k (+ v (get-stat e k)))))
