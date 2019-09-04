(in-package #:wolfs-den-lisp)

(defparameter +human-tags+ `(:humanoid :human))
(defparameter +wolf-tags+ `(:wolf :animal))
(defparameter +human-start+ `(:ration :ration :potion :potion))

(define-creature :keldun (list :name "Keldunian"
                               :desc "Human of Keldun"
                               :id "keldun"
                               :color "yellow"
                               :freq 5
                               :tags (copy-list +human-tags+)
                               :random-equip '((:sword :axe :light) (:armor))))

(define-creature :wolf (list :name "Wolf"
                             :desc "A large wolf."
                             :id "wolf"
                             :color "sepia"
                             :glyph #\W
                             :freq 10
                             :tags (copy-list +wolf-tags+)
                             :stats '(:str 15 :stam 15 :smt 5 :atp 2 :vis 10)
                             :tier 1))

(define-creature :native (list :name "Salabanian"
                               :desc "Human of Salaban"
                               :id "native"
                               :start-items (copy-list +human-start+)
                               :freq 10
                               :tags (copy-list +human-tags+)
                               :random-equip '((:axe) (:armor :leather))))

