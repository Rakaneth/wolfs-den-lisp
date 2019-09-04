(in-package #:wolfs-den-lisp)

(defparameter +human-tags+ `(:humanoid :human))
(defparameter +wolf-tags+ `(:wolf :animal))
(defparameter +human-start+ `(:ration :ration :potion :potion))

(define-creature :keldun (list :name "Keldunian"
                               :desc "Human of Keldun"
                               :id "keldun-human"
                               :color "yellow"
                               :glyph #\@
                               :freq 5
                               :tags (copy-list +human-tags+)
                               :random-equip '((:sword :axe :light) (:armor))))

(define-creature :wolf (list :name "Wolf"
                             :desc "A large wolf."
                             :id "wolf-basic"
                             :color "sepia"
                             :glyph #\W
                             :freq 10
                             :tags (copy-list +wolf-tags+)
                             :tier 1))

(define-creature :native (list :name "Salabanian"
                               :desc "Human of Salaban"
                               :start-items (copy-list +human-start+)
                               :freq 10
                               :tags (copy-list +human-tags+)
                               :random-equip '((:axe) (:armor :leather))))

