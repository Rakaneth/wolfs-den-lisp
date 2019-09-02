(in-package #:wolfs-den-lisp)

(defparameter +human-tags+ `(:humanoid :human))
(defparameter +wolf-tags+ `(:wolf :animal))

(define-template :creature :keldun `(:name "Human of Keldun"
                                     :id "keldun-human"
                                     :color "yellow"
                                     :glyph #\@
                                     :freq 5
                                     :tags ,(copy-list +human-tags+)))

(define-template :creature :wolf `(:name "Wolf"
                                   :id "wolf-basic"
                                   :color "sepia"
                                   :glyph #\W
                                   :freq 10
                                   :tags ,(copy-list +wolf-tags+)))
