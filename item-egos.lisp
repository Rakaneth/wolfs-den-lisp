(in-package #:wolfs-den-lisp)

(define-ego :sharp (list :name "sharp"
                         :types '(:item :weapon :sword :light)
                         :freq 10
                         :sword '(:atp 5 :dmg 1)
                         :light '(:atp 5 :dmg 1)
                         :tags '(:prefix)))

(define-ego :of-magic (list :name "of magic"
                            :types '(:item :weapon)
                            :sword '(:pwr 5 :wil 5)
                            :axe '(:pwr 5 :wil 5)
                            :hammer '(:pwr 5 :wil 5)
                            :staff '(:pwr 10 :wil 10)
                            :light '(:pwr 5 :wil 5)
                            :armor '(:pwr 10 :wil 10)
                            :freq 5
                            :tags '(:suffix)))
