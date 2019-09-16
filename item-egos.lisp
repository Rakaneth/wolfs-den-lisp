(in-package #:wolfs-den-lisp)

(define-ego :sharp (list :name "sharp"
                         :types '(:item :weapon :sword :light)
                         :freq 10
                         :sword '(:dmg 1)
                         :light '(:dmg 2)
                         :tags '(:prefix :sword :axe)
                         :tier 1))

(define-ego :heavy (list :name "heavy"
                         :freq 10
                         :sword '(:atp -5 :dmg 1)
                         :axe '(:atp -10 :dmg 3)
                         :hammer '(:atp -10 :dmg 5)
                         :tags '(:prefix :sword :axe :hammer)
                         :tier 2))

(define-ego :balanced (list :name "balanced"
                            :freq 10
                            :sword '(:atp 5 :dmg -1)
                            :light '(:atp 5)
                            :tags '(:prefix :sword :light)
                            :tier 1))

(define-ego :of-magic (list :name "of magic"
                            :types '(:item :weapon)
                            :sword '(:pwr 5 :wil 5)
                            :axe '(:pwr 5 :wil 5)
                            :hammer '(:pwr 5 :wil 5)
                            :staff '(:pwr 10 :wil 10)
                            :light '(:pwr 5 :wil 5)
                            :armor '(:pwr 10 :wil 10)
                            :freq 5
                            :tags '(:suffix :staff :armor 
                                    :axe :hammer :sword :light)
                            :tier 2))
