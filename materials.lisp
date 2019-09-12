(in-package #:wolfs-den-lisp)

(define-ego :oak (list :tier 1
                       :name "oak"
                       :types '(:item)
                       :color "oak"
                       :hardness 10
                       :staff '(:pwr 5 :wil 5)
                       :hammer '(:dmg 1)
                       :freq 30
                       :tags '(:wood :material)))

(define-ego :iron (list :tier 1
                        :name "iron"
                        :types '(:item)
                        :color "iron"
                        :hardness 20
                        :sword '(:dmg 2)
                        :axe '(:dmg 2)
                        :hammer '(:atp -5 :dmg 3)
                        :armor '(:atp -5)
                        :freq 50
                        :tags '(:metal :material)))

(define-ego :steel (list :tier 2
                         :name "steel"
                         :types '(:item)
                         :color "steel"
                         :hardness 30
                         :sword '(:dmg 3 :atp 5)
                         :axe '(:dmg 4)
                         :hammer '(:dmg 5)
                         :armor '(:atp -10)
                         :light '(:atp 10 :dmg 2)
                         :freq 40
                         :tags '(:metal :material)))

(define-ego :bone (list :tier 2
                        :name "bone"
                        :types '(:item)
                        :color "bone"
                        :hardness 5
                        :sword '(:atp 5 :dmg -1 :pwr 5)
                        :armor '(:pwr 5 :res 10)
                        :staff '(:pwr 10 :wil 5)
                        :freq 20
                        :tags '(:natural :material)))

(define-ego :blackiron (list :tier 5
                             :name "blackiron"
                             :types '(:item)
                             :color "blackiron"
                             :hardness 40
                             :sword '(:atp 5 :dmg 3 :res 5 :pwr -10 :wil -10)
                             :axe '(:dmg 4 :res 5 :pwr -10 :wil -10)
                             :light '(:atp 10 :dmg 2 :res 5 :pwr -10 :wil -10)
                             :hammer '(:dmg 5 :res 5 :pwr -10 :wil -10)
                             :armor '(:res 10 :pwr -20 :wil -20)
                             :freq 1
                             :tags '(:metal :material)))
