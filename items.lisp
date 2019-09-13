(in-package #:wolfs-den-lisp)

(defparameter *natural-weapon-tags* (list :natural :weapon))
(defparameter *mark-tags* (list :holy :dragon :trinket))

(defun mark-tags ()
  (copy-list *mark-tags*))

(defun natural-tags ()
  (copy-list *natural-weapon-tags*))

(define-item :white-mark (list :name "Mark of Ilma"
                               :desc "A holy symbol of the dragon Il'ma'to'ee'rey"
                               :slot :trinket
                               :glyph #\=
                               :dmg 2
                               :tags (mark-tags)))

(define-item :black-mark (list :name "Mark of Barma"
                               :desc "A holy symbol of the dragon Yalakhbarma"
                               :slot :trinket
                               :glyph #\=
                               :color "black-mark"
                               :tou 5
                               :res 5
                               :tags (mark-tags)))

(define-item :red-mark (list :name "Mark of Mekira"
                             :desc "A holy symbol of the dragon Xilomekira"
                             :slot :trinket
                             :color "red-mark"
                             :glyph #\=
                             :atp 5
                             :pwr 5
                             :tags (mark-tags)))

(define-item :green-mark (list :name "Mark of Sansa"
                               :desc "A holy symbol of the dragon Sansapradava"
                               :slot :trinket
                               :color "green-mark"
                               :glyph #\=
                               :dfp 5
                               :res 5
                               :tags (mark-tags)))

(define-item :blue-mark (list :name "Mark of Atara"
                              :desc "A holy symbol of the dragon Ataramakaris"
                              :slot :trinket
                              :color "blue-mark"
                              :wil 5
                              :pwr 5
                              :tags (mark-tags)))

(define-item :axe (list :name "axe"
                        :desc "A large axe made from ~a"
                        :slot :weapon
                        :glyph #\/
                        :material t
                        :atp -5
                        :dmg 10
                        :damage-type :slash
                        :equip-type :axe
                        :tags (list :metal :bone :axe :weapon)
                        :freq 5))

(define-item :sword (list :name "sword"
                          :desc "A shortsword made from ~a"
                          :slot :weapon
                          :glyph #\|
                          :material t
                          :atp 5
                          :dmg 6
                          :damage-type :slash
                          :equip-type :sword
                          :tags (list :metal :bone :sword :weapon)
                          :freq 5))

(define-item :staff (list :name "staff"
                          :desc "A tall staff made from ~a"
                          :slot :weapon
                          :glyph #\_
                          :material t
                          :pwr 5
                          :wil 5
                          :dmg 3
                          :damage-type :magic
                          :equip-type :staff
                          :tags (list :bone :wood)
                          :freq 3))

(define-item :rapier (list :name "rapier"
                           :desc "A slender rapier made from ~a"
                           :slot :weapon
                           :glyph #\DAGGER
                           :material t
                           :atp 10
                           :dmg 4
                           :damage-type :pierce
                           :equip-type :light
                           :tags (list :weapon :metal :light)
                           :freq 3))

(define-item :breastplate (list :name "breastplate"
                                :desc "Armor covering the chest and torso, made from ~a"
                                :slot :armor
                                :glyph #\}
                                :material t
                                :dfp 5
                                :equip-type :armor
                                :tags (list :armor :metal :leather :bone)
                                :freq 4))

(define-item :chain (list :name "chainmail"
                          :desc "A chain hauberk made from ~a"
                          :slot :armor
                          :glyph #\)
                          :material t
                          :dfp 7
                          :equip-type :armor
                          :tags (list :armor :metal)
                          :freq 2))

(define-item :sun-blade (list :name "Sun Blade"
                              :desc "A golden blade that shines like the sun"
                              :slot :weapon
                              :glyph #\|
                              :atp 10
                              :wil 10
                              :pwr 10
                              :dmg 10
                              :str 5
                              :stam 5
                              :equip-type :sword
                              :damage-type :magic
                              :tags (list :weapon :magic :sun :artifact)
                              :tier 2))

(define-item :wizard-staff (list :name "wizard's staff"
                                 :desc "A typical wizard's staff made from ~a"
                                 :slot :weapon
                                 :material t
                                 :glyph #\_
                                 :wil 10
                                 :pwr 10
                                 :res 10
                                 :equip-type :staff
                                 :damage-type :magic
                                 :tags (list :weapon :staff :magic :bone :wood)
                                 :freq 1
                                 :tier 2))

(define-item :fellhammer (list :name "The Fell Hammer"
                               :desc "A massive warhammer covered in bloody spikes"
                               :glyph #\\
                               :color "purple"
                               :slot :weapon
                               :equip-type :hammer
                               :damage-type :magic
                               :atp 20
                               :res 10
                               :pwr 20
                               :dmg 12
                               :tags (list :weapon :hammer :magic :dark :artifact)
                               :tier 3))
