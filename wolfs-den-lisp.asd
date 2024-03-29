;;;; wolfs-den-lisp.asd

(asdf:defsystem #:wolfs-den-lisp
  :description "Describe wolfs-den-lisp here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-blt #:rl-pcg)
  :components ((:file "package")
               (:file "load-blt")
               (:file "config")
               (:file "utility")
               (:file "queue")
               (:file "entity")
               (:file "directions")
               (:file "tile")
               (:file "rect")
               (:file "game-map")
               (:file "factory")
               (:file "creatures")
               (:file "maps")
               (:file "items")
               (:file "materials")
               (:file "item-egos")
               (:file "swatch")
               (:file "ui")
               (:file "title-screen")
               (:file "main-screen")
               (:file "menus")   
               (:file "wolfs-den-lisp")))
