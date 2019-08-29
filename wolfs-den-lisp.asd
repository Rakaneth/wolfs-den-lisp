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
               (:file "entity")
               (:file "directions")
               (:file "game-map")
               (:file "swatch")
               (:file "ui")
               (:file "title-screen")
               (:file "main-screen")
               (:file "menus")   
               (:file "wolfs-den-lisp")))
