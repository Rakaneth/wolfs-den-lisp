;;;; wolfs-den-lisp.asd

(asdf:defsystem #:wolfs-den-lisp
  :description "Describe wolfs-den-lisp here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-blt #:cl-pcg)
  :components ((:file "package")
               (:file "load-blt")
               (:file "config")
               (:file "ui")
               (:file "entity")
               (:file "directions")
               (:file "utility")
               (:file "game-map")
               (:file "swatch")
               (:file "wolfs-den-lisp")))
