;;;; package.lisp

(defpackage #:wolfs-den-lisp
  (:use #:cl))

(cffi:define-foreign-library blt:bearlibterminal
  (t "lib/BearLibTerminal.dll"))

(cffi:use-foreign-library blt:bearlibterminal)
