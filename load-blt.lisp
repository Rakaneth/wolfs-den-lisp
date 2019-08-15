(in-package :wolfs-den-lisp)

(cffi:define-foreign-library blt:bearlibterminal
  (t "lib/BearLibTerminal.dll"))

(cffi:use-foreign-library blt:bearlibterminal)
