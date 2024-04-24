(in-package %3b-tcod)

(define-foreign-library tcod
  (:windows (:or "libtcod.dll"))
  (:unix "libtcod.so")
  (:darwin "libtcod.dylib"))

(use-foreign-library tcod)
