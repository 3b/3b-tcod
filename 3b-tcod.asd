
(defsystem 3b-tcod
  :description "CL bindings for libtcod"
  :version "0.0.1"
  :author "Bart Botta <00003b at gmail.com>"
  :license "MIT"
  :depends-on (alexandria cffi)
  :components ((:file "package")
               (:file "tilesets")
               (:file "colors")
               (:file "library")
               (:file "low-level")
               (:file "high-level")))

(defsystem 3b-tcod/sdl2
  :description "utilities for using 3b-tcod with cl-sdl2"
  :version "0.0.1"
  :author "Bart Botta <00003b at gmail.com>"
  :license "MIT"
  :depends-on (3b-tcod sdl2)
  :components ((:file "sdl2")))


(defsystem 3b-tcod/sample
  :description "port of (most of) samples_c from libtcod to use 3b-tcod"
  :version "0.0.1"
  :author "Bart Botta <00003b at gmail.com>"
  :license "MIT"
  :depends-on (3b-tcod/sdl2 float-features)
  :components ((:file "sample")))


