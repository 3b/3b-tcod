(in-package %3b-tcod)

;;; console_drawing.h
(defcfun ("TCOD_console_put_rgb" console-put-rgb) :void
  (con console*)
  (x :int)
  (y :int)
  (ch :int)
  (fg color*)
  (bg color*)
  (flag bkgnd-flag))

(defcfun ("TCOD_console_draw_rect_rgb" console-draw-rect-rgb) :void
  (con console*)
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (ch :int)
  (fg color*)
  (bg color*)
  (flag bkgnd-flag))

(defcfun ("TCOD_console_draw_frame_rgb" console-draw-frame-rgb) :void
  (con console*)
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (decoration (:pointer :int)) ;; 9 elements
  (fg color*)
  (bg color*)
  (flag bkgnd-flag)
  (clear :bool))


;;; globals.h
(defcfun ("TCOD_get_default_tileset" get-default-tileset) tileset*)
(defcfun ("TCOD_set_default_tileset" set-default-tileset) :void
  (tileset tileset*))

;;; logging.h
(defcfun ("TCOD_log_verbose_" log-verbose-) :void
  (msg :string)
  (level :int)
  (source :string)
  (line :int))

;;; tileset.h
(defcfun ("TCOD_tileset_get_tile_width_" tileset-get-tile-width-) :int
  (tileset tileset*))

(defcfun ("TCOD_tileset_get_tile_height_" tileset-get-tile-height-) :int
  (tileset tileset*))

(defcfun ("TCOD_tileset_get_tile_" tileset-get-tile-) tcod-error
  (tileset tileset*)
  (codepoint :int)
  (buffer color-rgba*))

(defcfun ("TCOD_tileset_set_tile_" tileset-set-tile-) tcod-error
  (tileset tileset*)
  (codepoint :int)
  (buffer color-rgba*))
