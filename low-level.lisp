(in-package %3b-tcod)

;; need to call free() for some things allocated by tcod
(defcfun free :void (p :pointer))

;;; some definitions from libsdl, might not stay here
(defcenum sdl-log-priority
  (:verbose 1) :debug :info :warn :error :critical)

(defcfun ("SDL_LogSetAllPriority" sdl-log-set-all-priority) :void
  (priority sdl-log-priority))

(defbitfield sdl-window-flags
  (:centered 0)
  (:fullscreen 1)
  (:opengl 2)
  (:shown 4)
  (:hidden 8)
  (:borderless 16)
  (:resizable 32)
  (:minimized 64)
  (:maximized 128)
  (:mouse-grabbed 256)
  (:input-grabbed 256)
  (:input-focus 512)
  (:mouse-focus 1024)
  (:foreign 2048)
  (:fullscreen-desktop 4097)
  (:allow-highdpi 8192)
  (:mouse-capture 16384)
  (:always-on-top 32768)
  (:skip-taskbar 65536)
  (:utility 131072)
  (:tooltip 262144)
  (:popup-menu 524288)
  (:keyboard-grabbed 1048576)
  (:vulkan 268435456)
  (:metal 536870912))

;; todo: make this adjustable, and try to figure out if it needs to
;; match shared lib exactly or just need to match the struct
;; definitions we use
(defconstant +compiled-version+ ;; 1.24.0
  ;; hard-coding version for now, since it needs to match struct
  ;; definitions (at least context-params)
  (+ (* 1 #x10000)
     (* 24 #x100)
     0))

(defconstant +noise-default+ :perlin)
(defconstant +noise-max-octaves+ 128)
(defconstant +noise-max-dimensions+ 4)
(defconstant +noise-default-hurst+ 0.5)
(defconstant +noise-default-lacunarity+ 2.0)

;;; automatically translate to proper float types
(define-foreign-type ensure-float ()
  ()
  (:actual-type :float)
  (:simple-parser ensure-float))

(defmethod translate-to-foreign (value (type ensure-float))
  (coerce value 'single-float))

(defmethod expand-to-foreign (value (type ensure-float))
  (if (constantp value)
      (coerce (eval value) 'single-float)
      `(coerce ,value 'single-float)))

(define-foreign-type ensure-double ()
  ()
  (:actual-type :double)
  (:simple-parser ensure-double))

(defmethod translate-to-foreign (value (type ensure-double))
  (coerce value 'double-float))

(defmethod expand-to-foreign (value (type ensure-double))
  (if (constantp value)
      (coerce (eval value) 'double-float)
      `(coerce ,value 'double-float)))

;; todo: automatically check for errors (add a separate type for
;; checked tcod-error, so we can still distinguish things that return
;; a TCOD_error but shouldn't be automatically checked like set-error)
(defctype tcod-error :int) ;; negative = error, 0=success, other warn?
(defctype error/int :int) ;; negative = error, otherwise useful info

(defmacro checked (form)
  (let ((r (gensym "RET"))
        (err (gensym "ERROR")))
    `(let ((,r ,form))
       (cond
         ((zerop ,r) ;; TCOD_E_OK
          (values nil))
         ((= ,r 1)
          (warn "TCOD_E_WARN from ~s~@[~%~a~]" ',form
                (get-error)))
         ((> ,r 0)
          (let ((,err (get-error)))
            ;; >0 is warning or info, not sure if this should always
            ;; warn here or not?
            (when ,err
              (warn "unspecified warning ~s from ~s~%~a" ,r ',form
                    ,err))
            (values nil)))
         ((< ,r 0)
          (cerror "Ignore error and continue"
                  "~a from ~s" (case ,r
                                 (-1 "TCOD_E_ERROR")
                                 (-2 "TCOD_E_INVALID_ARGUMENT")
                                 (-3 "TCOD_E_OUT_OF_MEMORY")
                                 (-4 "TCOD_E_REQUIRES_ATTENTION")
                                 (t (format nil "unspecified error ~s" ,r)))
                  ',form)
          (values nil))))))

;; types

(defcenum noise-type
  (:perlin 1)
  (:simplex 2)
  (:wavelet 4))

(defcenum renderer-type
  :glsl   ;; removed 1.23
  :opengl ;; removed 1.23
  :sdl    ;; removed 1.23
  :sdl2
  :opengl2 ;; removed 1.23
  :xterm)

;; we assume pointer-sized integers for userdata since making
;; something to point to isn't free anyway, might as well just use an
;; int. If an arbitrary pointer is good enough, and arbitrary int is
;; just as good and doesn't require tracking foreign memory. If we
;; need to track actual objects, putting them in a hash with an
;; integer index is easier (and probably safer) to do than trying to
;; pin and get a pointer to a lisp object.
(defctype userdata :intptr)

;; misc opaque struct types accessed through pointers
(defctype randomizer* :pointer) ;; (:struct randomizer)
(defctype random* :pointer)
(defctype noise* :pointer)
(defctype console* :pointer) ;; (:struct console)
(defctype context* :pointer) ;; (:struct context)
(defctype tileset* :pointer) ;; (:struct tileset)
(defctype image* :pointer)
(defctype namegen* :pointer)

;;; bresenham.h

(defctype line-listener :pointer)

(defcstruct bresenham-data
  (stepx :int)
  (stepy :int)
  (e :int)
  (deltax :int)
  (deltay :int)
  (origx :int)
  (origy :int)
  (destx :int)
  (desty :int))

(defctype bresenham-data* (:pointer (:struct bresenham-data)))

;;; tree.h
(defcstruct tree
  (next :pointer)   ;; pointer to TREE
  (father :pointer) ;; pointer to TREE
  (sons :pointer))  ;; pointer to TREE

;;; bsp.h
(defcstruct bsp
  (tree (:struct tree))
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (position :int)
  (level :uint8)
  (horizontal :bool))

(defmacro with-bsp-node (node (&optional x y w h position level horizontal)
                         &body body)
  (alexandria:once-only (node)
    `(symbol-macrolet (,@(when x `((,x (foreign-slot-value ,node '(:struct bsp) 'x))))
                       ,@(when y `((,y (foreign-slot-value ,node '(:struct bsp) 'y))))
                       ,@(when w `((,w (foreign-slot-value ,node '(:struct bsp) 'w))))
                       ,@(when h `((,h (foreign-slot-value ,node '(:struct bsp) 'h))))
                       ,@(when position
                           `((,position (foreign-slot-value ,node '(:struct bsp) 'position))))
                       ,@(when level
                           `((,level (foreign-slot-value ,node '(:struct bsp) 'level))))
                       ,@(when horizontal
                           `((,horizontal (foreign-slot-value ,node '(:struct bsp) 'horizontal)))))
       (declare (ignorable ,@(remove 'nil (list x y w
                                                h position level horizontal))))
       ,@body)))

(defctype bsp* (:pointer (:struct bsp)))

(defctype bsp-callback :pointer) ;; bool (bsp* node, intptr userdata)

;;; color.h
(defcstruct color
  (r :uint8)
  (g :uint8)
  (b :uint8))

(defctype color-rgb (:struct color))

(defcstruct color-rgba
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(defctype color* (:pointer (:struct color)))
(defctype color-rgb* (:pointer (:struct color)))
(defctype color-rgba* (:pointer (:struct color-rgba)))

;; wrapper version of colors, also used in cl api. #xAABBGGRR
(defctype colornum :uint32)

;; version that tries to translate from named colors
(define-foreign-type ensure-colornum ()
  ()
  (:actual-type :uint32)
  (:simple-parser ensure-colornum))

(defmethod translate-to-foreign (value (type ensure-colornum))
  (3b-tcod:color value))

(defmethod expand-to-foreign (value (type ensure-colornum))
  (if (constantp value)
      (3b-tcod:color (eval value))
      `(3b-tcod:color ,value)))

;;; console.h

;; not sure if this can be used directly, since it stores extra alpha
;; values in some bytes? might be ok with :allow-undeclared-values?
(defcenum (%bkgnd-flag :int :allow-undeclared-values t)
  (:none 0)
  (:set 1)
  (:multiply 2)
  (:lighten 3)
  (:darken 4)
  (:screen 5)
  (:color-dodge 6)
  (:color-burn 7)
  (:add 8)
  (:adda 9)
  (:burn 10)
  (:overlay 11)
  (:alph 12)
  (:default 13))

(defctype bkgnd-flag %bkgnd-flag)

(defcenum alignment
  (:left 0)
  (:right 1)
  (:center 2))

;;; console_printing.h
(defcenum colctrl
  (:1 1)
  (:2 2)
  (:3 3)
  (:4 4)
  (:5 5)
  (:number 5)
  (:fore-rgb 6)
  (:back-rgb 7)
  (:stop 8))

;;; context.h
(defctype cli-output-callback :pointer) ;; void (void* userdata,char* output)

(defcstruct context-params
  ;; Must be +compiled-version+
  (version :int) ;; 1.24.0 = 71680
  ;; starting position of the window, or (sdl2:windowpos-centered) etc
  (window-x :int)
  (window-y :int)
  (pixel-width :int)
  (pixel-height :int)
  (columns :int)
  (rows :int)
  (renderer-type renderer-type)
  (tileset tileset*)
  (vsync :int)
  (sdl-window-flags :int)
  (window-title :string) ;; utf8
  (argc :int)
  (argv (:pointer :string))
  (cli-output-callback cli-output-callback)
  (cli-userdata userdata)
  (window-xy-defined :bool)
  (console console*))

(defctype context-params* (:pointer (:struct context-params)))

;;; context_viewport.h

(defcstruct viewport-options
  (version :int)
  (keep-aspect :bool)
  (integer-scaling :bool)
  (clear-color (:struct color-rgba))
  (align-x ensure-float)
  (align-y ensure-float))

(defctype viewport-options* (:pointer (:struct viewport-options)))

;;; fov_types.h
(defcstruct map-cell
  (transparent :bool)
  (walkable :bool)
  (fov :bool))

;; not sure if we will want to access this directly or not, seems to
;; have enough accessors to not need it, but might be worth for
;; performance eventually
(defcstruct tcod-map
  (width :int)
  (height :int)
  (nb-cells :int)
  (cells (:pointer (:struct map-cell))))

(defctype map* (:pointer (:struct tcod-map)))

(defcenum fov-algorigthm
  (:basic 0)
  (:diamond 1)
  (:shadow 2)
  (:permissive-0 3)
  (:permissive-1 4)
  (:permissive-2 5)
  (:permissive-3 6)
  (:permissive-4 7)
  (:permissive-5 8)
  (:permissive-6 9)
  (:permissive-7 10)
  (:permissive-8 11)
  (:restrictive 12)
  :symmetric-shadowcast)

;;; heightmap.h

(defcstruct heightmap
  (w :int)
  (h :int)
  (values (:pointer :float)))

(defctype heightmap* (:pointer (:struct heightmap)))

;;; list.h
(defctype tcod-list* :pointer)

;;; logging.h

;; these are apparently intended as just guidelines, and log level is
;; arbitrary, so define constants rather than trying to translate into
;; keywords
(defconstant +log-debug+ 10)
(defconstant +log-info+ 20)
(defconstant +log-warning+ 30)
(defconstant +log-error+ 40)
(defconstant +log-critical+ 50)

(defcstruct log-message
  (message :string)
  (level :int)
  (source :string)
  (line-no :int))

(defctype log-message* (:pointer (:struct log-message)))

;; :void (log-message*,userdata)
(defctype logging-callback :pointer)

;;; mersenne-types.h
(defcstruct dice
  (rolls :int)
  (faces :int)
  (multiplier ensure-float)
  (add-sub ensure-float))

(defctype dice* (:pointer (:struct dice)))

(defcenum random-algo
  (:mt 0)
  (:cmwc 1))

(defcenum distribution
  (:linear 0)
  (:gaussian 1)
  (:gaussian-range 2)
  (:gaussian-inverse 3)
  (:gaussian-range-inverse 4))

;;; mouse_types.h
(defcstruct mouse-transform
  (offset-x :double)
  (offset-y :double)
  (scale-x :double)
  (scale-y :double))

(defctype mouse-transform* (:pointer (:struct mouse-transform)))

;;; parser.h
(defcenum value-type
  (:none 0)
  (:bool 1)
  (:char 2)
  (:int 3)
  (:float 4)
  (:string 5)
  (:color 6)
  (:dice 7)
  (:valuelist00 8)
  (:valuelist01 9)
  (:valuelist02 10)
  (:valuelist03 11)
  (:valuelist04 12)
  (:valuelist05 13)
  (:valuelist06 14)
  (:valuelist07 15)
  (:valuelist08 16)
  (:valuelist09 17)
  (:valuelist10 18)
  (:valuelist11 19)
  (:valuelist12 20)
  (:valuelist13 21)
  (:valuelist14 22)
  (:valuelist15 23)
  (:list 1024))

(defctype parser-struct* :pointer)

(defctype parser* :pointer)

(defctype parser-listener* :pointer)

;; value-type (lex*, parser-listener*, parser-struct*, :string)
(defctype parser-custom-callback :pointer)

;;; path.h

;; :float (:int :int :int :int userdata)
(defctype path-func :pointer)

(defctype path* :pointer)

(defctype dijkstra* :pointer)

;;; pathfinder.h

(defctype pathfinder* :pointer)

;;; tileset.h
(defcstruct tileset
  (tile-width :int)
  (tile-height :int)
  (tile-length :int)
  (tiles-capacity :int)
  (tiles-count :int)
  (pixels :pointer)
  (character-map-length :int)
  (character-map (:pointer :int))
  (observer-list :pointer)
  (virtual-columns :int)
  (ref-count :int))

;;; txtfield.h
(defctype text* :pointer)

;;;;;;;; ffi defs

;;; bresenham.h

;; bool (:int x, :int y)
(defcfun ("TCOD_line" line) :bool
  (x-from :int)
  (y-from :int)
  (x-to :int)
  (y-to :int)
  (listener line-listener))

(declaim (inline line-line)) ;; cl-tcod compat
(defun line-line (x-from y-from x-to y-to listener-callback)
  (line x-from y-from x-to y-to listener-callback))

(defcfun ("TCOD_line_init_mt" line-init-mt) :void
  (x-from :int)
  (y-from :int)
  (x-to :int)
  (y-to :int)
  (data bresenham-data*))

(defcfun ("TCOD_line_step_mt" line-step-mt) :bool
  (x-cur (:pointer :int))
  (y-cur (:pointer :int))
  (data bresenham-data*))

;;; bsp.h

(defcfun ("TCOD_bsp_new" bsp-new) bsp*)

(defcfun ("TCOD_bsp_new_with_size" bsp-new-with-size) bsp*
  (x :int)
  (y :int)
  (w :int)
  (h :int))

(defcfun ("TCOD_bsp_delete" bsp-delete) :void
  (node bsp*))

(defcfun ("TCOD_bsp_left" bsp-left) bsp*
  (node bsp*))

(defcfun ("TCOD_bsp_right" bsp-right) bsp*
  (node bsp*))

(defcfun ("TCOD_bsp_father" bsp-father) bsp*
  (node bsp*))

(defcfun ("TCOD_bsp_is_leaf" bsp-is-leaf) :bool
  (node bsp*))

(defcfun ("TCOD_bsp_traverse_pre_order" bsp-traverse-pre-order) :bool
  (node bsp*)
  (callback bsp-callback)
  (userdata userdata))

(defcfun ("TCOD_bsp_traverse_in_order" bsp-traverse-in-order) :bool
  (node bsp*)
  (callback bsp-callback)
  (userdata userdata))

(defcfun ("TCOD_bsp_traverse_post_order" bsp-traverse-post-order) :bool
  (node bsp*)
  (callback bsp-callback)
  (userdata userdata))

(defcfun ("TCOD_bsp_traverse_level_order" bsp-traverse-level-order) :bool
  (node bsp*)
  (callback bsp-callback)
  (userdata userdata))

(defcfun ("TCOD_bsp_traverse_inverted_level_order" bsp-traverse-inverted-level-order) :bool
  (node bsp*)
  (callback bsp-callback)
  (userdata userdata))

(defcfun ("TCOD_bsp_contains" bsp-contains) :bool
  (node bsp*)
  (x :int)
  (y :int))

(defcfun ("TCOD_bsp_find_node" bsp-find-node) bsp*
  (node bsp*)
  (x :int)
  (y :int))

(defcfun ("TCOD_bsp_resize" bsp-resize) :void
  (node bsp*)
  (x :int)
  (y :int)
  (w :int)
  (h :int))

(defcfun ("TCOD_bsp_split_once" bsp-split-once) :void
  (node bsp*)
  (horizontal :bool)
  (position :int))

(defcfun ("TCOD_bsp_split_recursive" bsp-split-recursive) :void
  (node bsp*)
  (randomizer randomizer*)
  (nb :int)
  (min-h-size :int)
  (min-v-size :int)
  (max-h-ratio ensure-float)
  (max-v-ratio ensure-float))

(defcfun ("TCOD_bsp_remove_sons" bsp-remove-sons) :void
  (node bsp*))

;;; color.h

#++ ;; no wrappers for these 2?
(defcfun ("TCOD_color_HSV" color-hsv) Color
  (hue ensure-float)
  (saturation ensure-float)
  (value ensure-float))

#++
(defcfun ("TCOD_color_RGB" color-rgb) Color
  (r :uint8)
  (g :uint8)
  (b :uint8))

(defcfun ("TCOD_color_equals_wrapper" color-equals) :bool
  (c1 ensure-colornum)
  (c2 ensure-colornum))

(defcfun ("TCOD_color_add_wrapper" color-add) colornum
  (c1 ensure-colornum)
  (c2 ensure-colornum))

(defcfun ("TCOD_color_subtract_wrapper" color-subtract) colornum
  (c1 ensure-colornum)
  (c2 ensure-colornum))

(defcfun ("TCOD_color_multiply_wrapper" color-multiply) colornum
  (c1 ensure-colornum)
  (c2 ensure-colornum))

(defcfun ("TCOD_color_multiply_scalar_wrapper" color-multiply-scalar) colornum
  (c ensure-colornum)
  (value ensure-float))

(defcfun ("TCOD_color_lerp_wrapper" color-lerp) colornum
  (c1 ensure-colornum)
  (c2 ensure-colornum)
  (coef ensure-float))

(defcfun ("TCOD_color_alpha_blend" color-alpha-blend) :void
  (dst color-rgba*)
  (src color-rgba*))

(defcfun ("TCOD_color_set_HSV" color-set-hsv) :void
  (color color*)
  (hue ensure-float)
  (saturation ensure-float)
  (value ensure-float))

(defcfun ("TCOD_color_get_HSV_wrapper" color-get-hsv) :void
  (c ensure-colornum)
  (h (:pointer :float))
  (s (:pointer :float))
  (v (:pointer :float)))

(defcfun ("TCOD_color_get_hue_wrapper" color-get-hue) :float
  (c ensure-colornum))

(defcfun ("TCOD_color_set_hue" color-set-hue) :void
  (color color*)
  (hue ensure-float))

(defcfun ("TCOD_color_get_saturation_wrapper" color-get-saturation) :float
  (c ensure-colornum))

(defcfun ("TCOD_color_set_saturation" color-set-saturation) :void
  (color color*)
  (saturation ensure-float))

(defcfun ("TCOD_color_get_value_wrapper" color-get-value) :float
  (c ensure-colornum))

(defcfun ("TCOD_color_set_value" color-set-value) :void
  (color color*)
  (value ensure-float))

(defcfun ("TCOD_color_shift_hue" color-shift-hue) :void
  (color color*)
  (shift ensure-float))

(defcfun ("TCOD_color_scale_HSV" color-scale-hsv) :void
  (color color*)
  (saturation-coef ensure-float)
  (value-coef ensure-float))

(defcfun ("TCOD_color_gen_map" color-gen-map) :void
  (map color*)
  (nb-key :int)
  (key-color color*)
  (key-index (:pointer :int)))

;;; console.h

(defcfun ("TCOD_console_new" console-new) console*
  (w :int)
  (h :int))

(defcfun ("TCOD_console_get_width" console-get-width) :int
  (con console*))

(defcfun ("TCOD_console_get_height" console-get-height) :int
  (con console*))

(defcfun ("TCOD_console_set_key_color_wrapper" console-set-key-color) :void
  (con console*)
  (color ensure-colornum))

(defcfun ("TCOD_console_blit" console-blit) :void
  (src console*)
  (src-x :int)
  (src-y :int)
  (src-w :int)
  (src-h :int)
  (dst console*)
  (dst-x :int)
  (dst-y :int)
  (foreground-alpha ensure-float)
  (background-alpha ensure-float))

(defcfun ("TCOD_console_blit_key_color" console-blit-key-color) :void
  (src console*)
  (src-x :int)
  (src-y :int)
  (src-w :int)
  (src-h :int)
  (dst console*)
  (dst-x :int)
  (dst-y :int)
  (foreground-alpha :float)
  (background-alpha :float)
  (key-color color*))

(defcfun ("TCOD_console_delete" console-delete) :void
  (con console*))

;; deprecated?
(defcfun ("TCOD_console_set_default_background_wrapper" console-set-default-background) :void
  (con console*)
  (color ensure-colornum))

;; deprecated?
(defcfun ("TCOD_console_set_default_foreground_wrapper" console-set-default-foreground) :void
  (con console*)
  (color ensure-colornum))

(defcfun ("TCOD_console_clear" console-clear) :void
  (con console*))

(defcfun ("TCOD_console_set_char_background_wrapper" console-set-char-background) :void
  (con console*)
  (x :int)
  (y :int)
  (color ensure-colornum)
  (flag bkgnd-flag))

(defcfun ("TCOD_console_set_char_foreground_wrapper" console-set-char-foreground) :void
  (con console*)
  (x :int)
  (y :int)
  (a ensure-colornum))

(defcfun ("TCOD_console_set_char" console-set-char) :void
  (con console*)
  (x :int)
  (y :int)
  (c :int))

(defcfun ("TCOD_console_put_char" console-put-char) :void
  (con console*)
  (x :int)
  (y :int)
  (c :int)
  (flag bkgnd-flag))

(defcfun ("TCOD_console_put_char_ex_wrapper" console-put-char-ex) :void
  (con console*)
  (x :int)
  (y :int)
  (c :int)
  (fg ensure-colornum)
  (bg ensure-colornum))

;; deprecated
(defcfun ("TCOD_console_set_background_flag" console-set-background-flag) :void
  (con console*)
  (flag bkgnd-flag))

;; deprecated
(defcfun ("TCOD_console_get_background_flag" console-get-background-flag) :int
  (con console*))

;; deprecated
(defcfun ("TCOD_console_set_alignment" console-set-alignment) :void
  (con console*)
  (alignment alignment))

;; deprecated
(defcfun ("TCOD_console_get_alignment" console-get-alignment) alignment
  (con console*))

;; deprecated
(defcfun ("TCOD_console_get_default_background_wrapper" console-get-default-background) colornum
  (con console*))

;; deprecated
(defcfun ("TCOD_console_get_default_foreground_wrapper" console-get-default-foreground) colornum
  (con console*))

(defcfun ("TCOD_console_get_char_background_wrapper" console-get-char-background) colornum
  (con console*)
  (x :int)
  (y :int))

(defcfun ("TCOD_console_get_char_foreground_wrapper" console-get-char-foreground) colornum
  (con console*)
  (x :int)
  (y :int))

(defcfun ("TCOD_console_get_char" console-get-char) :int
  (con console*)
  (x :int)
  (y :int))

;;; console_drawing.h
(defcfun ("TCOD_console_rect" console-rect) :void
  (con console*)
  (x :int)
  (y :int)
  (rw :int)
  (rh :int)
  (clear :bool)
  (flag bkgnd-flag))

(defcfun ("TCOD_console_hline" console-hline) :void
  (con console*)
  (x :int)
  (y :int)
  (l :int)
  (flag bkgnd-flag))

(defcfun ("TCOD_console_vline" console-vline) :void
  (con console*)
  (x :int)
  (y :int)
  (l :int)
  (flag bkgnd-flag))

#++ ;; ??
(defcfun ("TCOD_console_draw_rect" console-draw-rect) :void
  (con console*)
  (x :int)
  (y :int)
  (l :int)
  (flag bkgnd-flag))

;;; console_etc.h

;; 1.16
(defcfun ("TCOD_console_flush_ex" console-flush-ex) tcod-error
  (con console*)
  (viewport viewport-options*))

(defcfun ("TCOD_console_flush" console-flush) tcod-error)

(defcfun ("TCOD_console_from_file" console-from-file) console*
  (filename :string))

(defcfun ("TCOD_console_load_asc" console-load-asc) :bool
  (con console*)
  (filename :string))

(defcfun ("TCOD_console_load_apf" console-load-apf) :bool
  (con console*)
  (filename :string))

(defcfun ("TCOD_console_save_asc" console-save-asc) :bool
  (con console*)
  (filename :string))

(defcfun ("TCOD_console_save_apf" console-save-apf) :bool
  (con console*)
  (filename :string))

;; deprecated, but credits_render_ex uses the var it resets, so keep
(defcfun ("TCOD_console_credits_reset" console-credits-reset) :void)

;; 1.19
(defcfun ("TCOD_console_credits_render_ex" console-credits-render-ex) :bool
  (con console*)
  (x :int)
  (y :int)
  (alpha :bool)
  (delta-time ensure-float))

;;; console_init.h
;; deprecated but convenient
(defcfun ("TCOD_console_is_fullscreen" console-is-fullscreen) :bool)

;;; console_printing.h

(defcfun ("TCOD_console_set_color_control_wrapper" console-set-color-control) :void
  (con colctrl)
  (fore ensure-colornum)
  (back ensure-colornum))

(defcfun ("TCOD_console_printf" console-printf) tcod-error
  (con console*)
  (x :int)
  (y :int)
  (fmt (:string :encoding :utf-8))
  &rest)

(defcfun ("TCOD_console_printf_ex" console-printf-ex) tcod-error
  (con console*)
  (x :int)
  (y :int)
  (flag bkgnd-flag)
  (alignment alignment)
  (fmt (:string :encoding :utf-8))
  &rest)

(defcfun ("TCOD_console_printf_rect" console-printf-rect) tcod-error
  (con console*)
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (fmt (:string :encoding :utf-8))
  &rest)

(defcfun ("TCOD_console_printf_rect_ex" console-printf-rect-ex) tcod-error
  (con console*)
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (flag bkgnd-flag)
  (alignment alignment)
  (fmt (:string :encoding :utf-8))
  &rest)

(defcfun ("TCOD_console_printf_frame" console-printf-frame) tcod-error
  (con console*)
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (empty :int)
  (flag bkgnd-flag)
  (fmt (:string :encoding :utf-8))
  &rest)

(defcfun ("TCOD_console_get_height_rect_fmt" console-get-height-rect-fmt) error/int
  (con console*)
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (fmt (:string :encoding :utf-8))
  &rest)

(defcfun ("TCOD_console_printn" console-printn) tcod-error
  (con console*)
  (x :int)
  (y :int)
  (n :int)
  ;; we pass these as char* instead of :string since we need to count
  ;; bytes after encoding, so need to encode manually
  (str (:pointer :char))
  (fg color-rgb*)
  (bg color-rgb*)
  (flag bkgnd-flag)
  (alignment alignment))

(defcfun ("TCOD_console_printn_rect" console-printn-rect) tcod-error
  (con console*)
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (n :int)
  ;; we pass these as char* instead of :string since we need to count
  ;; bytes after encoding, so need to encode manually
  (str (:pointer :char))
  (fg color-rgb*)
  (bg color-rgb*)
  (flag bkgnd-flag)
  (alignment alignment))

(defcfun ("TCOD_console_get_height_rect_n" console-get-height-rect-n) tcod-error
  (con console*)
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (n :int)
  (str (:string :encoding :utf-8)))

(defcfun ("TCOD_console_get_height_rect_wn" console-get-height-rect-wn) tcod-error
  (con console*)
  (width :int)
  (n :int)
  (str (:string :encoding :utf-8)))

#++
(defcstruct print-params-rgb
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (fg color-rgb*)
  (bg color-rgb*)
  (flag bkgnd-flag)
  (alignment alignment))

#++
(defcfun ("TCOD_printf_rgb" printf-rgb) tcod-error
  (con console*)
  (params print-params-rgb)
  (fmt :string)
  &rest)

#++
(defcfun ("TCOD_printn_rgb" printn-rgb) tcod-error
  (con console*)
  (params print-params-rgb)
  (n :int)
  (fmt :string)
  &rest)

;;; console_rexpaint.h
(defcfun ("TCOD_console_from_xp" console-from-xp) console*
  (filename :string))

(defcfun ("TCOD_console_load_xp" console-load-xp) :bool
  (con console*)
  (filename :string))

(defcfun ("TCOD_console_save_xp" console-save-xp) :bool
  (con console*)
  (filename :string)
  (compress-level :int))

(defcfun ("TCOD_load_xp_from_memory" load-xp-from-memory) error/int
  (n-data :int)
  (data (:pointer :unsigned-char))
  (n-out :int)
  (out (:pointer console*)))

(defcfun ("TCOD_save_xp_to_memory" save-xp-to-memory) error/int
  (n-consoles :int)
  (consoles (:pointer console*))
  (n-out :int)
  (out (:pointer :unsigned-char))
  (compress-level :int))

(defcfun ("TCOD_load_xp" load-xp) error/int
  (path :string)
  (n :int)
  (out (:pointer console*)))

(defcfun ("TCOD_save_xp" save-xp) tcod-error
  (n :int)
  (out (:pointer console*))
  (path :string)
  (compress-level :int))

;;; console_types.h

;;; context.h

(defcfun ("TCOD_context_delete" context-delete) :void
  (renderer context*))

(defcfun ("TCOD_context_present" context-present) tcod-error
  (context context*)
  (console console*)
  (viewport viewport-options*))

(defcfun ("TCOD_context_screen_pixel_to_tile_d" context-screen-pixel-to-tile-d) tcod-error
  (context context*)
  (x (:pointer :double))
  (y (:pointer :double)))

(defcfun ("TCOD_context_screen_pixel_to_tile_i" context-screen-pixel-to-tile-i) tcod-error
  (context context*)
  (x (:pointer :int))
  (y (:pointer :int)))

(defcfun ("TCOD_context_convert_event_coordinates" context-convert-event-coordinates) tcod-error
  (context context*)
  ;; pointer to SDL_Event
  (event :pointer))

(defcfun ("TCOD_context_save_screenshot" context-save-screenshot) tcod-error
  (context context*)
  (filename :string))

(defcfun ("TCOD_context_get_sdl_window" context-get-sdl-window)
    :pointer ;; to SDL_Window
  (context context*))

(defcfun ("TCOD_context_get_sdl_renderer" context-get-sdl-renderer)
    :pointer ;; to SDL_Renderer
  (context context*))

(defcfun ("TCOD_context_change_tileset" context-change-tileset) tcod-error
  (self context*)
  (tileset tileset*))

(defcfun ("TCOD_context_get_renderer_type" context-get-renderer-type) error/int ;; renderer enum if non-negative
  (context context*))

(defcfun ("TCOD_context_recommended_console_size" context-recommended-console-size) tcod-error
  (context context*)
  (magnification :float)
  (columns (:pointer :int))
  (rows (:pointer :int)))

(defcfun ("TCOD_context_screen_capture" context-screen-capture) tcod-error
  (context context*)
  (out-pixels color-rgba*)
  (width (:pointer :int))
  (height (:pointer :int)))

(defcfun ("TCOD_context_screen_capture_alloc" context-screen-capture-alloc)
    color-rgba* ;; free with free()
  (context context*)
  (width (:pointer :int))
  (height (:pointer :int)))

(defcfun ("TCOD_context_set_mouse_transform" context-set-mouse-transform) tcod-error
  (context context*)
  (transform mouse-transform*))

;;; context_init.h

(defcfun ("TCOD_context_new" context-new) tcod-error
  (params context-params*)
  (out (:pointer context*)))

;;; error.h
(defcfun ("TCOD_get_error" get-error) :string)

;; always returns TCOD_E_ERROR = -1
(defcfun ("TCOD_set_error" set-error) tcod-error
  (message :string))

(defcfun ("TCOD_set_errorf" set-errorf) tcod-error
  (fmt :string)
  &rest)

(defcfun ("TCOD_clear_error" clear-error) :void)

;;; fov.h
(defcfun ("TCOD_map_new" map-new) map*
  (w :int)
  (h :int))

(defcfun ("TCOD_map_clear" map-clear) :void
  (map map*)
  (transparent :bool)
  (walkable :bool))

(defcfun ("TCOD_map_copy" map-copy) tcod-error
  (source map*)
  (dest map*))

(defcfun ("TCOD_map_set_properties" map-set-properties) :void
  (map map*)
  (x :int)
  (y :int)
  (is-transparent :bool)
  (is-walkable :bool))

(defcfun ("TCOD_map_delete" map-delete) :void
  (map map*))

(defcfun ("TCOD_map_compute_fov" map-compute-fov) tcod-error
  (map map*)
  (pov-x :int)
  (pov-y :int)
  (max-radius :int)
  (light-walls :bool)
  (algorign fov-algorigthm))

(defcfun ("TCOD_map_is_in_fov" map-is-in-fov) :bool
  (map map*)
  (x :int)
  (y :int))

(defcfun ("TCOD_map_set_in_fov" map-set-in-fov) :void
  (map map*)
  (x :int)
  (y :int)
  (fov :bool))

(defcfun ("TCOD_map_is_transparent" map-is-transparent) :bool
  (map map*)
  (x :int)
  (y :int))

(defcfun ("TCOD_map_is_walkable" map-is-walkable) :bool
  (map map*)
  (x :int)
  (y :int))

(defcfun ("TCOD_map_get_width" map-get-width) :int
  (map map*))

(defcfun ("TCOD_map_get_height" map-get-height) :int
  (map map*))

(defcfun ("TCOD_map_get_nb_cells" map-get-nb-cells) :int
  (map map*))

;;; heapq.h
;; todo

;;; heightmap.h

(defcfun ("TCOD_heightmap_new" heightmap-new) heightmap*
  (w :int)
  (h :int))

(defcfun ("TCOD_heightmap_delete" heightmap-delete) :void
  (hm heightmap*))

(defcfun ("TCOD_heightmap_get_value" heightmap-get-value) :float
  (hm heightmap*)
  (x :int)
  (y :int))

(defcfun ("TCOD_heightmap_get_interpolated_value" heightmap-get-interpolated-value) :float
  (hm heightmap*)
  (x ensure-float)
  (y ensure-float))

(defcfun ("TCOD_heightmap_set_value" heightmap-set-value) :void
  (hm heightmap*)
  (x :int)
  (y :int)
  (value ensure-float))

(defcfun ("TCOD_heightmap_get_slope" heightmap-get-slope) :float
  (hm heightmap*)
  (x :int)
  (y :int))

(defcfun ("TCOD_heightmap_get_normal" heightmap-get-normal) :void
  (hm heightmap*)
  (x :float)
  (y :float)
  (n (:pointer :float)) ;; 3
  (water-level ensure-float))

(defcfun ("TCOD_heightmap_count_cells" heightmap-count-cells) :int
  (hm heightmap*)
  (min ensure-float)
  (max ensure-float))

(defcfun ("TCOD_heightmap_has_land_on_border" heightmap-has-land-on-border) :bool
  (hm heightmap*)
  (water-level ensure-float))

(defcfun ("TCOD_heightmap_get_minmax" heightmap-get-minmax) :void
  (hm heightmap*)
  (min (:pointer :float))
  (max (:pointer :float)))

(defcfun ("TCOD_heightmap_copy" heightmap-copy) :void
  (hm-source heightmap*)
  (hm-dest heightmap*))

(defcfun ("TCOD_heightmap_add" heightmap-add) :void
  (hm heightmap*)
  (value ensure-float))

(defcfun ("TCOD_heightmap_scale" heightmap-scale) :void
  (hm heightmap*)
  (value ensure-float))

(defcfun ("TCOD_heightmap_clamp" heightmap-clamp) :void
  (hm heightmap*)
  (min ensure-float)
  (max ensure-float))

(defcfun ("TCOD_heightmap_normalize" heightmap-normalize) :void
  (hm heightmap*)
  (min ensure-float)
  (max ensure-float))

(defcfun ("TCOD_heightmap_clear" heightmap-clear) :void
  (hm heightmap*))

(defcfun ("TCOD_heightmap_lerp_hm" heightmap-lerp-hm) :void
  (hm1 heightmap*)
  (hm2 heightmap*)
  (out heightmap*)
  (coef :float))

(defcfun ("TCOD_heightmap_add_hm" heightmap-add-hm) :void
  (hm1 heightmap*)
  (hm2 heightmap*)
  (out heightmap*))

(defcfun ("TCOD_heightmap_multiply_hm" heightmap-multiply-hm) :void
  (hm1 heightmap*)
  (hm2 heightmap*)
  (out heightmap*))

(defcfun ("TCOD_heightmap_add_hill" heightmap-add-hill) :void
  (hm heightmap*)
  (hx ensure-float)
  (hy ensure-float)
  (h-radius ensure-float)
  (h-height ensure-float))

(defcfun ("TCOD_heightmap_dig_hill" heightmap-dig-hill) :void
  (hm heightmap*)
  (hx ensure-float)
  (hy ensure-float)
  (h-radius ensure-float)
  (h-height ensure-float))

(defcfun ("TCOD_heightmap_dig_bezier" heightmap-dig-bezier) :void
  (hm heightmap*)
  (px (:pointer :int)) ;; [4]
  (py (:pointer :int)) ;; [4]
  (start-radius ensure-float)
  (start-depth ensure-float)
  (end-radius ensure-float)
  (end-depth ensure-float))

(defcfun ("TCOD_heightmap_rain_erosion" heightmap-rain-erosion) :void
  (hm heightmap*)
  (nb-drops :int)
  (erosion-coef ensure-float)
  (sedimentation-coef ensure-float)
  (rnd random*))

(defcfun ("TCOD_heightmap_kernel_transform" heightmap-kernel-transform) :void
  (hm heightmap*)
  (kernel-size :int)
  (dx (:pointer :int))
  (dy (:pointer :int))
  (weight (:pointer :float))
  (min-level ensure-float)
  (max-level ensure-float))

(defcfun ("TCOD_heightmap_add_voronoi" heightmap-add-voronoi) :void
  (hm heightmap*)
  (nb-points :int)
  (nb-coef :int)
  (coef (:pointer :float))
  (rnd random*))

(defcfun ("TCOD_heightmap_mid_point_displacement" heightmap-mid-point-displacement) :void
  (hm heightmap*)
  (rnd random*)
  (roughness ensure-float))

(defcfun ("TCOD_heightmap_add_fbm" heightmap-add-fbm) :void
  (hm heightmap*)
  (noise noise*)
  (mul-x ensure-float)
  (mul-y ensure-float)
  (add-x ensure-float)
  (add-y ensure-float)
  (octaves ensure-float)
  (delta ensure-float)
  (scale ensure-float))

(defcfun ("TCOD_heightmap_scale_fbm" heightmap-scale-fbm) :void
  (hm heightmap*)
  (noise noise*)
  (mul-x ensure-float)
  (mul-y ensure-float)
  (add-x ensure-float)
  (add-y ensure-float)
  (octaves ensure-float)
  (delta ensure-float)
  (scale ensure-float))

;;; image.h

(defcfun ("TCOD_image_new" image-new) image*
  (w :int)
  (h :int))

(defcfun ("TCOD_image_from_console" image-from-console) image*
  (con console*))

(defcfun ("TCOD_image_refresh_console" image-refresh-console) :void
  (image image*)
  (console console*))

(defcfun ("TCOD_image_clear_wrapper" image-clear) :void
  (image image*)
  (color ensure-colornum))

(defcfun ("TCOD_image_invert" image-invert) :void
  (image image*))

(defcfun ("TCOD_image_hflip" image-hflip) :void
  (image image*))

(defcfun ("TCOD_image_rotate90" image-rotate90) :void
  (image image*)
  (num-rotations :int))

(defcfun ("TCOD_image_vflip" image-vflip) :void
  (image image*))

(defcfun ("TCOD_image_scale" image-scale) :void
  (image image*)
  (new-w :int)
  (new-h :int))

(defcfun ("TCOD_image_load" image-load) image*
  (filename :string))

(defcfun ("TCOD_image_save" image-save) :void
  (image image*)
  (filename :string))

(defcfun ("TCOD_image_get_size" image-get-size) :void
  (image image*)
  (w (:pointer :int))
  (h (:pointer :int)))

(defcfun ("TCOD_image_get_pixel_wrapper" image-get-pixel) colornum
  (image image*)
  (x :int)
  (y :int))

(defcfun ("TCOD_image_get_alpha" image-get-alpha) :int
  (image image*)
  (x :int)
  (y :int))

(defcfun ("TCOD_image_get_mipmap_pixel_wrapper" image-get-mipmap-pixel) colornum
  (image image*)
  (x0 ensure-float)
  (y0 ensure-float)
  (x1 ensure-float)
  (y1 ensure-float))

(defcfun ("TCOD_image_put_pixel_wrapper" image-put-pixel) :void
  (image image*)
  (x :int)
  (y :int)
  (col ensure-colornum))

(defcfun ("TCOD_image_blit" image-blit) :void
  (image image*)
  (console console*)
  (x ensure-float)
  (y ensure-float)
  (flag bkgnd-flag)
  (scale-x ensure-float)
  (scale-y ensure-float)
  (angle ensure-float))

(defcfun ("TCOD_image_blit_rect" image-blit-rect) :void
  (image image*)
  (console console*)
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (flag bkgnd-flag))

(defcfun ("TCOD_image_blit_2x" image-blit-2x) :void
  (image image*)
  (console console*)
  (dx :int)
  (dy :int)
  (sx :int)
  (sy :int)
  (w :int)
  (h :int))

(defcfun ("TCOD_image_delete" image-delete) :void
  (image image*))

(defcfun ("TCOD_image_set_key_color_wrapper" image-set-key-color) :void
  (image image*)
  (key-color ensure-colornum))

(defcfun ("TCOD_image_is_pixel_transparent" image-is-pixel-transparent) :bool
  (image image*)
  (x :int)
  (y :int))

;;; lex.h
;; todo?

;;; list.h

(defcfun ("TCOD_list_new" list-new) tcod-list*)

(defcfun ("TCOD_list_allocate" list-allocate) tcod-list*
  (nb-elements :int))

(defcfun ("TCOD_list_duplicate" list-duplicate) tcod-list*
  (l tcod-list*))

(defcfun ("TCOD_list_delete" list-delete) :void
  (l tcod-list*))

(defcfun ("TCOD_list_push" list-push) :void
  (l tcod-list*)
  (elt (:pointer :void)))

(defcfun ("TCOD_list_pop" list-pop) (:pointer :void)
  (l tcod-list*))

(defcfun ("TCOD_list_peek" list-peek) (:pointer :void)
  (l tcod-list*))

(defcfun ("TCOD_list_add_all" list-add-all) :void
  (l tcod-list*)
  (l2 tcod-list*))

(defcfun ("TCOD_list_get" list-get) (:pointer :void)
  (l tcod-list*)
  (idx :int))

(defcfun ("TCOD_list_set" list-set) :void
  (l tcod-list*)
  (elt (:pointer :void))
  (idx :int))

(defcfun ("TCOD_list_begin" list-begin) (:pointer (:pointer :void))
  (l tcod-list*))

(defcfun ("TCOD_list_end" list-end) (:pointer (:pointer :void))
  (l tcod-list*))

(defcfun ("TCOD_list_reverse" list-reverse) :void
  (l tcod-list*))

(defcfun ("TCOD_list_remove_iterator" list-remove-iterator)
    (:pointer (:pointer :void))
  (l tcod-list*)
  (elt (:pointer (:pointer :void))))

(defcfun ("TCOD_list_remove" list-remove) :void
  (l tcod-list*)
  (elt (:pointer :void)))

(defcfun ("TCOD_list_remove_iterator_fast" list-remove-iterator-fast)
    (:pointer (:pointer :void))
  (l tcod-list*)
  (elt (:pointer (:pointer :void))))

(defcfun ("TCOD_list_remove_fast" list-remove-fast) :void
  (l tcod-list*)
  (elt (:pointer :void)))

(defcfun ("TCOD_list_contains" list-contains) :bool
  (l tcod-list*)
  (elt (:pointer :void)))

(defcfun ("TCOD_list_clear" list-clear) :void
  (l tcod-list*))

(defcfun ("TCOD_list_clear_and_delete" list-clear-and-delete) :void
  (l tcod-list*))

(defcfun ("TCOD_list_size" list-size) :int
  (l tcod-list*))

(defcfun ("TCOD_list_insert_before" list-insert-before)
    (:pointer (:pointer :void))
  (l tcod-list*)
  (elt (:pointer :void))
  (before :int))

(defcfun ("TCOD_list_is_empty" list-is-empty) :bool
  (l tcod-list*))

;;; logging.h

(defcfun ("TCOD_set_log_callback" set-log-callback)
    :void
  (callback logging-callback)
  (userdata userdata))

(defcfun ("TCOD_set_log_level" set-log-level) :void
  (level :int))

;;; mersenne.h

(defcfun ("TCOD_random_get_instance" random-get-instance) random*)

(defcfun ("TCOD_random_new" random-new) random*
  (algo random-algo))

(defcfun ("TCOD_random_save" random-save) random*
  (mersenne random*))

(defcfun ("TCOD_random_restore" random-restore) :void
  (mersenne random*)
  (backup random*))

(defcfun ("TCOD_random_new_from_seed" random-new-from-seed) random*
  (algo random-algo)
  (seed :uint32))

(defcfun ("TCOD_random_delete" random-delete) :void
  (mersenne random*))

(defcfun ("TCOD_random_set_distribution" random-set-distribution) :void
  (mersenne random*)
  (distribution distribution))

(defcfun ("TCOD_random_get_int" random-get-int) :int
  (mersenne random*)
  (min :int)
  (max :int))

(defcfun ("TCOD_random_get_float" random-get-float) :float
  (mersenne random*)
  (min ensure-float)
  (max ensure-float))

(defcfun ("TCOD_random_get_double" random-get-double) :double
  (mersenne random*)
  (min ensure-double)
  (max ensure-double))

(defcfun ("TCOD_random_get_int_mean" random-get-int-mean) :int
  (mersenne random*)
  (min :int)
  (max :int)
  (mean :int))

(defcfun ("TCOD_random_get_float_mean" random-get-float-mean) :float
  (mersenne random*)
  (min ensure-float)
  (max ensure-float)
  (mean ensure-float))

(defcfun ("TCOD_random_get_double_mean" random-get-double-mean) :double
  (mersenne random*)
  (min ensure-double)
  (max ensure-double)
  (mean ensure-double))

#++ ;; need libffi?
(defcfun ("TCOD_random_dice_new" random-dice-new) dice
  (s :string))

#++ ;; need libffi
(defcfun ("TCOD_random_dice_roll" random-dice-roll) :int
  (mersenne random*)
  (doce dice))

(defcfun ("TCOD_random_dice_roll_s" random-dice-roll-s) :int
  (mersenne random*)
  (s :string))
;;; namegen.h

(defcfun ("TCOD_namegen_parse" namegen-parse) :void
  (filename :string)
  (random random*))

;; return value of these might need to be free()d depending on
;; allocate, so don't translate with :string automatically
(defcfun ("TCOD_namegen_generate" namegen-generate) :pointer
  (name :string)
  (allocate :bool))

(defcfun ("TCOD_namegen_generate_custom" namegen-generate-custom)
    :pointer
  (name :string)
  (rule :string)
  (allocate :bool))

(defcfun ("TCOD_namegen_get_sets" namegen-get-sets) tcod-list*)

(defcfun ("TCOD_namegen_get_nb_sets_wrapper" namegen-get-nb-sets-wrapper) :int)

(defcfun ("TCOD_namegen_get_sets_wrapper" namegen-get-sets-wrapper) :void
  (sets (:pointer (:pointer :char))))

(defcfun ("TCOD_namegen_destroy" namegen-destroy) :void)

;;; noise.h

(defcfun ("TCOD_noise_new" noise-new) noise*
  (dimensions :int)
  (hurst ensure-float)
  (lacunarity ensure-float)
  (random random*))

(defcfun ("TCOD_noise_set_type" noise-set-type) :void
  (noise noise*)
  (a :int))

(defcfun ("TCOD_noise_get_ex" noise-get-ex) :float
  (noise noise*)
  (f (:pointer :float))
  (type noise-type))

(defcfun ("TCOD_noise_get_fbm_ex" noise-get-fbm-ex) :float
  (noise noise*)
  (f (:pointer :float))
  (octaves ensure-float)
  (type noise-type))

(defcfun ("TCOD_noise_get_turbulence_ex" noise-get-turbulence-ex) :float
  (noise noise*)
  (f (:pointer :float))
  (octaves ensure-float)
  (type noise-type))

(defcfun ("TCOD_noise_get" noise-get) :float
  (noise noise*)
  (f (:pointer :float)))

(defcfun ("TCOD_noise_get_fbm" noise-get-fbm) :float
  (noise noise*)
  (f (:pointer :float))
  (octaves ensure-float))

(defcfun ("TCOD_noise_get_turbulence" noise-get-turbulence) :float
  (noise noise*)
  (f (:pointer :float))
  (octaces ensure-float))

(defcfun ("TCOD_noise_delete" noise-delete) :void
  (noise noise*))

(defcfun ("TCOD_noise_get_vectorized" noise-get-vectorized) :void
  (noise noise*)
  (type noise-type)
  (n :int)
  (x (:pointer :float))
  (y (:pointer :float))
  (z (:pointer :float))
  (w (:pointer :float))
  (out (:pointer :float)))

(defcfun ("TCOD_noise_get_fbm_vectorized" noise-get-fbm-vectorized) :void
  (noise noise*)
  (type noise-type)
  (octaves ensure-float)
  (n :int)
  (x (:pointer :float))
  (y (:pointer :float))
  (z (:pointer :float))
  (w (:pointer :float))
  (out (:pointer :float)))

(defcfun ("TCOD_noise_get_turbulence_vectorized" noise-get-turbulence-vectorized) :void
  (noise noise*)
  (type noise-type)
  (octaves ensure-float)
  (n :int)
  (x (:pointer :float))
  (y (:pointer :float))
  (z (:pointer :float))
  (w (:pointer :float))
  (out (:pointer :float)))

;;; parser.h

(defcfun ("TCOD_struct_get_name" struct-get-name) :string
  (def parser-struct*))

(defcfun ("TCOD_struct_add_property" struct-add-property) :void
  (def parser-struct*)
  (name :string)
  (type value-type)
  (mandatory :bool))

(defcfun ("TCOD_struct_add_list_property" struct-add-list-property) :void
  (def parser-struct*)
  (name :string)
  (type value-type)
  (mandatory :bool))

(defcfun ("TCOD_struct_add_value_list" struct-add-value-list) :void
  (def parser-struct*)
  (name :string)
  (value-list (:pointer :string))
  (mandatory :bool))

(defcfun ("TCOD_struct_add_value_list_sized" struct-add-value-list-sized) :void
  (def parser-struct*)
  (name :string)
  (value-list (:pointer :string))
  (size :int)
  (mandatory :bool))

(defcfun ("TCOD_struct_add_flag" struct-add-flag) :void
  (def parser-struct*)
  (propname :string))

(defcfun ("TCOD_struct_add_structure" struct-add-structure) :void
  (def parser-struct*)
  (sub-structure parser-struct*))

(defcfun ("TCOD_struct_is_mandatory" struct-is-mandatory) :bool
  (def parser-struct*)
  (propname :string))

(defcfun ("TCOD_struct_get_type" struct-get-type) :int
  (def parser-struct*)
  (prop :string))

(defcfun ("TCOD_parser_new" parser-new) parser*)

(defcfun ("TCOD_parser_new_struct" parser-new-struct) parser-struct*
  (parser parser*)
  (name :string))

(defcfun ("TCOD_parser_new_custom_type" parser-new-custom-type) value-type
  (parser parser*)
  (custom-type-parser parser-custom-callback))

(defcfun ("TCOD_parser_run" parser-run) :void
  (parser parser*)
  (filename :string)
  (listener parser-listener*))

(defcfun ("TCOD_parser_delete" parser-delete) :void
  (parser parser*))

(defcfun ("TCOD_parser_error" parser-error) :void
  (msg :string)
  &rest)

(defcfun ("TCOD_parser_has_property" parser-has-property) :bool
  (parser parser*)
  (name :string))

(defcfun ("TCOD_parser_get_bool_property" parser-get-bool-property) :bool
  (parser parser*)
  (name :string))

(defcfun ("TCOD_parser_get_char_property" parser-get-char-property) :int
  (parser parser*)
  (name :string))

(defcfun ("TCOD_parser_get_int_property" parser-get-int-property) :int
  (parser parser*)
  (name :string))

(defcfun ("TCOD_parser_get_float_property" parser-get-float-property) :float
  (parser parser*)
  (name :string))

(defcfun ("TCOD_parser_get_string_property" parser-get-string-property) :string
  (parser parser*)
  (name :string))

(defcfun ("TCOD_parser_get_color_property_wrapper" parser-get-color-property) colornum
  (parser parser*)
  (name :string))

(defcfun ("TCOD_parser_get_dice_property_py" parser-get-dice-property-py) :void
  (parser parser*)
  (name :string)
  (dice Dice*))

(defcfun ("TCOD_parser_get_custom_property" parser-get-custom-property)
    (:pointer :void)
  (parser parser*)
  (name :string))

(defcfun ("TCOD_parser_get_list_property" parser-get-list-property) tcod-list*
  (parser parser*)
  (name :string)
  (type value-type))

;;; path.h

(defcfun ("TCOD_path_new_using_map" path-new-using-map) path*
  (map map*)
  (diagonal-cost ensure-float))

(defcfun ("TCOD_path_new_using_function" path-new-using-function) path*
  (map-width :int)
  (map-height :int)
  (func path-func)
  (userdata userdata)
  (diagonal-cost ensure-float))

(defcfun ("TCOD_path_compute" path-compute) :bool
  (path path*)
  (ox :int)
  (oy :int)
  (dx :int)
  (dy :int))

(defcfun ("TCOD_path_walk" path-walk) :bool
  (path path*)
  (x (:pointer :int))
  (y (:pointer :int))
  (recalculate-when-needed :bool))

(defcfun ("TCOD_path_is_empty" path-is-empty) :bool
  (path path*))

(defcfun ("TCOD_path_size" path-size) :int
  (path path*))

(defcfun ("TCOD_path_reverse" path-reverse) :void
  (path path*))

(defcfun ("TCOD_path_get" path-get) :void
  (path path*)
  (index :int)
  (x (:pointer :int))
  (y (:pointer :int)))

(defcfun ("TCOD_path_get_origin" path-get-origin) :void
  (path path*)
  (x (:pointer :int))
  (y (:pointer :int)))

(defcfun ("TCOD_path_get_destination" path-get-destination) :void
  (path path*)
  (x (:pointer :int))
  (y (:pointer :int)))

(defcfun ("TCOD_path_delete" path-delete) :void
  (path path*))

(defcfun ("TCOD_dijkstra_new" dijkstra-new) dijkstra*
  (map map*)
  (diagonal-cost ensure-float))

(defcfun ("TCOD_dijkstra_new_using_function" dijkstra-new-using-function) dijkstra*
  (map-width :int)
  (map-height :int)
  (func path-func)
  (userdata userdata)
  (diagonal-cost ensure-float))

(defcfun ("TCOD_dijkstra_compute" dijkstra-compute) :void
  (dijkstra dijkstra*)
  (root-x :int)
  (root-y :int))

(defcfun ("TCOD_dijkstra_get_distance" dijkstra-get-distance) :float
  (dijkstra dijkstra*)
  (x :int)
  (y :int))

(defcfun ("TCOD_dijkstra_path_set" dijkstra-path-set) :bool
  (dijkstra dijkstra*)
  (x :int)
  (y :int))

(defcfun ("TCOD_dijkstra_is_empty" dijkstra-is-empty) :bool
  (dijkstra dijkstra*))

(defcfun ("TCOD_dijkstra_size" dijkstra-size) :int
  (dijkstra dijkstra*))

(defcfun ("TCOD_dijkstra_reverse" dijkstra-reverse) :void
  (dijkstra dijkstra*))

(defcfun ("TCOD_dijkstra_get" dijkstra-get) :void
  (dijkstra dijkstra*)
  (index :int)
  (x (:pointer :int))
  (y (:pointer :int)))

(defcfun ("TCOD_dijkstra_path_walk" dijkstra-path-walk) :bool
  (dijkstra dijkstra*)
  (x (:pointer :int))
  (y (:pointer :int)))

(defcfun ("TCOD_dijkstra_delete" dijkstra-delete) :void
  (dijkstra dijkstra*))

;;; pathfinder.h
(defctype size-t :uintptr)

(defcfun ("TCOD_pf_new" pf-new) pathfinder*
  (ndim :int)
  (shape (:pointer size-t)))

(defcfun ("TCOD_pf_delete" pf-delete) :void
  (path pathfinder*))

(defcfun ("TCOD_pf_set_distance_pointer" pf-set-distance-pointer) :void
  (path pathfinder*)
  (data (:pointer :void))
  (int-type :int)
  (strides (:pointer size-t)))

(defcfun ("TCOD_pf_set_graph2d_pointer" pf-set-graph2d-pointer) :void
  (path pathfinder*)
  (data (:pointer :void))
  (int-type :int)
  (strides (:pointer size-t))
  (cardinal :int)
  (diagonal :int))

(defcfun ("TCOD_pf_set_traversal_pointer" pf-set-traversal-pointer) :void
  (path pathfinder*)
  (data (:pointer :void))
  (int-type :int)
  (strides (:pointer size-t)))

(defcfun ("TCOD_pf_recompile" pf-recompile) :int
  (path pathfinder*))

(defcfun ("TCOD_pf_compute" pf-compute) :int
  (path pathfinder*))

(defcfun ("TCOD_pf_compute_step" pf-compute-step) :int
  (path pathfinder*))

;;; pathfinder_frontier.h
;; todo?

;;; random.h
;; -

;;; sys.h
;; skip? file/directory ops, thread ops, deprecated stuff

;;; tileset.h

(defcfun ("TCOD_tileset_new" tileset-new) tileset*
  (tile-width :int)
  (tile-height :int))

(defcfun ("TCOD_tileset_delete" tileset-delete) :void
  (tileset tileset*))

(defcfun ("TCOD_tileset_load" tileset-load) tileset*
  (filename :string)
  (columns :int)
  (rows :int)
  (n :int)
  (charmap (:pointer :int)))

(defcfun ("TCOD_tileset_load_mem" tileset-load-mem) tileset*
  (buffer-length size-t)
  (buffer :pointer)
  (columns :int)
  (rows :int)
  (n :int)
  (charmap (:pointer :int)))

(defcfun ("TCOD_tileset_load_raw" tileset-load-raw) tileset*
  (width :int)
  (height :int)
  (pixels color-rgba*)
  (columns :int)
  (rows :int)
  (n :int)
  (charmap (:pointer :int)))

(defcfun ("TCOD_tileset_assign_tile" tileset-assign-tile) error/int
  (tileset tileset*)
  (tile-id :int)
  (codepoint :int))

(defcfun ("TCOD_tileset_get_tile" tileset-get-tile) color-rgba*
  (tileset tileset*)
  (codepoint :int))

;;; tileset_bdf.h

(defcfun ("TCOD_load_bdf" load-bdf) tileset*
  (path :string))

(defcfun ("TCOD_load_bdf_memory" tileset-load-bdf-memory) tileset*
  (size :int)
  (buffer :pointer))

;;; tileset_truetype.h
;; provisional

;;; txtfield.h

(defcfun ("TCOD_text_init" text-init) text*
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (max-chars :int))

(defcfun ("TCOD_text_init2" text-init2) text*
  (x :int)
  (y :int)
  (max-chars :int))

(defcfun ("TCOD_text_set_pos" text-set-pos) :void
  (txt text*)
  (x :int)
  (y :int))

(defcfun ("TCOD_text_set_properties" text-set-properties) :void
  (txt text*)
  (cursor-char :int)
  (blink-interval :int)
  (prompt :string)
  (tab-size :int))

#++ ;; need libffi
(defcfun ("TCOD_text_set_colors" text-set-colors) :void
  (txt text*)
  (fore (:struct color))
  (back (:struct color))
  (back-transparency ensure-float))

#++ ;; need libffi
(defcfun ("TCOD_text_update" text-update) :bool
  (txt text*)
  (key tcod-key))

(defcfun ("TCOD_text_render" text-render) :void
  (txt text*)
  (console console*))

(defcfun ("TCOD_text_get" text-get) :string
  (txt text*))

(defcfun ("TCOD_text_reset" text-reset) :void
  (txt text*))

(defcfun ("TCOD_text_delete" text-delete) :void
  (txt text*))

;;; wrappers.h
(defcfun ("TCOD_console_fill_background" console-fill-background) :void
  (con console*)
  (r (:pointer :int))
  (g (:pointer :int))
  (b (:pointer :int)))

(defcfun ("TCOD_console_fill_foreground" console-fill-foreground) :void
  (con console*)
  (r (:pointer :int))
  (g (:pointer :int))
  (b (:pointer :int)))

(defcfun ("TCOD_console_fill_char" console-fill-char) :void
  (con console*)
  (arr (:pointer :int)))

(defcfun ("TCOD_console_double_hline" console-double-hline) :void
  (con console*)
  (x :int)
  (y :int)
  (flag bkgnd-flag))

(defcfun ("TCOD_console_double_vline" console-double-vline) :void
  (con console*)
  (x :int)
  (y :int)
  (flag bkgnd-flag))

(defcfun ("TCOD_console_print_double_frame" console-print-double-frame) :void
  (console console*)
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (empty :bool)
  (flag bkgnd-flag)
  (fmt :string)
  &rest)

;; TCOD_console_print_return_string

;;; zip.h
;; todo?
