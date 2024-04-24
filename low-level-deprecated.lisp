(in-package %3b-tcod)

(defbitfield event
  (:none 0)
  (:key-press 1)
  (:key-release 2)
  (:mouse-move 4)
  (:mouse-press 8)
  (:mouse-release 16))

;;; bresenham.h
(defcfun ("TCOD_line_step" line-step) :bool
  (x-cur (:pointer :int))
  (y-cur (:pointer :int)))

;; console.h
cffi:foreign-type-size
;; (doesn't support contexts)
(defcfun ("TCOD_console_set_fade_wrapper" console-set-fade) :void
  (val :uint8)
  (color colornum))

(defcfun ("TCOD_console_get_fade" console-get-fade) :uint8)

(defcfun ("TCOD_console_get_fading_color_wrapper" console-get-fading-color) colornum)

;; console_etc

;; (doesn't support contexts)

(defcfun ("TCOD_console_set_custom_font" console-set-custom-font) :void
  (fontfile :string)
  (flags :int)
  (nb-char-horiz :int)
  (nb-char-vert :int))

(defcfun ("TCOD_console_map_ascii_code_to_font" console-map-ascii-code-to-font) :void
  (ascii-code :int)
  (font-char-x :int)
  (font-char-y :int))

(defcfun ("TCOD_console_map_ascii_codes_to_font" console-map-ascii-codes-to-font) :void
  (ascii-code :int)
  (nb-codes :int)
  (font-char-x :int)
  (font-char-y :int))

(defcfun ("TCOD_console_map_string_to_font" console-map-string-to-font) :void
  (s (:string :encoding :iso-8859-1))
  (font-char-x :int)
  (font-char-y :int))

(defcfun ("TCOD_console_map_string_to_font_utf" console-map-string-to-font-utf) :void
  (s (:string :encoding :utf-16))
  (font-char-x :int)
  (font-char-y :int))

;; does nothing
(defcfun ("TCOD_console_set_dirty" console-set-dirty) :void
  (x :int)
  (y :int)
  (w :int)
  (h :int))

;; use sdl to check key state
(defcfun ("TCOD_console_is_key_pressed" console-is-key-pressed) :bool
  (key keycode))

;; doesn't support context
(defcfun ("TCOD_console_credits" console-credits) :void)
#++
(defcfun ("TCOD_console_credits_reset" console-credits-reset) :void)
(defcfun ("TCOD_console_credits_render" console-credits-render) :bool
  (x :int)
  (y :int)
  (alpha :bool))

;; does nothing
(defcfun ("TCOD_console_set_keyboard_repeat" console-set-keyboard-repeat) :void
  (initial-delay :int)
  (interval :int))
(defcfun ("TCOD_console_disable_keyboard_repeat" console-set-keyboard-repeat) :void)


;;; console_init.h
;; use contexts instead
(defcfun ("TCOD_console_init_root" console-init-root) tcod-error
  (w :int)
  (h :int)
  (title :string)
  (fullscreen :bool)
  (renderer renderer))
(defcfun ("TCOD_console_init_root_" console-init-root-) tcod-error
  (w :int)
  (h :int)
  (title :string)
  (fullscreen :bool)
  (renderer renderer)
  (vsync :bool))

;; use sdl
(defcfun ("TCOD_console_set_window_title" console-set-window-title) :void
  (title (:string :encoding :utf-8)))
(defcfun ("TCOD_console_set_fullscreen" console-set-fullscreen) :void
  (fullscreen :bool))
(defcfun ("TCOD_console_is_fullscreen" console-is-fullscreen) :bool)
(defcfun ("TCOD_console_has_mouse_focus" console-has-mouse-focus) :bool)
(defcfun ("TCOD_console_is_active" console-is-active) :bool)
(defcfun ("TCOD_console_is_window_closed" console-is-window-closed) :bool)
;; not compatible with contexts
(defcfun ("TCOD_sys_get_sdl_window" sys-get-sdl-window) :pointer)
(defcfun ("TCOD_sys_get_sdl_renderer" sys-get-sdl-renderer) :pointer)
(defcfun ("TCOD_sys_accumulate_console" sys-accumulate-console) :int
  (con console*))
#++
(defcfun ("TCOD_sys_accumulate_console_" sys-accumulate-console-) :int
  (con console*)
  (viewport (:pointer sdl-rect)))
;; used to port from stuff using deprecated APIs
(defcfun ("TCOD_sys_get_internal_context" sys-get-internal-context) context*)
(defcfun ("TCOD_sys_get_internal_console" sys-get-internal-console) console*)
(defcfun ("TCOD_quit" quit) :void)


;;; console_printing.h
;; probably should just replace these with calls to the printf
;; versions and ignore the bindings since we are translating lisp
;; strings anyway

;; use console_printf_* instead for utf8
(defcfun ("TCOD_console_print" console-print) :void
  (con console*)
  (x :int)
  (y :int)
  (fmt (:string :encoding :iso-8859-1))
  &rest)

(defcfun ("TCOD_console_print_ex" console-print-ex) :void
  (con console*)
  (x :int)
  (y :int)
  (flag bkgnd-flag)
  (alignment alignment)
  (fmt (:string :encoding :iso-8859-1))
  &rest)

(defcfun ("TCOD_console_print_rect" console-print-rect) :void
  (con console*)
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (fmt (:string :encoding :iso-8859-1))
  &rest)

(defcfun ("TCOD_console_print_rect_ex" console-print-rect-ex) :void
  (con console*)
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (flag bkgnd-flag)
  (alignment alignment)
  (fmt (:string :encoding :iso-8859-1))
  &rest)

(defcfun ("TCOD_console_print_frame" console-print-frame) :void
  (con console*)
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (empty :bool)
  (flag bkgnd-flag)
  (fmt (:string :encoding :iso-8859-1))
  &rest)
;; use get_height_rect_fmt instead
(defcfun ("TCOD_console_get_height_rect" console-get-height-rect) :void
  (con console*)
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (fmt (:string :encoding :iso-8859-1))
  &rest)
;; use console_printf_* instead
#++
(defcfun ("TCOD_console_print_utf" console-print-utf) :void
  (con console*)
  (x :int)
  (y :int)
  (fmt (:pointer :wchar))
  &rest)
#++
(defcfun ("TCOD_console_print_ex_utf" console-print-ex-utf) :void
  (con console*)
  (x :int)
  (y :int)
  (flag bkgnd-flag)
  (alignment alignment)
  (fmt (:pointer :wchar))
  &rest)
#++
(defcfun ("TCOD_console_print_rect_utf" console-print-rect-utf) :void
  (con console*)
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (fmt (:pointer :wchar))
  &rest)
#++
(defcfun ("TCOD_console_print_rect_ex_utf" console-print-rect-ex-utf) :void
  (con console*)
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (flag bkgnd-flag)
  (alignment alignment)
  (fmt (:pointer :wchar))
  &rest)
#++
(defcfun ("TCOD_console_get_height_rect_utf" console-get-height-rect-utf) :void
  (con console*)
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (fmt (:pointer :wchar))
  &rest)

;;deprecated
(defcfun ("TCOD_console_printn_frame" console-printn-frame) tcod-error
  (con console*)
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (n :int)
  (title (:string :encoding :utf-8)) ;;?
  (fg color-rgb*)
  (bg color-rgb*)
  (flag bkgnd-flag)
  (clear :bool))

;;; console_rexpaint.h
;; use tcod_load_xp instead
#++
(defcfun ("TCOD_console_list_from_xp" console-list-from-xp) tcod-list
  (filename :string))
;; use tcod_save_xp instead
#++
(defcfun ("TCOD_console_list_save_xp" console-list-save-xp) :bool
  (con tcod-list)
  (filename :string)
  (compress-level :int))

;;; console_types.h
(defcenum keycode
  (:key-none 0)
  (:key-escape 1)
  (:key-backspace 2)
  (:key-tab 3)
  (:key-enter 4)
  (:key-shift 5)
  (:key-control 6)
  (:key-alt 7)
  (:key-pause 8)
  (:key-capslock 9)
  (:key-pageup 10)
  (:key-pagedown 11)
  (:key-end 12)
  (:key-home 13)
  (:key-up 14)
  (:key-left 15)
  (:key-right 16)
  (:key-down 17)
  (:key-printscreen 18)
  (:key-insert 19)
  (:key-delete 20)
  (:key-lwin 21)
  (:key-rwin 22)
  (:key-apps 23)
  (:key-0 24)
  (:key-1 25)
  (:key-2 26)
  (:key-3 27)
  (:key-4 28)
  (:key-5 29)
  (:key-6 30)
  (:key-7 31)
  (:key-8 32)
  (:key-9 33)
  (:key-kp0 34)
  (:key-kp1 35)
  (:key-kp2 36)
  (:key-kp3 37)
  (:key-kp4 38)
  (:key-kp5 39)
  (:key-kp6 40)
  (:key-kp7 41)
  (:key-kp8 42)
  (:key-kp9 43)
  (:key-kpadd 44)
  (:key-kpsub 45)
  (:key-kpdiv 46)
  (:key-kpmul 47)
  (:key-kpdec 48)
  (:key-kpenter 49)
  (:key-f1 50)
  (:key-f2 51)
  (:key-f3 52)
  (:key-f4 53)
  (:key-f5 54)
  (:key-f6 55)
  (:key-f7 56)
  (:key-f8 57)
  (:key-f9 58)
  (:key-f10 59)
  (:key-f11 60)
  (:key-f12 61)
  (:key-numlock 62)
  (:key-scrolllock 63)
  (:key-space 64)
  (:key-char 65)
  (:key-text 66))


(defcstruct tcod-key
  (vk keycode)
  (c :char)
  (text :char :count 32)
  (pressed :bool)
  (lalt :bool)
  (lctrl :bool)
  (lmeta :bool)
  (ralt :bool)
  (rctrl :bool)
  (rmeta :bool)
  (shift :bool))

(defcenum char
  (:SMILIE 1)
  (:SMILIE-INV 2)
  (:HEART 3)
  (:DIAMOND 4)
  (:CLUB 5)
  (:SPADE 6)
  (:BULLET 7)
  (:BULLET-INV 8)
  (:RADIO-UNSET 9)
  (:RADIO-SET 10)
  (:MALE 11)
  (:FEMALE 12)
  (:NOTE 13)
  (:NOTE-DOUBLE 14)
  (:LIGHT 15)
  (:ARROW2-E 16)
  (:ARROW2-W 17)
  (:DARROW-V 18)
  (:EXCLAM-DOUBLE 19)
  (:PILCROW 20)
  (:SECTION 21)
  (:ARROW-N 24)
  (:ARROW-S 25)
  (:ARROW-E 26)
  (:ARROW-W 27)
  (:DARROW-H 29)
  (:ARROW2-N 30)
  (:ARROW2-S 31)
  (:POUND 156)
  (:MULTIPLICATION 158)
  (:FUNCTION 159)
  (:RESERVED 169)
  (:HALF 171)
  (:ONE-QUARTER 172)
  (:BLOCK1 176)
  (:BLOCK2 177)
  (:BLOCK3 178)
  (:VLINE 179)
  (:TEEW 180)
  (:COPYRIGHT 184)
  (:DTEEW 185)
  (:DVLINE 186)
  (:DNE 187)
  (:DSE 188)
  (:CENT 189)
  (:YEN 190)
  (:NE 191)
  (:SW 192)
  (:TEEN 193)
  (:TEES 194)
  (:TEEE 195)
  (:HLINE 196)
  (:CROSS 197)
  (:DSW 200)
  (:DNW 201)
  (:DTEEN 202)
  (:DTEES 203)
  (:DTEEE 204)
  (:DHLINE 205)
  (:DCROSS 206)
  (:CURRENCY 207)
  (:SE 217)
  (:NW 218)
  (:CHECKBOX-UNSET 224)
  (:CHECKBOX-SET 225)
  (:SUBP-NW 226)
  (:SUBP-NE 227)
  (:SUBP-N 228)
  (:SUBP-SE 229)
  (:SUBP-DIAG 230)
  (:SUBP-E 231)
  (:SUBP-SW 232)
  (:THREE-QUARTERS 243)
  (:DIVISION 246)
  (:GRADE 248)
  (:UMLAUT 249)
  (:POW1 251)
  (:POW3 252)
  (:POW2 253)
  (:BULLET-SQUARE 254))

(defcenum key-status
  (:pressed 1)
  (:released 2))

(defbitfield font-flags
  (:layout-ascii-incol 1)
  (:layout-ascii-inrow 2)
  (:greyscale 4)
  (:grayscale 4)
  (:layout-tcod 8)
  (:layout-cp437 16))

;;; heightmap.h
;; does nothing
(defcfun ("TCOD_heightmap_islandify" heightmap-islandify) :void
  (hm heightmap*)
  (sea-level :float)
  (rnd random*))


;;; mouse.h
;; use sdl
(defcstruct mouse
  (x :int) (y :int)
  (dx :int) (dy :int)
  (cx :int) (cy :int)
  (dcx :int) (dcy :int)
  (lbutton :bool)
  (rbutton :bool)
  (mbutton :bool)
  (lbutton-pressed :bool)
  (rbutton-pressed :bool)
  (mbutton-pressed :bool)
  (wheel-up :bool)
  (wheel-down :bool))
(defctype mouse* (:pointer (:struct mouse)))
(defcfun ("TCOD_mouse_show_cursor" mouse-show-cursor) :void
  (visible :bool))

(defcfun ("TCOD_mouse_get_status_wrapper" mouse-get-status-wrapper) :void
  (mouse mouse*))

(defcfun ("TCOD_mouse_is_cursor_visible" mouse-is-cursor-visible) :bool)
(defcfun ("TCOD_mouse_move" mouse-move) :void
  (x :int)
  (y :int))

;;; sys.h
(defcfun ("TCOD_sys_check_for_event" sys-check-for-event) :int
  (a :int)
  (a (:pointer :void))
  (a (:pointer :void)))

(defcfun ("TCOD_sys_clipboard_get" sys-clipboard-get) (:pointer :char))

(defcfun ("TCOD_sys_clipboard_set" sys-clipboard-set) :bool
  (a (:pointer :char)))

(defcfun ("TCOD_sys_create_directory" sys-create-directory) :bool
  (a (:pointer :char)))

(defcfun ("TCOD_sys_delete_directory" sys-delete-directory) :bool
  (a (:pointer :char)))

(defcfun ("TCOD_sys_delete_file" sys-delete-file) :bool
  (a (:pointer :char)))

(defcfun ("TCOD_sys_elapsed_milli" sys-elapsed-milli) :int)

(defcfun ("TCOD_sys_elapsed_seconds" sys-elapsed-seconds) :float)

(defcfun ("TCOD_sys_file_exists" sys-file-exists) :bool)

(defcfun ("TCOD_sys_force_fullscreen_resolution" sys-force-fullscreen-resolution) :void
  (a :int)
  (a :int))

(defcfun ("TCOD_sys_get_char_size" sys-get-char-size) :void
  (a (:pointer :int))
  (a (:pointer :int)))

(defcfun ("TCOD_sys_get_current_resolution" sys-get-current-resolution) :void
  (a (:pointer :int))
  (a (:pointer :int)))

(defcfun ("TCOD_sys_get_directory_content" sys-get-directory-content) (:pointer
                                                                       :void)
  (a (:pointer :char))
  (a (:pointer :char)))

(defcfun ("TCOD_sys_get_fps" sys-get-fps) :int)

(defcfun ("TCOD_sys_get_fullscreen_offsets" sys-get-fullscreen-offsets) :void
  (a (:pointer :int))
  (a (:pointer :int)))

(defcfun ("TCOD_sys_get_last_frame_length" sys-get-last-frame-length) :float)

(defcfun ("TCOD_sys_get_num_cores" sys-get-num-cores) :int)

(defcfun ("TCOD_sys_get_renderer" sys-get-renderer) :int)

(defcfun ("TCOD_sys_is_directory" sys-is-directory) :bool
  (a (:pointer :char)))

(defcfun ("TCOD_sys_load_image" sys-load-image) (:pointer :void)
  (a (:pointer :char)))

(defcfun ("TCOD_sys_save_screenshot" sys-save-screenshot) :void
  (a (:pointer :char)))

(defcfun ("TCOD_sys_set_fps" sys-set-fps) :void
  (a :int))

(defcfun ("TCOD_sys_set_renderer" sys-set-renderer) :void
  (a :int))

(defcfun ("TCOD_sys_shutdown" sys-shutdown) :void)

(defcfun ("TCOD_sys_sleep_milli" sys-sleep-milli) :void
  (a :uint))

(defcfun ("TCOD_sys_startup" sys-startup) :void)

(defcfun ("TCOD_sys_update_char" sys-update-char) :void
  (a :int)
  (a :int)
  (a :int)
  (a (:pointer :void))
  (a :int)
  (a :int))

(defcfun ("TCOD_sys_wait_for_event" sys-wait-for-event) :int
  (a :int)
  (a (:pointer :void))
  (a (:pointer :void))
  (a :bool))

(defcfun ("TCOD_condition_broadcast" condition-broadcast) :void
  (a (:pointer :void)))

(defcfun ("TCOD_condition_delete" condition-delete) :void
  (a (:pointer :void)))

(defcfun ("TCOD_condition_new" condition-new) (:pointer :void))

(defcfun ("TCOD_condition_signal" condition-signal) :void
  (a (:pointer :void)))

(defcfun ("TCOD_condition_wait" condition-wait) :void
  (a (:pointer :void))
  (a (:pointer :void)))

(defcfun ("TCOD_thread_wait" thread-wait) :void
  (a (:pointer :void)))

(defcfun ("TCOD_semaphore_delete" semaphore-delete) :void
  (a (:pointer :void)))

(defcfun ("TCOD_semaphore_lock" semaphore-lock) :void
  (a (:pointer :void)))

(defcfun ("TCOD_semaphore_new" semaphore-new) (:pointer :void)
  (a :int))

(defcfun ("TCOD_semaphore_unlock" semaphore-unlock) :void
  (a (:pointer :void)))

(defcfun ("TCOD_mutex_delete" mutex-delete) :void
  (a (:pointer :void)))

(defcfun ("TCOD_mutex_in" mutex-in) :void
  (a (:pointer :void)))

(defcfun ("TCOD_mutex_new" mutex-new) (:pointer :void))

(defcfun ("TCOD_mutex_out" mutex-out) :void
  (a (:pointer :void)))
