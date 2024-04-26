(in-package #:3b-tcod)

;;; bresenham.h

;;; instead of line-init-mt, use line-mt-new and line-mt-delete, or with-line-mt
(defun line-mt-new (x-from y-from x-to y-to)
  (let ((l (cffi:foreign-alloc '(:struct %::bresenham-data))))
    (%:line-init-mt x-from y-from x-to y-to l)
    l))

(defun line-mt-delete (line*)
  (cffi:foreign-free line*))

(defmacro with-line-mt ((line x-from y-from x-to y-to) &body body)
  `(let ((,line (line-mt-new ,x-from ,y-from ,x-to ,y-to)))
     (unwind-protect (progn ,@body)
       (line-mt-delete (shiftf ,line nil)))))

;;; color.h

(defun color-rgb (r g b)
  (compose-color r g b))

(defun from-color* (color*)
  (cffi:with-foreign-slots ((%::r %::g %::b)
                            color*
                            (:struct %::color))
    (compose-color %::r %::g %::b)))

(defun from-rgba* (rgba*)
  (cffi:with-foreign-slots ((%::r %::g %::b %::a)
                            rgba*
                            (:struct %::color-rgba))
    (compose-color %::r %::g %::b %::a)))

(defun to-color* (color* colornum)
  (cffi:with-foreign-slots ((%::r %::g %::b)
                            color*
                            (:struct %::color))
    (setf (values %::r %::g %::b)
          (decompose-color colornum))))

(defun to-rgba* (rgba* colornum)
  (cffi:with-foreign-slots ((%::r %::g %::b %::a)
                            rgba*
                            (:struct %::color-rgba))
    (setf (values %::r %::g %::b %::a)
          (decompose-color colornum))))

(defmacro with-color* ((c &key init alpha) &body body)
  (assert (constantp alpha))
  (a:with-gensyms (tmp)
    ;; store a copy of INIT in case it is same name as C, so the scope
    ;; makes more sense
    `(let ,(when init
             `((,tmp ,init)))
       (cffi:with-foreign-object (,c ,(if alpha
                                          ''(:struct %::color-rgba)
                                          ''(:struct %::color)))
         ,@(when init (if alpha
                          `((to-rgba* ,c ,tmp))
                          `((to-color* ,c ,tmp))))
         (progn ,@body)))))

(defun color-hsv (h s v)
  (with-color* (c)
    (%:color-set-hsv c h s v)
    (from-color* c)))

(defun color-alpha-blend (a b)
  (with-color* (a :init a :alpha t)
    (with-color* (b :init b :alpha t)
      (%:color-alpha-blend a b))
    (from-rgba* a)))

(defun color-get-hsv (color)
  (cffi:with-foreign-objects ((h :float) (s :float) (v :float))
    (%:color-get-hsv color h s v)
    (values (cffi:mem-ref h :float)
            (cffi:mem-ref s :float)
            (cffi:mem-ref v :float))))

(defun color-set-hue (c hue)
  (with-color* (c :init c)
    (%:color-set-hue c hue)
    (from-color* c)))

(defun color-set-saturation (c saturation)
  (with-color* (c :init c)
    (%:color-set-saturation c saturation)
    (from-color* c)))

(defun color-set-value (c value)
  (with-color* (c :init c)
    (%:color-set-value c value)
    (from-color* c)))

(defun color-shift-hue (c shift)
  (with-color* (c :init c)
    (%:color-shift-hue c shift)
    (from-color* c)))

(defun color-scale-hsv (c saturation-coef value-coef)
  (with-color* (c :init c)
    (%:color-scale-hsv c saturation-coef value-coef)
    (from-color* c)))

(defun color-gen-map (keys)
  "Generate an interpolated gradient of colors.

 KEYS is a vector of (index . color) conses, where color is a name or
 colornum. If lowest index isn't 0, color 0 is assumed to be black.
  (color-gen-map #((0 . :black) (64 . :dark-amber) (192 . :cyan) (255 . :white)))
  Returns a vector of colornum"
  ;; just implementing in lisp since easier than dealing with pointers
  ;; and translating structs. (Unless something else accepts the
  ;; pointer form, in which case we might want something to make those
  ;; as well)
  (loop
    with a = (make-array (reduce 'max keys :key 'car)
                         :element-type '(unsigned-byte 32)
                         :initial-element 0)
    for k1 = 0 then k2
    for c1 = (color :black) then c2
    for (k2 . v2) across keys
    for c2 = (color v2)
    for d = (float (- k2 k1))
    do (assert (<= k1 k2))
       (loop for i from k1 below k2
             for c = (color-lerp c1 c2 (/ (- i k1) d))
             do (setf (aref a i) c))
    finally (return a)))

#++
(color-gen-map  #((0 . :black) (64 . :dark-amber) (192 . :cyan) (255 . :white)))
#++
(color-gen-map  #((32 . :green) (32 . :black) (64 . :red)))

;; console.h

(defun console-blit-key-color (src src-x src-y src-w src-h
                               dst dst-x dst-y
                               foreground-alpha background-alpha key-color)
  (with-color* (key-color :init key-color)
    (%:console-blit-key-color src src-x src-y src-w src-h
                              dst dst-x dst-y
                              foreground-alpha background-alpha key-color)))

(declaim (inline console-set-char console-put-char))

(defun console-set-char (con x y char)
  (%:console-set-char con x y (if (characterp char) (char-code char) char)))

(defun console-put-char (con x y char flag)
  (%:console-put-char con x y
                      (if (characterp char) (char-code char) char)
                      flag))

(defun console-put-char-ex (con x y char fg bg)
  (%:console-put-char-ex con x y
                         (if (characterp char) (char-code char) char)
                         fg bg))

;;; console_etc
(declaim (inline bkgnd-alpha bkgnd-add-alpha background-alpha background-alpha))

(defun bkgnd-alpha (alpha)
  (dpb (a:clamp (round (* alpha 255)) 0 255) (byte 8 8)
       #.(cffi:foreign-enum-value '%::bkgnd-flag :alph)))

(defun bkgnd-add-alpha (alpha)
  (dpb (a:clamp (round (* alpha 255)) 0 255) (byte 8 8)
       #.(cffi:foreign-enum-value '%::bkgnd-flag :adda)))

;; cl-tcod compat
(defun background-alpha (alpha) (bkgnd-alpha alpha))
(defun background-add-alpha (alpha) (bkgnd-add-alpha alpha))

(defun console-flush-ex (console &key (keep-aspect t)
                                   (integer-scaling t)
                                   (clear-color :black)
                                   (align-x 0.0)
                                   (align-y 0.0))
  (cffi:with-foreign-object (vo '(:struct %::viewport-options))
    (cffi:with-foreign-slots ((%::version
                               %::keep-aspect
                               %::integer-scaling
                               %::align-x %::align-y)
                              vo (:struct %::viewport-options))
      (setf %::version %::+compiled-version+
            %::keep-aspect keep-aspect
            %::integer-scaling integer-scaling
            %::align-x (coerce align-x 'single-float)
            %::align-y (coerce align-y 'single-float))
      (to-rgba* (cffi:foreign-slot-pointer
                 vo '(:struct %::viewport-options) '%::clear-color)
                (color clear-color))
      (%::checked (%:console-flush-ex console vo)))))

;; console_printing.h

(defun console-printf (console x y fmt &rest args)
  (%:console-printf console x y
                    "%s" :string
                    (apply #'format nil fmt args)))

(defun console-printf-ex (console x y flag alignment fmt &rest args)
  (%:console-printf-ex console x y flag alignment
                       "%s" :string
                       (apply #'format nil fmt args)))

(defun console-printf-rect (console x y w h fmt &rest args)
  (%:console-printf-rect console x y w h
                         "%s" :string
                         (apply #'format nil fmt args)))

(defun console-printf-rect-ex (console x y w h flag alignment fmt &rest args)
  (%:console-printf-rect-ex console x y w h flag alignment
                            "%s" :string
                            (apply #'format nil fmt args)))

(defun console-printf-frame (console x y w h empty flag fmt &rest args)
  (%:console-printf-frame console x y w h empty flag
                          "%s" :string
                          (apply #'format nil fmt args)))

(defun console-print-double-frame (console x y w h empty flag
                                   fmt &rest args)
  (%:console-print-double-frame console x y w h empty flag
                                "%s" :string
                                (apply #'format nil fmt args)))

(defun console-get-height-rect-fmt (console x y w h fmt &rest args)
  (%:console-get-height-rect-fmt console x y w h
                                 "%s" :string
                                 (apply #'format nil fmt args)))

(defun console-printn (console x y str fg bg flag alignment
                       &key start end)
  (with-color* (fg :init fg)
    (with-color* (bg :init bg)
      (cffi:with-foreign-string ((pstr n) (if (or start end)
                                              (subseq str (or start 0)
                                                      (or end (length str)))
                                              start))
        (%:console-printn console x y n str fg bg flag alignment)))))

(defun console-printn-rect (console x y w h str fg bg flag alignment
                            &key (start 0) (end (1- (length str))))
  (with-color* (fg :init fg)
    (with-color* (bg :init bg)
      (cffi:with-foreign-string ((pstr n) (if (or start end)
                                              (subseq str (or start 0)
                                                      (or end (length str)))
                                              start))
        (%:console-printn-rect console x y w h n str fg bg flag alignment)))))

;; add some aliases for -print- names. todo: add others
(defun console-print (console x y fmt &rest args)
  (apply #'console-printf console x y fmt args))

(defun console-print-ex (console x y flag alignment fmt &rest args)
  (apply #'console-printf-ex console x y flag alignment fmt args))

;;; context.h
(defun context-present (context console &key (keep-aspect t)
                                          (integer-scaling t)
                                          (clear-color :black)
                                          (align-x 1/2)
                                          (align-y 1/2))
  (cffi:with-foreign-object (vo '(:struct %::viewport-options))
    (macrolet ((o (s new)
                 `(setf (cffi:foreign-slot-value
                         vo '(:struct %::viewport-options) ',s)
                        ,new)))
      (o %::version %::+compiled-version+)
      (o %::keep-aspect (if keep-aspect 1 0))
      (o %::integer-scaling (if integer-scaling 1 0))
      (multiple-value-bind (r g b)
          (decompose-color (etypecase clear-color
                             (null 0)
                             (symbol (color :light-blue))
                             (integer clear-color)))
        (let ((cc (cffi:foreign-slot-pointer vo '(:struct %::viewport-options)
                                             '%::clear-color)))
          (setf (cffi:foreign-slot-value cc '(:struct %::color) '%::r) r)
          (setf (cffi:foreign-slot-value cc '(:struct %::color) '%::g) g)
          (setf (cffi:foreign-slot-value cc '(:struct %::color) '%::b) b)))
      (o %::align-x (coerce (or align-x 0.0) 'single-float))
      (o %::align-y (coerce (or align-y 0.0) 'single-float)))
    (%::checked (%:context-present context console vo))))

(defun context-screen-pixel-to-tile-d (context x y)
  (cffi:with-foreign-objects ((px :double) (py :double))
    (setf (cffi:mem-ref px :double) (coerce x 'double-float))
    (setf (cffi:mem-ref py :double) (coerce y 'double-float))
    (%::checked (%:context-screen-pixel-to-tile-d context px py))
    (values (cffi:mem-ref px :double) (cffi:mem-ref py :double))))

(defun context-screen-pixel-to-tile-i (context x y)
  (cffi:with-foreign-objects ((px :int) (py :int))
    (setf (cffi:mem-ref px :int) (coerce x 'integer))
    (setf (cffi:mem-ref py :int) (coerce y 'integer))
    (%::checked (%:context-screen-pixel-to-tile-i context px py))
    (values (cffi:mem-ref px :int) (cffi:mem-ref py :int))))

(defun context-get-renderer-type (context)
  (let ((r (%:context-get-renderer-type context)))
    (cffi:foreign-enum-keyword '%::renderer-type r :errorp nil)))

(defun context-screen-capture (context &key 2d-array)
  (cffi:with-foreign-objects ((w :int) (h :int))
    ;; could probably use context-screen-capture-alloc here and save a
    ;; tiny bit of work, but don't think cffi guarantees foreign-free
    ;; is free(), so just use cffi for allocation
    (when (zerop (%:context-screen-capture context (cffi:null-pointer) w h))
      (let ((s (* (cffi:mem-ref w :int)
                  (cffi:mem-ref h :int))))
        (format t "~s x ~s~%" (cffi:mem-ref w :int)
                (cffi:mem-ref h :int))
        ;; make sure both are positive and we have a somewhat reasonable
        ;; total size (256MB chosen arbitrarily, need to see what actual
        ;; bounds are for TCOD consoles)
        (assert (and (plusp (cffi:mem-ref w :int))
                     (< 0 s (expt 2 26))))
        (cffi:with-foreign-object (p :uint32 s)
          (print (%:context-screen-capture context p w h))
          (if 2d-array
              (cffi:foreign-array-to-lisp
               p `(:array :uint32 ,(cffi:mem-ref h :int) ,(cffi:mem-ref w :int))
               :element-type '(unsigned-byte 32))
              (cffi:foreign-array-to-lisp p `(:array :uint32 ,s)
                                          :element-type '(unsigned-byte 32))))))))

(defun context-set-mouse-transform (context offset-x offset-y scale-x scale-y)
  (cffi:with-foreign-object (p '(:struct %::mouse-transform))
    (cffi:with-foreign-slots ((%::offset-x %::offset-y %::scale-x %::scale-y)
                              p (:struct %::mouse-transform))
      (setf %::offset-x offset-x
            %::offset-y offset-y
            %::scale-x scale-x
            %::scale-y scale-y)
      (%:context-set-mouse-transform context p))))

(defun context-recommended-console-size (context &key (magnification 1.0))
  (cffi:with-foreign-objects ((w :int) (h :int))
    (%:context-recommended-console-size context
                                        (coerce magnification 'single-float)
                                        w h)
    (values (cffi:mem-ref w :int) (cffi:mem-ref h :int))))

(cffi:defcallback default-cli-output :void
    ((userdata :pointer) (output :string))
  ;; default in lib exits, so make our own default that
  ;; doesn't. Possibly should ERROR or CERROR or something here
  ;; instead of BREAK? if this function returns, context-new will
  ;; return TCOD_E_REQUIRES_ATTENTION, so possibly could just print a
  ;; message and let that error instead?
  (break "CLI output~@[ ~x~]: ~a" (unless (zerop userdata) userdata) output))

(defun context-new (title
                    &key x y width height
                      rows columns renderer-type
                      tileset
                      (vsync t)
                      sdl-window-flags
                      argv
                      cli-output-callback
                      cli-userdata
                      console)
  (cffi:with-foreign-objects ((cp '(:struct %::context-params))
                              (out '%::context*))
    (when argv (error "todo: support passing argv~% got ~s" argv))
    (macrolet ((p (s new)
                 `(setf (cffi:foreign-slot-value
                         cp '(:struct %::context-params) ',s)
                        ,new))
               (pz (s new)
                 `(setf (cffi:foreign-slot-value
                         cp '(:struct %::context-params) ',s)
                        (or ,new 0))))
      (p %::version %::+compiled-version+)
      (pz %::window-x x)
      (pz %::window-y y)
      (pz %::pixel-width width)
      (pz %::pixel-height height)
      (pz %::columns columns)
      (pz %::rows rows)
      (p %::renderer-type (if renderer-type
                              (print (cffi:foreign-enum-value
                                      '%::renderer renderer-type))
                              0))
      (p %::tileset (or tileset (cffi:null-pointer)))
      (p %::vsync (if vsync 1 0))
      (p %::sdl-window-flags (if (integerp sdl-window-flags)
                                 sdl-window-flags
                                 (cffi:foreign-bitfield-value
                                  '%::sdl-window-flags sdl-window-flags)))
      (p %::window-title title)
      #++(p argc (length argv))
      #++(p argv argp)
      (p %::argc 0)
      (p %::argv (cffi:null-pointer))
      (p %::cli-output-callback (or cli-output-callback
                                    (cffi:callback default-cli-output)))
      (p %::cli-userdata (etypecase cli-userdata
                           (unsigned-byte cli-userdata)
                           (cffi:foreign-pointer
                            (cffi:pointer-address cli-userdata))
                           (null 0)))
      (p %::window-xy-defined (if (or x y) 1 0))
      (p %::console (or console (cffi:null-pointer)))
      (%::checked (%:context-new cp out))
      (cffi:mem-ref out :pointer))))

;; not sure if this is generally useful, but nice for quick tests
(defmacro with-context ((var title &rest args
                         &key  x y width height
                           rows columns renderer-type
                           tileset
                           (vsync t)
                           sdl-window-flags
                           argv
                           cli-output-callback
                           cli-userdata
                           console)
                        &body body)
  (declare (ignore x y width height rows columns renderer-type tileset
                   vsync sdl-window-flags argv cli-output-callback cli-userdata
                   console))
  `(let ((,var (context-new ,title ,@args)))
     (unwind-protect
          (progn ,@body)
       (context-delete ,var))))

(defmacro with-console ((var w h) &body body)
  `(let ((,var (console-new ,w ,h)))
     (unwind-protect (progn ,@body)
       (console-delete (shiftf ,var nil)))))

;;; error.h
(defun set-errorf (fmt &rest args)
  (%:set-errorf "%s" :string (apply #'format nil fmt args)))

;;; fov.h

;;; heightmap.h
(defun heightmap-get-normal (heightmap x y &key (water-level 0.0))
  (cffi:with-foreign-object (n :float 3)
    (%:heightmap-get-normal heightmap x y n water-level)
    (make-array 3 :element-type 'single-float
                  :initial-contents (list (cffi:mem-aref n :float 0)
                                          (cffi:mem-aref n :float 1)
                                          (cffi:mem-aref n :float 2)))))

(defun heightmap-get-minmax (heightmap)
  (cffi:with-foreign-objects ((min :float) (max :float))
    (%:heightmap-get-minmax heightmap min max)
    (values (cffi:mem-ref min :float) (cffi:mem-ref max :float))))

(defun heightmap-dig-bezier (heightmap px py
                             start-radius start-depth end-radius end-depth)
  (cffi:with-foreign-objects ((ppx :int 4) (ppy :int 4))
    (loop for i below 4
          do (setf (cffi:mem-aref ppx :int i) (aref px i)
                   (cffi:mem-aref ppy :int i) (aref py i)))
    (%:heightmap-dig-bezier heightmap ppx ppy
                            start-radius start-depth end-radius end-depth)))

(defun heightmap-kernel-transform (heightmap dx dy weight min-level max-level)
  ;; DX,DY are integer offsets of sparse convolution kernel, with
  ;; weight in WEIGHT?
  (assert (= (length dx) (length dy) (length weight)))
  (let ((l (length weight)))
    (cffi:with-foreign-objects ((pdx :int l) (pdy :int l) (pw :float l))
      (loop for i below l
            do (setf (cffi:mem-aref pdx :int i) (aref dx i)
                     (cffi:mem-aref pdy :int i) (aref dy i)
                     (cffi:mem-aref pw :float i) (coerce (aref weight i)
                                                         'single-float)))
      (%:heightmap-kernel-transform heightmap l pdx pdy pw
                                    min-level max-level))))

(defun heightmap-add-voronoi (heightmap number-points coef rnd)
  (let ((l (length coef)))
    (cffi:with-foreign-object (pc :float l)
      (loop for i below l
            do (setf (cffi:mem-aref pc :float i) (coerce (aref coef i)
                                                         'single-float)))
      (%:heightmap-add-voronoi heightmap number-points l pc
                               (or rnd (cffi:null-pointer))))))

;;; image.h

(defun image-get-size (image)
  (cffi:with-foreign-objects ((w :int) (h :int))
    (%:image-get-size image w h)
    (values (cffi:mem-ref w :int) (cffi:mem-ref h :int))))

;;; list.h
;; list stuff probably isn't useful in generic form, so not including
;; in high-level for now

;;; logging.h
;; todo: wrap callback to hide ffi stuff?
;;; mersenne.h

;;; namegen.h
(defun namegen-generate (set-name)
  ;; wrap these so we can make sure return value gets free()d or not
  ;; properly, and since we wrapped it, just assume T so it doesn't
  ;; use a shared buffer
  (let ((p (%:namegen-generate set-name t)))
    (prog1 (cffi:foreign-string-to-lisp p)
      (%::free p))))

(defun namegen-generate-custom (set-name rule)
  (let ((p (%:namegen-generate-custom set-name rule t)))
    (prog1 (cffi:foreign-string-to-lisp p)
      (%::free p))))

(defun namegen-get-sets ()
  (let ((n (%:namegen-get-nb-sets-wrapper)))
    (cffi:with-foreign-object (p :pointer n)
      (%:namegen-get-sets-wrapper p)
      (let ((r (make-array n)))
        (loop for i below n
              do (setf (aref r i) (cffi:mem-aref p :string i)))
        r))))

;;; noise.h

(declaim (inline noise-new))
(defun noise-new (dimensions &key (hurst 0.5) (lacunarity 2.0) random)
  (%:noise-new dimensions hurst lacunarity (or random (cffi:null-pointer))))

(defun noise-get-ex (noise type &rest coords)
  (cffi:with-foreign-object(f :float (length coords))
    (loop for i from 0 for c in coords
          do (setf (cffi:mem-aref f :float i) (coerce c 'single-float)))
    (%:noise-get-ex noise f type)))

(defun noise-get-fbm-ex (noise octaves type &rest coords)
  (cffi:with-foreign-object (f :float (length coords))
    (loop for i from 0 for c in coords
          do (setf (cffi:mem-aref f :float i) (coerce c 'single-float)))
    (%:noise-get-fbm-ex noise f octaves type)))

(defun noise-get-turbulence-ex (noise octaves type &rest coords)
  (cffi:with-foreign-object (f :float (length coords))
    (loop for i from 0 for c in coords
          do (setf (cffi:mem-aref f :float i) (coerce c 'single-float)))
    (%:noise-get-turbulence-ex noise f octaves type)))

(defun noise-get (noise &rest coords)
  (cffi:with-foreign-object (f :float (length coords))
    (loop for i from 0 for c in coords
          do (setf (cffi:mem-aref f :float i) (coerce c 'single-float)))
    (%:noise-get noise f)))

(defun noise-get-fbm (noise octaves &rest coords)
  (cffi:with-foreign-object (f :float (length coords))
    (loop for i from 0 for c in coords
          do (setf (cffi:mem-aref f :float i) (coerce c 'single-float)))
    (%:noise-get-fbm noise f octaves)))

(defun noise-get-turbulence (noise octaves &rest coords)
  (cffi:with-foreign-object (f :float (length coords))
    (loop for i from 0 for c in coords
          do (setf (cffi:mem-aref f :float i) (coerce c 'single-float)))
    (%:noise-get-turbulence noise f octaves)))

;; todo: noise-get-vectorized noise-get-fbm-vectorized noise-get-turbulence-vectorized

;;; parser.h - skipping
;;;

(defun path-walk (path recalculate-when-needed)
  (cffi:with-foreign-objects ((x :int) (y :int))
    (%:path-walk path x y recalculate-when-needed)
    (values (cffi:mem-ref x :int) (cffi:mem-ref y :int))))

(defun path-get (path index)
  (cffi:with-foreign-objects ((x :int) (y :int))
    (%:path-get path index x y)
    (values (cffi:mem-ref x :int) (cffi:mem-ref y :int))))

(defun path-get-origin (path index)
  (cffi:with-foreign-objects ((x :int) (y :int))
    (%:path-get-origin path x y)
    (values (cffi:mem-ref x :int) (cffi:mem-ref y :int))))

(defun path-get-destination (path index)
  (cffi:with-foreign-objects ((x :int) (y :int))
    (%:path-get-destination path x y)
    (values (cffi:mem-ref x :int) (cffi:mem-ref y :int))))

(defun dijkstra-path-walk (dijkstra)
  (cffi:with-foreign-objects ((x :int) (y :int))
    (%:dijkstra-path-walk dijkstra x y)
    (values (cffi:mem-ref x :int) (cffi:mem-ref y :int))))

(defun dijkstra-get (dijkstra index)
  (cffi:with-foreign-objects ((x :int) (y :int))
    (%:dijkstra-get dijkstra index x y)
    (values (cffi:mem-ref x :int) (cffi:mem-ref y :int))))

;;; pathfinder.h

;;; tileset.h

(defun tileset-load (filename columns rows charmap)
  (cffi:with-foreign-array (p charmap `(:array :int ,(length charmap)))
    (%:tileset-load filename columns rows (length charmap) p)))

(defun tileset-load-mem (buffer columns rows charmap)
  ;; load a png from a cl octet vector, use %:tileset-load-mem
  ;; directly to load from a pointer
  (let ((lb (length buffer))
        (lc (length charmap)))
    (cffi:with-foreign-array (pc charmap `(:array :int ,lc))
      (cffi:with-foreign-array (pb buffer `(:array :uint8 ,lb))
        (%:tileset-load-mem lb pb columns rows lc pc)))))

(defun tileset-load-raw (buffer width height columns rows charmap)
  ;; load a raw image from a cl octet vector, use %:tileset-load-raw
  ;; directly to load from a pointer
  (let ((lb (length buffer))
        (lc (length charmap)))
    (assert (= lb (* width height)))
    (cffi:with-foreign-array (pc charmap `(:array :int ,lc))
      (cffi:with-foreign-array (pb buffer `(:array :uint32 ,lb))
        (%:tileset-load-raw width height pb columns rows lc pc)))))

(defun tileset-load-raw-2d (buffer columns rows charmap)
  ;; load a raw image from a 2d cl array, use %:tileset-load-raw
  ;; directly to load from a pointer
  (let ((lb (array-dimensions buffer))
        (lc (length charmap)))
    (cffi:with-foreign-array (pc charmap `(:array :int ,lc))
      (cffi:with-foreign-array (pb buffer `(:array :uint32 ,@lb))
        (%:tileset-load-raw (second lb) (first lb) pb columns rows lc pc)))))

;; todo: 2d array version
(defun tileset-get-tile (tileset char)
  (unless (cffi:null-pointer-p tileset)
    (let* ((l (cffi:foreign-slot-value tileset '(:struct %::tileset)
                                       '%::tile-length))
           (p (%:tileset-get-tile tileset (if (characterp char)
                                              (char-code char)
                                              char))))
      (when (and p (not (cffi:null-pointer-p p)))
        (loop with a = (make-array l :element-type '(unsigned-byte 32))
              for i below l
              do (setf (aref a i) (cffi:mem-aref p :uint32 i))
              finally (return a))))))

;;; tileset_bdf.h

;; todo tileset-load-bdf-memory

;;; txtfield.h

;; todo: figure out if txtfield can be made usable? seems to require
;; passing a struct by value to interact with it, and the struct is
;; from the pre-sdl event stuff?
