#++
(ql:quickload 'cffi)
#++
(push "d:/dl/libtcod-1.24.0-x86_64-mingw/" cffi:*foreign-library-directories*)
#++
(ql:quickload '(3b-tcod/sample))
#++
(ql:quickload '(3b-tcod/sdl2))

;; based on libtcod c sample
(defpackage #:3b-tcod/sample
  (:use :cl)
  (:local-nicknames (#:a #:alexandria-2)
                    (#:sdl #:sdl2)
                    (#:tcod #:3b-tcod)
                    (#:%tcod #:%3b-tcod)))

(in-package #:3b-tcod/sample)

(defvar *libtcod-data* "d:/dl/libtcod-1.24.0-x86_64-mingw/data/")

(defun tcod-data (f)
  (format nil "~a~a" *libtcod-data* f))

(defvar +sample-screen-width+ 46)
(defvar +sample-screen-height+ 20)

(defvar +sample-screen-x+ 20)
(defvar +sample-screen-y+ 10)

;; cl-sdl2 returns a keyword for type if we use (+ 0 sdl2-ffi:+sdl-userevent+)
(defvar +on-enter-userevent+ (+ 1 sdl2-ffi:+sdl-userevent+))
(defvar +on-draw-userevent+ (+ 2 sdl2-ffi:+sdl-userevent+))

(defun enter-event-p (event)
  (eql (sdl2:get-event-type event) +on-enter-userevent+))
(defun draw-event-p (event)
  (eql (sdl2:get-event-type event) +on-draw-userevent+))

(defvar *delta-time* 0.0)
(defvar +delta-samples-length+ 64)
(defvar *delta-samples* (make-array +delta-samples-length+
                                    :initial-element 0.0))
(defvar *last-delta-sample* 0)

(defun fatal (fmt &rest r)
  (apply #'error *error-output* fmt r))

;;* ***************************
;;* samples rendering functions
;;* ***************************

(defvar *g-context*)
(defvar *sample-console*)

;;* ***************************
;;* true colors sample
;;* ***************************

(defun noise-sample-u8 (noise &rest f)
  (floor (* (+ 1 (apply #'tcod:noise-get noise f))
            0.5 255.5)))

(defvar *tcs-noise* nil)
(defun render-colors (event)
  (unless *tcs-noise*
    ;; original uses lacunarity 0, which errors with
    ;; divide-by-zero. default of 2.0 from cl-tcod gets fp overflow :/
    (setf *tcs-noise* (tcod:noise-new 2 :hurst 0.0 :lacunarity 1.0)))
  (when (enter-event-p event)
    (tcod:console-clear *sample-console*))
  (let* ((tt (* 0.001 (sdl2:get-ticks)))
         (noise *tcs-noise*)
         ;; Generate color corners from noise samples.
         (colors (flet ((c (a)
                          (logior (ash (noise-sample-u8 noise tt (+ a 0)) 16)
                                  (ash (noise-sample-u8 noise tt (+ a 1)) 8)
                                  (ash (noise-sample-u8 noise tt (+ a 2)) 0))))
                   ;; topleft=0, topright=1, bottomleft=2, bottomright=3
                   `#(,(c 0)
                      ,(c 10)
                      ,(c 20)
                      ,(c 30)))))

    ;; ==== scan the whole screen, interpolating corner colors ====
    (loop for y below +sample-screen-height+
          for y-coef = (float (/ y (1- +sample-screen-height+)))
          for left = (tcod:color-lerp (aref colors 0) (aref colors 2) y-coef)
          for right = (tcod:color-lerp (aref colors 1) (aref colors 3) y-coef)
          do (loop for x below +sample-screen-width+
                   for x-coef = (float (/ x (1- +sample-screen-width+)))
                   for cur-color = (tcod:color-lerp left right x-coef)
                   do (tcod:console-set-char-background
                       *sample-console* x y cur-color :set)))

    ;; ==== print the text ====
    ;; get the background color at the text position
    (multiple-value-bind (r b g)
        (tcod:decompose-color
         (tcod:console-get-char-background *sample-console*
                                           (floor +sample-screen-width+ 2) 5))
      ;; and invert it
      (setf r (- 255 r)
            g (- 255 g)
            b (- 255 b))
      (tcod:console-set-default-foreground *sample-console*
                                           (tcod:color-rgb r g b))
      ;; put random text (for performance tests)
      (loop for y below +sample-screen-height+
            do (loop for x below +sample-screen-width+
                     for col = (tcod:console-get-char-background
                                *sample-console* x y)
                     for c = (+ (char-code #\a) (random 26))
                     do (setf col (tcod:color-lerp col (tcod:color :black) 0.5))
                        (tcod:console-set-default-foreground
                         *sample-console* col)
                        (tcod:console-put-char *sample-console* x y c :none)))
      ;; the background behind the text is slightly darkened using the
      ;; BKGND_MULTIPLY flag
      (tcod:console-set-default-background *sample-console* (tcod:color :grey))
      (tcod:console-printf-rect-ex
       *sample-console*
       (floor +sample-screen-width+ 2) 5
       (- +sample-screen-width+ 2) (- +sample-screen-width+ 1)
       :multiply
       :center
       "The Doryen library uses 24 bits colors, for both background and foreground."))))

;;* ***************************
;;* offscreen console sample
;;* ***************************

(defvar *ro-static* (make-hash-table))
(defmacro with-static (vars hash &body body)
  `(symbol-macrolet ,(loop for binding in vars
                           for var = (if (consp binding)
                                         (car binding)
                                         binding)
                           for init = (if (consp binding)
                                          (cadr binding)
                                          nil)
                           collect
                           `(,var (gethash ',var ,hash ,init)))
     ,@body))

(defun render-offscreen (event)
  (with-static ((secondary nil)
                (screenshot nil)
                (init nil)
                (counter 0)
                (x 0)
                (y 0)
                (x-dir 1)
                (y-dir 1))
      *ro-static*
    (let* ((w +sample-screen-width+)
           (h +sample-screen-height+)
           (w/2 (floor w 2))
           (h/2 (floor h 2))
           (w/4 (floor w 4)))
      (unless init
        (setf init t)
        (setf secondary (tcod:console-new w/2 h/2))
        (setf screenshot (tcod:console-new w h))
        (tcod:console-print-double-frame secondary 0 0 w/2 h/2 nil :set
                                         "Offscreen console")
        (tcod:console-printf-rect-ex
         secondary
         w/4 2 (- w/2 2) h/2
         :none :center
         "You can render to an offscreen console and blit in on another one, simulating alpha transparency."))
      (when (enter-event-p event)
        ;; get a "screenshot" of the current sample screen
        (setf counter (sdl2:get-ticks))
        (tcod:console-blit *sample-console* 0 0 w h screenshot 0 0 1.0 1.0))
      (when ;;(> (sdl2:get-ticks) (+ 1000 counter)) ;; once every second.
          (> (sdl2:get-ticks) (+ 330 counter)) ;; once every second.
        (setf counter (sdl2:get-ticks))
        (incf x x-dir)
        (incf y y-dir)
        (cond
          ((= x (+ w/2 5)) (setf x-dir -1))
          ((= x -5) (setf x-dir 1)))
        (cond
          ((= y (+ h/2 5)) (setf y-dir -1))
          ((= y -5) (setf y-dir 1))))
      ;; restore the initial screen
      (tcod:console-blit screenshot 0 0 w h *sample-console* 0 0 1.0 1.0)
      ;; blit the overlapping screen
      (tcod:console-blit secondary 0 0 w/2 h/2 *sample-console* x y 1.0 1.0))))

;;* ***************************
;;* line drawing sample
;;* ***************************

(defvar *rl-static* (make-hash-table))
(defvar *bk-flags* #(:NONE :SET :MULTIPLY :LIGHTEN :DARKEN :SCREEN
                     :COLOR-DODGE :COLOR-BURN
                     :ADD :ADDA :BURN :OVERLAY :ALPH))

(cffi:defcallback line-listener :boolean
    ((x :int) (y :int))
  (with-static ((bk-flag 1))
      *rl-static*
    (when (and (< -1 x +sample-screen-width+)
               (< -1 y +sample-screen-height+))
      (tcod:console-set-char-background *sample-console* x y
                                        (tcod:color :light-blue)
                                        bk-flag)))
  t)

(defun render-lines (event)
  (with-static ((bk nil)
                (init nil)
                (bk-flag 1))
      *rl-static*
    (tcod:sdl2-event-case event
      (:keydown
       (:keysym key)
       (tcod:sdl2-scancode-case key
         ((:scancode-return
           :scancode-return2
           :scancode-kp-enter)
          (unless (sdl2:mod-value-p (sdl2:mod-value key) :alt)
            (setf bk-flag (mod (1+ (ldb (byte 8 0) bk-flag)) (length *bk-flags*))))))))
    (let* ((alpha 0.0)
           (current-time (/ (sdl2:get-ticks) 1000.0))
           (w +sample-screen-width+)
           (h +sample-screen-height+)
           (w/2 (floor w 2))
           (h/2 (floor h 2)))
      (case (aref *bk-flags* (ldb (byte 8 0) bk-flag))
        (:alph
         ;; for the alpha mode, update alpha every frame
         (setf alpha (/ (+ 1 (cos (* current-time 2))) 2))
         (setf bk-flag (tcod:background-alpha alpha)))
        (:adda
         ;; for the add alpha mode, update alpha every frame
         (setf alpha (/ (+ 1 (cos (* current-time 2))) 2))
         (setf bk-flag (tcod:background-add-alpha alpha))))
      (unless init
        (setf bk (tcod:console-new w h))
        (loop for y below h
              do (loop for x below w
                       for r = (floor (* x 255) (1- w))
                       for g = (floor (* (+ x y) 255) (- (+ w h) 2))
                       for b = (floor (* y 255) (1- h))
                       do (tcod:console-set-char-background
                           bk x y (tcod:compose-color r g b) :set)))
        (setf init t))
      (when (enter-event-p event)
        (tcod:console-set-default-foreground *sample-console* (tcod:color :white)))
      ;; blit the background
      (tcod:console-blit bk 0 0 w h *sample-console* 0 0 1.0 1.0)
      ;; render the gradient
      ;; gradient vertical position
      (flet ((bg@ (x y r g b flag)
               (tcod:console-set-char-background *sample-console*
                                                 x y
                                                 (tcod:compose-color r g b)
                                                 flag)))
        (loop with rect-y = (round (* (- h 3) (/ (+ 1 (cos current-time)) 2)))
              with flag = bk-flag
              for x below w
              for c = (floor (* x 255) w)
              do (bg@ x rect-y c c c flag)
                 (bg@ x (+ rect-y 1) c c c flag)
                 (bg@ x (+ rect-y 2) c c c flag)))
      ;; calculate the segment ends
      (let* ((angle (* current-time 2.0)) ;; segment angle data
             ;; segment starting and ending positions
             (xo (floor (* w/2 (+ 1 (cos angle)))))
             (yo (floor (+ h/2 (* w/2 (sin angle)))))
             (xd (floor (* w/2 (- 1 (cos angle)))))
             (yd (floor (- h/2 (* w/2 (sin angle))))))
        ;; render the line
        (tcod:line xo yo xd yd (cffi:callback line-listener))
        ;; print the current flag
        (tcod:console-printf-ex *sample-console*
                                w/2 3 :set :center
                                "~s (ENTER to change)"
                                (aref *bk-flags* (ldb (byte 8 0) bk-flag)))))))

;;* ***************************
;;* noise sample
;;* ***************************

(defvar *rn-static* (make-hash-table))

(defun render-noise (event)
  (let* ((func-names (a:plist-alist
                      '(:perlin "1 : perlin noise       "
                        :simplex "2 : simplex noise      "
                        :wavelet "3 : wavelet noise      "
                        :fbm-perlin "4 : perlin fbm         "
                        :turbulence-perlin "5 : perlin turbulence  "
                        :fbm-simplex "6 : simplex fbm        "
                        :turbulence-simplex "7 : simplex turbulence "
                        :fbm-wavelet "8 : wavelet fbm        "
                        :turbulence-wavelet "9 : wavelet turbulence ")))
         (w +sample-screen-width+)
         (h +sample-screen-height+)
         (2w (* 2 w))
         (2h (* 2 h))
         ;; texture animation
         (dx (* (sdl:get-ticks) 0.0005))
         (dy dx))
    (with-static ((func :perlin)
                  (noise nil)
                  (octaves 4.0)
                  (hurst %tcod::+noise-default-hurst+)
                  (lacunarity %tcod::+noise-default-lacunarity+)
                  (img nil)
                  (zoom 3.0))
        *rn-static*
      (unless noise
        (setf noise (tcod:noise-new 2 :hurst hurst :lacunarity lacunarity))
        (setf img (tcod:image-new 2w 2h)))
      (tcod:console-clear *sample-console*)

      ;; render the 2d noise function
      (loop
        for y below 2h
        for sy = (+ (/ (* zoom y) 2h) dy)
        do (loop
             for x below 2w
             for sx = (+ (/ (* zoom x) 2w) dx)
             for value
               = (ecase func
                   (:perlin
                    (tcod:noise-get-ex noise :perlin sx sy))
                   (:simplex
                    (tcod:noise-get-ex noise :simplex sx sy))
                   (:wavelet
                    (tcod:noise-get-ex noise :wavelet sx sy))
                   (:fbm-perlin
                    (tcod:noise-get-fbm-ex noise octaves :perlin sx sy))
                   (:fbm-simplex
                    (tcod:noise-get-fbm-ex noise octaves :simplex sx sy))
                   (:fbm-wavelet
                    (tcod:noise-get-fbm-ex noise octaves :wavelet sx sy))
                   (:turbulence-perlin
                    (tcod:noise-get-turbulence-ex noise octaves
                                                  :perlin sx sy))
                   (:turbulence-simplex
                    (tcod:noise-get-turbulence-ex noise octaves
                                                  :simplex sx sy))
                   (:turbulence-wavelet
                    (tcod:noise-get-turbulence-ex noise octaves
                                                  :wavelet sx sy)))
             for c = (floor (* (/ (1+ value) 2) 255))
             ;; use a bluish color
             for col = (tcod:compose-color (floor c 2) (floor c 2) c)
             do (tcod:image-put-pixel img x y col)))
      ;; blit the noise image with subcell resolution
      (tcod:image-blit-2x img *sample-console* 0 0 0 0 -1 -1)
      ;; draw a transparent rectangle
      (tcod:console-set-default-background *sample-console* (tcod:color :grey))
      (let* ((simple (member func '(:perlin :simplex :wavelet)))
             (rh (if simple 10 13)))
        (tcod:console-rect *sample-console* 2 2 23 rh nil :multiply)
        (loop
          for y from 2 below (+ 2 rh)
          do (loop
               for x from 2 below (+ 2 23)
               for col = (tcod:console-get-char-foreground *sample-console* x y)
               do (setf col (tcod:color-multiply col (tcod:color :grey)))
                  (tcod:console-set-char-foreground *sample-console* x y col)))
        ;; draw the text
        (loop
          for (curfunc . text) in func-names
          for y from 2
          if (eql curfunc func)
            do (tcod:console-set-default-foreground *sample-console*
                                                    (tcod:color :white))
               (tcod:console-set-default-background *sample-console*
                                                    (tcod:color :light-blue))
               (tcod:console-printf-ex *sample-console*
                                       2 y :set :left "~a" text)
          else
            do (tcod:console-set-default-foreground *sample-console*
                                                    (tcod:color :grey))
               (tcod:console-printf *sample-console* 2 y "~a" text))
        ;; draw parameters
        (tcod:console-set-default-foreground *sample-console* (tcod:color :white))
        (tcod:console-set-alignment *sample-console* :left)
        (tcod:console-print *sample-console* 2 11 "Y/H : zoom (~3,1f)" zoom)
        (unless simple
          (tcod:console-print *sample-console* 2 12 "E/D : hurst (~3,1f)" hurst)
          (tcod:console-print *sample-console* 2 13 "R/F : lacunarity (~3,1f)" lacunarity)
          (tcod:console-print *sample-console* 2 14 "T/G : octaves (~3,1f)" octaves))
        ;; handle keypress
        (macrolet ((update-noise (var delta)
                     `(progn
                        (incf ,var ,delta)
                        (tcod:noise-delete noise)
                        (setf noise (tcod:noise-new 2 :hurst hurst
                                                      :lacunarity lacunarity)))))
          (tcod::sdl2-event-case event
            (:keydown
             (:keysym key)
             (tcod::sdl2-scancode-case key
               (:scancode-e ;; increase hurst
                (update-noise hurst 0.1))
               (:scancode-d ;; decrease hurst
                (update-noise hurst -0.1))
               (:scancode-r ;; increase lacunarity
                (update-noise lacunarity 0.5))
               (:scancode-f ;; decrease lacunarity
                (update-noise lacunarity -0.5))
               (:scancode-t ;; increase octaves
                (update-noise octaves 0.5))
               (:scancode-g ;; decrease octaves
                (update-noise octaves -0.5))
               (:scancode-y ;; increase zoom
                (update-noise zoom 0.5))
               (:scancode-h ;; decrease zoom
                (update-noise zoom -0.5))
               ;; change the noise function
               ((:scancode-1 :scancode-kp-0)
                (setf func :perlin))
               ((:scancode-2 :scancode-kp-2)
                (setf func :simplex))
               ((:scancode-3 :scancode-kp-3)
                (setf func :wavelet))
               ((:scancode-4 :scancode-kp-4)
                (setf func :fbm-perlin))
               ((:scancode-5 :scancode-kp-5)
                (setf func :turbulence-perlin))
               ((:scancode-6 :scancode-kp-6)
                (setf func :fbm-simplex))
               ((:scancode-7 :scancode-kp-7)
                (setf func :turbulence-simplex))
               ((:scancode-8 :scancode-kp-8)
                (setf func :fbm-wavelet))
               ((:scancode-9 :scancode-kp-9)
                (setf func :turbulence-wavelet)))))))
      nil)))

;;* ***************************
;;* fov sample
;;* ***************************

(defvar *rf-static* (make-hash-table))
(defvar *sample-map* #("##############################################"
                       "#######################      #################"
                       "#####################    #     ###############"
                       "######################  ###        ###########"
                       "##################      #####             ####"
                       "################       ########    ###### ####"
                       "###############      #################### ####"
                       "################    ######                  ##"
                       "########   #######  ######   #     #     #  ##"
                       "########   ######      ###                  ##"
                       "########                                    ##"
                       "####       ######      ###   #     #     #  ##"
                       "#### ###   ########## ####                  ##"
                       "#### ###   ##########   ###########=##########"
                       "#### ##################   #####          #####"
                       "#### ###             #### #####          #####"
                       "####           #     ####                #####"
                       "########       #     #### #####          #####"
                       "########       #####      ####################"
                       "##############################################"))

(defvar *char-window* #x2550) ;; "═" glyph.
(defvar *torch-radius* 10.0)
(defvar *squared-torch-radius* (expt *torch-radius* 2))

(defun render-fov (event)
  (with-static ((px 20) ;; player position
                (py 20)
                (recompute-fov t)
                (torch t)
                (light-walls t)
                (map nil)
                (dark-wall (tcod:compose-color 0 0 100))
                (dark-ground (tcod:compose-color 50 50 150))
                (light-ground (tcod:compose-color 200 180 50))
                (light-wall (tcod:compose-color 130 110 50))
                (noise nil)
                (algonum 0)
                (torch-x 0.0) ;; torch light position in the perlin noise
                ;; torch position and intensity variation
                (dx 0.0)
                (dy 0.0)
                (di 0.0))
      *rf-static*
    (let ((w +sample-screen-width+)
          (h +sample-screen-height+)
          (algorithms #(:BASIC
                        :DIAMOND
                        :SHADOW
                        :PERMISSIVE-0
                        :PERMISSIVE-1
                        :PERMISSIVE-2
                        :PERMISSIVE-3
                        :PERMISSIVE-4
                        :PERMISSIVE-5
                        :PERMISSIVE-6
                        :PERMISSIVE-7
                        :PERMISSIVE-8
                        :RESTRICTIVE))
          (algo-names #("BASIC               "
                        "DIAMOND             "
                        "SHADOW              "
                        "PERMISSIVE0         "
                        "PERMISSIVE1         "
                        "PERMISSIVE2         "
                        "PERMISSIVE3         "
                        "PERMISSIVE4         "
                        "PERMISSIVE5         "
                        "PERMISSIVE6         "
                        "PERMISSIVE7         "
                        "PERMISSIVE8         "
                        "RESTRICTIVE         "
                        "SYMMETRIC_SHADOWCAST")))
      (labels ((fg (c)
                 (tcod:console-set-default-foreground *sample-console*
                                                      (tcod:color c)))
               (bg (c &optional x y (flag :set))
                 (if y
                     (tcod:console-set-char-background *sample-console*
                                                       x y
                                                       (tcod:color c)
                                                       flag)
                     (tcod:console-set-default-background *sample-console*
                                                          (tcod:color c))))
               (c (x y c &optional (flag :none))
                 (tcod:console-put-char *sample-console* x y
                                        (if (characterp c)
                                            (char-code c)
                                            c)
                                        flag))
               (m (x y)
                 (when (and (array-in-bounds-p *sample-map* y)
                            (array-in-bounds-p (aref *sample-map* y) x))
                   (aref (aref *sample-map* y) x)))
               (map= (x y c)
                 (eql (m x y) c))
               (redraw-fg ()
                 (fg :white)
                 (tcod:console-print *sample-console* 1 0
                                     "IJKL : move around~%T : torch fx ~a~%W : light walls ~a~%+-: algo ~a"
                                     (if torch "on" "off")
                                     (if light-walls "on" "off")
                                     (aref algo-names algonum))
                 (fg :black)
                 (c px py #\@)
                 ;; draw windows
                 (loop for y below h
                       do (loop for x below w
                                when (map= x y #\=)
                                  do (c x y *char-window*)))))
        (unless map
          (setf map (tcod:map-new w h))
          (loop for y below h
                do (loop for x below w
                         do (case (m x y)
                              (#\space ;; ground
                               (tcod:map-set-properties map x y t t)
                               ;; make sure player starts in ground
                               (unless (map= px py #\space)
                                 (setf px x py y)))
                              (#\= ;; window
                               (tcod:map-set-properties map x y t nil)))))
          (setf noise (tcod:noise-new 1 :hurst 1.0 :lacunarity 1.0)))
        (when (enter-event-p event)
          ;; we draw the foreground only the first time. During the player
          ;; movement, only the @ is redrawn. The rest impacts only the
          ;; background color.

          ;; draw the help text & player @
          (bg :dark-slate-grey)
          (tcod:console-clear *sample-console*)
          (redraw-fg))
        (when recompute-fov
          (setf recompute-fov nil)
          (tcod:map-compute-fov map px py
                                (if torch (floor *torch-radius*) 0)
                                light-walls
                                (aref algorithms algonum)))
        (when torch
          ;; slightly change the perlin noise parameter
          (incf torch-x 0.2)
          ;; randomize the light position between ±1.5
          (let ((tdx (+ torch-x 20)))
            (setf dx (* 1.5 (tcod:noise-get noise tdx)))
            (incf tdx 30)
            (setf dy (* 1.5 (tcod:noise-get noise tdx)))
            (setf di (* 0.2 (tcod:noise-get noise torch-x)))))
        (loop for y below h
              do (loop for x below w
                       for visible = (tcod:map-is-in-fov map x y)
                       for wall = (map= x y #\#)
                       do (cond
                            ((not visible)
                             (bg (if wall dark-wall dark-ground) x y))
                            ((not torch)
                             (bg (if wall light-wall light-ground) x y))
                            (t
                             (let ((base (if wall dark-wall dark-ground))
                                   (light (if wall light-wall light-ground))
                                   (r² (+ (expt (- x (+ px dx)) 2)
                                          (expt (- y (+ py dy)) 2))))
                               (when (< r² *squared-torch-radius*)
                                 (let ((l (+ (/ (- *squared-torch-radius* r²)
                                                *squared-torch-radius*)
                                             di)))
                                   (setf base
                                         (tcod:color-lerp base light
                                                          (float (a:clamp l 0 1))))))
                               (bg base x y))))))
        (flet ((move (dx dy)
                 (let ((x2 (+ px dx))
                       (y2 (+ py dy)))
                   (when (map= x2 y2 #\space)
                     (c px py #\space)
                     (c x2 y2 #\@)
                     (setf px x2 py y2)
                     (setf recompute-fov t))))
               (algo (d)
                 (setf algonum (mod (+ algonum d) (length algorithms)))
                 (redraw-fg)
                 (fg :black)
                 (setf recompute-fov t)))
          (tcod::sdl2-event-case event
            (:keydown
             (:keysym key)
             (tcod::sdl2-scancode-case key
               (:scancode-i (move 0 -1))
               (:scancode-k (move 0 +1))
               (:scancode-j (move -1 0))
               (:scancode-l (move +1 0))
               (:scancode-t
                (setf torch (not torch))
                (redraw-fg)
                (fg :black))
               (:scancode-w
                (setf light-walls (not light-walls))
                (redraw-fg)
                (fg :black)
                (setf recompute-fov t))
               ((:scancode-equals :scancode-kp-plus)
                (algo -1))
               ((:scancode-minus :scancode-kp-minus)
                (algo +1))))))))))

;;* ***************************
;;* image sample
;;* ***************************

(defvar *ri-static* (make-hash-table))

(defun render-image (event)
  (with-static ((img nil)
                (circle nil)
                (blue (tcod:compose-color 0 0 255))
                (green (tcod:compose-color 0 255 0)))
      *ri-static*
    (sdl2:show-window (tcod:context-get-sdl-window *g-context*))
    (unless img
      (setf img (tcod:image-load (tcod-data "img/skull.png")))
      (setf circle (tcod:image-load (tcod-data "img/circle.png"))))
    (flet ((bg (c)
             (tcod:console-set-default-background *sample-console*
                                                  (tcod:color c))))
      (when (draw-event-p event)
        (bg :black)
        (tcod:console-clear *sample-console*)
        (let* ((current-time (/ (sdl:get-ticks) 1000.0))
               (x (+ (/ +sample-screen-width+ 2.0)
                     (* 10 (cos current-time))))
               (y (+ (/ +sample-screen-height+ 2.0)))
               (scale-x (+ 0.2
                           (* 1.8 (/ (+ 1 (cos (/ current-time 2)))
                                     2))))
               (scale-y scale-x)
               (angle current-time))
          (cond
            ;; split the color channels of circle.png
            ((oddp (floor (sdl2:get-ticks) 2000))
             ;; the red channel
             (bg :red)
             (tcod:console-rect *sample-console* 0 3 15 15 nil :set)
             (tcod:image-blit-rect circle *sample-console* 0 3 -1 -1 :multiply)
             ;; the green channel
             (bg :green)
             (tcod:console-rect *sample-console* 15 3 15 15 nil :set)
             (tcod:image-blit-rect circle *sample-console* 15 3 -1 -1 :multiply)
             ;; the blue channel
             (bg :blue)
             (tcod:console-rect *sample-console* 30 3 15 15 nil :set)
             (tcod:image-blit-rect circle *sample-console* 30 3 -1 -1 :multiply))
            (t
             ;; render circle.png with normal blitting
             (tcod:image-blit-rect circle *sample-console* 0 3 -1 -1 :set)
             (tcod:image-blit-rect circle *sample-console* 15 3 -1 -1 :set)
             (tcod:image-blit-rect circle *sample-console* 30 3 -1 -1 :set)))
          (tcod:image-blit img *sample-console* x y :set scale-x scale-y
                           (coerce angle 'single-float)))))))

;;* ***************************
;;* mouse sample
;;* ***************************
(defvar *rm-static* (make-hash-table))

(defun render-mouse (event)
  (with-static ((mouse nil)
                (tile-motion-x 0)
                (tile-motion-y 0)
                (old-x 0)
                (old-y 0)
                (wheel-x 0.0)
                (wheel-y 0.0))
      *rm-static*
    (multiple-value-bind (pixel-x pixel-y mouse-state)
        (sdl2:mouse-state)
      (multiple-value-bind (tile-x tile-y)
          (tcod:context-screen-pixel-to-tile-i *g-context* pixel-x pixel-y)
        (let* ((console-tile-x (- tile-x +sample-screen-x+))
               (console-tile-y (- tile-y +sample-screen-y+))
               (in-console (and (< -1 console-tile-x +sample-screen-width+)
                                (< -1 console-tile-y +sample-screen-height+))))
          (when (enter-event-p event)
            (tcod:console-set-default-background *sample-console*
                                                 (tcod:color :grey))
            (tcod:console-set-default-foreground *sample-console*
                                                 (tcod:color :light-yellow))
            (sdl2:warp-mouse-in-window nil 320 200)
            (sdl2:show-cursor))
          (tcod::sdl2-event-case event
            (:keydown
             (:keysym k)
             (tcod::sdl2-scancode-case k
               ((:scancode-1 :scancode-kp-1)
                (sdl2:hide-cursor))
               ((:scancode-2 :scancode-kp-2)
                (sdl2:show-cursor))))
            (:mousemotion
             (:xrel dx :yrel dy)
             (setf tile-motion-x dx
                   tile-motion-y dy))
            (:mousewheel
             (:precise-x x :precise-y y)
             (incf wheel-x x)
             (incf wheel-y y)))
          (tcod:console-clear *sample-console*)
          (tcod:console-print *sample-console*
                              1 1
                              "Pixel position : ~4d,~4d~%~
                               Tile position  : ~4d,~4d ~@[(~{~4d,~4d~})~]~%~
                               Tile movement  : ~4d,~4d~%~
                               Wheel          :~5,f,~5f~%~
                               Buttons        : ~{~a~}~%~
                               Left Button    : ~a"
                              pixel-x
                              pixel-y
                              tile-x
                              tile-y
                              (when in-console
                                (list console-tile-x
                                      console-tile-y))
                              tile-motion-x
                              tile-motion-y
                              wheel-x
                              wheel-y
                              (loop with bn = '("L" "M" "R")
                                    for b below 32
                                    for n = (or (pop bn) b)
                                    collect (if (logbitp b mouse-state)
                                                (format nil "~32r" n)
                                                "."))
                              (if (logtest mouse-state sdl2-ffi:+sdl-button-lmask+)
                                  "ON" "OFF"))
          (tcod:console-print *sample-console* 1 10 "1: Hide cursor~%2: Show cursor")

          (when in-console
            (tcod:console-set-char-background *sample-console*
                                              console-tile-x console-tile-y
                                              (tcod:color :dark-red) :set)))))))

;;* ***************************
;;* path sample
;;* ***************************
(defvar *rp-static* (make-hash-table))
(defun render-path (event)
  (with-static ((px 10) ;; player position
                (py 10)
                (dx 10) ;; destination
                (dy 1)
                (map nil)
                (dark-wall (tcod:compose-color 0 0 100))
                (dark-ground (tcod:compose-color 50 50 150))
                (light-ground (tcod:compose-color 200 180 50))
                (path nil)
                (using-a* t)
                (dijkstra-dist 1.0)
                (dijkstra nil)
                (recalculate-path nil)
                (busy 0.0)
                (old-char #\space))
      *rp-static*
    (let ((w +sample-screen-width+)
          (h +sample-screen-height+))
      (labels ((fg (c)
                 (tcod:console-set-default-foreground *sample-console*
                                                      (tcod:color c)))
               (bg (c &optional x y (flag :set))
                 (if y
                     (tcod:console-set-char-background *sample-console*
                                                       x y
                                                       (tcod:color c)
                                                       flag)
                     (tcod:console-set-default-background *sample-console*
                                                          (tcod:color c))))
               (c (x y c &optional (flag :none))
                 (tcod:console-put-char *sample-console* x y
                                        (if (characterp c)
                                            (char-code c)
                                            c)
                                        flag))
               (m (x y)
                 (when (and (array-in-bounds-p *sample-map* y)
                            (array-in-bounds-p (aref *sample-map* y) x))
                   (aref (aref *sample-map* y) x)))
               (map= (x y c)
                 (eql (m x y) c)))
        (unless map
          ;; initialize the map
          (setf map (tcod:map-new w h))
          (loop for y below h
                do (loop for x below w
                         do (cond
                              ((map= x y #\space) ;; ground
                               (tcod:map-set-properties map x y t t))
                              ((map= x y #\=) ;; window
                               (tcod:map-set-properties map x y t nil)))))
          (setf path (tcod:path-new-using-map map (sqrt 2)))
          (setf dijkstra (tcod:dijkstra-new map (sqrt 2))))
        (when (enter-event-p event)
          ;; we draw the foreground only the first time. During the player
          ;; movement, only the @ is redrawn. The rest impacts only the
          ;; background color.

          ;; draw the help text & player @
          (bg :dark-slate-grey)
          (tcod:console-clear *sample-console*)
          (fg :white)
          (c dx dy #\+)
          (c px py #\@)
          (tcod:console-print *sample-console* 1 1 "IJKL : move around~% ~
                                                      TAB : A*/dijkstra")
          (tcod:console-print *sample-console* 1 4 "Using : A*")
          ;; draw windows
          (loop for y below h
                do (loop for x below w
                         when (map= x y #\=)
                           do (c x y *char-window*)))
          (setf recalculate-path t))

        (when recalculate-path
          (cond
            (using-a*
             (tcod:path-compute path px py dx dy))
            (t
             (setf dijkstra-dist 1.0)
             ;; compute the distance grid
             (tcod:dijkstra-compute dijkstra px py)
             ;; get the maximum distance (needed for ground shading only)
             (loop for y below h
                   do (loop for x below w
                            for d = (tcod:dijkstra-get-distance dijkstra x y)
                            when (> d dijkstra-dist)
                              do (setf dijkstra-dist d)))
             ;; compute the path
             (tcod:dijkstra-path-set dijkstra dx dy)))
          (setf recalculate-path nil)
          (unless (plusp busy)
            (setf busy 0.2)))

        ;; draw the dungeon
        (loop for y below h
              do (loop for x below w
                       for wall = (map= x y #\#)
                       do (bg (if wall dark-wall dark-ground) x y)))

        ;; draw the path
        (cond
          (using-a*
           (loop for i below (tcod:path-size path)
                 for (x y) = (multiple-value-list (tcod:path-get path i))
                 do (bg light-ground x y)))
          (t
           (loop for y below h
                 do (loop for x below w
                          for wall = (map= x y #\#)
                          for d = (tcod:dijkstra-get-distance dijkstra x y)
                          unless wall
                            do (bg (tcod:color-lerp light-ground
                                                    dark-ground
                                                    (* 0.9
                                                       (/ d dijkstra-dist)))
                                   x y)))
           (loop for i below (tcod:dijkstra-size dijkstra)
                 for (x y) = (multiple-value-list
                              (tcod:dijkstra-get dijkstra i))
                 do (bg light-ground x y))))

        ;; move the creature
        (decf busy *delta-time*)
        (unless (plusp busy)
          (setf busy 0.2)
          (cond
            (using-a*
             (unless (tcod:path-is-empty path)
               (c px py #\space)
               (setf (values px py) (tcod:path-walk path t))
               (c px py #\@)))
            (t
             (unless (tcod:dijkstra-is-empty dijkstra)
               (c px py #\space)
               (setf (values px py) (tcod:dijkstra-path-walk dijkstra))
               (c px py #\@)
               (setf recalculate-path t)))))
        (flet ((move (x2 y2)
                 (c dx dy old-char)
                 (setf dx (a:clamp x2 0 (1- w))
                       dy (a:clamp y2 0 (1- h)))
                 (setf old-char
                       (tcod:console-get-char *sample-console* dx dy))
                 (c dx dy #\+)
                 (when (map= dx dy #\space)
                   (setf recalculate-path t))))
          (tcod::sdl2-event-case event
            (:keydown
             (:keysym key)
             (tcod::sdl2-scancode-case key
               ((:scancode-tab :scancode-kp-tab)
                (setf using-a* (not using-a*))
                (if using-a*
                    (tcod:console-print *sample-console* 1 4 "Using : A*")
                    (tcod:console-print *sample-console* 1 4 "Using : Dijkstra")))
               (:scancode-i (move dx (1- dy)))
               (:scancode-k (move dx (1+ dy)))
               (:scancode-j (move (1- dx) dy))
               (:scancode-l (move (1+ dx) dy))))
            (:mousemotion
             (:x mx :y my)
             (multiple-value-bind (tile-x tile-y)
                 (tcod:context-screen-pixel-to-tile-i *g-context* mx my)
               (move (- tile-x +sample-screen-x+) (- tile-y +sample-screen-y+))))))))))

;;* ***************************
;;* bsp sample
;;* ***************************
(defvar *rb-static* (make-hash-table))
(defvar *bsp-depth* 8)
(defvar *min-room-size* 4)
;; a room fills a random part of the node or the maximum available space ?
(defvar *random-room* nil)
;; if true, there is always a wall on north & west side of a room
(defvar *room-walls* t)

;; draw a vertical line
(defun vline (map x y1 y2)
  (loop for y from (min y1 y2) to (max y1 y2)
        do (setf (aref map x y) #\space)))

;; draw a vertical line up until we reach an empty space
(defun vline-up (map x y)
  (loop for y from y downto 0
        until (eql (aref map x y) #\space)
        do (setf (aref map x y) #\space)))

;; draw a vertical line down until we reach an empty space
(defun vline-down (map x y)
  (loop for y from y below +sample-screen-height+
        until (eql (aref map x y) #\space)
        do (setf (aref map x y) #\space)))

;; draw a horizontal line
(defun hline (map x1 y x2)
  (loop for x from (min x1 x2) to (max x1 x2)
        do (setf (aref map x y) #\space)))

;; draw a horizontal line left until we reach an empty space
(defun hline-left (map x y)
  (loop for x from x downto 0
        until (eql (aref map x y) #\space)
        do (setf (aref map x y) #\space)))

;; draw a horizontal line right until we reach an empty space
(defun hline-right (map x y)
  (loop for x from x below +sample-screen-width+
        until (eql (aref map x y) #\space)
        do (setf (aref map x y) #\space)))

;; the class building the dungeon from the bsp nodes
(cffi:defcallback traverse-node :bool
    ((node %tcod::bsp*) (userdata %tcod::userdata))
  (declare (ignore userdata))
  #++
  (tcod::with-bsp-node node (x y w h  position level horizontal)
    (format t "bsp: ~s,~s@~sx~s, pos=~s, lev=~s, hor=~s leaf~s~%"
            x y w h position level horizontal (tcod:bsp-is-leaf? node)))
  (with-static ((map nil))
      *rb-static*
    (flet ((random-int (min max)
             (tcod:random-get-int (cffi:null-pointer) min max)))
      (cond
        ((tcod:bsp-is-leaf node)
         ;; calculate the room size
         (%tcod::with-bsp-node node (x y w h)
           (let ((min-x (1+ x))
                 (max-x (1- (min (+ x w) +sample-screen-width+)))
                 (min-y (1+ y))
                 (max-y (1- (min (+ y h) +sample-screen-height+))))
             (unless *room-walls*
               (when (> min-x 1) (decf min-x))
               (when (> min-y 1) (decf min-y)))
             (when *random-room*
               (setf min-x (random-int min-x (1+ (- max-x *min-room-size*))))
               (setf min-y (random-int min-y (1+ (- max-y *min-room-size*))))
               (setf max-x (random-int (1- (+ min-x *min-room-size*)) max-x))
               (setf max-y (random-int (1- (+ min-y *min-room-size*)) max-y)))
             ;; resize the node to fit the room
             (setf x min-x)
             (setf y min-y)
             (setf w (1+ (- max-x min-x)))
             (setf h (1+ (- max-y min-y)))
             ;; dig the room
             (loop for x from min-x to max-x
                   do (loop for y from min-y to max-y
                            do (setf (aref map x y) #\Space))))))
        (t
         ;; resize the node to fit its sons
         (let ((left (tcod:bsp-left node))
               (right (tcod:bsp-right node)))
           (%tcod::with-bsp-node node (x y w h nil nil horizontal)
             (%tcod::with-bsp-node left (lx ly lw lh)
               (%tcod::with-bsp-node right (rx ry rw rh)
                 (setf x (min lx rx))
                 (setf y (min ly ry))
                 (setf w (- (max (+ lx lw) (+ rx rw)) x))
                 (setf h (- (max (+ ly lh) (+ ry rh)) y))
                 (cond
                   (horizontal
                    ;; vertical corridor
                    (cond
                      ((or (< (+ lx lw -1) rx) (< (+ rx rw -1) lx))
                       ;; no overlapping zone, we need a Z shaped corridor
                       (let ((x1 (random-int lx (+ lx lw -1)))
                             (x2 (random-int rx (+ rx rw -1)))
                             (y (random-int (+ ly lh) ry)))
                         (vline-up map x1 (1- y))
                         (hline map x1 y x2)
                         (vline-down map x2 (1+ y))))
                      (t
                       ;; straight vertical corridor
                       (let* ((minx (max lx rx))
                              (maxx (min (+ lx lw -1) (+ rx rw -1)))
                              (x (random-int minx maxx)))
                         (vline-down map x ry)
                         (vline-up map x (1- ry))))))
                   (t
                    ;; horizontal corridor
                    (cond
                      ((or (< (+ ly lh -1) ry) (< (+ ry rh -1) ly))
                       ;; no overlapping zone, we need a Z shaped corridor
                       (let ((y1 (random-int ly (+ ly lh -1)))
                             (y2 (random-int ry (+ ry rh -1)))
                             (x (random-int (+ lx lw) rx)))
                         (hline-left map (1- x) y1)
                         (vline map x y1 y2)
                         (hline-right map (1+ x) y2)))
                      (t
                       ;; straight horizontal corridor
                       (let* ((miny (max ly ry))
                              (maxy (min (+ ly lh -1) (+ ry rh -1)))
                              (y (random-int miny maxy)))
                         (hline-left map (1- rx) y)
                         (hline-right map rx y)))))))))))))
    t))

(defun render-bsp (event)
  (with-static ((bsp nil)
                (generate t)
                (refresh nil)
                (map)
                (dark-wall (tcod:compose-color 0 0 100))
                (dark-ground (tcod:compose-color 50 50 150)))
      *rb-static*
    (unless map
      (setf map (make-array (list +sample-screen-width+
                                  +sample-screen-height+)
                            :initial-element #\#)))
    (tcod::sdl2-event-case event
      (:keydown
       (:keysym key)
       (tcod::sdl2-scancode-case key
         ((:scancode-return
           :scancode-return2
           :scancode-kp-enter)
          (unless (sdl2:mod-value-p (sdl2:mod-value key) :alt)
            (setf generate t)))
         ((:scancode-space :scancode-kp-space)
          (setf refresh t))
         ((:scancode-equals :scancode-kp-equals)
          (incf *bsp-depth*)
          (setf generate t))
         ((:scancode-minus :scancode-kp-minus)
          (when (> *bsp-depth* 1)
            (decf *bsp-depth*))
          (setf generate t))
         ((:scancode-8 :scancode-kp-multiply)
          (incf *min-room-size*)
          (setf generate t))
         ((:scancode-slash :scancode-kp-divide)
          (when (> *min-room-size* 2)
            (decf *min-room-size*))
          (setf generate t))
         ((:scancode-1 :scancode-kp-1)
          (setf *random-room* (not *random-room*))
          (unless *random-room* (setf *room-walls* t))
          (setf refresh t))
         ((:scancode-2 :scancode-kp-2)
          (setf *room-walls* (not *room-walls*))
          (setf refresh t)))))
    (when (or generate refresh)
      ;; dungeon generation
      (if (not bsp)
          ;; create the bsp
          (setf bsp (tcod:bsp-new-with-size
                     0 0 +sample-screen-width+ +sample-screen-height+))
          ;; restore the nodes size
          (tcod:bsp-resize bsp 0 0
                           +sample-screen-width+ +sample-screen-height+))
      (loop for i below (array-total-size map)
            do (setf (row-major-aref map i) #\#))
      (when generate
        ;; build a new random bsp tree
        (tcod:bsp-remove-sons bsp)
        (tcod:bsp-split-recursive bsp (cffi:null-pointer)
                                  *bsp-depth*
                                  (+ *min-room-size* (if *room-walls* 1 0))
                                  (+ *min-room-size* (if *room-walls* 1 0))
                                  1.5 1.5))
      ;; create the dungeon from the bsp
      (tcod:bsp-traverse-inverted-level-order bsp (cffi:callback traverse-node)
                                              0)
      (setf generate nil
            refresh nil))
    (tcod:console-clear *sample-console*)
    (tcod:console-set-default-foreground *sample-console* (tcod:color :white))
    (tcod:console-print
     *sample-console*
     1 1
     "ENTER : rebuild bsp~%
SPACE : rebuild dungeon~%!
   +- : bsp depth ~d~%
   */ : room size ~d~%
    1 : random room size ~:[OFF~;ON~%
    2 : room walls ~:[OFF~;ON~]~]"
     *bsp-depth*
     *min-room-size*
     *random-room*
     *room-walls*)
    ;; render the level
    (loop for y below +sample-screen-height+
          do (loop for x below +sample-screen-width+
                   for wall = (eql (aref map x y) #\#)
                   do (tcod:console-set-char-background
                       *sample-console* x y (if wall dark-wall dark-ground)
                       :set)))))

;;* ***************************
;;* name generator sample
;;* ***************************
(defun render-name (event))

;;* ***************************
;;* SDL callback sample
;;* ***************************
(defun render-sdl (event))

;;* ***************************
;;* the list of samples
;;* ***************************

(defvar *samples* #(("  True colors        " render-colors)
                    ("  Offscreen console  " render-offscreen)
                    ("  Line drawing       " render-lines)
                    ("  Noise              " render-noise)
                    ("  Field of view      " render-fov)
                    ("  Path finding       " render-path)
                    ("  Bsp toolkit        " render-bsp)
                    ("  Image toolkit      " render-image)
                    ("  Mouse support      " render-mouse)
                    #++("  Name generator     " render-name)
                    #++("  SDL callback       " render-sdl)))

(defun sample-render (sample-number event)
  (with-simple-restart (:continue "skip this render")
    (funcall (second (aref *samples* sample-number)) event)))

;; A rendered selection.
(defvar *renderer-option* #(("OPENGL" :renderer-opengl)
                            ("SDL" :renderer-SDL)
                            ("SDL2" :renderer-SDL2)
                            ("OPENGL2" :renderer-opengl2)
                            ("XTERM" :renderer-xterm)
                            ("GLSL" :renderer-glsl)))

;; Return the current framerate from the sampled frames.
(defun get-framerate ()
  (let ((total (reduce '+ *delta-samples*)))
    (if (zerop total)
        0.0
        (coerce (/ +delta-samples-length+ total) 'single-float))))

;; Print libtcod log messages.
(cffi:defcallback print-log :void
    ((message (:pointer (:struct %tcod::log-message)))
     (userdata %tcod::userdata))
  (declare (ignore userdata))
  (cffi:with-foreign-slots ((%tcod::message
                             %tcod::level
                             %tcod::source
                             %tcod::line-no)
                            message
                            (:struct %tcod::log-message))
    (format *trace-output* "~a@~a:~a:~a~%"
            %tcod::source %tcod::line-no %tcod::level %tcod::message)))

;;* ***************************
;;* the main function
;;* ***************************

(defvar *shown* nil)
(defun main (&key integer-scaling (keep-aspect t)
               (font (tcod-data "fonts/dejavu12x12_gs_tc.png"))
               (font-rows 8)
               (font-columns 32)
               (font-charset %tcod::+charmap-tcod+))
  (%tcod::sdl-log-set-all-priority :verbose)
  (tcod:set-log-callback (cffi:callback print-log) 0)
  (float-features:with-float-traps-masked (:overflow :invalid)
    (sdl2:with-sdl-event (on-enter-event +on-enter-userevent+)
      (sdl2:with-sdl-event (on-draw-event +on-draw-userevent+)
        (let* ((tileset (tcod:tileset-load font
                                           font-columns font-rows
                                           font-charset))
               (main-console (tcod:console-new 80 50))
               (vsync t)
               (*g-context* (tcod:context-new "libtcod CL sample"
                                              :console main-console
                                              :sdl-window-flags :resizable
                                              :renderer-type :sdl2
                                              :tileset tileset
                                              :vsync vsync
                                              :columns 80 :rows 50))
               (credits-end nil)
               (start-time (sdl2:get-ticks))
               (last-time (sdl2:get-ticks))
               ;; initialize the offscreen console for the samples
               (*sample-console* (tcod:console-new +sample-screen-width+
                                                   +sample-screen-height+))
               (cur-sample 0))
          (tcod:console-credits-reset)
          #+windows
          (unless *shown*
            ;; work around windows oddness with first window created
            (setf *shown* t)
            (sdl2:hide-window (tcod:context-get-sdl-window *g-context*))
            (sdl2:show-window (tcod:context-get-sdl-window *g-context*)))
          (tcod:console-set-alignment main-console :left)
          (funcall (second (aref *samples* cur-sample)) on-enter-event)
          (flet ((fg (c)
                   (tcod:console-set-default-foreground main-console (tcod:color c)))
                 (bg (c)
                   (tcod:console-set-default-background main-console
                                                        (tcod:color c)))
                 (vsync ()
                   (sdl2-ffi.functions:sdl-render-set-v-sync
                    (tcod:context-get-sdl-renderer *g-context*)
                    (if vsync 1 0))))
            (vsync)
            (unwind-protect
                 (loop named outer ;; infinite loop
                       for current-time = (sdl2:get-ticks)
                       for delta-time-ms = (max 0 (- current-time last-time))
                       do (setf last-time current-time)
                          (setf *delta-time* (/ delta-time-ms 1000.0))
                          (setf (aref *delta-samples* *last-delta-sample*)
                                *delta-time*)
                          (setf *last-delta-sample* (mod (1+ *last-delta-sample*)
                                                         (length *delta-samples*)))

                          (unless credits-end
                            (setf credits-end
                                  (tcod:console-credits-render-ex main-console
                                                                  56 43
                                                                  nil *delta-time*)))
                          ;; print the list of samples
                          (loop for (n f) across *samples*
                                for i from 0
                                when (= i cur-sample)
                                  ;; set colors for currently selected sample
                                  do (fg :white)
                                     (bg :light-blue)
                                else
                                  ;; set colors for other samples
                                  do (fg :grey)
                                     (bg :black)
                                do ;; print the sample name
                                   (tcod:console-print-ex main-console
                                                          2
                                                          (- 46 (- (length *samples*) i))
                                                          :set :left
                                                          "~a" n))
                          ;; print the help message
                          (fg :grey)
                          (tcod:console-print-ex main-console
                                                 79 46
                                                 :none :right
                                                 ;; extra spaces in
                                                 ;; case width changes
                                                 "       last frame : ~3dms (~7,2f fps)"
                                                 delta-time-ms (get-framerate))
                          (loop for i below 80
                                do (tcod:console-print-ex main-console
                                                          i 0
                                                          :none :left
                                                          "~a"
                                                          (floor i 10))
                                   (tcod:console-print-ex main-console
                                                          i 1
                                                          :none :left
                                                          "~a"
                                                          (mod i 10)))
                          (let ((dt (- (sdl2:get-ticks) start-time)))
                            (tcod:console-print-ex main-console
                                                   79 47
                                                   :none :right
                                                   "elapsed : ~10dms ~5,2fs"
                                                   dt (/ dt 1000.0)))
                          (tcod:console-print main-console
                                              2 47 "↑↓ : select a sample")
                          (tcod:console-print main-console
                                              2 48
                                              "ALT-ENTER : switch to ~a"
                                              (if (tcod:console-is-fullscreen)
                                                  "windowed mode  "
                                                  "fullscreen mode"))
                          ;; blit the sample console on the root console
                          (tcod:console-blit
                           *sample-console*
                           0 0
                           +sample-screen-width+ +sample-screen-height+
                           main-console
                           +sample-screen-x+ +sample-screen-y+
                           1.0 1.0)
                          ;; display renderer list and current renderer
                          (let ((current-renderer (tcod:context-get-renderer-type
                                                   *g-context*)))
                            (fg :grey)
                            (bg :black)
                            (tcod:console-print-ex
                             main-console 42 (- 45 (1+ (length *renderer-option*)))
                             :set :left "Renderer : (can't switch currently)")
                            (loop
                              for (n r) across *renderer-option*
                              for i from 0
                              when (eql r current-renderer)
                                do (fg :white)
                                   (bg :light-blue)
                              else
                                do (fg :grey)
                                   (bg :black)
                              do (tcod:console-print-ex main-console
                                                        42 (- 45 (- (length *renderer-option*) i))
                                                        :set :left
                                                        "F~d ~a"
                                                        (1+ i) n)))
                          (tcod:console-print-ex main-console
                                                 41 45
                                                 :set :left
                                                 "F12 Toggle VSync (currently ~:[off~;on~])"
                                                 vsync)
                          (sample-render cur-sample on-draw-event)

                          ;; update the game screen
                          (tcod:context-present *g-context* main-console
                                                :integer-scaling integer-scaling
                                                :keep-aspect keep-aspect
                                                :clear-color :dark-blue)

                          ;; check for events
                          (sdl2:with-sdl-event (event)
                            (loop
                              for r = (sdl2:next-event event :poll)
                              until (or (eql r :pollsentinel)
                                        (eql r 0))
                              do (tcod::sdl2-event-case event
                                   (:keydown
                                    (:keysym key)
                                    (tcod::sdl2-scancode-case key
                                      ((:scancode-escape :scancode-q)
                                       (return-from outer nil))
                                      (:scancode-down ;; next sample
                                       (setf cur-sample (mod (1+ cur-sample)
                                                             (length *samples*)))
                                       (sample-render cur-sample on-enter-event))
                                      (:scancode-up ;; previous sample
                                       (setf cur-sample (mod (1- cur-sample)
                                                             (length *samples*)))
                                       (sample-render cur-sample on-enter-event))
                                      ((:scancode-return
                                        :scancode-return2
                                        :scancode-kp-enter)
                                       ;; toggle fullscreen with alt+enter
                                       (when (sdl2:mod-value-p (sdl2:mod-value key) :alt)
                                         (let ((window (tcod:context-get-sdl-window *g-context*))
                                               (fullscreen-flags
                                                 (list :fullscreen :fullscreen-desktop)))
                                           (when window
                                             (if (intersection
                                                  fullscreen-flags
                                                  (sdl2:get-window-flags window))
                                                 (sdl2:set-window-fullscreen window nil)
                                                 (sdl2:set-window-fullscreen window :desktop))))))
                                      (:scancode-printscreen
                                       ;; todo: figure out why save-screenshot segfaults
                                       #++
                                       (with-simple-restart (continue "continue")
                                         (tcod:context-save-screenshot *g-context* nil)))
                                      (:scancode-c
                                       (tcod:console-credits-reset)
                                       (setf credits-end nil))
                                      (:scancode-f12
                                       (setf vsync (not vsync))
                                       (vsync))
                                      (t
                                       ;; todo: switch renderers with the fkeys
                                       #++
                                       (format t "press ~s~%"
                                               (sdl2:scancode-name
                                                (sdl2:scancode key))))))
                                   (:quit () (return-from outer nil))
                                   (:dropfile
                                    (:file file)
                                    (format t "drop ~s~%" file)
                                    (let ((new-tileset nil))
                                      (cond
                                        ((alexandria:ends-with-subseq ".bdf" file)
                                         (ignore-errors
                                          (setf new-tileset (tcod:load-bdf file))))
                                        ((alexandria:ends-with-subseq "_tc.png" file)
                                         (ignore-errors
                                          (setf new-tileset
                                                (tcod:tileset-load
                                                 file 32 8 %tcod::+charmap-tcod+))))
                                        (t
                                         (ignore-errors
                                          (setf new-tileset
                                                (tcod:tileset-load
                                                 file 16 16 %tcod::+charmap-cp437+)))))
                                      (when (and new-tileset
                                                 (not (cffi:null-pointer-p new-tileset)))
                                        (tcod:tileset-delete tileset)
                                        (setf tileset new-tileset)
                                        (tcod:context-change-tileset *g-context* tileset))))
                                   (:windowevent
                                    (:event event :data1 w :data2 h)
                                    (declare (ignorable w h))
                                    (when (= event sdl2-ffi:+sdl-windowevent-close+)
                                      (return-from outer nil))))
                                 (sample-render cur-sample event))))
              (tcod:console-delete *sample-console*)
              (tcod:console-delete main-console)
              (tcod:context-delete *g-context*)
              (tcod:tileset-delete tileset))))))))

#++
(main)
#++
(main :font (tcod-data "fonts/dejavu16x16_gs_tc.png"))
