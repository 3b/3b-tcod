(in-package #:3b-tcod)

#+windows
(progn
  ;; windows
  (defun windows-first-window-hack (context)
    (sdl2:hide-window (context-get-sdl-window context))
    (sdl2:show-window (context-get-sdl-window context)))
  (pushnew 'windows-first-window-hack *with-context-init-hook*))

(defmacro sdl2-event-case (event &body event-handlers)
  (let ((sdl-event-type (gensym "SDL-EVENT-TYPE")))
    `(let* ((,sdl-event-type (sdl2:get-event-type ,event)))
       (case ,sdl-event-type
         ,@(remove nil
            (loop for (type params . forms) :in event-handlers
                  collect (sdl2:expand-handler event type params forms)))))))

(defmacro sdl2-process-events ((&key wait event-var)
                               &body event-handlers)
  (let ((sdl-event (or event-var (gensym "SDL-EVENT"))))
   (a:with-gensyms (rc method)
     (a:once-only (wait)
       `(sdl2:with-sdl-event (,sdl-event)
          (loop
            for ,method = (if ,wait :wait :poll) then :poll
            for ,rc = (sdl2:next-event ,sdl-event ,method 1.0)
            until (or (eql :pollsentinel ,rc) (eql 0 ,rc))
            do (sdl2-event-case ,sdl-event
                 ,@event-handlers)))))))

(defmacro sdl2-do-events ((event &key wait)
                          &body body)
  (a:with-gensyms (rc method)
    (a:once-only (wait)
      `(sdl2:with-sdl-event (,event)
         (loop
           for ,method = (if ,wait :wait :poll) then :poll
           for ,rc = (sdl2:next-event ,event ,method)
           until (or (eql :pollsentinel ,rc) (eql 0 ,rc))
           do (progn ,@body))))))

(defmacro sdl2-scancode-case (keysym &body cases)
  (let ((sv (gensym "SCANCODE-VALUE")))
    `(let ((,sv (sdl2:scancode-value ,keysym)))
       (cond
         ,@(loop for (k . b) in cases
                 for test = (cond
                              ((consp k)
                               `(or ,@(loop for i in k
                                            collect `(sdl2:scancode= ,sv ,i))))
                              ((eql k t)
                               k)
                              (t `(sdl2:scancode= ,sv ,k)))
                 collect `(,test ,@b))))))

(defun sdl2-toggle-fullscreen (context &key (type :desktop))
  (let ((window (context-get-sdl-window context))
        (fullscreen-flags
          (list :fullscreen :fullscreen-desktop)))
    (when window
      (if (intersection
           fullscreen-flags
           (sdl2:get-window-flags window))
          (sdl2:set-window-fullscreen window nil)
          (sdl2:set-window-fullscreen window type)))))
