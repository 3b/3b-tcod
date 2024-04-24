(in-package #:3b-tcod)

(defmacro sdl2-event-case (event &body event-handlers)
  (let ((sdl-event-type (gensym "SDL-EVENT-TYPE")))
    `(let* ((,sdl-event-type (sdl2:get-event-type ,event)))
       (case ,sdl-event-type
         ,@(remove nil
            (loop for (type params . forms) :in event-handlers
                  collect (sdl2:expand-handler event type params forms)))))))

(defmacro sdl2-poll-events (()
                            &body event-handlers)
  (let ((sdl-event (gensym "SDL-EVENT"))
        (rc (gensym "RC-")))
    `(sdl2:with-sdl-event (,sdl-event)
       ,@(cddr (find :initialize event-handlers :key #'first))
       (loop
         for ,rc = (sdl2:next-event ,sdl-event :poll)
         until (or (eql :pollsentinel ,rc) (eql 0 ,rc))
         do (sdl2-event-case ,sdl-event
              ,@event-handlers)))))

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
