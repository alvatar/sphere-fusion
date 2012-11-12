;;; Copyright (c) 2012 by Álvaro Castro Castilla
;;; Test for Cairo with OpenGL

;(%include base: ffi#)
;(%include sdl2: sdl2#)

(define (main)
  ((fusion:create-simple-gl-cairo config: '(width: 1280 height: 752))
   draw:
   (let ((posx 180.0))
     (lambda (cr)
       (SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION (object->string (SDL_GL_Extension_Supported "GL_OES_draw_texture")))
       (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
       (cairo_rectangle cr 0.0 0.0 280.0 500.0)
       (cairo_fill cr)
       (cairo_arc cr posx 180.0 150.0 0.0 6.28)
       (cairo_set_source_rgb cr 0.5 0.5 0.0)
       (cairo_fill cr)
       (set! posx (+ 1.0 posx))))
   handle-event:
   (lambda (event)
     (let ((type (SDL_Event-type event)))
       (cond
        ((= type SDL_QUIT)
         'exit)
        ((= type SDL_MOUSEBUTTONDOWN)
         (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION "Button down"))
        ((= type SDL_KEYDOWN)
         (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION "Key down")
         (let* ((kevt (SDL_Event-key event))
                (key (SDL_Keysym-sym
                      (SDL_KeyboardEvent-keysym kevt))))
           (cond ((= key SDLK_ESCAPE)
                  'exit)
                 (else
                  (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION (string-append "Key: " (number->string key)))))))
        ((= type SDL_WINDOWEVENT)
         (let* ((wevt (SDL_Event-window event))
                (event (SDL_WindowEvent-event wevt)))
           (cond
            ((= event SDL_WINDOWEVENT_SIZE_CHANGED)
             (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION "Window Size Changed")
             'size-changed)
            ((= event SDL_WINDOWEVENT_RESIZED)
             (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION "Window Resized")
             'resized)
            ((= event SDL_WINDOWEVENT_MINIMIZED)
             (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION "Window Minimized"))
            ((= event SDL_WINDOWEVENT_RESTORED)
             (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION "Window Restored")))))
        ((= type SDL_FINGERDOWN)
         (SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION "FINGER DOWN!")))))))
