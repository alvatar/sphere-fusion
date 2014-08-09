;; Usage:
;; Run 'sense' server
;; compile and run with 'sake android'
;; It will spawn an Xterm with a Gambit REPL

(define (get-key-code event)
  (SDL_Keysym-sym
   (SDL_KeyboardEvent-keysym
    (SDL_Event-key event))))

(define (resize-event? event)
  (and
   (= SDL_WINDOWEVENT (SDL_Event-type event))
   (or (= SDL_WINDOWEVENT_SIZE_CHANGED (SDL_WindowEvent-event (SDL_Event-window event)))
       (= SDL_WINDOWEVENT_RESIZED (SDL_WindowEvent-event (SDL_Event-window event))))))

(define (click-action? event)
  (or (= SDL_FINGERDOWN (SDL_Event-type event))
      (= SDL_MOUSEBUTTONDOWN (SDL_Event-type event))))

(define (exit-application? event)
  (or (= SDL_QUIT (SDL_Event-type event))
      (and
       (= SDL_KEYUP (SDL_Event-type event))
       (or (= (get-key-code event) SDLK_ESCAPE)
           (= (get-key-code event) SDLK_AC_BACK)))))

(define (draw time-step window)
  (draw-gui time-step window)

  (SDL_GL_SwapWindow window))

(define (main)
  ;; Install and run the remote REPL: IP address of the computer running the debug server
  (if (remote-repl-setup! "192.168.0.102" port: 20000)
      (begin
       (remote-repl-run!)
       (SDL_Log "***** Successfully connected to Gambit Debug Server *****"))
      (SDL_Log "***** Unable to connect to Gambit Debug Server. Are you running 'sense'? *****"))

  ;; A very basic loop to test SDL
  (let ((mode* (alloc-SDL_DisplayMode))
        (flags-img (bitwise-ior IMG_INIT_JPG IMG_INIT_PNG))
        (flags-mix (bitwise-ior MIX_INIT_OGG)))

    (when (< (SDL_Init (bitwise-ior SDL_INIT_VIDEO SDL_INIT_AUDIO)) 0)
          (fusion:error "Couldn't initialize SDL!"))

    (when (not (= (IMG_Init flags-img) flags-img))
          (fusion:error "Couldn't initialize SDL Image!"))

    (when (not (= (Mix_Init flags-mix) flags-mix))
          (fusion:error "Couldn't initialize SDL Mixer!"))

    (cond-expand
     (mobile
      (SDL_GL_SetAttribute SDL_GL_CONTEXT_PROFILE_MASK SDL_GL_CONTEXT_PROFILE_ES)
      (SDL_GL_SetAttribute SDL_GL_CONTEXT_MAJOR_VERSION 2)
      (SDL_GL_SetAttribute SDL_GL_CONTEXT_MINOR_VERSION 0))
     (else #!void))

    (SDL_GL_SetAttribute SDL_GL_DOUBLEBUFFER 1)
    (SDL_GetDisplayMode 0 0 mode*)

    (let* ((screen-width (SDL_DisplayMode-w mode*))
           (screen-height (SDL_DisplayMode-h mode*))
           #;(screen-width 640)
           #;(screen-height 480)
           (window (SDL_CreateWindow
                    "SDL/GL" SDL_WINDOWPOS_CENTERED SDL_WINDOWPOS_CENTERED screen-width screen-height
                    (bitwise-ior SDL_WINDOW_OPENGL SDL_WINDOW_RESIZABLE))))
      (unless window (fusion:error "Unable to create render window" (SDL_GetError)))

      (let ((event (alloc-SDL_Event))
            (ctx (SDL_GL_CreateContext window))
            (current-ticks 0)
            (previous-ticks 0)
            (time-step 0)
            (exit-app #f))
        (SDL_Log (string-append "SDL screen size: " (object->string screen-width) " x " (object->string screen-height)))
        (SDL_Log (string-append "OpenGL Version: " (*->string (glGetString GL_VERSION))))

        ;; Glew: initialize extensions
        (cond-expand
         (host (glewInit))
         (else #!void))

        ;; OpenGL viewport
        (glViewport 0 0 screen-width screen-height)
        (glScissor 0 0 screen-width screen-height)

        (init-gui window screen-width screen-height)
        (init-audio)

        (let recur ((iteration 0))
          (set! current-ticks (SDL_GetTicks))
          (set! time-step (/ (- current-ticks previous-ticks) 1000.0))
          (set! previous-ticks current-ticks)

          ; Poll events queue
          (let ev-poll ()
            (when (= 1 (SDL_PollEvent event))
                  (cond ((exit-application? event)
                         (set! exit-app #t))
                        ((click-action? event)
                         (play-sound))
                        ((resize-event? event)
                         (let ((resize (SDL_Event-window event)))
                           (resize-gui (SDL_WindowEvent-data1 resize) (SDL_WindowEvent-data2 resize)))))

                  (ev-poll)))

          (draw time-step window)

          (SDL_Delay 20)

          (unless exit-app
                  (recur (++ iteration))))

        (destroy-gui)
        (destroy-audio)

        (SDL_DestroyWindow window)
        (SDL_Quit)
        (exit)))))
