(%include base: repl-server#)

(define num-textures 1)
(define textures (make-GLuint* 1))
(define sdl-window #f)
(define sdl-context #f)

(define (critical-error . msgs)
  (apply println msgs)
  (exit 1))

(define screen-width 512)
(define screen-height 512)

(define (setup-viewport width height)
  (glViewport 0 0 width height)
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (cond-expand
   (arm
    (glOrthof 0.0 (exact->inexact width)
              0.0 (exact->inexact height)
              1.0 -1.0))
   (else
    (glOrtho 0.0 (exact->inexact width)
             0.0 (exact->inexact height)
             1.0 -1.0))))

(define (init-gl)
  (println (glGetString GL_VERSION)))

(define (setup-opengl-surfaces)
  (glDeleteTextures num-textures textures)
  (glGenTextures num-textures textures))

(define (handle-event event k)
  (let ((type (SDL_Event-type event)))
    (cond
     ((= type SDL_QUIT)
      (exit 0))
     ((= type SDL_MOUSEBUTTONDOWN)
      (println "Button down"))
     ((= type SDL_KEYDOWN)
      (println "Key down")
      (let* ((kevt (SDL_Event-key event))
	     (key (SDL_Keysym-sym
		   (SDL_KeyboardEvent-keysym kevt))))
	(cond ((= key SDLK_ESCAPE)
	       (println "Bye.")
	       (k))
              (else
	       (println "Key: " key)))))
     ((= type SDL_WINDOWEVENT)
      (let* ((wevt (SDL_Event-window event))
             (event (SDL_WindowEvent-event wevt)))
        (cond
         ((= event SDL_WINDOWEVENT_MINIMIZED)
          (println "Window Minimized"))
         ((= event SDL_WINDOWEVENT_RESTORED)
          (println "Window Restored")))))
     ((= type SDL_FINGERDOWN)
      (println "FINGER!")))))

(define-macro (to-repl local)
  (let ((global-serial (string->symbol (string-append "~" (symbol->string local)))))
    `(eval '(define ,global-serial ,(object->serial-number local)))))

(define (main)
  (if (< (SDL_Init SDL_INIT_VIDEO) 0)
      (critical-error "Couldn't initialize SDL!"))
  ;; SDL
  (let* ((win (SDL_CreateWindow
               ""
               SDL_WINDOWPOS_CENTERED
               SDL_WINDOWPOS_CENTERED
               screen-width
               screen-height
               (bitwise-ior SDL_WINDOW_OPENGL
                            SDL_WINDOW_BORDERLESS)))
         (check (if (not win) (critical-error "Unable to create render window"(SDL_GetError))))
         (ctx (SDL_GL_CreateContext win)))
    ;; OpenGL
    (init-gl)
    (setup-viewport screen-width screen-height)
    (setup-opengl-surfaces)
    (let* ((event (make-SDL_Event))
           (event* (SDL_Event-pointer event)))
      (call/cc
       (lambda (k)
         (let main-loop ()
           (let event-loop ()
             (if (= (SDL_PollEvent event*) 1)
                 (begin (handle-event event k)
                        (event-loop))))
                                        ;(to-repl win)
                                        ;(repl-server "playground")

           (glClearColor 1.0 (random-real) 0.0 1.0)
           (glClear GL_COLOR_BUFFER_BIT)
           (SDL_GL_SwapWindow win)
           (SDL_Delay 1000)
           (main-loop))))
      (glDeleteTextures num-textures textures)
      (SDL_GL_DeleteContext ctx)
      (SDL_DestroyWindow win)
      (SDL_Quit))))
