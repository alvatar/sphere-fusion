;; Executes the given form and checks if GL's state is valid
(define-macro (check-gl-error exp)
  `(let* ((result ,exp)
          (error (glGetError)))
     (unless (= error GL_NO_ERROR)
             (error-log (string-append "GL Error -- " (object->string error)
                                       " - " (object->string ',exp))))
     result))


;;-------------------------------------------------------------------------------
;; Application Life Cycle

;;! Initializes the App
(define (init-app!)
  (let ((mode* (alloc-SDL_DisplayMode))
        (flags-sdl (bitwise-ior SDL_INIT_VIDEO SDL_INIT_AUDIO))
        (flags-img (bitwise-ior IMG_INIT_JPG IMG_INIT_PNG)))
    (when (< (SDL_Init flags-sdl) 0)
          (error-log "Couldn't initialize SDL!"))
    ;; Initialize events. Do it as soon as possible, for platforms such as iOS
    (init-events!)
    ;; SDL_image initialization
    (when (not (= (IMG_Init flags-img) flags-img))
          (error-log "Couldn't initialize SDL Image!"))
    (cond-expand
     (host
      (SDL_GL_SetAttribute SDL_GL_CONTEXT_MAJOR_VERSION 2)
      (SDL_GL_SetAttribute SDL_GL_CONTEXT_MINOR_VERSION 1))
     (else
      (SDL_GL_SetAttribute SDL_GL_CONTEXT_PROFILE_MASK SDL_GL_CONTEXT_PROFILE_ES)
      (SDL_GL_SetAttribute SDL_GL_CONTEXT_MAJOR_VERSION 2)
      (SDL_GL_SetAttribute SDL_GL_CONTEXT_MINOR_VERSION 0)))
    (SDL_GL_SetAttribute SDL_GL_ALPHA_SIZE 8)
    (SDL_GL_SetAttribute SDL_GL_RED_SIZE 8)
    (SDL_GL_SetAttribute SDL_GL_GREEN_SIZE 8)
    (SDL_GL_SetAttribute SDL_GL_BLUE_SIZE 8)
    (SDL_GL_SetAttribute SDL_GL_DOUBLEBUFFER 1)
    (SDL_GL_SetAttribute SDL_GL_DEPTH_SIZE 0)
    (SDL_GL_SetAttribute SDL_GL_RETAINED_BACKING 1)
    ;; Get screen size, Portrait orientation by default
    (SDL_GetDisplayMode 0 0 mode*)
    (let ((reported-width (SDL_DisplayMode-w mode*))
          (reported-height (SDL_DisplayMode-h mode*)))
      (cond-expand
       (ios (set! *screen-width* (min reported-width reported-height))
            (set! *screen-height* (max reported-width reported-height)))
       (else (set! *screen-width* 640)
             (set! *screen-height* 960))))
    (set! *window*
          (SDL_CreateWindow "SDL/GL" SDL_WINDOWPOS_CENTERED SDL_WINDOWPOS_CENTERED
                            *screen-width* *screen-height*
                            (bitwise-ior SDL_WINDOW_OPENGL
                                         SDL_WINDOW_RESIZABLE
                                         SDL_WINDOW_BORDERLESS
                                         SDL_WINDOW_ALLOW_HIGHDPI)))
    (unless *window* (error-log "Unable to create render window" (SDL_GetError)))
    ;; OpenGL/ES context
    (let ((ctx (SDL_GL_CreateContext *window*)))
      (unless ctx (error-log "Unable to create GL context" (SDL_GetError)))
      (SDL_Log (string-append "SDL screen size: " (object->string *screen-width*) " x " (object->string *screen-height*)))
      (SDL_Log (string-append "OpenGL Version: " (*->string (glGetString GL_VERSION))))
      ;; Glew: initialize extensions
      (cond-expand (host (glewInit)) (else #!void)))
    ;; OpenGL viewport
    (glViewport 0 0 *screen-width* *screen-height*)
    (glScissor 0 0 *screen-width* *screen-height*)
    ;; Init graphics: shaders, textures...
    (init-graphics!)))

;;! Cleanup resources and quit the application
(define (destroy-app!)
  (destroy-graphics!)
  (SDL_DestroyWindow *window*)
  (SDL_Quit)
  (exit))

;;! Single command for running the app and initializing if necessary
(define* (make-app (create-world:)
                   (update-world: (lambda (world) world))
                   (pre-render: (lambda (world) world))
                   (post-render: (lambda (world) world)))
  (unless (procedure? create-world) (error-log make-app: "create-world parameter should be a procedure"))
  (unless (procedure? update-world) (error-log make-app: "update-world parameter should be a procedure"))
  (unless (procedure? pre-render) (error-log make-app: "pre-render parameter should be a procedure"))
  (unless (procedure? post-render) (error-log make-app: "post-render parameter should be a procedure"))
  (when (zero? (SDL_WasInit 0)) (init-app!))
  ;; Generate a procedure that runs the app in a new thread
  (lambda ()
    (cond-expand
     ;; This was necessary for iOS, but a current hack in SDL makes it unnecessary.
     ;; The callback gets called in a loop controlled by the host system. The REPL thread gets
     ;; resumed as well as soon as this callback is re-entered. For this callback to work, it
     ;; is essential to leave the main() function. This is achieved by continuing the main thread,
     ;; which is by default blocked waiting for messages.
     (ios-sdl-callback
      (sdl-ios-animation-callback-set!
       (let ((world (create-world-wrapper create-world)))
         (lambda (params)
           (when *world-injected* ;; Injected world
                 (set! world *world-injected*)
                 (set! *world-injected* #f))
           (set! *world* world) ;; Global world link
           (set! world          ;; Update world
                 (draw-world-wrapper pre-render
                                     post-render
                                     (update-world-wrapper
                                      update-world
                                      (process-events-wrapper world)))))))
      (log "setting iOS callback")
      (SDL_iPhoneSetAnimationCallback *window* 1 *sdl-ios-animation-callback-proxy* #f)
      (thread-send *main-thread* 'continue))
     ;; Static compilation / Host version: no threads
     ((or host static)
      (let loop ((world (create-world-wrapper create-world)))
        (when *world-injected*
              (set! world *world-injected*)
              (set! *world-injected* #f))
        (set! *world* world)
        (loop
         (draw-world-wrapper pre-render
                             post-render
                             (update-world-wrapper update-world
                                                   (process-events-wrapper world))))))
     ;; Mobile version: thread supports remote REPL
     (else
      (if *app-thread* (thread-terminate! *app-thread*))
      (set! *app-thread*
            (make-thread
             (lambda ()
               (let loop ((world (create-world-wrapper create-world)))
                 (when *world-injected*
                       (set! world *world-injected*)
                       (set! *world-injected* #f))
                 (set! *world* world)
                 (loop
                  (draw-world-wrapper pre-render
                                      post-render
                                      (update-world-wrapper update-world
                                                            (process-events-wrapper world))))))))
      (thread-start! *app-thread*)))))

;;-------------------------------------------------------------------------------
;; Events

(define (init-events!)
  (SDL_SetEventFilter *sdl-events-filter-proxy* #f)
  (sdl-events-filter-set!
   (lambda (userdata event)
     (let ((event-type (SDL_Event-type event)))
       (cond ((= SDL_APP_TERMINATING event-type)
              ;; Terminate the app. Shut everything down before returning from this function.
              0)
             ((= SDL_APP_LOWMEMORY event-type)
              ;; You will get this when your app is paused and iOS wants more memory.
              ;; Release as much memory as possible.
              0)
             ((= SDL_APP_WILLENTERBACKGROUND event-type)
              ;; Prepare your app to go into the background.  Stop loops, etc.
              ;; This gets called when the user hits the home button, or gets a call.
              0)
             ((= SDL_APP_DIDENTERBACKGROUND event-type)
              ;; This will get called if the user accepted whatever sent your app to the background.
              ;; If the user got a phone call and canceled it, you'll instead get an SDL_APP_DIDENTERFOREGROUND event and restart your loops.
              ;; When you get this, you have 5 seconds to save all your state or the app will be terminated.
              ;; Your app is NOT active at this point.
              0)
             ((= SDL_APP_WILLENTERFOREGROUND event-type)
              ;; This call happens when your app is coming back to the foreground.
              ;; Restore all your state here.
              0)
             ((= SDL_APP_DIDENTERFOREGROUND event-type)
              ;; Restart your loops here.
              ;; Your app is interactive and getting CPU again.
              0)
             (else
              ;; No special processing, add it to the event queue
              1))))))

;; Process events and input
(define (process-events-wrapper world)
  (define (get-key-code event)
    (SDL_Keysym-sym
     (SDL_KeyboardEvent-keysym
      (SDL_Event-key event))))
  (let ((event (alloc-SDL_Event))
        (filled #f))
    (let ev-poll ((output '()))
      (if (= 1 (SDL_PollEvent event))
          (let ((event-type (SDL_Event-type event)))
            (cond
             ((or (= SDL_QUIT event-type)
                  (and
                   (= SDL_KEYUP event-type)
                   (or (= (get-key-code event) SDLK_ESCAPE)
                       (= (get-key-code event) SDLK_AC_BACK))))
              (destroy-app!))
             ;; Touch events
             ((= SDL_FINGERMOTION event-type)
              ;;(ev-poll (cons '(fingermotion) output))
              (ev-poll output))
             ((= SDL_FINGERDOWN event-type)
              ;;(ev-poll (cons '(fingerdown) output))
              (ev-poll output))
             ((= SDL_FINGERUP event-type)
              ;; (let ((finger-event (SDL_Event-tfinger event)))
              ;;   (log "Finger -"
              ;;        "X:" (SDL_TouchFingerEvent-x finger-event)
              ;;        "Y:" (SDL_TouchFingerEvent-y finger-event)))
              ;;(ev-poll (cons '(fingerup) output))
              (ev-poll output))
             ;; Mouse events
             ((= SDL_MOUSEMOTION event-type)
              (let ((mouse-event (SDL_Event-motion event)))
                (let ((x (SDL_MouseMotionEvent-x mouse-event))
                      (y (SDL_MouseMotionEvent-y mouse-event))
                      (x-relative (SDL_MouseMotionEvent-xrel mouse-event))
                      (y-relative (SDL_MouseMotionEvent-yrel mouse-event)))
                  (ev-poll (cons `(mousemotion x: ,x y: ,y x-relative: ,x-relative y-relative: ,y-relative)
                                 output)))))
             ((= SDL_MOUSEBUTTONDOWN event-type)
              (let ((mouse-event (SDL_Event-button event)))
                (let ((x (SDL_MouseButtonEvent-x mouse-event))
                      (y (SDL_MouseButtonEvent-y mouse-event)))
                  (ev-poll (cons `(mousedown x: ,x y: ,y) output)))))
             ((= SDL_MOUSEBUTTONUP event-type)
              (let ((mouse-event (SDL_Event-button event)))
                (let ((x (SDL_MouseButtonEvent-x mouse-event))
                      (y (SDL_MouseButtonEvent-y mouse-event)))
                  (ev-poll (cons `(mouseup x: ,x y: ,y) output)))))
             ;; Window events
             ((= SDL_WINDOWEVENT event-type)
              (let ((window-event (SDL_WindowEvent-event (SDL_Event-window event))))
                (if (or (= SDL_WINDOWEVENT_SIZE_CHANGED window-event)
                        (= SDL_WINDOWEVENT_RESIZED window-event))
                    (let* ((resize (SDL_Event-window event))
                           (width (SDL_WindowEvent-data1 resize))
                           (height (SDL_WindowEvent-data2 resize)))
                      (ev-poll (cons `(window-resized width: ,width height: ,height) output))))))
             (else
              (log "Unhandled event - " event-type)
              (ev-poll output))))
          (world-events-set! world (reverse! output))))
    world))

;;-------------------------------------------------------------------------------
;; Graphics

;; Initialize the Openg GL/ES graphics and assets
(define (init-graphics!)
  (define (init-shaders!)
    ;; Creates a new program with the given vertex and shader files paths.
    ;; A callback function to set up the attributes must be provided
    (let ((color-program-id
           (gl-create-program (list (gl-create-shader
                                     GL_VERTEX_SHADER
                                     (load-text-file "assets/shaders/color.vert"))
                                    (gl-create-shader
                                     GL_FRAGMENT_SHADER
                                     (load-text-file "assets/shaders/color.frag")))
                              (lambda (program-id)
                                (glBindAttribLocation program-id 0 "position"))
                              delete-shaders?: #t)))
      (table-set! *gl-programs* 'color color-program-id)
      (glUseProgram color-program-id)
      (check-gl-error
       (table-set! *gl-uniforms* 'perspective
                   (glGetUniformLocation color-program-id "perspectiveMatrix")))
      (glUseProgram 0))
    (let ((tex2d-program-id
           (gl-create-program (list (gl-create-shader
                                     GL_VERTEX_SHADER
                                     (load-text-file "assets/shaders/tex2d.vert"))
                                    (gl-create-shader
                                     GL_FRAGMENT_SHADER
                                     (load-text-file "assets/shaders/tex2d.frag")))
                              (lambda (program-id)
                                (glBindAttribLocation program-id 0 "position")
                                (glBindAttribLocation program-id 1 "texCoord"))
                              delete-shaders?: #t)))
      (table-set! *gl-programs* 'tex2d tex2d-program-id)
      (glUseProgram tex2d-program-id)
      (check-gl-error
       (table-set! *gl-uniforms* 'texture
                   (glGetUniformLocation tex2d-program-id "colorTexture")))
      (check-gl-error
       (table-set! *gl-uniforms* 'perspective (glGetUniformLocation tex2d-program-id "perspectiveMatrix")))
      (glUseProgram 0)))
  (resize-graphics! *screen-width* *screen-height*)
  (init-shaders!)
  ;; OpenGL 3.3+
  ;; Init sampler
  ;; (cond-expand
  ;;  (host (glGenSamplers 1 texture-sampler*)
  ;;        (let ((sampler-id (*->GLuint texture-sampler*)))
  ;;          (glSamplerParameteri sampler-id GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
  ;;          (glSamplerParameteri sampler-id GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
  ;;          (glSamplerParameteri sampler-id GL_TEXTURE_MAG_FILTER GL_NEAREST)
  ;;          (glSamplerParameteri sampler-id GL_TEXTURE_MIN_FILTER GL_NEAREST)))
  ;;  (else #!void))
  )

;; Handle window resizing and orientation
(define (resize-graphics! *screen-width* *screen-height*)
  (set! *screen-width* *screen-width*)
  (set! *screen-height* *screen-height*)
  (set! *perspective-matrix*
        (matrix:* (make-translation-matrix -1.0 1.0 0.0)
                  (matrix:* (make-scaling-matrix (/ 2.0 *screen-width*)
                                                 (/ -2.0 *screen-height*)
                                                 1.0)
                            (make-identity-matrix))))
  (set! *gl-perspective-matrix* (matrix->GLfloat*
                               (matrix:map exact->inexact
                                           *perspective-matrix*))))

;; Tear down all OpenGL structures
(define (destroy-graphics!)
  (table-for-each (lambda (key buffer) (glDeleteBuffers 1 (buffer-id buffer))) *gl-buffers*)
  (set! *gl-buffers* (make-table))
  (table-for-each (lambda (key tex) (glDeleteTextures 1 (texture-id tex))) *gl-textures*)
  (set! *gl-textures* (make-table)))

;;! Draw the given sprite in the current window
(define (draw-sprite sprite)
  (gl-draw-vbo (buffer-id (sprite-buffer sprite))
               (table-ref *gl-programs* 'tex2d)
               GL_TRIANGLES 6
               (lambda ()
                 ;; OpenGL 3.3+
                 ;; (cond-expand (host (glBindSampler 0 (*->GLuint texture-sampler*)))
                 ;;              (else #!void))
                 (check-gl-error
                  (glUniform1i (table-ref *gl-uniforms* 'texture) 0))
                 (check-gl-error
                  (glUniformMatrix4fv (table-ref *gl-uniforms* 'perspective) 1 GL_FALSE *gl-perspective-matrix*))
                 (glEnableVertexAttribArray 0)
                 (glVertexAttribPointer 0 2 GL_FLOAT GL_FALSE (* 4 GLfloat-size) #f)
                 (glEnableVertexAttribArray 1)
                 (glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE (* 4 GLfloat-size)
                                        (integer->void* (* 2 GLfloat-size)))
                 (glActiveTexture GL_TEXTURE0)
                 (glBindTexture GL_TEXTURE_2D (*->GLuint (sprite-texture-id sprite))))))

;;! Draw all elements in the world (internal wrapper)
(define (draw-world-wrapper pre-render-proc post-render-proc world)
  ;; post-render callback
  (pre-render-proc world)
  ;; Draw the different elements
  (for-each draw-sprite (world-sprites world))
  ;; Render and post-render callback
  (post-render-proc world)
  (SDL_GL_SwapWindow *window*)
  world)

;;-------------------------------------------------------------------------------
;; Application World

(define (create-world-wrapper create-proc)
  (create-proc
   (make-world '())))

(define (update-world-wrapper update-world-proc world)
  (let* ((events (world-events world))
         (sprites (world-sprites world))
         (new-world
          (let recur ((events events)
                      (world world))
            (let ((handle-event-up&down
                   (lambda (event-proc) ;; produce a lambda with event-proc specialization
                     (lambda (event)
                       (recur
                        (cdr events)
                        (let ((x (cadr (memq x: event)))
                              (y (cadr (memq y: event))))
                          ;; If a world isn't produced, then pass along the original one
                          (aif new-world world?
                               (call/cc
                                (lambda (leave) ;; Consume the event if captured by one element
                                  (fold (lambda (element world)
                                          (aif proc (event-proc element)
                                               (let ((element-x (sprite-x element))
                                                     (element-y (sprite-y element))
                                                     (element-width (sprite-width element))
                                                     (element-height (sprite-height element)))
                                                 (if (and (> x element-x)
                                                          (> y element-y)
                                                          (< x (+ element-x element-width))
                                                          (< y (+ element-y element-height)))
                                                     (leave (proc element world event))
                                                     world))
                                               world))
                                        world
                                        sprites)))
                               new-world
                               world))))))
                  (handle-event-motion
                   (lambda (event)
                     (recur
                      (cdr events)
                      (let ((x (cadr (memq x: event)))
                            (y (cadr (memq y: event)))
                            (x-relative (cadr (memq x-relative: event)))
                            (y-relative (cadr (memq y-relative: event))))
                        (aif new-world world?
                             (call/cc
                              (lambda (leave) ;; Consume the event once is captured by one element
                                (fold (lambda (element world)
                                        (let ((mouseover-proc (interactive-on-mouseover element))
                                              (mouseout-proc (interactive-on-mouseout element))
                                              (mousemove-proc (interactive-on-mousemove element)))
                                          (if (or mouseover-proc mouseout-proc mousemove-proc)
                                              (let ((element-x (sprite-x element))
                                                    (element-y (sprite-y element))
                                                    (element-width (sprite-width element))
                                                    (element-height (sprite-height element))
                                                    (x-previous (- x x-relative))
                                                    (y-previous (- y y-relative)))
                                                (let ((element-x2 (+ element-x element-width))
                                                      (element-y2 (+ element-y element-height)))
                                                  (cond
                                                   ;; If mouse entered the element (wasn't previously inside)
                                                   ((and (> x element-x)
                                                         (> y element-y)
                                                         (< x element-x2)
                                                         (< y element-y2)
                                                         (not (and (> x-previous element-x)
                                                                   (> y-previous element-y)
                                                                   (< x-previous element-x2)
                                                                   (< y-previous element-y2))))
                                                    (leave (mouseover-proc element world event)))
                                                   ;; If mouse left the element (was previously inside)
                                                   ((and (> x-previous element-x)
                                                         (> y-previous element-y)
                                                         (< x-previous element-x2)
                                                         (< y-previous element-y2)
                                                         (not (and (> x element-x)
                                                                   (> y element-y)
                                                                   (< x element-x2)
                                                                   (< y element-y2))))
                                                    (leave (mouseout-proc element world event)))
                                                   ;; If mouse is moving within the element (is AND was inside)
                                                   ((and (> x-previous element-x)
                                                         (> y-previous element-y)
                                                         (< x-previous element-x2)
                                                         (< y-previous element-y2)
                                                         (> x element-x)
                                                         (> y element-y)
                                                         (< x element-x2)
                                                         (< y element-y2))
                                                    (leave (mousemove-proc element world event)))
                                                   (else world)))))))
                                      world
                                      sprites)))
                             new-world
                             world))))))
              (cond ((null? events) world)
                    ((car events) (lambda (e) (eq? (car e) 'mousedown)) =>
                     (handle-event-up&down interactive-on-mousedown))
                    ((car events) (lambda (e) (eq? (car e) 'mouseup)) =>
                     (handle-event-up&down interactive-on-mouseup))
                    ((car events) (lambda (e) (eq? (car e) 'mousemotion)) =>
                     handle-event-motion)
                    (else
                     (recur (cdr events) world)))))))
    ;; Handle resize event
    (aif resize-event (assq 'window-resized events)
         (let ((width (cadr (memq width: resize-event)))
               (height (cadr (memq height: resize-event))))
           (resize-graphics! width height)))
    ;; Update world time
    (let* ((time (world-time new-world))
           (current-ticks (SDL_GetTicks))
           (previous-ticks (cadr (assq current-ticks: time)))
           (time-step (/ (- current-ticks previous-ticks) 1000.0)))
      (world-time-set! new-world
                       `((current-ticks: ,current-ticks)
                         (previous-ticks: ,previous-ticks)
                         (time-step: ,time-step)))
      ;; Process the world with the custom procedure provided by the user
      (update-world-proc new-world))))

;;! Inject a world copy and substitute current one
(define (inject-world! new-world)
  (set! *world-injected* new-world))

;;! Get a copy of the current world
(define (get-world-copy)
  (make-world/copy *world*))
