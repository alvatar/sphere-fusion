;;; Copyright (c) 2012 by Álvaro Castro Castilla
;;; Core functionality of Fusion

(define (fusion:error . msgs)
  (SDL_LogError SDL_LOG_CATEGORY_APPLICATION
                (apply string-append
                       (map (lambda (m) (string-append
                                    (if (string? m) m (object->string m))
                                    " "))
                            msgs)))
  ;; FIX: this doesn't work for Android
  (exit 1))

(define (fusion:error-log . msgs)
  (SDL_LogError SDL_LOG_CATEGORY_APPLICATION
                (apply string-append
                       (map (lambda (m) (string-append
                                    (if (string? m) m (object->string m))
                                    " "))
                            msgs))))

(define (fusion:pot . vals)
  (let ((largest (apply max vals)))
    (let recur ((pot 2))
      (if (< pot largest) (recur (* 2 pot)) pot))))

(define (fusion:create-cairo-surface width height channels)
  ;; Cairo buffer
  (let ((cairo-buffer (calloc (* channels width height)
                              sizeof-unsigned-char)))
    ;; POT buffer for non-supporting devices
    (let* ((pot-size (fusion:pot width height))
           (pot-buffer
            (cond-expand (mobile (calloc (* channels pot-size pot-size)
                                           sizeof-unsigned-char))
                         (else #f))))
      (unless cairo-buffer (error "Unable to allocate Cairo buffer"))
      (let ((cairo-surface
             (cairo_image_surface_create_for_data
              (void*->unsigned-char* cairo-buffer)
              CAIRO_FORMAT_ARGB32
              width
              height
              (* channels width))))
        (unless (equal? (cairo_surface_status cairo-surface) CAIRO_STATUS_SUCCESS)
                (free cairo-buffer)
                (free pot-buffer)
                (fusion:error "Couldn't create cairo surface"))
        (let ((cairo (cairo_create cairo-surface)))
          (unless (equal? (cairo_status cairo) CAIRO_STATUS_SUCCESS)
                  (free cairo-buffer)
                  (free pot-buffer)
                  (fusion:error "Couldn't create context"))
          ;; FIX: Is this really executing?
          (make-will cairo
                     (lambda (c)
                       (free cairo-buffer)
                       (free pot-buffer)
                       (cairo_destroy c)))
          (values cairo
                  cairo-buffer
                  pot-buffer))))))

(define (fusion:create-gl-cairo-surface-renderer texture x0 y0 width height channels)
  (let* ((pot-size (fusion:pot width height))
         (update-pot-texture
          (cond-expand (mobile
                        (lambda (cairo-surface-data pot-surface-data)
                          (cairo:copy-surface-to-pot-buffer cairo-surface-data width height
                                                            pot-surface-data pot-size channels)))
                       (else
                        (lambda (c p) (fusion:error "Calling update-pot-texture in a npot-supporting system"))))))
    (lambda (cairo-surface-data pot-surface-data)
      (unless cairo-surface-data
              (fusion:error "No valid surface data passed"))
      (glEnable GL_TEXTURE_2D)
      (glEnable GL_BLEND)
      (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
      (glPushMatrix)
      (glBindTexture GL_TEXTURE_2D texture)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
      (glMatrixMode GL_MODELVIEW)
      (glLoadIdentity)
      (cond-expand
       ;; NPOT not supported in GLES 1.0/1.1
       (mobile
        (update-pot-texture cairo-surface-data pot-surface-data)
        (glTexImage2D GL_TEXTURE_2D 0 GL_BGRA_EXT pot-size pot-size 0 GL_BGRA_EXT GL_UNSIGNED_BYTE pot-surface-data))
       ;; NPOT supported in OpenGL
       (else
        (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA width height 0 GL_BGRA GL_UNSIGNED_BYTE cairo-surface-data)))
      (let ((x0 (exact->inexact x0))
            (y0 (exact->inexact y0))
            (x1 (exact->inexact (+ x0 width)))
            (y1 (exact->inexact (+ y0 height))))
        (let ((coords
               (cond-expand
                (mobile
                 (let ((x-tex (exact->inexact (/ width pot-size)))
                       (y-tex (exact->inexact (/ height pot-size))))
                   (vector->GLfloat* `#(,x0 ,y1 ;; Rect coordinates
                                            0.0 0.0 ;; Tex coordinates
                                            ,x1 ,y1
                                            ,x-tex 0.0
                                            ,x1 ,y0
                                            ,x-tex ,y-tex
                                            ,x0 ,y0
                                            0.0 ,y-tex))))
                (else
                 (vector->GLfloat* `#(,x0 ,y1     ;; Rect coordinates
                                          0.0 0.0 ;; Tex coordinates
                                          ,x1 ,y1
                                          1.0 0.0
                                          ,x1 ,y0
                                          1.0 1.0
                                          ,x0 ,y0
                                          0.0 1.0)))))
              (indices (vector->GLushort* '#(0 1 2 0 3 2)))
              (vertex-size (* (+ 2 2) sizeof-GLfloat)))
          (glEnableClientState GL_VERTEX_ARRAY)
          (glEnableClientState GL_TEXTURE_COORD_ARRAY)
          (let* ((vertex-pointer (->void* coords))
                 (texcoords-pointer (void*-offset vertex-pointer
                                                  (* 2 sizeof-GLfloat))))
            (glVertexPointer 2 GL_FLOAT vertex-size vertex-pointer)
            (glTexCoordPointer 2 GL_FLOAT vertex-size texcoords-pointer)
            (glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_SHORT (->void* indices)))))
      (glPopMatrix)
      (glDisableClientState GL_TEXTURE_COORD_ARRAY)
      (glDisableClientState GL_VERTEX_ARRAY)
      (glDisable GL_BLEND)
      (glDisable GL_TEXTURE_2D))))

(define (fusion:create-simple-gl-cairo config)
  (lambda (handle-event draw init-input)
    ;; If default feeds are given, then you need two: initial-events-feed and default-events-return
    (let ((init-screen-width (cadr (memq 'width: config)))
          (init-screen-height (cadr (memq 'height: config))))
      (if (< (SDL_Init SDL_INIT_VIDEO) 0)
          (fusion:error "Couldn't initialize SDL!"))
      ;; SDL
      (let ((win (SDL_CreateWindow
                  ""
                  SDL_WINDOWPOS_CENTERED
                  SDL_WINDOWPOS_CENTERED
                  (cond-expand (mobile 0) (else init-screen-width))
                  (cond-expand (mobile 0) (else init-screen-height))
                  SDL_WINDOW_OPENGL
                  #;
                  (bitwise-ior SDL_WINDOW_OPENGL
                               SDL_WINDOW_BORDERLESS)))
            (screen-width* (make-int* 1))
            (screen-height* (make-int* 1)))
        (SDL_GetWindowSize win screen-width* screen-height*)
        (let ((check (if (not win) (fusion:error "Unable to create render window" (SDL_GetError))))
              (ctx (SDL_GL_CreateContext win))
              (screen-width (pointer->int screen-width*))
              (screen-height (pointer->int screen-height*))
              (texture-channels 4))
          (SDL_Log (object->string screen-width))
          (SDL_Log (object->string screen-height))
          ;; OpenGL
          (let* ((num-textures 1)
                 (textures (make-GLuint* 1))
                 (render-surface (fusion:create-gl-cairo-surface-renderer
                                  (GLuint*-ref textures 0)
                                  0
                                  0
                                  screen-width
                                  screen-height
                                  texture-channels)))
            (SDL_Log (string-append "OpenGL Version: "(glGetString GL_VERSION)))
            ;; OpenGL viewport
            (glViewport 0 0 screen-width screen-height)
            (glMatrixMode GL_PROJECTION)
            (glLoadIdentity)
            (cond-expand
             (mobile
              (glOrthof 0.0 (exact->inexact screen-width)
                        0.0 (exact->inexact screen-height)
                        1.0 -1.0))
             (else
              (glOrtho 0.0 (exact->inexact screen-width)
                       0.0 (exact->inexact screen-height)
                       1.0 -1.0)))
            ;; OpenGL texture
            (glDeleteTextures num-textures textures)
            (glGenTextures num-textures textures)
            ;; Cairo
            (receive
             (cairo cairo-surface-data pot-surface-data)
             (fusion:create-cairo-surface screen-width screen-height texture-channels)
             (let* ((event (make-SDL_Event))
                    (event* (SDL_Event-pointer event)))
               (call/cc
                (lambda (leave)
                  (let draw-loop ((current-draw-world init-input))
                    (let ((draw-output-world
                           (draw cairo
                                 (SDL_GetTicks)
                                 ;; First, events are processed. If no events are in queue, world input is passed as output
                                 (let event-loop ((current-events-world current-draw-world))
                                   (if (= 1 (SDL_PollEvent event*))
                                       ;; Event-handling output (the world) is fed back to event handling...
                                       (let ((handle-event-output (handle-event event current-events-world)))
                                         (when (eq? handle-event-output 'exit)
                                               (leave))
                                         (event-loop handle-event-output))
                                       ;; ...until there are no remaining events to process
                                       current-events-world)))))
                      (glClearColor 1.0 0.0 0.0 1.0)
                      (glClear GL_COLOR_BUFFER_BIT)
                      (render-surface cairo-surface-data pot-surface-data)
                      (SDL_GL_SwapWindow win)
                      (draw-loop draw-output-world)))))
               (free (->void* event*))
                (SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION "Bye.")
                (glDeleteTextures num-textures textures)
                (SDL_GL_DeleteContext ctx)
                (SDL_DestroyWindow win)
                (SDL_Quit)))))))
    (##gc)))
