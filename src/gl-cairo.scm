;;; Copyright (c) 2012 by Álvaro Castro Castilla
;;; Core functionality of Fusion

(define (fusion:critical-error . msgs)
  (SDL_LogError SDL_LOG_CATEGORY_APPLICATION
                (apply string-append
                       (map (lambda (m) (string-append
                                    (if (string? m) m (object->string m))
                                    " "))
                            msgs)))
  (exit 1))

(define (fusion:create-cairo-surface width height channels)
  (let ((buffer (calloc (* channels width height)
                        sizeof-unsigned-char)))
    (unless buffer (error "Unable to allocate buffer"))
    (let ((cairo-surface
           (cairo_image_surface_create_for_data
            (void*->unsigned-char* buffer)
            CAIRO_FORMAT_ARGB32
            width
            height
            (* channels width))))
      (unless (equal? (cairo_surface_status cairo-surface) CAIRO_STATUS_SUCCESS)
              (free buffer)
              (error "Couldn't create cairo surface"))
      (let ((cairo (cairo_create cairo-surface)))
        (unless (equal? (cairo_status cairo) CAIRO_STATUS_SUCCESS)
                (free buffer)
                (error "Couldn't create context"))
        (make-will cairo
                   (lambda (c)
                     (free buffer)
                     (cairo_destroy c)))
        (values cairo
                buffer)))))

(define (fusion:gl-render-cairo-surface cairo-surface-data texture x0 y0 width height)
  (unless cairo-surface-data
          (fusion:critical-error "No valid surface data passed"))
  (glEnable GL_TEXTURE_2D)
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (glPushMatrix)
  (glBindTexture GL_TEXTURE_2D texture)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)
  (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA width height 0 GL_RGBA GL_UNSIGNED_BYTE cairo-surface-data)
  (let ((x0 (exact->inexact x0))
        (y0 (exact->inexact y0))
        (x1 (exact->inexact (+ x0 width)))
        (y1 (exact->inexact (+ y0 height))))
    (let ((coords
           (vector->GLfloat* `#(,x0 ,y1
                                    0.0 0.0
                                    ,x1 ,y1
                                    1.0 0.0
                                    ,x1 ,y0
                                    1.0 1.0
                                    ,x0 ,y0
                                    0.0 1.0)))
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
  (glDisable GL_TEXTURE_2D))

(define (fusion:create-simple-gl-cairo #!key
                                       (config '(width: 1280 height: 752)))
  (lambda (#!key draw handle-event)
    (let ((init-screen-width (cadr (member width: config)))
          (init-screen-height (cadr (member height: config))))
      (if (< (SDL_Init SDL_INIT_VIDEO) 0)
          (fusion:critical-error "Couldn't initialize SDL!"))
      ;; SDL
      (let ((win (SDL_CreateWindow
                  ""
                  SDL_WINDOWPOS_CENTERED
                  SDL_WINDOWPOS_CENTERED
                  (cond-expand (mobile 0) (else init-screen-width))
                  (cond-expand (mobile 0) (else init-screen-height))
                  (bitwise-ior SDL_WINDOW_OPENGL
                               SDL_WINDOW_BORDERLESS)))
            (screen-width* (make-int* 1))
            (screen-height* (make-int* 1)))
        (SDL_GetWindowSize win screen-width* screen-height*)
        (let ((check (if (not win) (fusion:critical-error "Unable to create render window" (SDL_GetError))))
              (ctx (SDL_GL_CreateContext win))
              (screen-width (pointer->int screen-width*))
              (screen-height (pointer->int screen-height*)))
          (SDL_Log (object->string screen-width))
          (SDL_Log (object->string screen-height))
          ;; OpenGL
          (let* ((num-textures 1)
                 (textures (make-GLuint* 1)))
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
             (cairo cairo-surface-data)
             (fusion:create-cairo-surface screen-width screen-height 4)
             (let* ((event (make-SDL_Event))
                    (event* (SDL_Event-pointer event)))
               (call/cc
                (lambda (leave)
                  (let main-loop ()
                    (let event-loop ()
                      (when (= (SDL_PollEvent event*) 1)
                            (case (handle-event event)
                              ((exit)
                               (leave)))
                            (event-loop)))
                    (glClearColor 1.0 0.0 0.0 1.0)
                    (glClear GL_COLOR_BUFFER_BIT)
                    (draw cairo)
                    (fusion:gl-render-cairo-surface cairo-surface-data (GLuint*-ref textures 0) 0 0 screen-width screen-height)
                    (SDL_GL_SwapWindow win)
                    (SDL_Delay 400)
                    (main-loop))))
               (SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION "Bye.")
               (glDeleteTextures num-textures textures)
               (SDL_GL_DeleteContext ctx)
               (SDL_DestroyWindow win)
               (SDL_Quit)))))))
    (##gc)))
