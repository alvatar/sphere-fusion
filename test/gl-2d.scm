;;; Copyright (c) 2012 by Álvaro Castro Castilla
;;; Test for 2d and texturing with OpenGL

(##import-include core: base-macros)
(##import sdl2: sdl2 version: (debug))
(##import cairo: cairo version: (debug))
(##import opengl: gl version: (debug))

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

(define vertex-shader #<<end-of-shader

#version 330
layout(location = 0) in vec4 position;
void main()
{
  gl_Position = position;
}

end-of-shader
)

(define fragment-shader #<<end-of-shader
   
#version 330
out vec4 outputColor;
void main()
{
  outputColor = vec4(1.0f, 1.0f, 0.0f, 1.0f);
}

end-of-shader
)

(define (fusion:create-shader shader-type shader-code)
  (let ((shader-id (glCreateShader shader-type))
        (shader-status* (make-GLint* 1)))
    (glShaderSource shader-id 1 (list shader-code) #f)
    (glCompileShader shader-id)
    (glGetShaderiv shader-id GL_COMPILE_STATUS shader-status*)
    (if (= GL_FALSE (*->GLint shader-status*))
        (let ((info-log-length* (make-GLint* 1)))
          (glGetShaderiv shader-id GL_INFO_LOG_LENGTH info-log-length*)
          (let* ((info-log-length (*->GLint info-log-length*))
                 (info-log* (make-GLchar* info-log-length)))
            (glGetShaderInfoLog shader-id info-log-length #f info-log*)
            (error (string-append "GL Shading Language compilation -- " (char*->string info-log*))))
          ;;(free info-log-length*)
          ;;(free info-log*)
          ))
    ;;(free shader-status*)
    shader-id))

(define (fusion:create-program shaders)
  (let ((program-id (glCreateProgram))
        (program-status* (make-GLint* 1)))
   (for-each (lambda (s) (glAttachShader program-id s)) shaders)
   (glLinkProgram program-id)
   (glGetProgramiv program-id GL_LINK_STATUS program-status*)
   (if (= GL_FALSE (*->GLint program-status*))
       (let ((info-log-length* (make-GLint* 1)))
         (glGetShaderiv shader-id GL_INFO_LOG_LENGTH info-log-length*)
         (let* ((info-log-length (*->GLint info-log-length*))
                (info-log* (make-GLchar* info-log-length)))
           (glGetShaderInfoLog shader-id info-log-length #f info-log*)
           (error (string-append "GL Shading Language linkage -- " (char*->string info-log*))))
         ;;(free info-log-length*)
         ;;(free info-log*)
         ))
   (for-each (lambda (s) (glDetachShader program-id s)) shaders)
   program-id))

(define main
  (lambda (config)
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
                  SDL_WINDOW_OPENGL))
            (screen-width* (make-int* 1))
            (screen-height* (make-int* 1)))
        (SDL_GetWindowSize win screen-width* screen-height*)
        (let ((check (if (not win) (fusion:error "Unable to create render window" (SDL_GetError))))
              (ctx (SDL_GL_CreateContext win))
              (screen-width (pointer->int screen-width*))
              (screen-height (pointer->int screen-height*))
              (texture-channels 4))
          (SDL_Log (string-append "SDL screen size: " (object->string screen-width) " x " (object->string screen-height)))
          ;; OpenGL
          (SDL_Log (string-append "OpenGL Version: " (unsigned-char*->string (glGetString GL_VERSION))))
          ;; Glew: initialize extensions
          (glewInit)
          ;; OpenGL viewport
          (glViewport 0 0 screen-width screen-height)
          (glScissor 0 0 screen-width screen-height)

          ;; Generate programs and buffers
          (let* ((vertex-positions-vector '#(0.75 0.75 0.0 1.0
                                                  0.75 -0.75 0.0 1.0
                                                  -0.75 -0.75 0.0 1.0))
                 (vertex-positions (vector->GLfloat* vertex-positions-vector))
                 (position-buffer-object* (make-GLuint* 1))
                 (main-vao* (make-GLuint* 1))
                 (shaders (list (fusion:create-shader GL_VERTEX_SHADER vertex-shader)
                                (fusion:create-shader GL_FRAGMENT_SHADER fragment-shader)))
                 (shader-program (fusion:create-program shaders)))
            (for-each glDeleteShader shaders)

            ;; Initialize Vertex Buffer
            (glGenBuffers 1 position-buffer-object*)
            (glBindBuffer GL_ARRAY_BUFFER (*->GLuint position-buffer-object*))
            (glBufferData GL_ARRAY_BUFFER
                          (* (vector-length vertex-positions-vector) sizeof-GLfloat)
                          (->void* vertex-positions)
                          GL_STATIC_DRAW)
            (glBindBuffer GL_ARRAY_BUFFER 0)

            ;; Vertex Array Object
            (glGenVertexArrays 1 main-vao*)
            (glBindVertexArray (*->GLuint main-vao*))
            
            (let* ((event (make-SDL_Event))
                   (event* (SDL_Event-pointer event)))
              (call/cc
               (lambda (quit)
                 (let main-loop ()
                   (let event-loop ()
                     (when (= 1 (SDL_PollEvent event*))
                           (let ((event-type (SDL_Event-type event)))
                             (cond
                              ((= event-type SDL_KEYDOWN)
                               (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION "Key down")
                               (let* ((kevt (SDL_Event-key event))
                                      (key (SDL_Keysym-sym
                                            (SDL_KeyboardEvent-keysym kevt))))
                                 (cond ((= key SDLK_ESCAPE)
                                        (quit))
                                       (else
                                        (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION (string-append "Key: " (number->string key)))))))
                              (else #f)))
                           (event-loop)))
                   (glClearColor 1.0 0.2 0.0 0.0)
                   (glClear GL_COLOR_BUFFER_BIT)

                   (glUseProgram shader-program)
                   
                   (glBindBuffer GL_ARRAY_BUFFER (*->GLuint position-buffer-object*))
                   (glEnableVertexAttribArray 0)
                   (glVertexAttribPointer 0 4 GL_FLOAT GL_FALSE 0 #f)
                   (glDrawArrays GL_TRIANGLES 0 3)
                   
                   (glDisableVertexAttribArray 0)
                   (glUseProgram 0)
                   
                   (SDL_GL_SwapWindow win)
                   (main-loop))))
              ;(free (->void* event*))
              (SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION "Bye.")
              (SDL_GL_DeleteContext ctx)
              (SDL_DestroyWindow win)
              (SDL_Quit))))))
    (##gc)))

(main '(width: 1280 height: 752))
