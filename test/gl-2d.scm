;;; Copyright (c) 2013 by Álvaro Castro Castilla
;;; Test for 2d and texturing with OpenGL

(##import-include core: base-macros)
(##import-include core: assert-macros)
(##import math: matrix)
(##import fusion: core)

(define vertex-shader #<<end-of-shader

#version 330
layout(location = 0) in vec2 position;
layout(location = 5) in vec2 texCoord;

out vec2 colorCoord;

uniform mat4 perspectiveMatrix;

void main()
{
  gl_Position = perspectiveMatrix * vec4(position, 0.0, 1.0);
  colorCoord = texCoord;
}

end-of-shader
)

(define fragment-shader #<<end-of-shader
   
#version 330

in vec2 colorCoord;
uniform sampler2D colorTexture;
out vec4 outputColor;

void main()
{
  outputColor = texture(colorTexture, colorCoord);
}

end-of-shader
)


(define main
  (lambda (config)
    (let ((init-screen-width (cadr (memq 'width: config)))
          (init-screen-height (cadr (memq 'height: config)))
          (screen-width* (alloc-int* 1))
          (screen-height* (alloc-int* 1)))
      (when (< (SDL_Init SDL_INIT_VIDEO) 0) report: (fusion:error "Couldn't initialize SDL!"))
      ;; SDL
      (let ((win (SDL_CreateWindow
                  ""
                  SDL_WINDOWPOS_CENTERED
                  SDL_WINDOWPOS_CENTERED
                  (cond-expand (mobile 0) (else init-screen-width))
                  (cond-expand (mobile 0) (else init-screen-height))
                  SDL_WINDOW_OPENGL)))
        (unless win (fusion:error "Unable to create render window" (SDL_GetError)))
        (SDL_GetWindowSize win screen-width* screen-height*)
        (let ((screen-width (*->int screen-width*))
              (screen-height (*->int screen-height*))
              (ctx (SDL_GL_CreateContext win)))
          (SDL_Log (string-append "SDL screen size: " (object->string screen-width) " x " (object->string screen-height)))
          ;; OpenGL
          (SDL_Log (string-append "OpenGL Version: " (unsigned-char*->string (glGetString GL_VERSION))))
          ;; Glew: initialize extensions
          (glewInit)
          ;; OpenGL viewport
          (glViewport 0 0 screen-width screen-height)
          (glScissor 0 0 screen-width screen-height)

          ;; Generate programs, buffers, textures
          (let* ((perspective-matrix (matrix:* (make-translation-matrix -1.0 1.0 0.0)
                                               (matrix:* (make-scaling-matrix (/ 2.0 screen-width) (/ -2.0 screen-height) 1.0)
                                                         (make-identity-matrix))))
                 (position-buffer-object-id* (alloc-GLuint* 1))
                 (main-vao-id* (alloc-GLuint* 1))
                 (surface-id* (alloc-GLuint* 1))
                 (texture-id* (alloc-GLuint* 1))
                 (texture-unit 0)
                 (sampler-id* (alloc-GLuint* 1))
                 (vertex-data-vector '#f32(
                                           50.0 50.0 0.0 0.0
                                           150.0 50.0 0.0 1.0
                                           150.0 100.0 1.0 1.0
                                           50.0 100.0 1.0 0.0))
                 (vertex-data (f32vector->GLfloat* vertex-data-vector))
                 (shaders (list (fusion:create-shader GL_VERTEX_SHADER vertex-shader)
                                (fusion:create-shader GL_FRAGMENT_SHADER fragment-shader)))
                 (shader-program (fusion:create-program shaders))
                 (texture-image* (SDL_LoadBMP "test/128x128.bmp")))
            ;; Clean up shaders once the program has been compiled and linked
            (for-each glDeleteShader shaders)

            ;; Texture
            (glGenTextures 1 texture-id*)
            (glBindTexture GL_TEXTURE_2D (*->GLuint texture-id*))
            (glTexImage2D GL_TEXTURE_2D 0 3
                          (SDL_Surface-w texture-image*) (SDL_Surface-h texture-image*)
                          0 GL_BGR GL_UNSIGNED_BYTE
                          (SDL_Surface-pixels texture-image*))
            (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_BASE_LEVEL 0)
            (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAX_LEVEL 0)
            (glBindTexture GL_TEXTURE_2D 0)
            (SDL_FreeSurface texture-image*)

            ;; Uniforms
            (glUseProgram shader-program)
            (glUniformMatrix4fv (glGetUniformLocation shader-program "perspectiveMatrix")
                                1 GL_FALSE
                                (matrix->GLfloat*
                                 (matrix:map exact->inexact
                                             perspective-matrix)))
            (glUniform1i (glGetUniformLocation shader-program "colorTexture") texture-unit)
            (glUseProgram 0)

            ;; Sampler
            (glGenSamplers 1 sampler-id*)
            (let ((sampler-id (*->GLuint sampler-id*)))
              (glSamplerParameteri sampler-id GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
              (glSamplerParameteri sampler-id GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
              (glSamplerParameteri sampler-id GL_TEXTURE_MAG_FILTER GL_NEAREST)
              (glSamplerParameteri sampler-id GL_TEXTURE_MIN_FILTER GL_NEAREST))
            	            
            ;; Vertex Array Object
            (glGenBuffers 1 position-buffer-object-id*)
            (let ((position-buffer-object-id (*->GLuint position-buffer-object-id*)))
              ;; Upload buffer
              (glBindBuffer GL_ARRAY_BUFFER position-buffer-object-id)
              (glBufferData GL_ARRAY_BUFFER
                            (* (f32vector-length vertex-data-vector) GLfloat-size)
                            (*->void* vertex-data)
                            GL_DYNAMIC_DRAW)
              ;; Create VAO
              (glGenVertexArrays 1 main-vao-id*)
              (glBindVertexArray (*->GLuint main-vao-id*))
              (glBindBuffer GL_ARRAY_BUFFER position-buffer-object-id)
              
              (glEnableVertexAttribArray 0)
              (glVertexAttribPointer 0 2 GL_FLOAT GL_FALSE (* 4 GLfloat-size) #f)
              (glEnableVertexAttribArray 5)
              (glVertexAttribPointer 5 2 GL_FLOAT GL_FALSE (* 4 GLfloat-size) (integer->void* (* 2 GLfloat-size)))
              
              (glBindBuffer GL_ARRAY_BUFFER 0)
              (glBindVertexArray 0))

            ;; Game loop
            (let ((event* (alloc-SDL_Event 1)))
              (call/cc
               (lambda (quit)
                 (let main-loop ()
                   (let event-loop ()
                     (when (= 1 (SDL_PollEvent event*))
                           (let ((event-type (SDL_Event-type event*)))
                             (cond
                              ((= event-type SDL_KEYDOWN)
                               (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION "Key down")
                               (let* ((kevt* (SDL_Event-key event*))
                                      (key (SDL_Keysym-sym
                                            (SDL_KeyboardEvent-keysym kevt*))))
                                 (cond ((= key SDLK_ESCAPE)
                                        (quit))
                                       (else
                                        (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION (string-append "Key: " (number->string key)))))))
                              (else #f)))
                           (event-loop)))
                   (glClearColor 1.0 0.2 0.0 0.0)
                   (glClear GL_COLOR_BUFFER_BIT)

                   (glActiveTexture (+ GL_TEXTURE0 texture-unit))
                   (glBindTexture GL_TEXTURE_2D (*->GLuint texture-id*))
                   (glBindSampler texture-unit (*->GLuint sampler-id*))

                   (glBindVertexArray (*->GLuint main-vao-id*))
                   (glUseProgram shader-program)
                   (glDrawArrays GL_QUADS 0 4)
                   
                   (glBindVertexArray 0)
                   (glUseProgram 0)
                   
                   (SDL_GL_SwapWindow win)
                   (main-loop))))
              (free (*->void* event*))
              (SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION "Bye.")
              (SDL_GL_DeleteContext ctx)
              (SDL_DestroyWindow win)
              (SDL_Quit))))))
    (##gc)))

(main '(width: 1280 height: 752))
