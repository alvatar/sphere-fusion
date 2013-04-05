;;; Copyright (c) 2012 by Álvaro Castro Castilla
;;; Test for 2d and texturing with OpenGL

(##import-include core: base-macros)
(##import-include core: assert-macros)
(##import sdl2: sdl2 version: (debug))
(##import cairo: cairo version: (debug))
(##import opengl: gl version: (debug))


;;! make-matrix creates a matrix (a vector of vectors).
(define (make-matrix rows columns)
  (do ((m (make-vector rows))
       (i 0 (+ i 1)))
      ((= i rows) m)
    (vector-set! m i (make-vector columns)))) 

;;! matrix? checks to see if its argument is a matrix.
;;! It isn't foolproof, but it's generally good enough.
(define (matrix? x)
  (and (vector? x)
       (> (vector-length x) 0)
       (vector? (vector-ref x 0)))) 

;;! matrix-rows returns the number of rows in a matrix.
(define (matrix-rows x)
  (vector-length x)) 

;;! matrix-columns returns the number of columns in a matrix.
(define (matrix-columns x)
  (vector-length (vector-ref x 0))) 

;;! matrix-ref returns the jth element of the ith row.
(define (matrix-ref m i j)
  (vector-ref (vector-ref m i) j)) 

;;! matrix-set! changes the jth element of the ith row.
(define (matrix-set! m i j x)
  (vector-set! (vector-ref m i) j x)) 

;;! map for matrices
(define (matrix:map f m)
  (let* ((nr (matrix-rows m))
         (nc (matrix-columns m))
         (r (make-matrix nr nc)))
    (do ((i 0 (+ i 1)))
        ((= i nr) r)
      (do ((j 0 (+ j 1)))
          ((= j nc))
        (matrix-set! r i j
                     (f (matrix-ref m i j)))))))

;;! * is the generic matrix/scalar multiplication procedure
(define (matrix:* x y)
  ;; mat-sca-mul multiplies a matrix by a scalar.
  (define mat-sca-mul
    (lambda (m x)
      (let* ((nr (matrix-rows m))
             (nc (matrix-columns m))
             (r (make-matrix nr nc)))
        (do ((i 0 (+ i 1)))
            ((= i nr) r)
          (do ((j 0 (+ j 1)))
              ((= j nc))
            (matrix-set! r i j
                         (* x (matrix-ref m i j)))))))) 
  ;; mat-mat-mul multiplies one matrix by another, after verifying
  ;; that the first matrix has as many columns as the second
  ;; matrix has rows.
  (define mat-mat-mul
    (lambda (m1 m2)
      (let* ((nr1 (matrix-rows m1))
             (nr2 (matrix-rows m2))
             (nc2 (matrix-columns m2))
             (r   (make-matrix nr1 nc2)))
        (if (not (= (matrix-columns m1) nr2))
            (match-error m1 m2))
        (do ((i 0 (+ i 1)))
            ((= i nr1) r)
          (do ((j 0 (+ j 1)))
              ((= j nc2))
            (do ((k 0 (+ k 1))
                 (a 0
                    (+ a
                       (* (matrix-ref m1 i k)
                          (matrix-ref m2 k j)))))
                ((= k nr2)
                 (matrix-set! r i j a)))))))) 
  ;; type-error is called to complain when mul receives an invalid
  ;; type of argument.
  (define type-error
    (lambda (what)
      (error 'mul
             "~s is not a number or matrix"
             what))) 
  ;; match-error is called to complain when mul receives a pair of
  ;; incompatible arguments.
  (define match-error
    (lambda (what1 what2)
      (error 'mul
             "~s and ~s are incompatible operands"
             what1
             what2))) 
  (cond
   ((number? x)
    (cond
     ((number? y) (* x y))
     ((matrix? y) (mat-sca-mul y x))
     (else (type-error y))))
   ((matrix? x)
    (cond
     ((number? y) (mat-sca-mul x y))
     ((matrix? y) (mat-mat-mul x y))
     (else (type-error y))))
   (else (type-error x))))

(define (make-identity-matrix)
  '#(#(1 0 0 0)
     #(0 1 0 0)
     #(0 0 1 0)
     #(0 0 0 1)))

(define (make-translation-matrix x y z)
  `#(#(1 0 0 ,x)
     #(0 1 0 ,y)
     #(0 0 1 ,z)
     #(0 0 0 1)))

(define (make-scaling-matrix x y z)
  `#(#(,x 0 0 0)
     #(0 ,y 0 0)
     #(0 0 ,z 0)
     #(0 0 0 1)))

(define (make-x-rotation-matrix omega)
  (let ((cosO (cos omega))
        (sinO (sin omega)))
    (let ((-sinO (- sinO)))
      `#(#(1 0     0      0)
         #(0 ,cosO ,-sinO 0)
         #(0 ,sinO ,cosO  0)
         #(0 0     0      1)))))

(define (make-y-rotation-matrix omega)
  (let ((cosO (cos omega))
        (sinO (sin omega)))
    (let ((-sinO (- sinO)))
      `#(#(,cosO  0 ,sinO 0)
         #(0      1 0     0)
         #(,-sinO 0 ,cosO 0)
         #(0      0 0     1)))))

(define (make-z-rotation-matrix omega)
  (let ((cosO (cos omega))
        (sinO (sin omega)))
    (let ((-sinO (- sinO)))
      `#(#(,cosO ,-sinO 0 0)
         #(,sinO ,cosO  0 0)
         #(0     0      1 0)
         #(0     0      0 1)))))

(define (make-rotation-matrix x y z omega)
  (let ((C (cos omega))
        (S (sin omega)))
    (let ((iC (- 1 C))
          (iS (- 1 S)))
      `#(#(,(let ((x2 (* x x))) (+ x2
                                   (* (- 1 x2)
                                      C)))
           ,(- (* iC x y)
               (* z S))
           ,(+ (* iC x z)
               (* y S))
           0)
         #(,(+ (* iC x y)
               (* z S))
           ,(let ((y2 (* y y))) (+ y2
                                   (* (- 1 y2)
                                      C)))
           ,(- (* iC y z)
               (* x S))
           0)
         #(,(- (* iC x z)
               (* y S))
           ,(+ (* iC y z)
               (* x S))
           ,(let ((z2 (* z z))) (+ z2
                                   (* (- 1 z2)
                                      C)))
           0)
         #(0
           0
           0
           1)))))






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
          (init-screen-height (cadr (memq 'height: config)))
          (screen-width* (make-int* 1))
          (screen-height* (make-int* 1)))
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
                 (position-buffer-object-id* (make-GLuint* 1))
                 (main-vao-id* (make-GLuint* 1))
                 (surface-id* (make-GLuint* 1))
                 (texture-id* (make-GLuint* 1))
                 (texture-unit 0)
                 (sampler-id* (make-GLuint* 1))
                 (vertex-data-vector '#(
                                        50.0 50.0 0.0 0.0
                                        150.0 50.0 0.0 1.0
                                        150.0 100.0 1.0 1.0
                                        50.0 100.0 1.0 0.0))
                 (vertex-data (vector->GLfloat* vertex-data-vector))
                 (shaders (list (fusion:create-shader GL_VERTEX_SHADER vertex-shader)
                                (fusion:create-shader GL_FRAGMENT_SHADER fragment-shader)))
                 (shader-program (fusion:create-program shaders))
                 (texture-image* (SDL_LoadBMP "test/128x128.bmp"))
                 (texture-image (pointer->SDL_Surface texture-image*)))
            ;; Clean up shaders once the program has been compiled and linked
            (for-each glDeleteShader shaders)

            ;; Texture
            (glGenTextures 1 texture-id*)
            (glBindTexture GL_TEXTURE_2D (*->GLuint texture-id*))
            (glTexImage2D GL_TEXTURE_2D 0 3
                          (SDL_Surface-w texture-image) (SDL_Surface-h texture-image)
                          0 GL_BGR GL_UNSIGNED_BYTE
                          (SDL_Surface-pixels texture-image))
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
                            (* (vector-length vertex-data-vector) sizeof-GLfloat)
                            (*->void* vertex-data)
                            GL_STATIC_DRAW)
              ;; Create VAO
              (glGenVertexArrays 1 main-vao-id*)
              (glBindVertexArray (*->GLuint main-vao-id*))
              (glBindBuffer GL_ARRAY_BUFFER position-buffer-object-id)
              
              (glEnableVertexAttribArray 0)
              (glVertexAttribPointer 0 2 GL_FLOAT GL_FALSE (* 4 sizeof-GLfloat) #f)
              (glEnableVertexAttribArray 5)
              (glVertexAttribPointer 5 2 GL_FLOAT GL_FALSE (* 4 sizeof-GLfloat) (integer->void* (* 2 sizeof-GLfloat)))
              
              (glBindBuffer GL_ARRAY_BUFFER 0)
              (glBindVertexArray 0))

            ;; Game loop
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
                                        ;(free (*->void* event*))
              (SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION "Bye.")
              (SDL_GL_DeleteContext ctx)
              (SDL_DestroyWindow win)
              (SDL_Quit))))))
    (##gc)))

(main '(width: 1280 height: 752))
