;; Executes the given form and checks if GL's state is valid
(define-macro (check-gl-error exp)
  `(begin
     ,exp
     (let ((error (glGetError)))
       (if (not (= error GL_NO_ERROR))
           (begin
             (SDL_Log (string-append "GL Error: " (object->string error) " - " (object->string ',exp)))
             #f)
           #t))))

;; Loads an image from the given path and creates a texture object to hold it
(define (load-texture window path)
  (let* ((texture-img* (IMG_Load path))
         (texture-id* (alloc-GLuint* 1)))

    (glGenTextures 1 texture-id*)
    (glBindTexture GL_TEXTURE_2D (*->GLuint texture-id*))

    (check-gl-error (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA
                                  (SDL_Surface-w texture-img*) (SDL_Surface-h texture-img*)
                                  0 GL_RGBA GL_UNSIGNED_BYTE
                                  (SDL_Surface-pixels texture-img*)))
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
    (glBindTexture GL_TEXTURE_2D 0)
    (SDL_FreeSurface texture-img*)

    texture-id*))

;; Loads a shader using the given relative path
(define (load-shader path)
  (fusion:load-text-file (string-append "assets/shaders/" path)))

;; Creates a new program with the given vertex and shader files paths.
;; A callback function to set up the attributes must be provided
(define (create-program vertex-shader fragment-shader callback)
  (let*
      ((shaders (list (fusion:create-shader GL_VERTEX_SHADER
                                            (load-shader vertex-shader))
                      (fusion:create-shader GL_FRAGMENT_SHADER
                                            (load-shader fragment-shader))))
       (program-id (fusion:create-program shaders callback)))
    (for-each glDeleteShader shaders)
    program-id))

;; Creates a new OpenGL VBO from a given f32vector.
(define* (create-buffer-from-vector vertex-data-vector (buffer-type GL_STATIC_DRAW))
  (let ((buffer-id* (alloc-GLuint* 1)))
    (glGenBuffers 1 buffer-id*)

    (let* ((buffer-id (*->GLuint buffer-id*))
           (vertex-data (f32vector->GLfloat* vertex-data-vector)))
      (glBindBuffer GL_ARRAY_BUFFER buffer-id)
      (glBufferData GL_ARRAY_BUFFER
                    (* (f32vector-length vertex-data-vector) GLfloat-size)
                    vertex-data
                    GL_STATIC_DRAW)
      (glBindBuffer GL_ARRAY_BUFFER 0))
    buffer-id*))

;; Draws the given vbo  with a particular program. The callback is
;; used to set up the attributes of the dynamic attributes
(define (draw-vbo vbo-id* program-id type count attribs-callback)
  (let ((vbo-id (*->GLuint vbo-id*)))
    (glUseProgram program-id)

    (if (check-gl-error (glBindBuffer GL_ARRAY_BUFFER vbo-id))
        (begin
          (attribs-callback)
          (check-gl-error (glDrawArrays type 0 count))
          (glBindBuffer GL_ARRAY_BUFFER 0)))

    (glUseProgram 0)))
