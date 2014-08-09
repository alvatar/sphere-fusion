(define ellapsed-time 0)
(define bkg-color-period 1)
(define current-color '(0.0 0.0 0.0 1.0))

(define color-program-id #f)
(define tex2d-program-id #f)

(define tri-vertex-id* #f)
(define quad-vertex-id* #f)

(define sprite-id* #f)
(define sprite-sampler* (alloc-GLuint* 1))

(define perspective-matrix #f)
(define float-matrix #f)

(define attr1 #f)
(define attr2 #f)

; Vertex coordinates for the triangle
(define tx1 -0.25)
(define tx2 0.25)
(define ty1 0.25)
(define ty2 -0.25)

; Vertex coordinates for the quad (two triangles)
(define qx1 50.0)
(define qx2 100.0)
(define qy1 50.0)
(define qy2 100.0)

(define triangle-data-vector (f32vector tx1 ty1 0.0 1.0
                                        tx1 ty2 0.0 1.0
                                        tx2 ty2 0.0 1.0 ))

(define quad-data-vector (f32vector qx1 qy1 0.0 0.0
                                    qx1 qy2 0.0 1.0
                                    qx2 qy1 1.0 0.0

                                    qx2 qy1 1.0 0.0
                                    qx1 qy2 0.0 1.0
                                    qx2 qy2 1.0 1.0))

;; Executes the given form and checks if GL's state is valid
(define-macro (check-gl-error exp)
  `(begin
     ,exp
     (let ((error (glGetError)))
       (if (not (= error GL_NO_ERROR))
           (begin
             (SDL_Log (string-append "GL Error: " (object->string error) " - " (object->string ',exp))))))))

;; Loads the shaders and sets the location of the necessary attributes
(define (init-shaders)
  (set! color-program-id
        (create-program "color.vert"
                        "color.frag"
                        (lambda (program-id)
                          (glBindAttribLocation program-id 0 "position"))))

  (set! tex2d-program-id
        (create-program "tex2d.vert"
                        "tex2d.frag"
                        (lambda (program-id)
                          (glBindAttribLocation program-id 0 "position")
                          (glBindAttribLocation program-id 1 "texCoord"))))

  (glUseProgram tex2d-program-id)

  (check-gl-error (set! attr1 (glGetUniformLocation tex2d-program-id "colorTexture")))
  (check-gl-error (set! attr2 (glGetUniformLocation tex2d-program-id "perspectiveMatrix")))

  (glUseProgram 0)
)

;; Creates VBOs from the static vectors defined at the top of the file
(define (init-buffers)
  (set! tri-vertex-id* (create-buffer-from-vector triangle-data-vector))
  (set! quad-vertex-id* (create-buffer-from-vector quad-data-vector)))

;; Loads the texture used by the quad and creates a sampler if running on host
(define (init-images window)
  #;(set! sprite-id* (load-texture window "assets/images/128x128.bmp"))
  (set! sprite-id* (load-texture window "assets/images/smiley-face.png"))

  ;; Sampler
  (cond-expand
   (host (glGenSamplers 1 sprite-sampler*)
         (let ((sampler-id (*->GLuint sprite-sampler*)))
           (glSamplerParameteri sampler-id GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
           (glSamplerParameteri sampler-id GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
           (glSamplerParameteri sampler-id GL_TEXTURE_MAG_FILTER GL_NEAREST)
           (glSamplerParameteri sampler-id GL_TEXTURE_MIN_FILTER GL_NEAREST)))
   (else #!void)))

(define (resize-gui screen-width screen-height)
  (set! perspective-matrix
        (matrix:* (make-translation-matrix -1.0 1.0 0.0)
                  (matrix:* (make-scaling-matrix (/ 2.0 screen-width)
                                                 (/ -2.0 screen-height) 1.0)
                            (make-identity-matrix))))

  (set! float-matrix (matrix->GLfloat*
                      (matrix:map exact->inexact
                                  perspective-matrix))))

;; Initializes the required components for drawing
(define (init-gui window screen-width screen-height)
  (resize-gui screen-width screen-height)
  (init-shaders)
  (init-buffers)
  (init-images window))

(define (destroy-gui)
  (glDeleteBuffers 1 tri-vertex-id*)
  (glDeleteBuffers 1 quad-vertex-id*)
  (glDeleteTextures 1 sprite-id*)
  (glDeleteProgram color-program-id)
  (glDeleteProgram tex2d-program-id))

(define (draw-triangle window)
  (draw-vbo tri-vertex-id* color-program-id GL_TRIANGLES 3
            (lambda ()
              (glEnableVertexAttribArray 0)
              (glVertexAttribPointer 0 4 GL_FLOAT GL_FALSE 0 #f))))

(define (draw-sprite window)
  (draw-vbo quad-vertex-id* tex2d-program-id GL_TRIANGLES 6
            (lambda ()
              (cond-expand
               (host (glBindSampler 0 (*->GLuint sprite-sampler*)))
               (else #!void))

              (check-gl-error (glUniform1i attr1 0))
              (check-gl-error (glUniformMatrix4fv attr2 1 GL_FALSE float-matrix))

              (glEnableVertexAttribArray 0)
              (glVertexAttribPointer 0 2 GL_FLOAT GL_FALSE (* 4 GLfloat-size) #f)
              (glEnableVertexAttribArray 1)
              (glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE (* 4 GLfloat-size) (integer->void* (* 2 GLfloat-size)))

              (glActiveTexture GL_TEXTURE0)
              (glBindTexture GL_TEXTURE_2D (*->GLuint sprite-id*)))))

(define (draw-gui time-step window)
  (set! ellapsed-time (+ ellapsed-time time-step))

  (if (> ellapsed-time bkg-color-period)
      (begin
        (set! current-color (list (random-real) (random-real) (random-real) 1.0))
        (set! ellapsed-time 0)))

  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (glEnable GL_BLEND)

  (glDisable GL_CULL_FACE)
  (glCullFace GL_BACK)

  (apply glClearColor current-color)

  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT))

  (draw-triangle window)
  (draw-sprite window))
