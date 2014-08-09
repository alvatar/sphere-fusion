;; Executes the given form and checks if GL's state is valid
(define-macro (check-gl-error exp)
  `(let* ((result ,exp)
          (error (glGetError)))
     (unless (= error GL_NO_ERROR)
             (error-log (string-append "GL Error -- " (object->string error)
                                       " - " (object->string ',exp))))
     result))


;;-------------------------------------------------------------------------------
;;!! GL structures

;;! Type: Buffers
(define-type buffer
  constructor: buffer-constructor
  uuid
  id
  usage
  type
  data)

(define make-buffer
  (let ((f32vector->gl-buffer
         (lambda* (vertex-data-vector (buffer-type GL_STATIC_DRAW))
           (let ((buffer-id* (alloc-GLuint* 1)))
             (glGenBuffers 1 buffer-id*)
             (glBindBuffer GL_ARRAY_BUFFER (*->GLuint buffer-id*))
             (glBufferData GL_ARRAY_BUFFER
                           (* (f32vector-length vertex-data-vector) GLfloat-size)
                           (f32vector->GLfloat* vertex-data-vector)
                           GL_STATIC_DRAW)
             (glBindBuffer GL_ARRAY_BUFFER 0)
             buffer-id*))))
    ;; TODO: handle usage and data type
    (lambda* (uuid type data (usage 'static))
      (let ((instance (buffer-constructor
                       uuid
                       (case type
                         ((f32vector)
                          (f32vector->gl-buffer data))
                         (else (error-log "make-buffer - unknown data type")))
                       usage
                       type
                       data)))
        (table-set! *gl-buffers* uuid instance)
        instance))))

;;! Type: Texture
(define-type texture
  constructor: texture-constructor
  id ;; the OpenGL identifier
  key ;; a symbol identifier
  width
  height)

;;! Textures are automatically registered and can be later accessed from the global table
(define make-texture
  (let ((load-texture->gl-texture
         (lambda (path)
           (let ((texture-img* (IMG_Load path)) ;; default format: ARGB8888
                 (texture-id* (alloc-GLuint* 1)))
             (unless texture-img* (error-log (IMG_GetError)))
             (let ((texture-height (SDL_Surface-h texture-img*))
                   (texture-width (SDL_Surface-w texture-img*)))
               ;; Alternative method (using GL_RGBA). Remember that PixelFormat is backwards in SDL
               ;; (SDL_ConvertSurfaceFormat texture-img-unformatted* SDL_PIXELFORMAT_ABGR8888 0)
               ;; Generate and bind texture
               (glGenTextures 1 texture-id*)
               (glBindTexture GL_TEXTURE_2D (*->GLuint texture-id*))
               ;; Check errors
               (check-gl-error
                (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA ; internal format
                              texture-width texture-height
                              0 (cond-expand (ios GL_BGRA_EXT) (else GL_BGRA)) GL_UNSIGNED_BYTE
                              (SDL_Surface-pixels texture-img*)))
               ;; FILTER: Necessary for NPOT textures in GLES2
               (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
               (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
               ;; WRAP: Necessary for NPOT textures in GLES2
               (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
               (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
               ;; Unbind and free the surface
               (glBindTexture GL_TEXTURE_2D 0)
               (SDL_FreeSurface texture-img*)
               (values texture-id*
                       texture-width
                       texture-height))))))
    (lambda* (key path (register?: #t))
        (receive (texture-id* w h)
                 (load-texture->gl-texture path)
                 (let ((instance (texture-constructor texture-id* key w h)))
                   (if register? (table-set! *gl-textures* key instance))
                   instance)))))

;;-------------------------------------------------------------------------------
;;!! World elements

;;! Type: interactive
(define-type interactive
  extender: define-type-of-interactive
  (on-mouseover init: #f)
  (on-mouseout init: #f)
  (on-mousedown init: #f)
  (on-mouseup init: #f)
  (on-mousemove init: #f))

;;! Type: Sprite
(define-type-of-interactive sprite
  constructor: sprite-constructor
  uuid
  x
  y
  width
  height
  texture-key
  
  unprintable:
  texture-id
  buffer)

;; make-sprite
;; .parameter x The x coordinate of the top-left corner
;; .parameter y The y coordinate of the top-left corner
;; .parameter texture/key The texture or the texture key associated to the sprite
(define* (make-sprite x y texture/key
                      (on-mouseover:)
                      (on-mouseout:)
                      (on-mousedown:)
                      (on-mouseup:)
                      (on-mousemove:))
  (let* ((tex (cond ((texture? texture/key) texture/key)
                    ((table-ref *gl-textures* texture/key #f) => values)
                    (else
                     (error-log make-sprite:
                                "texture/key parameter requires either a texture or a texture key"))))
         (texture-w (texture-width tex))
         (texture-h (texture-height tex))
         (buffer-uuid (random-integer 9999999999999999999)) ;; UID: TODO
         (sprite
          (sprite-constructor
           (random-integer 99999999999999999999) ;; UID: TODO
           x
           y
           (when tex texture-w)
           (when tex texture-h)
           texture-key
           (texture-id tex)
           (make-buffer buffer-uuid
                        'f32vector
                        (let ((qx1 x)
                              (qy1 y))
                          (let ((qx2 (+ qx1 texture-w))
                                (qy2 (+ qy1 texture-h)))
                            (f32vector qx1 qy1 0.0 0.0
                                       qx1 qy2 0.0 1.0
                                       qx2 qy1 1.0 0.0
                                       qx2 qy1 1.0 0.0
                                       qx1 qy2 0.0 1.0
                                       qx2 qy2 1.0 1.0)))))))
    (when on-mouseover (interactive-on-mouseover-set! sprite on-mouseover))
    (when on-mouseout (interactive-on-mouseout-set! sprite on-mouseout))
    (when on-mousedown (interactive-on-mousedown-set! sprite on-mousedown))
    (when on-mouseup (interactive-on-mouseup-set! sprite on-mouseup))
    (when on-mousemove (interactive-on-mousemove-set! sprite on-mousemove))
    ;; Will to automatically remove the associated OpenGL/ES vertex buffer that was created
    ;; along with this sprite, as soon as this instance is destroyed
    (make-will sprite (lambda (s) (table-set! *gl-buffers* buffer-uuid)))
    sprite))

;;! Type: World
(define-type world
  constructor: world-constructor
  time
  events
  sprites)

(define (make-world elements)
  (world-constructor
   '((current-ticks: 0)
     (previous-ticks: 0)
     (time-step: 0))
   ;; bootstrapped world has no events in queue
   '()
   ;; elements get splitted into different kinds
   elements))

(define (make-world/copy world)
  (world-constructor
   (world-time world)
   (world-events world)
   (world-sprites world)))

(define (world-update world attribute value)
  (let ((new-world (make-world/copy world)))
    (cond ((eq? attribute 'sprites) (world-sprites-set! new-world value))
          ((eq? attribute 'events) (world-events-set! new-world value))
          ((eq? attribute 'time) (world-time-set! new-world value))
          (else (error-log world-update: "non-existent attribute")))
    new-world))
