;;! Executes the given form and checks if GL's state is valid
(define-macro (check-gl-error exp)
  `(let* ((result ,exp)
          (error (glGetError)))
     (unless (= error GL_NO_ERROR)
             (error-log (string-append "GL Error -- " (object->string error)
                                       " - " (object->string ',exp))))
     result))


;;-------------------------------------------------------------------------------
;;!! SDL

(define *window* #f)
(define *screen-width* #f)
(define *screen-height* #f)

;;-------------------------------------------------------------------------------
;;!! OpenGL

;;! Matrices
(define *perspective-matrix* #f)
(define *gl-perspective-matrix* #f)

;;! Shader programs
(define *gl-programs* (make-table))

;;! Buffers
(define *gl-buffers* (make-table))

;;! OpenGL Textures registry
(define *gl-textures* (make-table))

;;! Texture sampler (OpenGL 4.x)
(cond-expand
 (host (define texture-sampler* (alloc-GLuint* 1)))
 (else #!void))

;;! Uniform variables
(define *gl-uniforms* (make-table))

;;-------------------------------------------------------------------------------
;;!! App

(define *main-thread* #f)
(define *app-thread* #f)

(define *world* #f)
(define *world-injected* #f)
