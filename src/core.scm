;;; Copyright (c) 2013 by Álvaro Castro Castilla
;;; Core and basic functionality for Sphere Fusion

(cond-expand
 (optimize
  (declare (standard-bindings) (extended-bindings) (not safe) (block)))
 (debug
  (declare (safe) (debug) (debug-location) (debug-source) (debug-environments)))
 (else (void)))

(define (fusion:error . msgs)
  (SDL_LogError SDL_LOG_CATEGORY_APPLICATION
                (apply string-append
                       (map (lambda (m) (string-append
                                    (if (string? m) m (object->string m))
                                    " "))
                            msgs)))
  ;; FIX: this doesn't work for Android
  (SDL_Quit))

(define (fusion:error-log . msgs)
  (SDL_LogError SDL_LOG_CATEGORY_APPLICATION
                (apply string-append
                       (map (lambda (m) (string-append
                                    (if (string? m) m (object->string m))
                                    " "))
                            msgs))))

;;! Create a shader
;; .parameter Type of shader
;; .parameter Shader string
(define (fusion:create-shader shader-type shader-code)
  (let ((shader-id (glCreateShader shader-type))
        (shader-status* (alloc-GLint* 1)))
    (glShaderSource shader-id 1 (list shader-code) #f)
    (glCompileShader shader-id)
    (glGetShaderiv shader-id GL_COMPILE_STATUS shader-status*)
    (if (= GL_FALSE (*->GLint shader-status*))
        (let ((info-log-length* (alloc-GLint* 1)))
          (glGetShaderiv shader-id GL_INFO_LOG_LENGTH info-log-length*)
          (let* ((info-log-length (*->GLint info-log-length*))
                 (info-log* (alloc-GLchar* info-log-length)))
            (glGetShaderInfoLog shader-id info-log-length #f info-log*)
            (fusion:error (string-append "GL Shading Language compilation -- " (*->string info-log*))))))
    shader-id))

;;!! Link a list of shaders
(define* (fusion:create-program shaders (bind-callback #f))
  (let ((program-id (glCreateProgram))
        (program-status* (alloc-GLint* 1)))
    ;; Run bind-callback if provided
    (if bind-callback (bind-callback program-id))
    ;; Link shader
    (for-each (lambda (s) (glAttachShader program-id s)) shaders)
    (glLinkProgram program-id)
    (glGetProgramiv program-id GL_LINK_STATUS program-status*)
    (if (= GL_FALSE (*->GLint program-status*))
        (let ((info-log-length* (alloc-GLint* 1)))
          (glGetProgramiv program-id GL_INFO_LOG_LENGTH info-log-length*)
          (let* ((info-log-length (*->GLint info-log-length*))
                 (info-log* (alloc-GLchar* info-log-length)))
            (glGetProgramInfoLog program-id info-log-length #f info-log*)
            (fusion:error (string-append "GL Shading Language linkage -- " (*->string info-log*))))))
    (for-each (lambda (s) (glDetachShader program-id s)) shaders)
    program-id))
