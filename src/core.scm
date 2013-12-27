;;; Copyright (c) 2013 by Álvaro Castro Castilla
;;; Core and basic functionality for Sphere Fusion

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
            (fusion:error "in fusion:create-shader. Implement ERROR RETRIEVAL.")
            ;;(glGetShaderInfoLog shader-id info-log-length #f info-log*)
            ;;(error (string-append "GL Shading Language compilation -- " (char*->string info-log*)))
            )))
    shader-id))

(define (fusion:create-program shaders)
  (let ((program-id (glCreateProgram))
        (program-status* (alloc-GLint* 1)))
   (for-each (lambda (s) (glAttachShader program-id s)) shaders)
   (glLinkProgram program-id)
   (glGetProgramiv program-id GL_LINK_STATUS program-status*)
   (if (= GL_FALSE (*->GLint program-status*))
       (let ((info-log-length* (alloc-GLint* 1)))
         (glGetProgramiv program-id GL_INFO_LOG_LENGTH info-log-length*)
         (let* ((info-log-length (*->GLint info-log-length*))
                (info-log* (alloc-GLchar* info-log-length)))
           (fusion:error "in fusion:create-shader. Implement ERROR RETRIEVAL.")
           ;;(glGetProgramInfoLog program-id info-log-length #f info-log*)
           ;;(error (string-append "GL Shading Language linkage -- " (char*->string info-log*)))
           )))
   (for-each (lambda (s) (glDetachShader program-id s)) shaders)
   program-id))
