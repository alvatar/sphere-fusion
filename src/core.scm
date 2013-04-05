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
