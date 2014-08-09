;;-------------------------------------------------------------------------------
;; App

(define app
  (make-app
   create-world:
   (lambda (world)
     (let ((texture1 (make-texture 'texture1 "assets/images/texture1.png"))
           (texture2 (make-texture 'texture2 "assets/images/texture2.png"))
           (texture3 (make-texture 'texture3 "assets/images/texture3.png")))
       (make-world (list (make-sprite 20.0 170.0 texture1
                                      on-mouseup: (lambda (self world event)
                                                    (SDL_Log "MOUSE UP")
                                                    (world-update world 'sprites
                                                                  (cons (make-sprite 20.0 670.0 texture3)
                                                                        (world-sprites world))))
                                      on-mouseover: (lambda args (SDL_Log "MOUSE OVER"))
                                      on-mouseout: (lambda args (SDL_Log "MOUSE OUT"))
                                      on-mousemove: (lambda args (SDL_Log "MOUSE MOVE")))
                         (make-sprite 20.0 420.0 texture2
                                      on-mouseup: (lambda (self world event)
                                                    (world-update world 'sprites
                                                                  (delete self (world-sprites world)))))))))
   pre-render: (let ((color-r (random-real))
                     (op +))
                 (lambda (world)
                   (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
                   (glEnable GL_BLEND)
                   (glDisable GL_CULL_FACE)
                   (glCullFace GL_BACK)
                   (apply glClearColor (list color-r 0.0 0.8 1.0))
                   (glClear (bitwise-ior GL_COLOR_BUFFER_BIT))
                   (cond ((>= color-r 1.0) (set! op -))
                         ((<= color-r 0.0) (set! op +)))
                   (set! color-r (op color-r 0.02))))
   post-render: (lambda (world) (SDL_Delay 100))))

(app)


