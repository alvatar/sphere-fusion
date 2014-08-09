(sphere: "sdl-opengl")
(dependencies:
 (main
  (load (= app)
        (core: ffi)))
 (app
  (include
   (core: base-macros)
   (core: assert-macros))
  (load
   (energy: remote/debuggee)
   (sdl2: sdl2)
   (cond-expand
    (android
     (opengl: gl-es2))
    (else
     (opengl: gl)))
   (math: matrix))))
