(sphere: "fusion")

(paths:
 (base: "/data/projects/scheme-base")
 (sdl2: "/data/projects/scheme-sdl2"))

(dependencies:
 (driver
  (load
   (sdl2: sdl2)))
 ((= driver version: (debug))
  (load
   (sdl2: sdl2 version: (debug))))
 (gl-cairo
  (include
   (base: ffi-macros)
   (sdl2: sdl2-macros))
  (load
   (base: ffi)
   (sdl2: sdl2)
   (cairo: cairo)
   (cond-expand (mobile (opengl: gl-es))
                (else (opengl: gl)))))
 ((= gl-cairo version: (debug))
  (include
   (base: ffi-macros)
   (sdl2: sdl2-macros))
  (load
   (base: ffi version: (debug))
   (sdl2: sdl2 version: (debug))
   (cairo: cairo version: (debug))
   (cond-expand (mobile (opengl: gl-es version: (debug)))
                (else (opengl: gl version: (debug)))))))
