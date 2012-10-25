(sphere: "fusion")

(paths:
 (base: "/data/projects/scheme-base")
 (sdl2: "/data/projects/scheme-sdl2"))

(dependencies:
 (driver
  (include
   (sdl2: sdl2#))
  (load
   (sdl2: sdl2)))
 ((= driver version: (debug))
  (include
   (sdl2: sdl2#))
  (load
   (sdl2: sdl2 version: (debug))))
 (gl-cairo
  (include
   (sdl2: sdl2#))
  (load
   (sdl2: sdl2)
   (cairo: cairo)
   (cond-expand (mobile (opengl: gl-es))
                (else (opengl: gl)))))
 ((= gl-cairo version: (debug))
  (include
   (sdl2: sdl2#))
  (load
   (sdl2: sdl2 version: (debug))
   (cairo: cairo version: (debug))
   (cond-expand (mobile (opengl: gl-es version: (debug)))
                (else (opengl: gl version: (debug)))))))
