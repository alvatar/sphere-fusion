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
  (load
   (sdl2: sdl2)
   (cairo: cairo)
   (cond-expand (mobile (opengl: gl-es)
                        (opengl: gl-es-ext))
                (else (opengl: gl)))))
 ((= gl-cairo version: (debug))
  (load
   (sdl2: sdl2 version: (debug))
   (cairo: cairo version: (debug))
   (cond-expand (mobile (opengl: gl-es version: (debug))
                        (opengl: gl-es-ext version: (debug)))
                (else (opengl: gl version: (debug)))))))
