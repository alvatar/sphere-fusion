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
   (opengl: gl)))
 ((= gl-cairo version: (debug))
  (include
   (sdl2: sdl2#))
  (load
   (sdl2: sdl2 version: (debug))
   (cairo: cairo version: (debug))
   (opengl: gl version: (debug)))))
