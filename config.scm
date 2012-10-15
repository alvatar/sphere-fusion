(sphere: "fusion")
(paths:
 (base: "/data/projects/scheme-base")
 (sdl2: "/data/projects/scheme-sdl2"))
(dependencies:
 (driver (include
          (sdl2: sdl2#))
         (load
          (sdl2: sdl2)))
 ((fusion: driver version: (debug))
  (include
   (sdl2: sdl2# version: (debug)))
  (load
   (sdl2: sdl2 version: (debug)))))
