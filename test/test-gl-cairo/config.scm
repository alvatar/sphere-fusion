(sphere: "test-gl-cairo")
(paths:
 (fusion: "/data/projects/scheme-fusion"))
(dependencies:
 (main
  (include
   (sdl2: sdl2#))
  (load
   (fusion: gl-cairo)
   (base: debug/debuggee)))
 ((= main version: (debug))
  (include
   (sdl2: sdl2#))
  (load
   (base: debug/debuggee version: (debug))
   (fusion: gl-cairo version: (debug)))))
