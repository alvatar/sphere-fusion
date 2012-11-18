(sphere: "test-gl-cairo")
(paths:
 (fusion: "/data/projects/scheme-fusion"))
(dependencies:
 (main
  (include
   (base: ffi-macros) ; This one should actually be handled by spheres
   (sdl2: sdl2-macros))
  (load
   (fusion: gl-cairo)
   (base: debug/debuggee)))
 ((= main version: (debug))
  (include
   (base: ffi-macros) ; This one should actually be handled by spheres
   (sdl2: sdl2-macros))
  (load
   (base: debug/debuggee version: (debug))
   (fusion: gl-cairo version: (debug)))))
