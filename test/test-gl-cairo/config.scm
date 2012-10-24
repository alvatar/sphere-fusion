(sphere: "test-gl-cairo")
(paths:
 (fusion: "/data/projects/scheme-fusion"))
(dependencies:
 (main
  (include
   (cairo: cairo#))
  (load
   (sdl2: sdl2)
   (cairo: cairo)
   (opengl: gl-es)
   (base: debug/debuggee)))
 ((test-cairo: main version: (debug))
  (include
   (cairo: cairo#))
  (load
   (sdl2: sdl2 version: (debug))
   (cairo: cairo version: (debug))
   (opengl: gl-es)
   (base: debug/debuggee version: (debug)))))
