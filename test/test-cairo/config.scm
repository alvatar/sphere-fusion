(sphere: "test-cairo")
(paths:
 (fusion: "/data/projects/scheme-fusion"))
(dependencies:
 (main
  (include
   (cairo: cairo#))
  (load
   (sdl2: sdl2)
   (cairo: cairo)
   (base: debug/debuggee)))
 ((test-cairo: main version: (debug))
  (include
   (cairo: cairo#))
  (load
   (sdl2: sdl2 version: (debug))
   (cairo: cairo version: (debug))
   (base: debug/debuggee version: (debug)))))
