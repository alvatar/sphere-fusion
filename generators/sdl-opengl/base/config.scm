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
   (math: matrix)
   (fusion: core))))
