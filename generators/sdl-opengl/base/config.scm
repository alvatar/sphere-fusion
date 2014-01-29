(sphere: "sdl-opengl")
(dependencies:
 (main
  (prelude (core: ffi-header))
  (load (= app)))
 (app
  (include
   (core: base-macros)
   (core: assert-macros))
  (load
   (energy: remote/debuggee)
   (math: matrix)
   (fusion: core))))
