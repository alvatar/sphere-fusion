(sphere: "sdl-opengl-full")
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
   (fusion: core)
   (sdl2: sdl2-image)
   (sdl2: sdl2-mixer)
   (sdl2: sdl2-ttf)
   (= gui)
   (= audio)))
  (gui
   (include
    (core: base-macros))
   (load
    (math: matrix)
    (= gui-util)))
  (gui-util
   (include
    (core: base-macros)))
  (audio
   (load
    (sdl2: sdl2-mixer))))
