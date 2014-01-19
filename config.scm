(sphere: "fusion")
(dependencies:
 (sfusion
  (include
   (core: base-macros)
   (energy: functional-arguments-macros))
  (load
   (core: functional)
   (fabric: algorithm/list)
   (energy: error-code)
   (energy: filesystem)
   (energy: functional-arguments)
   (energy: log)
   (energy: template)))
 (core
  (load
   (opengl: gl)
   (sdl2: sdl2))))
