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
  (include
   (core: base-macros))
  (load
   (cond-expand
    (mobile (opengl: gl-es2))
    (else (opengl: gl)))
   (sdl2: sdl2))))
