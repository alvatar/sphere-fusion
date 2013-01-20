(sphere: "fusion")
(dependencies:
 (sfusion
  (include
   (core: base-macros)
   (energy: program-arguments-macros))
  (load
   (fabric: algorithm/list)
   (energy: error-code)
   (energy: functional)
   (energy: program-arguments)
   (energy: template)))
 (gl-cairo
  (include
   (core: base-macros))
  (load
   (sdl2: sdl2)
   (cairo: cairo)
   (cond-expand (mobile (opengl: gl-es)
                        (opengl: gl-es-ext))
                (else (opengl: gl))))))
