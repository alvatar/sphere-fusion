(sphere: "fusion")
(dependencies:
 (sfusion
  (include
   (core: base-macros))
  (load
   (fabric: algorithm/list)
   (energy: program-arguments)
   (energy: template)
   (energy: error-code)))
 (gl-cairo
  (load
   (sdl2: sdl2)
   (cairo: cairo)
   (cond-expand (mobile (opengl: gl-es)
                        (opengl: gl-es-ext))
                (else (opengl: gl))))))
