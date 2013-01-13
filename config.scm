(sphere: "fusion")
(dependencies:
 (sfusion
  (load
   (energy: program-arguments)))
 (gl-cairo
  (load
   (sdl2: sdl2)
   (cairo: cairo)
   (cond-expand (mobile (opengl: gl-es)
                        (opengl: gl-es-ext))
                (else (opengl: gl))))))
