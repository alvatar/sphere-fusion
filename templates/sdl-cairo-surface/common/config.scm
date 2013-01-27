(sphere: "test-gl-cairo")
(dependencies:
 (main
  (load
   (= gl-cairo)))
 (gl-cairo
  (include
   (core: base-macros))
  (load
   (sdl2: sdl2)
   (cairo: cairo)
   (cond-expand (mobile (opengl: gl-es)
                        (opengl: gl-es-ext))
                (else (opengl: gl))))))
