(sphere: "my-app")
(dependencies:
 (loader
  (include
   (core: base-macros))
  (load
   (energy: remote/debuggee)
   (fusion: spheres-remote)
   (sdl2: sdl2)
   (sdl2: sdl2-image)
   (cond-expand
    ((or android ios)
     (opengl: gl-es2)
     (fusion: ios))
    (else
     (opengl: gl)))
   (math: matrix)
   (fabric: algorithm/list)))
 (app
  (include
   (core: base-macros)
   (core: assert-macros)
   (= engine-types))
  (load
   (cond-expand
    ;; iOS static compilation
    ((and ios static)
     (energy: remote/debuggee)
     (fusion: spheres-remote)
     (sdl2: sdl2)
     (sdl2: sdl2-image)
     (opengl: gl-es2)
     (fusion: ios)
     (math: matrix)
     (fabric: algorithm/list)
     (= globals)
     (= gl-utils)
     (= engine-types)
     (= engine))
    ;; iOS development
    (ios
     (= globals)
     (= gl-utils)
     (= engine))
    ;; Host static compilation
    ((and host static)
     (energy: remote/debuggee)
     (sdl2: sdl2)
     (sdl2: sdl2-image)
     (opengl: gl)
     (math: matrix)
     (fabric: algorithm/list)
     (= globals)
     (= gl-utils)
     (= engine-types)
     (= engine))
    ;; Host development
    (else
     (= globals)
     (= gl-utils)
     (= engine)))))
 (globals
  (include
   (core: base-macros)))
 (gl-utils
  (include
   (core: base-macros)))
 (engine-types
  (include
   (core: base-macros)))
 (engine
  (include
   (core: base-macros))))
