(sphere: "minimal")
(dependencies:
 (main
  (prelude
   (core: ffi-header))
  (load (= app)))
 (app
  (include
   (core: base-macros)
   (core: assert-macros))
  (load
   (core: functional)
   (energy: repl-server))))

