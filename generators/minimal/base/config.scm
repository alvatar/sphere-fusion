(sphere: "minimal")
(dependencies:
 (main
  (include
   (core: ffi-macros))
  (prelude
   (core: ffi-prelude))
  (load
   (= app)))
 (app
  (include
   (core: base-macros)
   (core: assert-macros))
  (load
   (core: base))))
