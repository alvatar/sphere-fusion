(sphere: "test-gl-cairo")
(paths:
 (fusion: "/data/projects/scheme-fusion"))
(dependencies:
 (main
  (include
   (cairo: cairo#))
  (load
   (fusion: gl-cairo)
   (base: debug/debuggee)))
 ((= main version: (debug))
  (include
   (cairo: cairo#))
  (load
   (base: debug/debuggee version: (debug))
   (fusion: gl-cairo version: (debug)))))
