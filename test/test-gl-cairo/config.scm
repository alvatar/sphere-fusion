(sphere: "test-gl-cairo")
(paths:
 (fusion: "/data/projects/scheme-fusion"))
(dependencies:
 (main
  (load
   (fusion: gl-cairo)
   (base: debug/debuggee)))
 ((= main version: (debug))
  (load
   (base: debug/debuggee version: (debug))
   (fusion: gl-cairo version: (debug)))))
