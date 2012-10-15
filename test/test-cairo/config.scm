(sphere: "test-cairo")
(paths:
 (fusion: "/data/projects/scheme-fusion"))
(dependencies:
 (main
  (include
   (cairo: cairo#))
  (load
   (cairo: cairo)
   (base: debug/debuggee)))
 ((test-cairo: main version: (debug))
  (include
   (cairo: cairo#))
  (load
   (cairo: cairo version: (debug))
   (base: debug/debuggee version: (debug)))))
