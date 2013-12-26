(include "~~spheres/fusion/src/sake-fusion.scm")

;;------------------------------------------------------------------------------

;;!! Tasks

(define-task host:run ()
  (fusion#host-run-interpreted '(minimal: main))) 

(define-task host:compile ()
  (error "Executable compilation NOT implemented yet.")
  (fusion#host-compile-and-link '(main)))

(define-task all (host:run)
  'all)
