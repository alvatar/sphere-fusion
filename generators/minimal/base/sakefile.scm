(include "~~spheres/fusion/src/sake-fusion.scm")

;;------------------------------------------------------------------------------

;;!! Tasks

(define-task host:run ()
  (fusion#host-run-interpreted '(minimal: main))) 

(define-task host:compile ()
  (fusion#host-compile-exe "main" 'main))

(define-task all (host:run)
  'all)
