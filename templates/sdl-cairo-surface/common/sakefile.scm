(include "/data/projects/sphere-fusion/src/sake-fusion.scm")

;-------------------------------------------------------------------------------
; Tasks
;-------------------------------------------------------------------------------

(define-task desktop:run ()
  (fusion:desktop-run-interpreted '(test-gl-cairo: main version: (debug))))

(define-task desktop:compile ()
  (fusion:desktop-compile-and-link '(main)))

(define-task all (desktop:compile)
  'all)
