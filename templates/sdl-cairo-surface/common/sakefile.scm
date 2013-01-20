(include "/data/projects/sphere-fusion/src/sake-fusion.scm")

;-------------------------------------------------------------------------------
; Tasks
;-------------------------------------------------------------------------------

(define-task desktop:run ()
  (fusion:desktop-run-interpreted 'main))

(define-task desktop:compile ()
  (fusion:desktop-compile-and-link '(main)))

(define-task all (desktop:compile)
  'all)
