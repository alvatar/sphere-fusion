(include "/data/projects/sphere-fusion/src/sake-fusion.scm")

;-------------------------------------------------------------------------------
; Tasks
;-------------------------------------------------------------------------------

(define-task android ()
  (fusion:setup)
  (fusion:android-generate-manifest-and-properties
   api-level: 8
   app-name: "Cairo-OpenGL Fusion App")
  (fusion:android-import-addon '(cairo gl))
  (fusion:android-compile-and-link '(main))
  (fusion:android-run-app))

(define-task desktop:run ()
  (fusion:desktop-run-interpreted 'main))

(define-task desktop:compile ()
  (fusion:desktop-compile-and-link '(main)))

(define-task all (desktop:compile android)
  'all)
