;-------------------------------------------------------------------------------
; Configuration
;-------------------------------------------------------------------------------

(include "~~spheres/prelude#.scm")
(%include fusion: fusion-sake#)

;-------------------------------------------------------------------------------
; Tasks
;-------------------------------------------------------------------------------

(define-task android ()
  (fusion:setup)
  (fusion:android-generate-manifest-and-properties
   api-level: 8
   app-name: "Example Fusion App")
  (fusion:android-import-addon '(gl))
  (fusion:android-compile-and-link
   compile-modules: '(main)
   import-modules: '((opengl: gl-es version: (debug)))
   compiler-options: '(debug)))

(define-task all (android)
  'all)
