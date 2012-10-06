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
  (fusion:android-import-addon '(cairo gl))
  (fusion:android-compile-and-link
   compile-modules: '(main)
   import-modules: '((cairo: cairo)
                     (opengl: gl-es))))

(define-task all (android)
  'all)
