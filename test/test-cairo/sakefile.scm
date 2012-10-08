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
  (fusion:android-import-addon '(cairo))
  (fusion:android-compile-and-link
   compile-modules: '(main)
   import-modules: '((cairo: cairo)
                     (base: debug/debuggee)))
  (fusion:android-run-app))

(define-task all (android)
  'all)
