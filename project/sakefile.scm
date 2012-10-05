;-------------------------------------------------------------------------------
; Configuration
;-------------------------------------------------------------------------------

(include "~~spheres/prelude#.scm")
(%include fusion: fusion-sake#)

;-------------------------------------------------------------------------------
; Tasks
;-------------------------------------------------------------------------------

(define-task android ()
  (unless (file-exists? (fusion-setup-directory))
          (fusion:setup))
  ;(android-add-asset-to-apk "logo.png")
  ;(android-add-asset-directory-to-apk "images")
  (fusion:android-generate-manifest-and-properties
   api-level: 8
   app-name: "Example Fusion App")
  ;(android-generate-custom-manifest "")
  ;(android-generate-custom-local-properties "")
  ;(android-generate-custom-project-properties "")
  (fusion:android-compile-and-link
   compile-modules: '(main
                      nested/test)
   import-modules: '((opengl: gl-es))))

(define-task android-upload-assets ()
  (android-upload-file-to-sd "music.mp3"))
