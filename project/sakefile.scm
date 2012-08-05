;-------------------------------------------------------------------------------
; Configuration
;-------------------------------------------------------------------------------

(include "~~base/prelude#.scm")
(%include playground: sake)

(define project-name (make-parameter "example"))

(define project-modules
  '(main))

;-------------------------------------------------------------------------------
; Tasks
;-------------------------------------------------------------------------------

(define-task init ()
  (if (file-exists? (playgroynd-setup-directory))
      (error "It appears that the project has been initialized, please execute task \"force-init\" to re-initialize it")
      (setup-playground)))

(define-task clean ()
  (clean-playground))

(define-task force-init ()
  (clean-playground)
  (setup-playground))

(define-task run ()
  '())

(define-task android ()
  (unless (file-exists? (playground-setup-directory))
          (setup-playground))
  ;(android-add-asset-to-apk "logo.png")
  ;(android-add-asset-directory-to-apk "images")
  (android-generate-manifest-and-properties
   api-level: 8
   app-name: "Example Playground App")
  ;(android-generate-custom-manifest "")
  ;(android-generate-custom-local-properties "")
  ;(android-generate-custom-project-properties "")
  (android-compile-with-modules
   modules: project-modules
   report-scheme: #f))

(define-task android-upload-assets ()
  (android-upload-file-to-sd "music.mp3"))
