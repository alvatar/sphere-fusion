;-------------------------------------------------------------------------------
; Configuration
;-------------------------------------------------------------------------------

(include "~~base/prelude#.scm")
(%include playground: sake)

(define project-name (make-parameter "example"))

(define project-modules
  '(main
    nested/test))

;-------------------------------------------------------------------------------
; Tasks
;-------------------------------------------------------------------------------

(define-task init ()
  (if (file-exists? (playgroynd-setup-directory))
      (error "It appears that the project has been initialized, please execute task \"force-init\" to re-initialize it")
      (playground-setup)))

(define-task clean ()
  (playground-clean))

(define-task force-init ()
  (playground-clean)
  (playground-setup))

(define-task run ()
  '())

(define-task update ()
  (if (file-exists? (playground-setup-directory))
      (playground-update)
      (playground-setup)))

(define-task android ()
  (unless (file-exists? (playground-setup-directory))
          (playground-setup))
  ;(android-add-asset-to-apk "logo.png")
  ;(android-add-asset-directory-to-apk "images")
  (android-generate-manifest-and-properties
   api-level: 8
   app-name: "Example Playground App")
  ;(android-generate-custom-manifest "")
  ;(android-generate-custom-local-properties "")
  ;(android-generate-custom-project-properties "")
  (android-compile-and-link
   modules: project-modules
   report-scheme: #f))

(define-task android-upload-assets ()
  (android-upload-file-to-sd "music.mp3"))
