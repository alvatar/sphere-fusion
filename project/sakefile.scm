;-------------------------------------------------------------------------------
; Configuration
;-------------------------------------------------------------------------------

(include "~~base/prelude#.scm")
(%include fusion: sake-utils#)

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
      (fusion-setup)))

(define-task clean ()
  (fusion-clean)
  (delete-file (current-build-directory)))

(define-task force-init ()
  (fusion-clean)
  (fusion-setup))

(define-task run ()
  '())

(define-task update ()
  (if (file-exists? (fusion-setup-directory))
      (fusion-update)
      (fusion-setup)))

(define-task android ()
  (unless (file-exists? (fusion-setup-directory))
          (fusion-setup))
  ;(android-add-asset-to-apk "logo.png")
  ;(android-add-asset-directory-to-apk "images")
  (android-generate-manifest-and-properties
   api-level: 8
   app-name: "Example Fusion App")
  ;(android-generate-custom-manifest "")
  ;(android-generate-custom-local-properties "")
  ;(android-generate-custom-project-properties "")
  (android-compile-and-link
   modules: project-modules
   report-scheme: #f))

(define-task android-upload-assets ()
  (android-upload-file-to-sd "music.mp3"))
