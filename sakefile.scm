;-------------------------------------------------------------------------------
; Configuration
;-------------------------------------------------------------------------------

(include "~~spheres/prelude#.scm")
(%include fusion-sake#)

;-------------------------------------------------------------------------------
; Host OS: Windows, Mac, Linux
;-------------------------------------------------------------------------------

;; TODO

;-------------------------------------------------------------------------------
; Android
;-------------------------------------------------------------------------------

(define-task android:compile ()
  (parameterize
   ((fusion-setup-directory ""))
   (make-directory (default-lib-directory))
   (make-directory (android-build-directory))
   ;; Generate internal Fusion modules
   (fusion:android-generate-modules
    (fusion:select-modules (android-base-modules) spheres: 'fusion))
   (fusion:android-generate-modules
    (fusion:select-modules (android-base-debug-modules) spheres: 'fusion))
   ;; Copy all versions of the spheres' generated C files to Android directories
   (fusion:android-install-c-files (android-base-modules))
   (fusion:android-install-c-files (android-base-debug-modules))))

(define-task android:clean ()
  (parameterize
   ((fusion-setup-directory ""))
   (delete-file (android-build-directory))
   (fusion:android-clean)))

;-------------------------------------------------------------------------------
; Common
;-------------------------------------------------------------------------------

(define-task compile (android:compile)
  'init)

(define-task clean (android:clean)
  (delete-file (current-build-directory))
  'clean)

(define-task all (init)
  'all)