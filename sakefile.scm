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
    '((fusion: driver)))
   (fusion:android-generate-modules
    '((fusion: driver version: (debug))))
   ;; Copy all versions of the spheres' generated C files to Android directories
   (fusion:android-install-modules-c-files
    (%module-deep-dependencies-to-load
     '(fusion: driver)))
   (fusion:android-install-modules-c-files
    (%module-deep-dependencies-to-load
     '(fusion: driver version: (debug))))))

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

(define-task all (compile)
  'all)
