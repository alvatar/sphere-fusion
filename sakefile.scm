;-------------------------------------------------------------------------------
; Configuration
;-------------------------------------------------------------------------------

(include "~~spheres/prelude#.scm")
(%include fusion-sake#)

;-------------------------------------------------------------------------------
; Host OS: Windows, Mac, Linux
;-------------------------------------------------------------------------------

(define-task host:test-gl ()
  (gambit-eval-here
   `(begin
      (define-cond-expand-feature sdl)
      (include "~~spheres/prelude#.scm")
      (%load base: ffi)
      (%load sdl2: sdl2)
      (%load test-gl))))

;-------------------------------------------------------------------------------
; Android
;-------------------------------------------------------------------------------

(define-task android:init ()
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
   (fusion:android-clean))
  (task-run android:test-clean))

(define-task android:test-gl-es (android:init)
  (let ((modules '(test-gl-es))
        (provided-modules '((base: debug/debuggee version: (debug))
                            (opengl: gl-es version: (debug)))))
    (parameterize
     ((fusion-setup-directory "tmp/"))
     (unless (fusion:ready?)
             (fusion:setup))
     ;; Compile the app (takes care of generating necessary C files)
     (fusion:android-compile-and-link compile-modules: modules
                                      import-modules: provided-modules
                                      compiler-options: '(debug)))))

(define-task android:test (android:test-gl-es)
  'android:test)

(define-task android:test-clean ()
  (delete-file "tmp"))

;-------------------------------------------------------------------------------
; Common
;-------------------------------------------------------------------------------

(define-task init (android:init)
  'init)

(define-task clean (android:clean)
  (delete-file (current-build-directory))
  'clean)

(define-task test (android:test)
  'test)

(define-task all (init)
  'all)