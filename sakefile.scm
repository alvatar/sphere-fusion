;-------------------------------------------------------------------------------
; Configuration
;-------------------------------------------------------------------------------

(include "~~base/prelude#.scm")
(%include sake-utils#)

(define project-name "fusion-prototype")

;-------------------------------------------------------------------------------
; Host OS: Windows, Mac, Linux
;-------------------------------------------------------------------------------

(define project-modules
  '("driver" "main"))

(define include-flags "-I/usr/include/cairo -I/usr/include/freetype2 -I/usr/local/include/SDL2 -I/usr/include/GL")

(define library-flags "-lcairo -lGL -L/usr/local/lib/ -lSDL2")

(define flags "-w")

(define-task host:init ()
  (make-directory (current-build-directory)))

(define-task host:clean ()
  (make-directory (current-build-directory)))

(define-task host:test-gl (host:init)
  (gambit-eval-here
   `(begin
      (define-cond-expand-feature sdl)
      (include "~~base/prelude#.scm")
      (%load base: ffi)
      (%load sdl2: sdl2)
      (%load test-gl))))

;-------------------------------------------------------------------------------
; Android
;-------------------------------------------------------------------------------

(define-task android:init ()
  (parameterize
   ((fusion-setup-directory ""))
   (make-directory (lib-directory))
   (make-directory (android-build-directory))
   ;; Generate internal Fusion modules
   (fusion:android-generate-modules
    (fusion:select-modules (android-base-modules) libraries: 'fusion))
   (fusion:android-generate-modules
    (fusion:select-modules (android-base-modules) libraries: 'fusion)
    options: '(debug))
   ;; Copy generated C files to Android directories
   (fusion:android-install-c-files (android-base-modules))
   (fusion:android-install-c-files (android-base-modules) features: '(debug))))

(define-task android:clean ()
  (parameterize
   ((fusion-setup-directory ""))
   (delete-file (android-build-directory))
   (delete-file (lib-directory))
   (delete-file "tmp")
   (fusion:android-clean))
  (task-run android:tests-clean))

(define-task android:test-gl-es (android:init)
  (let ((modules '(test-gl-es))
        (provided-modules '((base: debug/debuggee)
                            (opengl: gl-es))))
    (parameterize
     ((fusion-setup-directory "tmp/"))
     (unless (fusion:ready?)
             (fusion:setup))
     ;; Compile the app (takes care of generating necessary C files)
     (fusion:android-compile-and-link compile-modules: modules
                                      import-modules: provided-modules
                                      options: '(debug)))))

(define-task android:test (android:test-gl-es)
  'android:test)

(define-task android:tests-clean ()
  (delete-file "tmp"))

;-------------------------------------------------------------------------------
; Common
;-------------------------------------------------------------------------------

(define-task init (android:init)
  'init)

(define-task clean (android:clean host:clean)
  (delete-file (current-build-directory))
  'clean)

(define-task test (android:test)
  'test)