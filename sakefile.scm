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

(define-task run (host:init)
  (gambit-eval-here
   `(begin
      (define-cond-expand-feature sdl)
      (include "~~base/prelude#.scm")
      (%load base: ffi)
      (%load math: math)
      (%load opengl: gl)
      (%load sdl2: sdl2)
      (%load main))))

;-------------------------------------------------------------------------------
; Android
;-------------------------------------------------------------------------------

(define-task android:init ()
  (parameterize
   ((fusion-setup-directory ""))
   (make-directory (lib-directory))
   (make-directory (android-build-directory))))

(define-task android:clean ()
  (task-run android:apptest-clean)
  (parameterize
   ((fusion-setup-directory ""))
   (delete-file (android-build-directory))
   (delete-file (lib-directory))
   (delete-file "tmp")
   (android-clean)))

(define-task android:prepare (android:init)
  (parameterize
   ((fusion-setup-directory ""))
   ;; Generate C files (only those belonging to fusion library)
   ;; TODO: Instead of select and generate, do (android-generate-modules (android-fusion-internal-modules)
   (android-select-and-generate-modules (android-base-modules))
   ;; Copy generated C files to Android directories
   (android-install-c-files (android-base-modules))))

(define test-app-modules
  '((base: debug/debuggee)
    (fusion: app-test)))

(define-task android:apptest (android:prepare)
  (parameterize
   ((fusion-setup-directory "tmp/"))
   (unless (fusion-ready?)
           (fusion-setup))
   ;; HERE: You really need to select? Or at lest you can have a simpler "android-generate-modules" too
   (android-select-and-generate-modules test-app-modules)
   (android-compile-and-link modules: test-app-modules options: '(debug))))

(define-task android:apptest-clean ()
  (delete-file "tmp"))

;-------------------------------------------------------------------------------
; Common
;-------------------------------------------------------------------------------

(define-task clean (android:clean host:clean)
  (delete-file (current-build-directory)))
