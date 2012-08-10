;-------------------------------------------------------------------------------
; Configuration
;-------------------------------------------------------------------------------

(include "~~base/prelude#.scm")
(%include sake)

(define project-name "playground-prototype")

;-------------------------------------------------------------------------------
; Host OS: Windows, Mac, Linux
;-------------------------------------------------------------------------------

(define project-modules
  '("driver" "main"))

(define includes "-I/usr/include/cairo -I/usr/include/freetype2 -I/usr/local/include/SDL2 -I/usr/include/GL")

(define libraries "-lcairo -lGL -L/usr/local/lib/ -lSDL2")

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
   ((playground-setup-directory ""))
   (make-directory (lib-directory))
   (make-directory (android-build-directory))))

(define-task android:clean ()
  (task-run android:apptest-clean)
  (parameterize
   ((playground-setup-directory ""))
   (delete-file (android-build-directory))
   (delete-file (lib-directory))
   (delete-file "tmp")
   (android-clean)))

(define-task android:prepare (android:init)
  (parameterize
   ((playground-setup-directory ""))
   ;; Generate C files (only those belonging to playground library)
   (android-select-and-generate-modules (android-base-modules))
   ;; Copy generated C files to Android directories
   (android-install-c-files (android-base-modules))))

(define test-app-modules '((playground: app-test)))

(define-task android:apptest (android:prepare)
  (parameterize
   ((playground-setup-directory "tmp/"))
   (unless (playground-ready?)
           (playground-setup))
   (android-select-and-generate-modules test-app-modules)
   (android-compile-and-link modules: test-app-modules)))

(define-task android:apptest-clean ()
  (delete-file "tmp"))

;-------------------------------------------------------------------------------
; Common
;-------------------------------------------------------------------------------

(define-task clean (android:clean host:clean)
  (delete-file (current-build-directory)))
