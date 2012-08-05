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

(define-task host-init ()
  (make-directory (current-build-directory)))

(define-task host-clean ()
  (make-directory (current-build-directory)))

(define-task run (host-init)
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

(define test-app-modules '((playground: app-test)))

(define-task android-init ()
  (parameterize
   ((playground-setup-directory ""))
   (make-directory (lib-directory))
   (make-directory (android-build-directory))))

(define-task android-clean ()
  (task-run android-apptest-clean)
  (parameterize
   ((playground-setup-directory ""))
   (delete-file (android-build-directory))
   (delete-file (lib-directory))
   (delete-file "tmp")
   (gambit-eval-here
   '(shell-command "ant -s android/build.xml clean"))))

(define-task android-prepare (android-init)
  (parameterize
   ((playground-setup-directory ""))
   ;; Generate C files (only those belonging to playground library)
   (android-generate-project-c-files
    (let recur ((modules (android-base-modules)))
      (cond ((null? modules) '())
            ((eq? (caar modules) playground:)
             (cons (car modules) (recur (cdr modules))))
            (else (recur (cdr modules))))))
   ;; Copy generated C files to Android directories
   (for-each
    (lambda (m) (copy-file
            (string-append (%module-path-lib m) (%module-filename-c m))
            (string-append (android-build-directory)
                           (%module-filename-c m))))
    (android-base-modules))))

(define-task android-apptest (android-prepare)
  (parameterize
   ((playground-setup-directory "tmp/"))
   (unless (file-exists? (playground-setup-directory))
           (setup-playground))
   (android-generate-project-c-files test-app-modules)
   (android-compile-and-link modules: test-app-modules)))

(define-task android-apptest-clean ()
  (delete-file "tmp"))

(define-task android (android-prepare)
  (gambit-eval-here
   '(shell-command "ant -s android/build.xml clean debug install")))

;-------------------------------------------------------------------------------
; Common
;-------------------------------------------------------------------------------

(define-task clean (android-clean host-clean)
  (delete-file (current-build-directory)))
