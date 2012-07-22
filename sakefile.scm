;-------------------------------------------------------------------------------
; Configuration
;-------------------------------------------------------------------------------

(include "~~prelude/prelude#.scm")

(define project-name "playground-prototype")

;-------------------------------------------------------------------------------
; Host OS: Windows, Mac, Linux
;-------------------------------------------------------------------------------

(define project-modules
  '("driver" "fib"))

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
      (include "~~prelude/prelude#.scm")
      (%load base: ffi)
      (%load math: math)
      (%load cairo: cairo)
      (%load opengl: gl)
      (%load sdl2: sdl2)
      (%load main))))

;-------------------------------------------------------------------------------
; Android
;-------------------------------------------------------------------------------

;;; Configuration

(define android-build-directory
  "android/jni/build/")

(define android-modules
  '("driver" "fib"))

(define arm-modules
  '((base: ffi)
    (math: math)
    (opengl: gl-es)
    (sdl2: sdl2)))

;;; Filenames and paths
  
(define android-modules-filenames
  (map (lambda (m) (string-append m ".c"))
       android-modules))

(define android-modules-relative-paths
  (map (lambda (m) (string-append android-build-directory m ".c"))
       android-modules))

(define arm-modules-filenames
  (map (lambda (m) (receive (path file-name)
                       (module-path (car m) (cadr m))
                       (string-append (keyword->string (car m)) "_" file-name)))
       arm-modules))

(define arm-modules-relative-paths
  (map (lambda (m fn) (receive (path file-name)
                       (module-path (car m) (cadr m))
                       (string-append android-build-directory fn)))
       arm-modules
       arm-modules-filenames))

(define link-module-filename
  (string-append project-name "_.c"))

;;; Tasks

(define-task android-init ()
  (make-directory android-build-directory))

(define-task android-clean ()
  (gambit-eval-here
   '(shell-command "ant -s android/build.xml clean")))

(define-task android-generate-mk ()
  (call-with-output-file
      "android/jni/build/Android.mk"
    (lambda (file)
      (display
       (string-append
        "
LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)

LOCAL_MODULE := main
LOCAL_SRC_FILES :=  \\
../SDL/src/main/android/SDL_android_main.cpp \\
"
        (string-append
         (let recur ((files (append arm-modules-filenames android-modules-filenames))
                     (str ""))
           (if (null? files)
               str
               (recur (cdr files)
                      (string-append str
                                     (car files)
                                     " \\
"))))
         link-module-filename)
"
LOCAL_CFLAGS += -O2 -fno-short-enums -Wno-missing-field-initializers -I./gambit -I. -I./SDL/include
LOCAL_LDLIBS := -ldl -fno-short-enums -lc -landroid -llog -lEGL -lGLESv1_CM -L./gambit -lgambc
LOCAL_SHARED_LIBRARIES := SDL2
#LOCAL_STATIC_LIBRARIES := android_native_app_glue

include $(BUILD_SHARED_LIBRARY)

$(call import-module,android/native_app_glue)

")
       file))))

(define-task android-compile (android-init android-generate-mk)
  (for-each
    (lambda (m fn)
      (receive (path file-name)
                       (module-path (car m) (cadr m))
                       (copy-file (string-append path file-name)
                                  (string-append (current-directory) android-build-directory fn))))
      arm-modules
      arm-modules-filenames)
  (gambit-eval-here
   `(begin
      (define-cond-expand-feature arm)
      (include "~~prelude/prelude#.scm")
      (let ((exe-file (string-append ,android-build-directory ,project-name)))
        (for-each
         (lambda (m)
           (compile-file-to-target
            (string-append "android/jni/" m)
            options: '(report)
            output: (string-append ,android-build-directory m ".c")))
         ',android-modules)
        (link-incremental ',(append arm-modules-relative-paths android-modules-relative-paths)
                          output: (string-append ,android-build-directory ,project-name "_.c"))))))

(define-task android (android-compile)
  (gambit-eval-here
   '(shell-command "ant -s android/build.xml clean debug install")))

;-------------------------------------------------------------------------------
; Common
;-------------------------------------------------------------------------------

(define-task clean (android-clean host-clean)
  (delete-file (current-build-directory)))
