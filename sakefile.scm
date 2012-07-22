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

(define android-current-build-directory
  "android/jni/build/")

(define android-modules
  '("fib"))
  ;'("driver" "fib"))

#;
(define arm-modules
  '((base: ffi)
    (math: math)
    (cairo: cairo)
    (opengl: gl-es)
    (sdl2: sdl2)))

;;; Path and module searching
  
(define arm-modules
  '())

(define here-modules
  (map (lambda (m) (string-append android-current-build-directory m ".c"))
       android-modules))

(define external-modules
  (map (lambda (m) (receive (path file-name)
                       (module-path (car m) (cadr m))
                       (string-append path file-name)))
       arm-modules))

(define link-module
  (string-append project-name "_.c"))

;;; Tasks

(define-task android-init ()
  (parameterize
   ((current-build-directory android-current-build-directory))
   (make-directory (current-build-directory))))

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
         (let recur ((files android-modules)
                     (str ""))
           (if (null? files)
               str
               (recur (cdr files)
                      (string-append str
                                     (car files)
                                     ".c \\
"))))
         link-module)
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
  (gambit-eval-here
   `(begin
      (define-cond-expand-feature arm)
      (include "~~prelude/prelude#.scm")
      (let ((exe-file (string-append ,android-current-build-directory ,project-name)))
        (for-each
         (lambda (m)
           (compile-file-to-target
            (string-append "android/jni/" m)
            options: '(report)
            output: (string-append ,android-current-build-directory m ".c")))
         ',android-modules)
        (link-incremental ',(append external-modules here-modules)
                          output: (string-append ,android-current-build-directory ,project-name "_.c"))))))

(define-task android (android-compile)
  (gambit-eval-here
   '(shell-command "ant -s android/build.xml clean debug install")))

;-------------------------------------------------------------------------------
; Common
;-------------------------------------------------------------------------------

(define-task clean (android-clean host-clean)
  (delete-file (current-build-directory)))
