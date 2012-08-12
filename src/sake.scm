;-------------------------------------------------------------------------------
; Setup and initialization
;-------------------------------------------------------------------------------

(define playground-setup-directory
  (make-parameter "playground/"))

(define lib-directory
  (make-parameter "lib/"))

(define src-directory
  (make-parameter "src/"))

(define android-directory-suffix
  (make-parameter "android/"))

(define (android-directory)
  (string-append (playground-setup-directory) (android-directory-suffix)))

(define android-jni-directory-suffix
  (make-parameter "jni/"))

(define (android-jni-directory)
  (string-append (android-directory) (android-jni-directory-suffix)))

(define android-build-directory-suffix
  (make-parameter "build/"))

(define (android-build-directory)
  (string-append (android-jni-directory) (android-build-directory-suffix)))

(define (android-manifest-file)
  (string-append (android-directory) "AndroidManifest.xml"))

(define (android-project-properties-file)
  (string-append (android-directory) "project.properties"))

(define android-base-modules
  (make-parameter '((base: ffi)
                    (sdl2: sdl2)
                    (opengl: gl-es)
                    (playground: driver))))

(define android-link-file
  (make-parameter "linkfile_.c"))

;;; Check whether playground is precompiled

(define (playground-precompiled?)
  (file-exists? (string-append (%library-path 'playground)
                               (android-directory-suffix)
                               (android-jni-directory-suffix)
                               (android-build-directory-suffix))))

;;; Clean all playground files for current project

(define (playground-clean)
  (delete-file (playground-setup-directory))
  (delete-file (lib-directory)))

;;; Update playground generated C files

(define (playground-update)
  (unless (playground-precompiled?)
          (playground-clean)
          (error "Prior to creating a Playground project, you need to run android:prepare in Playground Framework"))
  (copy-file (string-append (%library-path 'playground)
                            (android-directory-suffix)
                            (android-jni-directory-suffix)
                            (android-build-directory-suffix))
             (android-build-directory-suffix)))

;;; Setup playground files for current project

(define (playground-setup)
  (let* ((playground-path (%library-path 'playground))
         (opath (string-append playground-path (android-directory-suffix))))
    (playground-clean)
    (unless (playground-precompiled?)
            (playground-clean)
            (error "Prior to creating a Playground project, you need to run android:prepare in Playground Framework"))
    (playground-precompiled?)    
    (make-directory (playground-setup-directory))
    (make-directory (lib-directory))
    (make-directory (android-jni-directory))
    (copy-files (map (lambda (n) (string-append opath n))
                     '("build.xml"
                       "local.properties"
                       "proguard-project.txt"
                       "src"))
                (android-directory))
    (copy-files (map (lambda (n) (string-append opath n))
                     '("jni/Android.mk"
                       "jni/SDL"
                       "jni/build"
                       "jni/gambit"))
                (android-jni-directory))))

;;; Check whether playground is ready and setup

(define (playground-ready?)
  (file-exists? (playground-setup-directory)))

;-------------------------------------------------------------------------------
; Android
;-------------------------------------------------------------------------------

;;; Generate default Manifest and properties file

(define (android-generate-manifest-and-properties #!key
                                                  (api-level 8)
                                                  (app-name "Playground App"))
  (info "")
  (info "Generate Manifest and properties files")
  (info "")
  (call-with-output-file
      (android-manifest-file)
    (lambda (file)
      (display
       (string-append
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<manifest xmlns:android=\"http://schemas.android.com/apk/res/android\"
      package=\"org.libsdl.app\"
  android:versionCode=\"1\"
  android:versionName=\"1.0\"
  android:installLocation=\"preferExternal\">
  <!-- android:debuggable=\"true\" -->

  <uses-sdk android:minSdkVersion=\""
        (number->string api-level)
        "\" />
  <uses-permission android:name=\"android.permission.WRITE_EXTERNALSTORAGE\" />
  <uses-permission android:name=\"android.permission.WAKE_LOCK\" />
  <uses-permission android:name=\"android.permission.INTERNET\" />

  <application android:label=\""
        app-name
        "\">
    <!-- Our activity is the built-in NativeActivity framework class. 
         This will take care of integrating with our NDK code. -->
    <activity android:name=\"SDLActivity\"
      android:label=\""
        app-name
        "\"
      android:configChanges=\"orientation|keyboardHidden\">
      <!-- Tell NativeActivity the name of or .so -->
      <meta-data android:name=\"android.app.lib_name\"
        android:value=\"native-activity\" />
      <intent-filter>
        <action android:name=\"android.intent.action.MAIN\" />
        <category android:name=\"android.intent.category.LAUNCHER\" />
      </intent-filter>
    </activity>
  </application>
</manifest> 
"
        )
       file)))
  (call-with-output-file
      (android-project-properties-file)
    (lambda (file)
      (display
       (string-append "target=android-" (number->string api-level) "\n")
       file))))

;;; Generate Android.mk given a set of moduels and optional libraries

(define (android-generate-mk modules #!key (libraries #f))
  (info "")
  (info "Generate Android.mk")
  (info "")
  (let ((c-files (map %module-filename-c modules)))
    (call-with-output-file
        (string-append (android-build-directory) "Android.mk")
      (lambda (file)
        (display
         (string-append
          "LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)

LOCAL_MODULE := main
LOCAL_SRC_FILES := \\
../SDL/src/main/android/SDL_android_main.cpp \\
"
          (string-append
           (let recur ((files c-files)
                       (str ""))
             (if (null? files)
                 str
                 (recur (cdr files)
                        (string-append str
                                       (car files)
                                       " \\
"))))
           (android-link-file))
          "
LOCAL_CFLAGS += -O2 -fno-short-enums -Wno-missing-field-initializers -I./gambit -I. -I./SDL/include
LOCAL_LDLIBS := -ldl -fno-short-enums -lc -llog -lGLESv1_CM -L./gambit -lgambc
LOCAL_SHARED_LIBRARIES := "
          (unless libraries "SDL2")
          "
include $(BUILD_SHARED_LIBRARY)
")
         file)))))

(define (android-select-and-generate-modules modules
                                             #!key
                                             (options #f)
                                             (select '(playground)))
  (info "")
  (info "Generate C Code")
  (info "")
  (let* ((any-eq? (lambda (k l)
                    (let recur ((l l))
                      (cond ((null? l) #f)
                            ((eq? k (car l)) #t)
                            (else (recur (cdr l)))))))
         (modules (if (or (not select) (null? select))
                      modules
                      (let recur ((output modules))
                        (cond ((null? output) '())
                              ((any-eq? (%module-library (car output)) select)
                               (cons (car output) (recur (cdr output))))
                              (else (recur (cdr output)))))))
         (output-filenames (map (lambda (m) (string-append (lib-directory) (%module-filename-c m))) modules)))
    (gambit-eval-here
     `(begin
        (define-cond-expand-feature arm)
        (include "~~base/prelude#.scm")
        (for-each
         (lambda (module output-filename)
           ,(if options
                `(compile-file-to-target
                  (string-append (%module-path-src module) (%module-path-scm module))
                  options: ',options
                  output: output-filename)
                `(compile-file-to-target
                  (string-append (%module-path-src module) (%module-path-scm module))
                  output: output-filename)))
         ',modules
         ',output-filenames)))))

(define (android-generate-link-file modules)
  (info "")
  (info "Generate Link File")
  (info "")
  (gambit-eval-here
   `(begin
      (include "~~base/prelude#.scm")
      (link-incremental ',(map (lambda (m) (string-append (android-build-directory)
                                                     (%module-filename-c m)))
                               modules)
                        output: ,(string-append (android-build-directory) (android-link-file))))))

;;; Copies passed files to Android build directory

(define (android-install-c-files modules)
  (for-each
   (lambda (m) (copy-file
           (string-append (%module-path-lib m) (%module-filename-c m))
           (string-append (android-build-directory)
                          (%module-filename-c m))))
   modules))

;;; Calls Sake android task injecting modules and 

(define (android-compile-and-link #!key
                                  (modules '())
                                  (supplied-modules '())
                                  (options #f))
  (unless (file-exists? (playground-setup-directory))
          (error "You need to use (playground-setup) before compiling the project"))
  (when (null? modules) (error "You must supply modules to compile"))
  (let ((all-modules (append (android-base-modules)
                             supplied-modules
                             modules)))
    ;; Generate C from injected modules (only the local ones)
    (android-select-and-generate-modules
     modules
     select: #f
     options: options)
    ;; Copy generated C from both requested and supplied modules into android build directory
    (for-each
     (lambda (m) (copy-file
             (string-append (%module-path-lib m) (%module-filename-c m))
             (string-append (android-build-directory)
                            (%module-filename-c m))))
     (append supplied-modules modules))
    (android-generate-link-file all-modules)
    ;; Generate Android.mk
    (android-generate-mk all-modules)
    ;; If Manifest or properties files are missing, write default ones
    (unless (and (file-exists? (android-manifest-file))
                 (file-exists? (android-project-properties-file)))
            (android-generate-manifest-and-properties))
    ;; Call Android build system
    (shell-command (string-append "ant -s " (android-directory)  "build.xml clean debug install"))))

;;; Call Android clean ant task

(define (android-clean)
  (gambit-eval-here
   '(shell-command "ant -s android/build.xml clean")))

;;; Upload file to SD card

(define (android-upload-file-to-sd relative-path)
  (error "unimplemented"))