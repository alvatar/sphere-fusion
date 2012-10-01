;-------------------------------------------------------------------------------
; Setup and initialization
;-------------------------------------------------------------------------------

(define fusion-setup-directory
  (make-parameter "fusion/"))

(define lib-directory
  (make-parameter "lib/"))

(define src-directory
  (make-parameter "src/"))

(define android-directory-suffix
  (make-parameter "android/"))

(define (android-directory)
  (string-append (fusion-setup-directory) (android-directory-suffix)))

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
                    (fusion: driver))))

(define android-base-debug-modules
  (make-parameter '((base: ffi version: (debug))
                    (sdl2: sdl2 version: (debug))
                    (fusion: driver version: (debug)))))

(define android-link-file
  (make-parameter "linkfile_.c"))

;;; Check whether fusion is precompiled

(define (fusion:precompiled?)
  (file-exists? (string-append (%library-path 'fusion)
                               (android-directory-suffix)
                               (android-jni-directory-suffix)
                               (android-build-directory-suffix))))

;;; Clean all fusion files for current project

(define (fusion:clean)
  (delete-file (fusion-setup-directory))
  (delete-file (lib-directory)))

;;; Update fusion generated C files

(define (fusion:update)
  (unless (fusion:precompiled?)
          (fusion:clean)
          (error "Prior to creating a Fusion project, you need to run 'sake init' in Fusion Framework"))
  (copy-file (string-append (%library-path 'fusion)
                            (android-directory-suffix)
                            (android-jni-directory-suffix)
                            (android-build-directory-suffix))
             (android-build-directory-suffix)))

;;; Setup fusion files for current project

(define (fusion:setup)
  (let* ((fusion-path (%library-path 'fusion))
         (opath (string-append fusion-path (android-directory-suffix))))
    (fusion:clean)
    (unless (fusion:precompiled?)
            (fusion:clean)
            (error "Prior to creating a Fusion project, you need to run 'sake init' in Fusion Framework"))
    (fusion:precompiled?)    
    (make-directory (fusion-setup-directory))
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

;;; Check whether fusion is ready and setup

(define (fusion:ready?)
  (file-exists? (fusion-setup-directory)))

;-------------------------------------------------------------------------------
; Android
;-------------------------------------------------------------------------------

;;; Generate default Manifest and properties file

(define (fusion:android-generate-manifest-and-properties #!key
                                                         (api-level 8)
                                                         (app-name "Fusion App"))
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

(define (fusion:android-generate-mk modules #!key (libraries #f))
  (info "")
  (info "Generate Android.mk")
  (info "")
  (let ((c-files (map (lambda (m) (%module-filename-c m)) modules)))
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

;;; Generate C files

(define (fusion:android-generate-modules modules
                                         #!key
                                         (version '())
                                         (compiler-options '()))
  (info "")
  (info "Generate C Code")
  (info "")
  (let ((output-filenames
         (map (lambda (m)
                (string-append (lib-directory)
                               (%module-filename-c m version: version)))
              modules)))
    (gambit-eval-here
     `(begin
        (include "~~base/prelude#.scm")
        (define-cond-expand-feature mobile)
        (define-cond-expand-feature android)
        (for-each
         (lambda (module output-filename)
           (compile-file-to-target
            (string-append (%module-path-src module) (%module-filename-scm module))
            output: output-filename
            options: ',compiler-options))
         ',modules
         ',output-filenames)))))

;;; Select (filter) modules from a list

(define (fusion:select-modules modules #!key (libraries '(fusion)))
  (let* ((select (if (pair? libraries) libraries (list libraries)))
         (any-eq? (lambda (k l)
                    (let recur ((l l))
                      (cond ((null? l) #f)
                            ((eq? k (car l)) #t)
                            (else (recur (cdr l))))))))
    (let recur ((output modules))
      (cond ((null? output) '())
            ((any-eq? (%module-library (car output)) select)
             (cons (car output) (recur (cdr output))))
            (else (recur (cdr output)))))))

;;; Generate Gambit link file

(define (fusion:android-generate-link-file modules)
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

;;; TODO: What about installing ALL versions from a module???

(define (fusion:android-install-c-files modules)
  (for-each
   (lambda (m) (copy-file
           (string-append (%module-path-lib m) (%module-filename-c m))
           (string-append (android-build-directory)
                          (%module-filename-c m))))
   modules))

;;; Calls Sake android task injecting modules and
;;; modules: modules to compile and link
;;; provided-modules: modules already generated, to compile and link
;;; compiler-options: options for Gambit compiler

(define (fusion:android-compile-and-link #!key
                                         (compile-modules '())
                                         (compiler-options '())
                                         (import-modules '()))
  (unless (file-exists? (fusion-setup-directory))
          (error "You need to use (fusion-setup) before compiling the project"))
  (let* ((android-modules (if (memq 'debug compiler-options)
                              (android-base-debug-modules)
                              (android-base-modules)))
         (all-modules (append android-modules
                              import-modules
                              compile-modules)))
    ;; Generate modules (generates C code)
    (fusion:android-generate-modules compile-modules
                                     compiler-options: compiler-options)
    ;; Copy generated C from both compiled and imported modules into android build directory
    (for-each
     (lambda (m) (copy-file
             (string-append (%module-path-lib m)
                            (%module-filename-c m))
             (string-append (android-build-directory)
                            (%module-filename-c m))))
     (append import-modules compile-modules))
    (fusion:android-generate-link-file all-modules)
    ;; Generate Android.mk
    (fusion:android-generate-mk all-modules)
    ;; If Manifest or properties files are missing, write default ones
    (unless (and (file-exists? (android-manifest-file))
                 (file-exists? (android-project-properties-file)))
            (fusion:android-generate-manifest-and-properties))
    ;; Call Android build system
    (shell-command (string-append "ant -s " (android-directory)  "build.xml clean debug install"))))

;;; Call Android clean ant task

(define (fusion:android-clean)
  (gambit-eval-here
   '(shell-command "ant -s android/build.xml clean")))

;;; Upload file to SD card

(define (fusion:android-upload-file-to-sd relative-path)
  (error "unimplemented"))

;-------------------------------------------------------------------------------
; Main tasks
;-------------------------------------------------------------------------------

(define-task init ()
  (if (file-exists? (fusion-setup-directory))
      (error "It appears that the project has been initialized, please execute task \"clean\" prior to initialization")
      (fusion:setup)))

(define-task update ()
  (if (file-exists? (fusion-setup-directory))
      (fusion:update)
      (fusion:setup)))

(define-task clean ()
  (fusion:clean)
  (delete-file (current-build-directory)))
