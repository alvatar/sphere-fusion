(%include sake: utils#)

;-------------------------------------------------------------------------------
; Setup and initialization
;-------------------------------------------------------------------------------

(define fusion-setup-directory
  (make-parameter "fusion/"))

(define fusion-addons-directory
  (make-parameter "addons/"))

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

;; (define android-driver-module
;;   (make-parameter '(fusion: driver)))

;; (define android-driver-debug-module
;;   (make-parameter '(fusion: driver version: (debug))))

(define android-link-file
  (make-parameter "linkfile_.c"))

;;; Check whether fusion is precompiled
(define (fusion:precompiled?)
  (file-exists? (string-append (%sphere-path 'fusion)
                               (android-directory-suffix)
                               (android-jni-directory-suffix)
                               (android-build-directory-suffix))))

;;; Clean all fusion files for current project
(define (fusion:clean)
  (delete-file (fusion-setup-directory))
  (delete-file (default-lib-directory)))

;;; Update fusion-generated C files
(define (fusion:update)
  (unless (fusion:precompiled?)
          (fusion:clean)
          (error "Prior to creating a Fusion project, you need to run 'sake compile' in Fusion Framework"))
  (copy-files (list (string-append (%sphere-path 'fusion) (android-directory-suffix) "jni/build"))
              (android-jni-directory)))

;;; Check whether fusion is ready and setup for project usage
(define (fusion:ready?)
  (file-exists? (fusion-setup-directory)))

;;; Setup fusion files for current project
(define (fusion:setup #!key (force #f))
  (when (or force
            (not (fusion:ready?)))
        (let* ((fusion-path (%sphere-path 'fusion))
               (opath (string-append fusion-path (android-directory-suffix))))
          (fusion:clean)
          (unless (fusion:precompiled?)
                  (fusion:clean)
                  (error "Prior to creating a Fusion project, you need to run 'sake compile' in Fusion Framework"))
          (make-directory (fusion-setup-directory))
          (make-directory (default-lib-directory))
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
                      (android-jni-directory)))))

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

;;; Global describing necessary info for importing addons
(define *fusion:addons-info*
  '((gl ((directories: (""))
         (dependencies: ())
         (c-flags: "")
         (ld-flags: "-lGLESv1_CM")))
    (pixman ((directories: ("pixman/" "pixman-extra/"))
             (dependencies: ())
             (c-flags: "")
             (ld-flags: "")
             (android-c-includes: "pixman pixman-extra")
             (android-static-libraries: "libpixman cpufeatures")
             (android-shared-libraries: "pixman pixman-extra")))
    (cairo ((directories: ("cairo/" "cairo-extra/"))
            (dependencies: (pixman))
            (c-flags: "")
            (ld-flags: "")
            (android-c-includes: "cairo/src cairo-extra")
            (android-static-libraries: "libcairo")
            (android-shared-libraries: "cairo cairo-extra")))))

;;; Build the system with this addon
(define (fusion:android-import-addon libs #!key (fresh-copy #f))
  (let ((fusion-path (%sphere-path 'fusion)))
    (for-each
     (lambda (lib)
       (let ((lib-info (assq lib *fusion:addons-info*)))
         (assure lib-info (error "internal error in fusion:android-import-addon"))
         ;; Check whether it has been imported
         (let ((lib-id (car lib-info)))
           (unless (memq lib-id *fusion:imported-addons*)
                   (set! *fusion:imported-addons*
                         (cons lib-id *fusion:imported-addons*))
                   (let ((lib-info-list (cadr lib-info)))
                     (for-each (lambda (dep) (fusion:android-import-addon (list dep)))
                               (uif (assq dependencies: lib-info-list)
                                    (cadr ?it)
                                    '()))
                     (for-each (lambda (dir)
                                 (let ((destination-path (string-append (android-jni-directory) dir)))
                                   (when (or fresh-copy
                                             (not (file-exists? destination-path)))
                                         (make-directory destination-path)
                                         (copy-files (fileset dir: (string-append fusion-path
                                                                                  (fusion-addons-directory)
                                                                                  dir)
                                                              recursive: #f)
                                                     destination-path))))
                               (cadr (assq directories: lib-info-list))))))))
     libs)))

;;; Global holding the list of imported addons
(define *fusion:imported-addons* '())

;;; Generate Android.mk given a set of moduels and optional spheres
(define (fusion:android-generate-mk modules)
  (info "")
  (info "Generate Android.mk")
  (info "")
  (let ((c-files (map (lambda (m) (%module-filename-c m)) modules))
        (produce-addon-string
         (lambda (key)
           (let ((produced-string
                  (apply string-append (reverse
                                        (map (lambda (lib)
                                               (uif (assq key
                                                          (cadr
                                                           (assq lib *fusion:addons-info*)))
                                                    (string-append " " (cadr ?it))
                                                    ""))
                                             *fusion:imported-addons*)))))
             (if (string? produced-string)
                 produced-string
                 "")))))
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
          ;; IMPORTANT: using -w, -Wno-write-strings doesn't work in all cases
          "
LOCAL_CFLAGS += -O2 -fno-short-enums -w -I./gambit -I. -I./SDL/include"
          (produce-addon-string c-flags:)
          "
LOCAL_LDLIBS := -ldl -lc -lm -llog -fno-short-enums -L./gambit -lgambc"
          (produce-addon-string ld-flags:)
          "
"
          (let ((static-libs (produce-addon-string android-c-includes:)))
            (if (string-null? static-libs)
                ""
                (string-append "LOCAL_C_INCLUDES :=  $(LOCAL_PATH)/../SDL/include " static-libs "\n")))
          (let ((static-libs (produce-addon-string android-static-libraries:)))
            (if (string-null? static-libs)
                ""
                (string-append "LOCAL_STATIC_LIBRARIES :=" static-libs "\n")))
          (string-append "LOCAL_SHARED_LIBRARIES := SDL2" (produce-addon-string android-shared-libraries:))
          "
include $(BUILD_SHARED_LIBRARY)
")
         file)))))

;;; Generate C files
(define (fusion:android-generate-modules modules
                                         #!key
                                         (version '())
                                         (compiler-options '())
                                         (verbose #f))
  (info "")
  (info "Generate C code")
  (info "")
  (let* ((output-filenames
          (map (lambda (m)
                 (string-append (default-lib-directory)
                                (%module-filename-c m version: version)))
               modules))
         (code `(begin
                  (include "~~spheres/prelude#.scm")
                  (define-cond-expand-feature mobile)
                  (define-cond-expand-feature android)
                  (for-each
                   (lambda (module output-filename)
                     (compile-file-to-target
                      (string-append (%module-path-src module) (%module-filename-scm module))
                      output: output-filename
                      options: ',compiler-options))
                   ',modules
                   ',output-filenames))))
    (if verbose
        (pp code))
    (unless (= 0 (gambit-eval-here code))
            (error "error generating Android C code"))))

;;; Generate Gambit link file
(define (fusion:android-generate-link-file modules #!key (verbose #f))
  (info "")
  (info "Generate link file")
  (info "")
  (let ((code
         `(begin
            (include "~~spheres/prelude#.scm")
            (link-incremental ',(map (lambda (m) (string-append (android-build-directory)
                                                           (%module-filename-c m)))
                                     modules)
                              output: ,(string-append (android-build-directory) (android-link-file))))))
    (if verbose
        (pp code))
    (unless (= 0 (gambit-eval-here code))
            (error "error generating link file"))))

;;; Copies passed files to Android build directory
(define (fusion:android-install-modules-c-files modules)
  (for-each
   (lambda (m) (copy-file
           (string-append (%module-path-lib m) (%module-filename-c m))
           (string-append (android-build-directory)
                          (%module-filename-c m))))
   modules))

;;; Calls Sake android task injecting modules and
;;; compile-modules: modules to compile and link
;;; compiler-options: options for Gambit compiler
;;; import-modules: modules already generated to be linked as well
(define (fusion:android-compile-and-link compile-modules
                                         #!key
                                         (compiler-options '())
                                         (import-modules '()))
  (unless (file-exists? (fusion-setup-directory))
          (error "You need to use (fusion-setup) before compiling the project"))
  (let* ((core-modules (if (memq 'debug compiler-options)
                           (%module-deep-dependencies-to-load '(fusion: driver version: (debug)))
                           (%module-deep-dependencies-to-load '(fusion: driver))))
         (all-modules (%merge-module-lists
                       (%merge-module-lists core-modules import-modules)
                       ;; FIX: ideally this should fold with %module-deep-dependencies, instead of applying append
                       ;; but any duplicate is cleaned up in next call to %module-deep-dependencies,
                       ;; and order shouldn't be a problem
                       (apply append (map %module-deep-dependencies-to-load compile-modules)))))
    
    (%module-deep-dependencies-to-load '(fusion: driver version: (debug)))
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
     all-modules)
    (fusion:android-generate-link-file all-modules)
    ;; Generate Android.mk
    (fusion:android-generate-mk all-modules)
    ;; If Manifest or properties files are missing, write default ones
    (unless (and (file-exists? (android-manifest-file))
                 (file-exists? (android-project-properties-file)))
            (fusion:android-generate-manifest-and-properties))
    ;; Call Android build system
    (shell-command (string-append "ant -s " (android-directory)  "build.xml clean debug install"))))

;;; Run the Android app in the device
(define (fusion:android-run-app)
  (shell-command "adb shell am start -n org.libsdl.app/.SDLActivity"))

;;; Call Android clean ant task
(define (fusion:android-clean)
  (unless (= 0 (gambit-eval-here
                '(shell-command "ant -s android/build.xml clean")))
          (error "error in \"ant clean\" command")))

;;; Upload file to SD card
(define (fusion:android-upload-file-to-sd relative-path)
  (error "unimplemented"))

;-------------------------------------------------------------------------------
; Desktop
;-------------------------------------------------------------------------------

(define (fusion:current-desktop-platform)
  (let ((uname (shell-command "uname -o")))
    (cond ((equal? "GNU/Linux" uname) 'linux)
          ((equal? "Darwin" uname) 'osx)
          (else (error "fusion:current-desltop-platform -> can't detect current platform")))))

(define (fusion:desktop-run-interpreted main-module)
  (gambit-eval-here '(%load (sdl2: sdl2))
                    `(%load ,main-module)
                    '(main)))

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
