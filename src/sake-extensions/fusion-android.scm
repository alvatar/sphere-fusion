;;; Copyright (c) 2012-2014, Álvaro Castro-Castilla
;;; Extensions for Sake (Android)

;;------------------------------------------------------------------------------
;;!! Setup and initialization

(define android-directory
  (make-parameter "android/"))

(define android-jni-directory-suffix
  (make-parameter "jni/"))

(define android-jni-generator-directory-suffix
  (make-parameter "jni-generator/"))

(define android-build-directory-suffix
  (make-parameter "build/"))

(define android-base-assets-directory-suffix
  (make-parameter "assets/"))

;; Android expects an assets directory. Since Fusion already uses assests as a prefix, it needs to be nested.
;; XXX: REMOVE THIS
(define android-assets-directory-suffix
  (make-parameter "assets/assets/"))

(define android-jni-directory
  (make-parameter
   (string-append (android-directory) (android-jni-directory-suffix))))

(define android-jni-generator-directory
  (make-parameter
   (string-append (android-directory) (android-jni-generator-directory-suffix))))

(define android-build-directory
  (make-parameter
   (string-append (android-directory) (android-jni-directory-suffix) (android-build-directory-suffix))))

(define android-assets-directory
  (make-parameter
   (string-append (android-directory) (android-assets-directory-suffix))))

(define android-src-directory-suffix
  (make-parameter "src/"))

(define android-src-generator-directory-suffix
  (make-parameter "src-generator/"))

(define android-src-directory
  (make-parameter
   (string-append (android-directory) (android-src-directory-suffix))))

(define android-src-generator-directory
  (make-parameter
   (string-append (android-directory) (android-src-generator-directory-suffix))))

(define android-link-file
  (make-parameter "linkfile_.c"))

;;------------------------------------------------------------------------------
;;!! Host programs

(define host-ant-path
  (make-parameter
   (if (zero? (shell-command "ant -version &>/dev/null"))
       "ant"
       #f)))

;;------------------------------------------------------------------------------
;;!! Android SDK

;;!! Android SDK path
(define android-sdk-path
  (make-parameter
   (with-exception-catcher
    (lambda (e)
      (if (unbound-os-environment-variable-exception? e)
          #f
          (error "internal error in android-sdk-path")))
    (lambda () (getenv "ANDROID_SDK_PATH")))))

;;! Program: android
(define android-android-path
  (make-parameter
   (and (android-sdk-path)
        (let ((path (string-append (android-sdk-path)
                                   "/tools/android")))
          (if (file-exists? path) path #f)))))

;;! Program: adb
(define android-adb-path
  (make-parameter
   (and (android-sdk-path)
        (let ((path (string-append (android-sdk-path)
                                   "/platform-tools/adb")))
          (if (file-exists? path) path #f)))))

;;------------------------------------------------------------------------------
;; Android NDK

;;! Android NDK path
(define android-ndk-path
  (make-parameter
   (with-exception-catcher
    (lambda (e)
      (if (unbound-os-environment-variable-exception? e)
          #f
          (error "internal error in android-ndk-path")))
    (lambda () (getenv "ANDROID_NDK_PATH")))))

;;! Program: ndk-build
(define android-ndk-build-path
  (make-parameter
   (and (android-ndk-path)
        (let ((path (string-append (android-ndk-path) "/ndk-build")))
          (if (file-exists? path) path #f)))))

;;------------------------------------------------------------------------------
;;!! Android tasks

;;! Check if Android seems to be installed in the system
(define (fusion#android-installed?)
  (unless (android-sdk-path)
          (err "Cannot find Android SDK path. Is ANDROID_SDK_PATH environment variable set?"))
  (unless (android-ndk-path)
          (err "Cannot find Android NDK path. Is ANDROID_NDK_PATH environment variable set?"))
  (unless (android-android-path)
          (err "Cannot find \"android\" program in " (android-sdk-path)  " -- Is ANDROID_SDK_PATH set to the right directory?"))
  (unless (android-adb-path)
          (err "Cannot find \"adb\" program in " (android-sdk-path)  " -- Is ANDROID_SDK_PATH set to the right directory?"))
  (unless (android-ndk-build-path)
          (err "Cannot find \"ndk-build\" program in " (android-ndk-path)  " -- Is ANDROID_NDK_PATH set to the right directory?"))
  (unless (host-ant-path)
          (err "Cannot find \"ant\" program. Is it installed?")))

;;! Check whether the project seems to be prepared for Android
(define (fusion#android-project-supported?)
  (unless (file-exists? (android-directory))
          (err "Android directory doesn't exist. Please run Android setup task."))
  (unless (or (file-exists? (android-jni-directory))
              (file-exists? (android-jni-generator-directory)))
          (err "Android 'jni' or 'jni-generator' directory doesn't exist. Please run Android setup task."))
  (unless (or (file-exists? (android-src-directory))
              (file-exists? (android-src-generator-directory)))
          (err "Android 'src' or 'src-generator' directory doesn't exist. Please run Android setup task."))
  (unless (file-exists? (string-append (android-directory) "/build.xml"))
          (err "Android build.xml file doesn't exist. Please run Android setup task."))
  (unless (file-exists? (string-append (android-directory) "/AndroidManifest.xml"))
          (err "AndroidManifest.xml file doesn't exist. Please run Android setup task."))
  (unless (file-exists? (string-append (android-directory) "/default.properties"))
          (err "Android default.properties file doesn't exist. Please run Android setup task."))
  (unless (file-exists? (string-append (android-directory) "/project.properties"))
          (err "Android project.properties file doesn't exist. Please run Android setup task."))
  (unless (file-exists? (string-append (android-directory) "/local.properties"))
          (err "Android local.properties file doesn't exist. Please run (fusion#android-project-set-target <target>) in a setup task.")))

;;! Check update project
(define (fusion#android-project-set-target target)
  (shell-command
   (string-append (android-android-path) " update project --path " (android-directory) " --target " target)))

;;! Compile app
;; .parameter main-module main-module of the Android App
;; .parameter import-modules modules already generated to be linked as well
(define (fusion#android-compile-app app-name
                                    main-module
                                    #!key
                                    (cond-expand-features '())
                                    (compiler-options '())
                                    (version compiler-options)
                                    (compiled-modules '())
                                    (jni-files '())
                                    (target 'debug)
                                    (num-threads 1)
                                    (verbose #f))
  ;; Defines
  (##cond-expand-features (append '(mobile android) (##cond-expand-features)))

  ;; Checks
  (fusion#android-installed?)
  (fusion#android-project-supported?)
  (unless (number? num-threads) (err "fusion#android-compile-app: num-threads must be a positive number, or +inf.0"))
  ;; Compute dependencies
  (let* ((modules-to-compile (append (%module-deep-dependencies-to-load main-module) (list main-module)))
         (all-modules (append compiled-modules modules-to-compile)))
    ;; List files generated by compiling modules, C files from the JNI, and the linkfile
    (let ((all-c-files
           (append (map (lambda (m) (string-append "../" (android-build-directory-suffix)
                                              (%module-filename-c m version: version)))
                        all-modules)
                   (list (string-append "../" (android-build-directory-suffix) (android-link-file)))
                   jni-files)))
      ;; Create Android build directory if it doesn't exist
      (unless (file-exists? (android-build-directory))
              (make-directory (android-build-directory)))
      ;; Create Android assets directory if it doesn't exist
      (unless (file-exists? (android-base-assets-directory-suffix))
              (make-directory (android-base-assets-directory-suffix)))
      (unless (file-exists? (android-assets-directory))
              (make-directory (android-assets-directory)))
      (let ((arguments '(app-name main-module-name c-files-string))
            (arg-values `("main"
                          "main"
                          ,(apply string-append (map (lambda (file) (string-append file " ")) all-c-files)))))
        ;; Process 'assets'
        (fusion#process-directory-with-templates (android-assets-directory)
                                                 "assets/"
                                                 arguments
                                                 arg-values)
        ;; Process 'src'
        (fusion#process-directory-with-templates (android-src-directory)
                                                 (string-append (android-directory) (android-src-generator-directory-suffix))
                                                 arguments
                                                 arg-values)
        ;; Process 'jni'. Make sure Android.mk is updated if config.scm is more recent than Android.mk
        (fusion#process-directory-with-templates (android-jni-directory)
                                                 (string-append (android-directory) (android-jni-generator-directory-suffix))
                                                 arguments
                                                 arg-values
                                                 ;; force overwriting Android.mk if config.scm is newer
                                                 overwrite-files:
                                                 (let ((android-mk
                                                        (string-append (android-directory) "jni/src/Android.mk")))
                                                   (if ((newer-than? android-mk)
                                                        (string-append (current-directory) "config.scm"))
                                                       (begin
                                                         (info/color 'yellow "Android.mk updated due to changes in config.scm")
                                                         (list android-mk))
                                                       '()))))
      ;; Generate modules (generates C code)
      (let ((something-generated? #f))
        (for-each (lambda (m)
                    (if ((newer-than? (string-append (android-build-directory)
                                                     (%module-filename-c m version: version)))
                         (string-append (%module-path-src m) (%module-filename-scm m)))
                        (begin
                          (set! something-generated? #t)
                          (sake#compile-to-c m
                                             cond-expand-features: (append cond-expand-features '(android mobile))
                                             compiler-options: compiler-options
                                             verbose: verbose))))
                  modules-to-compile)
        (if something-generated?
            (info/color 'blue "generating C files")
            (info/color 'blue "no Scheme files need C recompilation"))
        ;; Copy C files from both compiled and imported modules into android build directory (if updated)
        (for-each
         (lambda (m) (let ((source (string-append (current-build-directory)
                                             (%module-filename-c m version: version)))
                      (destination (string-append (android-build-directory)
                                                  (%module-filename-c m version: version))))
                  (if ((newer-than? destination) source)
                      (copy-file source destination))))
         all-modules)
        ;; Generate link file
        (if something-generated?
            (fusion#android-generate-link-file all-modules version: version))))
    (info/color 'blue (string-append "compiling JNI C/Scheme code (with " (if (= num-threads +inf.0) "MAX" num-threads) " threads)"))
    ;; Call "ndk-build". We dump the output to a file, as Gambit has issues with the "-j" flag for concurrent compilation
    (let ((ndk-command-str
           (string-append (android-ndk-build-path)
                          (cond ((< num-threads 1) (err "fusion#android-compile-app: num-threads must larger than 1 or +inf.0"))
                                ((= num-threads 1) "")
                                ((= num-threads +inf.0) " -j ")
                                ((> num-threads 1) (string-append " -j " (number->string num-threads))))
                          " -C " (android-directory) " &>ndk-log")))
      (if verbose (println ndk-command-str))
      (unless (zero? (shell-command ndk-command-str))
              (err "error building native code\n\n" (sake#read-file "ndk-log"))))
    (delete-file "ndk-log")
    (if verbose (display (sake#read-file "ndk-log")))
    (info/color 'blue "compiling Java code")
    ;; Call "ant"
    (unless (zero? (shell-command (string-append (host-ant-path) " -s " (android-directory) "build.xml "
                                                 (cond ((eq? 'debug target) "debug")
                                                       ((eq? 'release target) "release")
                                                       (else (err "Unknown target parameter: fusion#android-compile-app"))))))
            (err "error building Java code"))))

;;! Generate Gambit link file
(define (fusion#android-generate-link-file modules #!key (verbose #f) (version '()))
  (info/color 'blue "generating link file")
  (let* ((output-file (string-append (android-build-directory) (android-link-file)))
         (code
          `(                      ;(##include "~~spheres/prelude.scm")
            (link-incremental ',(map (lambda (m) (string-append (android-build-directory)
                                                           (%module-filename-c m version: version)))
                                     modules)
                              output: ,output-file))))
    (if verbose (pp code))
    (unless (= 0 (gambit-eval-here code))
            (err "error generating Gambit link file"))))

;;! Process a directory containing templates
(define (fusion#process-directory-with-templates destination-path source-path parameters values #!key
                                                 (overwrite-all #f)
                                                 (overwrite-files '()))
  (let recur ((relative-path ""))
    (for-each
     (lambda (file)
       (case (file-info-type (file-info (string-append source-path relative-path file)))
         ((regular)
          (let* ((source-file (string-append source-path relative-path file))
                 (is-source-a-template? (string=? ".sct" (path-extension source-file)))
                 (destination-file ((if is-source-a-template? path-strip-extension (lambda (x) x))
                                    (string-append destination-path relative-path file))))
            ;; Do the work if:
            (if (or (not (file-exists? destination-file)) ; the file doesn't exist
                    overwrite-all ; the source file is newer than the destination file
                    (if (member destination-file overwrite-files)
                        (begin (info/color 'yellow "forced file update: " destination-file) #t)
                        #f)
                    (if ((newer-than? destination-file) source-file) ; the source-file has changed.
                        (begin (info/color 'yellow "file updated from generator: " destination-file) #t)
                        #f))
                ;; Test whether it's a template file to process or a simple file to copy
                (if is-source-a-template?
                    (call-with-output-file
                        destination-file ; we already stripped the extension before
                      (lambda (f) (display
                              (apply
                               (apply
                                build-template-from-file
                                (cons (string-append source-path relative-path file)
                                      parameters))
                               values)
                              f)))
                    (sake#copy-file source-file destination-file force: #t)))))
         ((directory)
          (make-directory (string-append destination-path relative-path file))
          (recur (string-append relative-path file "/")))))
     (directory-files (string-append source-path relative-path)))))

;;! Clean up all generated files
(define (fusion#android-clean)
  (define (delete-if-exists dir)
    (when (file-exists? dir)
          (sake#delete-directory dir recursive: #t force: #t)))
  (delete-if-exists (string-append (android-directory) "assets/"))
  (delete-if-exists (string-append (android-directory) "bin/"))
  (delete-if-exists (string-append (android-directory) "gen/"))
  (delete-if-exists (string-append (android-directory) "libs/"))
  (delete-if-exists (string-append (android-directory) "obj/"))
  (delete-if-exists (string-append (android-directory) "src/"))
  (delete-if-exists (string-append (android-directory) "jni/"))
  (delete-if-exists (android-build-directory)))

;;! Install App in Android device
(define (fusion#android-install version)
  (let ((version-str (case version ((debug) "d") ((release) "r") (else (err "Wrong version argument: fusion#android-install")))))
    (unless (zero? (shell-command (string-append (host-ant-path) " -s " (android-directory)
                                                 "build.xml install" version-str)))
            (err "App couldn't be installed in Android device."))))

;;! Run App in Android device
(define (fusion#android-run activity)
  (unless (zero? (shell-command (string-append (android-adb-path) " shell am start -n " activity)))
          (err "App couldn't be run in Android device.")))
