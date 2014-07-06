;;; Copyright (c) 2014 by Álvaro Castro Castilla
;;; Extensions for Sake, to use with Fusion projects

(define ios-directory
  (make-parameter "ios/"))

(define ios-source-directory-suffix
  (make-parameter "src/"))

(define ios-source-directory
  (make-parameter
   (string-append (ios-directory) (ios-source-directory-suffix))))

(define ios-build-directory-suffix
  (make-parameter "build/"))

(define ios-build-directory
  (make-parameter
   (string-append (ios-source-directory) (ios-build-directory-suffix))))

(define ios-assets-directory-suffix
  (make-parameter "assets/"))

(define ios-assets-directory
  (make-parameter (string-append (ios-directory) (ios-assets-directory-suffix))))

(define ios-device-sdk-directory
  (make-parameter
   (let* ((sdk-dir-process
           (open-process (list path: (string-append (%sphere-path 'fusion) "tools/get-ios-sdk-dir")
                               arguments: '("iPhoneOS"))))
          (result (read-line sdk-dir-process)))
     (unless (zero? (process-status sdk-dir-process))
             (err "fusion#compile-ios-app: error running script tools/get_ios_sdk_dir"))
     (close-port sdk-dir-process)
     result)))

(define ios-simulator-sdk-directory
  (make-parameter
   (let* ((sdk-dir-process
           (open-process (list path: (string-append (%sphere-path 'fusion) "tools/get-ios-sdk-dir")
                               arguments: '("iPhoneSimulator"))))
          (result (read-line sdk-dir-process)))
     (unless (zero? (process-status sdk-dir-process))
             (err "fusion#compile-ios-app: error running script tools/get_ios_sdk_dir"))
     (close-port sdk-dir-process)
     result)))


;;------------------------------------------------------------------------------
;;!! Toolchain

(define xcodebuild-path
  (make-parameter
   (if (zero? (shell-command "xcodebuild -usage &>/dev/null"))
       "xcodebuild"
       #f)))

(define ios-sim-path
  (make-parameter
   (if (zero? (shell-command "ios-sim --version &>/dev/null"))
       "ios-sim"
       #f)))


;;------------------------------------------------------------------------------
;;!! iOS support procedures

;;! Check whether the project seems to be prepared for Android
(define (fusion#ios-project-supported?)
  (unless (file-exists? (ios-directory))
          (err "iOS directory doesn't exist. Please run iOS setup task."))
  (when (null? (directory-files (ios-directory)))
        (err "iOS directory doesn't seem to have anything. Please run iOS setup task."))
  (unless (file-exists? (ios-source-directory))
          (err "iOS source directory doesn't exist. Please run iOS setup task."))
  (when (null? (fileset dir: (ios-directory) test: (extension=? "xcodeproj")))
        (err "iOS Xcode project doesn't exist. Please run iOS setup task.")))

;;! Default clean for iOS
(define (fusion#ios-clean)
  (define (delete-if-exists dir)
    (when (file-exists? dir)
          (sake#delete-file dir recursive: #t force: #t)))
  (delete-if-exists (ios-assets-directory))
  (delete-if-exists (ios-build-directory))
  (delete-if-exists (string-append (ios-source-directory) "libspheres.a"))
  (delete-if-exists (string-append (ios-directory) "build/")))

;;! Generate incremental link file for iOS given a set of modules

;;; XXX TODO  GENERALIZE
(define (fusion#ios-link-incremental link-file
                                     modules
                                     #!key
                                     (verbose #f)
                                     (version '()))
  (info/color 'blue "generating incremental link file")
  (let* ((output-file (string-append (ios-build-directory) link-file))
         (code
          `((link-incremental
             ',(map (lambda (m) (string-append (ios-build-directory)
                                          (%module-filename-c m version: version)))
                    modules)
             output: ,output-file))))
    (if verbose (pp code))
    (unless (= 0 (gambit-eval-here code))
            (err "error generating Gambit incremental link file"))
    output-file))

;;! Generate flat link file for iOS given a set of modules

;;; XXX TODO  GENERALIZE
(define (fusion#ios-link-flat link-file
                              modules
                              #!key
                              (verbose #f)
                              (version '()))
  (info/color 'blue (string-append "generating flat link file: " link-file))
  (let* ((output-file (string-append (ios-build-directory) link-file))
         (code
          `((link-flat
             ',(map (lambda (m) (string-append (ios-build-directory)
                                          (%module-filename-c m version: version)))
                    modules)
             output: ,output-file
             ;; with flat link files we remove warnings if non-verbose, since it includes used standard procedures too
             warnings?: ,verbose))))
    (if verbose (pp code))
    (unless (= 0 (gambit-eval-here code))
            (err "error generating Gambit flat link file"))
    output-file))

;;! Runs the compiler with the iOS environment
(define (fusion#ios-run-compiler #!key
                                 arch
                                 platform-type
                                 arguments
                                 (compiler 'gcc)
                                 (verbose #f))
  (let ((arch-str (symbol->string arch))
        (sdk-name (case platform-type ((device) "iphoneos") ((simulator) "iphonesimulator")))
        (ios-sdk-directory (case platform-type
                             ((device) (ios-device-sdk-directory))
                             ((simulator) (ios-simulator-sdk-directory)))))
    ;; Checks
    (unless (or (eq? platform-type 'simulator) (eq? platform-type 'device))
            (err "fusion#compile-ios-app: wrong platform-type"))
    (unless (or (eq? compiler 'gcc) (eq? compiler 'g++))
            (err "fusion#compile-ios-app: wrong compiler"))
    ;; Construct compiler strings
    ;; XXX TODO: this should be done in the proper string list format, so no split is nacessary later on
    (let* ((ios-cc-cli (string-append
                        "-sdk " sdk-name
                        " gcc"
                        " -isysroot " ios-sdk-directory
                        " -arch " arch-str
                        " -miphoneos-version-min=7.1"
                        ;; XXX TODO: These should come externally, and be handled according to platform
                        " -fmessage-length=105 -fdiagnostics-show-note-include-stack -fmacro-backtrace-limit=0 -fcolor-diagnostics -std=c99 -Wno-trigraphs -fpascal-strings -O0 -Wno-trigraphs -Wreturn-type"
                        ;; For armv7s
                        ;; /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/clang -x objective-c -arch armv7s -fmessage-length=105 -fdiagnostics-show-note-include-stack -fmacro-backtrace-limit=0 -fcolor-diagnostics -std=c99 -Wno-trigraphs -fpascal-strings -O0 -Wno-missing-field-initializers -Wno-missing-prototypes -Wno-implicit-atomic-properties -Wno-receiver-is-weak -Wno-arc-repeated-use-of-weak -Wno-missing-braces -Wparentheses -Wswitch -Wno-unused-function -Wno-unused-label -Wno-unused-parameter -Wno-unused-variable -Wno-unused-value -Wno-empty-body -Wno-uninitialized -Wno-unknown-pragmas -Wno-shadow -Wno-four-char-constants -Wno-conversion -Wno-constant-conversion -Wno-int-conversion -Wno-bool-conversion -Wno-enum-conversion -Wno-shorten-64-to-32 -Wpointer-sign -Wno-newline-eof -Wno-selector -Wno-strict-selector-match -Wno-undeclared-selector -Wno-deprecated-implementations -isysroot /Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS7.1.sdk -fstrict-aliasing -Wprotocol -Wdeprecated-declarations -g -fvisibility=hidden -Wno-sign-conversion -miphoneos-version-min=7.1 -iquote /Users/Alvaro/Dropbox/working/DaTest/ios/build/SchemeSpheres.build/Debug-iphoneos/SchemeSpheres.build/SchemeSpheres-generated-files.hmap -I/Users/Alvaro/Dropbox/working/DaTest/ios/build/SchemeSpheres.build/Debug-iphoneos/SchemeSpheres.build/SchemeSpheres-own-target-headers.hmap -I/Users/Alvaro/Dropbox/working/DaTest/ios/build/SchemeSpheres.build/Debug-iphoneos/SchemeSpheres.build/SchemeSpheres-all-target-headers.hmap -iquote /Users/Alvaro/Dropbox/working/DaTest/ios/build/SchemeSpheres.build/Debug-iphoneos/SchemeSpheres.build/SchemeSpheres-project-headers.hmap -I/Users/Alvaro/Dropbox/working/DaTest/ios/build/Debug-iphoneos/include -I/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include -I/Users/Alvaro/Dropbox/working/DaTest/ios/build/SchemeSpheres.build/Debug-iphoneos/SchemeSpheres.build/DerivedSources/armv7s -I/Users/Alvaro/Dropbox/working/DaTest/ios/build/SchemeSpheres.build/Debug-iphoneos/SchemeSpheres.build/DerivedSources -F/Users/Alvaro/Dropbox/working/DaTest/ios/build/Debug-iphoneos -x objective-c -D___LIBRARY -v -D___LIBRARY -MMD -MT dependencies -MF /Users/Alvaro/Dropbox/working/DaTest/ios/build/SchemeSpheres.build/Debug-iphoneos/SchemeSpheres.build/Objects-normal/armv7s/UIDevice+Hardware.d --serialize-diagnostics /Users/Alvaro/Dropbox/working/DaTest/ios/build/SchemeSpheres.build/Debug-iphoneos/SchemeSpheres.build/Objects-normal/armv7s/UIDevice+Hardware.dia -c /Users/Alvaro/Dropbox/working/DaTest/ios/src/UIDevice+Hardware.m -o /Users/Alvaro/Dropbox/working/DaTest/ios/build/SchemeSpheres.build/Debug-iphoneos/SchemeSpheres.build/Objects-normal/armv7s/UIDevice+Hardware.o
                        ))
           (ios-cxx-cli (string-append
                         "-sdk " sdk-name
                         " g++"
                         " -isysroot " ios-sdk-directory
                         " -arch " arch-str
                         " -miphoneos-version-min=7.1"))
           (selected-compiler-cli (case compiler ((gcc) ios-cc-cli) ((g++) ios-cxx-cli))))
      (when verbose
            (info/color 'green "Compiler command:")
            (println selected-compiler-cli)
            (info/color 'green "Compiler args:")
            (println (string-join arguments)))
      (let ((compilation-process
             (open-process
              (list path: "xcrun"
                    arguments: (append ((string-split #\space) selected-compiler-cli)
                                       arguments)
                    environment:
                    (list (string-append "ARCH=" arch-str)
                          (string-append "CC=\"xcrun " ios-cc-cli "\"")
                          (string-append "CC=\"xcrun " ios-cxx-cli "\"")
                          (string-append "CFLAGS=\"-Wno-trigraphs -Wreturn-type -Wunused-variable\"")
                          "CXXFLAGS=\"-Wno-trigraphs -Wreturn-type -Wunused-variable\""
                          (string-append "LD=\"ld -arch " arch-str "\"")
                          "LDFLAGS=\"\""
                          "IPHONEOS_DEPLOYMENT_TARGET=7.1"))))) ;; XXX TODO
        (unless (zero? (process-status compilation-process))
                (err "fusion#ios-run-compiler: error running command"))
        (close-port compilation-process)))))

;;! Runs the linker with the iOS environment
(define (fusion#ios-run-linker #!key
                                 arch
                                 platform-type
                                 arguments
                                 (verbose #f))
  (let ((arch-str (symbol->string arch))
        (sdk-name (case platform-type ((device) "iphoneos") ((simulator) "iphonesimulator")))
        (ios-sdk-directory (case platform-type
                             ((device) (ios-device-sdk-directory))
                             ((simulator) (ios-simulator-sdk-directory)))))
    ;; Checks
    (unless (or (eq? platform-type 'simulator) (eq? platform-type 'device))
            (err "fusion#ios-run-linker: wrong platform-type"))
    ;; Construct compiler strings
    (let ((ios-ld-cli (string-append
                       "-sdk " sdk-name
                       " ld"
                       " -syslibroot " ios-sdk-directory
                       " -arch " arch-str)))
      (when verbose
            (info/color 'green "Linker command:")
            (println ios-ld-cli)
            (info/color 'green "Linker args:")
            (println (string-join arguments)))
      (let ((compilation-process
             (open-process
              (list path: "xcrun"
                    arguments: (append ((string-split #\space) ios-ld-cli)
                                       arguments)
                    environment:
                    (list (string-append "ARCH=" arch-str)
                          (string-append "LD=\"ld -arch " arch-str "\"")
                          "LDFLAGS=\"\"")))))
        (unless (zero? (process-status compilation-process))
                (err "fusion#ios-run-linker: error running command"))
        (close-port compilation-process)))))

;;! Create an archive (static library) for iOS given a set of object files
(define (fusion#ios-create-library-archive lib-name o-files #!key (verbose #f))
  (shell-command (string-append "ar r" (if verbose "cv " " ") lib-name " " (string-join o-files))))

;;! Compile App
;; .parameter main-module main-module of the Android App
;; .parameter arch: target architecture
(define (fusion#ios-compile-app main-module
                                #!key
                                arch
                                (target 'debug)
                                (merge-modules #f)
                                (cond-expand-features '())
                                (compiler-options '())
                                (version compiler-options)
                                (compiled-modules '())
                                (verbose #f))
  ;; Cond-expand features (relevant within the Sake environment)
  (##cond-expand-features (cons 'ios (##cond-expand-features)))
  ;; Checks
  (fusion#ios-project-supported?)
  (unless arch (err "fusion#ios-compile-app: arch argument is mandatory"))
  (unless (or (eq? arch 'i386) (eq? arch 'armv7) (eq? arch 'armv7s))
          (err "fusion#ios-compile-app: wrong arch argument"))
  (when merge-modules
        (err "fusion#ios-compile-app: merge-modules options is not yet implemented"))
  ;; Compute dependencies
  (let* ((modules-to-compile (append (%module-deep-dependencies-to-load main-module) (list main-module)))
         (all-modules (append compiled-modules modules-to-compile))
         (platform-type (case arch
                          ((i386) 'simulator)
                          ((armv7 armv7s) 'device)))
         (link-file "linkfile_.c"))
    ;; List files generated by compiling modules and the linkfile
    (let ((all-c-files
           (append (map (lambda (m) (string-append (ios-build-directory) (%module-filename-c m version: version))) all-modules)
                   (list (string-append (ios-build-directory) link-file)))))
      ;; Create iOS build directory if it doesn't exist
      (unless (file-exists? (ios-build-directory))
              (make-directory (ios-build-directory)))
      ;; Create iOS assets directory if it doesn't exist
      (unless (file-exists? (ios-assets-directory))
              (make-directory (ios-assets-directory)))
      ;; Generate modules (generates C code)
      (let ((something-generated? #f))
        (for-each
         (lambda (m)
           (let ((output-c-file (string-append (ios-build-directory) (%module-filename-c m version: version))))
             (if ((newer-than? output-c-file)
                  (string-append (%module-path-src m) (%module-filename-scm m)))
                 (begin
                   (set! something-generated? #t)
                   (sake#compile-to-c m
                                      cond-expand-features: (cons 'ios cond-expand-features)
                                      compiler-options: compiler-options
                                      verbose: verbose
                                      output: output-c-file)))))
         modules-to-compile)
        (when something-generated?
              (info/color 'blue "new C files generated")
              (fusion#ios-link-incremental link-file all-modules version: version))
        (set! something-generated? #f)
        (info/color 'green "compiling C/Scheme code into a static lib")
        (let ((o-files
               (map (lambda (f)
                      (let ((output-o-file (string-append (path-strip-extension f) ".o")))
                        (when ((newer-than? output-o-file) f)
                              (unless something-generated?
                                      (info/color 'blue "compiling updated C files:"))
                              (info/color 'browns (string-append " >>>  " f))
                              (set! something-generated? #t)
                              (fusion#ios-run-compiler
                               arch: arch
                               platform-type: platform-type
                               arguments: `("-x"
                                            "objective-c"
                                            ,(string-append "-I" (ios-directory) "gambit/include")
                                            "-D___LIBRARY"
                                            ,(string-append "-I" (ios-source-directory))
                                            "-I/usr/local/Gambit-C/spheres/sdl2/deps/SDL2-2.0.3/include" ;;XXX TODO
                                            ;;,(string-append "-I" ios-sdk-directory "/System/Library/Frameworks/OpenGLES.framework/Headers") ;; XXX TODO
                                            ;; "-w" ;; XXX TODO
                                            ;; **************
                                            ;; **************
                                            ;; **************
                                            ;; **************
                                            "-c"
                                            ,f
                                            "-o"
                                            ,output-o-file)
                               compiler: 'gcc
                               verbose: verbose))
                        output-o-file))
                    all-c-files)))
          (fusion#ios-create-library-archive (string-append (ios-source-directory) "libspheres.a")
                                             o-files
                                             verbose: verbose))))
    (info/color 'blue "compiling iOS app")
    (parameterize
     ((current-directory (ios-directory)))
     (shell-command (string-append
                     (xcodebuild-path) " build -configuration Debug -sdk "
                     (case platform-type ((device) "iphoneos") ((simulator) "iphonesimulator"))
                     " -arch " (symbol->string arch))))))

;;! Generate a loadable object from a module and its dependencies for iOS
;; Warning! This only works on the simulator
(define (fusion#ios-compile-loadable-set output-file
                                         main-module
                                         #!key
                                         arch
                                         (target 'debug)
                                         (merge-modules #f)
                                         (cond-expand-features '())
                                         (compiler-options '())
                                         (version compiler-options) ;; XXX TODO: this alright?
                                         (compiled-modules '())
                                         (verbose #f))
  ;; Cond-expand features (relevant within the Sake environment)
  (##cond-expand-features (cons 'ios (##cond-expand-features)))
  ;; Checks
  (fusion#ios-project-supported?)
  (unless arch (err "fusion#ios-compile-loadable-set: arch argument is mandatory"))
  (unless (or (eq? arch 'i386) (eq? arch 'armv7) (eq? arch 'armv7s))
          (err "fusion#ios-compile-loadable-set: wrong arch argument"))
  (when merge-modules
        (err "fusion#ios-compile-loadable-set: merge-modules options is not yet implemented"))
  ;; Compute dependencies
  (let* ((modules-to-compile (append (%module-deep-dependencies-to-load main-module) (list main-module)))
         (all-modules (append compiled-modules modules-to-compile))
         (platform-type (case arch
                          ((i386) 'simulator)
                          ((armv7 armv7s) 'device)))
         (link-file (string-append output-file ".c")))
    ;; List files generated by compiling modules and the linkfile
    (let ((all-c-files
           (append (map (lambda (m) (string-append (ios-build-directory) (%module-filename-c m version: version))) all-modules)
                   (list (string-append (ios-build-directory) link-file)))))
      ;; Create iOS build directory if it doesn't exist
      (unless (file-exists? (ios-build-directory))
              (make-directory (ios-build-directory)))
      ;; Create iOS assets directory if it doesn't exist
      (unless (file-exists? (ios-assets-directory))
              (make-directory (ios-assets-directory)))
      ;; Generate modules (generates C code)
      (let ((something-generated? #f))
        (for-each
         (lambda (m)
           (let ((output-c-file (string-append (ios-build-directory) (%module-filename-c m version: version))))
             (if ((newer-than? output-c-file)
                  (string-append (%module-path-src m) (%module-filename-scm m)))
                 (begin
                   (set! something-generated? #t)
                   (sake#compile-to-c m
                                      cond-expand-features: (cons 'ios cond-expand-features)
                                      compiler-options: compiler-options
                                      verbose: verbose
                                      output: output-c-file)))))
         modules-to-compile)
        (when something-generated?
              (info/color 'blue "new C files generated")
              (fusion#ios-link-flat link-file all-modules version: version))
        ;; Compile objects
        (set! something-generated? #f)
        (let ((o-files
               (map (lambda (f)
                      (let* ((output-o-file (string-append (path-strip-extension f) ".o"))
                             (args `("-x"
                                     "objective-c"
                                     ,(string-append "-I" (ios-directory) "gambit/include")
                                     "-D___DYNAMIC"
                                     ,(string-append "-I" (ios-source-directory))
                                     ;;"-I/usr/local/Gambit-C/spheres/sdl2/deps/SDL2-2.0.3/include" ;;XXX TODO
                                     ;;,(string-append "-I" ios-sdk-directory "/System/Library/Frameworks/OpenGLES.framework/Headers") ;; XXX TODO
                                     ;; "-w" ;; XXX TODO
                                     ;; **************
                                     ;; **************
                                     ;; **************
                                     ;; **************
                                     "-c"
                                     ,f
                                     "-o"
                                     ,output-o-file)))
                        (when ((newer-than? output-o-file) f)
                              (unless something-generated?
                                      (info/color 'blue "compiling updated C files:"))
                              (info/color 'brown (string-append " >>>  " f))
                              (set! something-generated? #t)
                              (fusion#ios-run-compiler
                               arch: arch
                               platform-type: platform-type
                               compiler: 'gcc
                               arguments: args
                               verbose: verbose))
                        output-o-file))
                    all-c-files)))
          ;; Make bundle
          (info/color 'green "compiling C/Scheme code into a loadable object")
          (fusion#ios-run-linker
           arch: arch
           platform-type: platform-type
           ;; Flags not included:
           ;; "-lcrt1.o" This one is for the executables
           arguments: `( ;; TODO XXX         MOVE ESSENTIAL TO PROCEDURE
                        "-bundle"
                        "-demangle"
                        ;;"-dynamic"
                        "-ObjC"
                        "-all_load"
                        "-dead_strip"
                        ;;"-ios_simulator_version_min" "7.1.0" ;; XXXX TODO
                        "-iphoneos_version_min" "7.1.0"

                        "-objc_abi_version" "2"
                        ;;"-no_implicit_dylibs"
                        "-framework" "Foundation"
                        "-framework" "UIKit"
                        "-lobjc"
                        "-lSystem"
                        ;;"-Lios/gambit/lib"
                        ;;"-lgambc"

                        ;; For device
                        ;; /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/ld -demangle -dynamic -arch armv7s -all_load -dead_strip -iphoneos_version_min 7.1.0 -syslibroot /Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS7.1.sdk -ObjC -o /Users/Alvaro/Library/Developer/Xcode/DerivedData/SchemeSpheres-cplbjdvmioedvvdtudhkdgprgyrw/Build/Products/Debug-iphoneos/SchemeSpheres.app/SchemeSpheres -L/Users/Alvaro/Library/Developer/Xcode/DerivedData/SchemeSpheres-cplbjdvmioedvvdtudhkdgprgyrw/Build/Products/Debug-iphoneos -L/Users/Alvaro/Dropbox/working/DaTest/ios/SDL/lib -L/Users/Alvaro/Dropbox/working/DaTest/ios/gambit -L/Users/Alvaro/Dropbox/working/DaTest/ios/gambit/lib -L/Users/Alvaro/Dropbox/working/DaTest/ios/src -filelist /Users/Alvaro/Library/Developer/Xcode/DerivedData/SchemeSpheres-cplbjdvmioedvvdtudhkdgprgyrw/Build/Intermediates/SchemeSpheres.build/Debug-iphoneos/SchemeSpheres.build/Objects-normal/armv7s/SchemeSpheres.LinkFileList -framework Foundation -framework UIKit -framework OpenGLES -framework QuartzCore -framework CoreAudio -framework AudioToolbox -framework CoreGraphics -lSDL2 -lspheres -lgambc -dependency_info /Users/Alvaro/Library/Developer/Xcode/DerivedData/SchemeSpheres-cplbjdvmioedvvdtudhkdgprgyrw/Build/Intermediates/SchemeSpheres.build/Debug-iphoneos/SchemeSpheres.build/Objects-normal/armv7s/SchemeSpheres_dependency_info.dat -framework Foundation -lobjc -lSystem /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/5.1/lib/darwin/libclang_rt.ios.a -F/Users/Alvaro/Library/Developer/Xcode/DerivedData/SchemeSpheres-cplbjdvmioedvvdtudhkdgprgyrw/Build/Products/Debug-iphoneos
                        
                        ;; For simulator
                        ;;"-demangle -dynamic -arch i386 -all_load -dead_strip -ios_simulator_version_min 7.1.0 -syslibroot /Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator7.1.sdk -ObjC -o /Users/Alvaro/Library/Developer/Xcode/DerivedData/SchemeSpheres-cplbjdvmioedvvdtudhkdgprgyrw/Build/Products/Debug-iphonesimulator/SchemeSpheres.app/SchemeSpheres -lcrt1.o -L/Users/Alvaro/Library/Developer/Xcode/DerivedData/SchemeSpheres-cplbjdvmioedvvdtudhkdgprgyrw/Build/Products/Debug-iphonesimulator -L/Users/Alvaro/Dropbox/working/DaTest/ios/SDL/lib -L/Users/Alvaro/Dropbox/working/DaTest/ios/gambit -L/Users/Alvaro/Dropbox/working/DaTest/ios/gambit/lib -L/Users/Alvaro/Dropbox/working/DaTest/ios/src -filelist /Users/Alvaro/Library/Developer/Xcode/DerivedData/SchemeSpheres-cplbjdvmioedvvdtudhkdgprgyrw/Build/Intermediates/SchemeSpheres.build/Debug-iphonesimulator/SchemeSpheres.build/Objects-normal/i386/SchemeSpheres.LinkFileList -objc_abi_version 2 -no_implicit_dylibs -framework Foundation -framework UIKit -framework OpenGLES -framework QuartzCore -framework CoreAudio -framework AudioToolbox -framework CoreGraphics -lSDL2 -lgambc -dependency_info /Users/Alvaro/Library/Developer/Xcode/DerivedData/SchemeSpheres-cplbjdvmioedvvdtudhkdgprgyrw/Build/Intermediates/SchemeSpheres.build/Debug-iphonesimulator/SchemeSpheres.build/Objects-normal/i386/SchemeSpheres_dependency_info.dat -framework Foundation -lobjc -lSystem /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/5.1/lib/darwin/libclang_rt.ios.a -F/Users/Alvaro/Library/Developer/Xcode/DerivedData/SchemeSpheres-cplbjdvmioedvvdtudhkdgprgyrw/Build/Products/Debug-iphonesimulator"
                        ,@o-files
                        "-o"
                        ,(string-append (ios-directory) output-file))
           verbose: verbose))))))
