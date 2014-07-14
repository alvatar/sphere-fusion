(include "src/sake-extensions/fusion-android.scm")
(include "src/sake-extensions/fusion-ios.scm")


(define non-compiled-modules
  '(core))

(define-task compile ()
  ;; Compile Sfusion
  (sake#compile-to-exe "sfusion" '(sfusion) cond-expand-features: '(optimize)))

(define-task install-1 ()
  ;; Install Sfusion
  (copy-file (string-append (current-bin-directory) "sfusion")
             "~~bin/sfusion")
  ;; Install Sake extensions
  (copy-file (string-append (current-source-directory) "sake-extensions/template.scm")
             "~~spheres/sake-extensions/src/template.scm")
  (copy-file (string-append (current-source-directory) "sake-extensions/fusion-host.scm")
             "~~spheres/sake-extensions/src/fusion-host.scm")
  (copy-file (string-append (current-source-directory) "sake-extensions/fusion-android.scm")
             "~~spheres/sake-extensions/src/fusion-android.scm")
  (copy-file (string-append (current-source-directory) "sake-extensions/fusion-ios.scm")
             "~~spheres/sake-extensions/src/fusion-ios.scm"))

(define-task install-2 ()
  ;; Delete Android toolchain prior to installing the directory
  (if (file-exists? "test/tmp")
      (sake#delete-file (string-append (android-toolchain-directory)) recursive: #t))
  ;; Install the Sphere's modules and extra directories
  (sake#install-sphere-to-system extra-directories: '("generators" "tools" "android" "ios")))

(define-task install (install-1 install-2)
  'install)

(define-task clean ()
  (sake#default-clean)
  (if (file-exists? "test/tmp")
      (sake#delete-file "test/tmp" force: #t recursive: #t))
  (let ((android-sc (string-append (android-directory) "syntax-case.o1")))
    (if (file-exists? android-sc)
        (sake#delete-file android-sc force: #t)))
  (let ((android-sc (string-append (ios-directory) "syntax-case.o1")))
    (if (file-exists? android-sc)
        (sake#delete-file android-sc force: #t))))

(define-task all (compile)
  'all)

;;------------------------------------------------------------------------------
;; Precompiled Android & iOS modules

(define-task compile-android-syntax-case ()
  (fusion#android-installed?)
  ;; Generate Android toolchain
  (let ((android-platform-str "android-15"))
    (shell-command
     (string-append (android-ndk-path) "build/tools/make-standalone-toolchain.sh"
                    " --platform=\"" android-platform-str "\""
                    " --install-dir=\"" (android-toolchain-directory) "\"")))
  ;; Compile syntax-case to C
  (gambit-eval-here
   `((parameterize ((current-readtable (readtable-sharing-allowed?-set (current-readtable) 'serialize)))
                   (compile-file-to-target
                    ,(string-append (%sphere-path 'core) "src/scsc/syntax-case.scm")
                    output: ,(string-append (current-build-directory) "syntax-case.o1.c"))))
   flags-string: "-f")
  ;; Compile .o1 module
  (fusion#android-run-compiler arch: 'arm
                               compiler: 'gcc
                               arguments:
                               `(,(string-append "-I" (path-expand "~~/include/"))
                                 "-shared"
                                 "-Wno-unused" "-Wno-write-strings"
                                 "-fno-math-errno" "-fschedule-insns2" "-fno-common"
                                 "-fno-trapping-math" "-fno-strict-aliasing" "-fwrapv" "-fomit-frame-pointer" "-fPIC"
                                 ;; "-mieee-fp " "-flat_namespace" "-undefined" "suppress" ;; (not available in ARM gcc)
                                 "-D___DYNAMIC"
                                 "-D___SINGLE_HOST" "-O2" ;; optimizations
                                 "-o" ,(string-append (android-directory) "syntax-case.o1")
                                 ,(string-append (current-build-directory) "syntax-case.o1.c"))
                               verbose: #t)
  ;; Cleanup Android toolchain
  (sake#delete-file (string-append (android-toolchain-directory)) recursive: #t))

(define-task compile-ios-syntax-case ()
  ;; Compile syntax-case to C
  (gambit-eval-here
   `((parameterize ((current-readtable (readtable-sharing-allowed?-set (current-readtable) 'serialize)))
                   (compile-file-to-target
                    ,(string-append (%sphere-path 'core) "src/scsc/syntax-case.scm")
                    output: ,(string-append (current-build-directory) "syntax-case.o1.c"))))
   flags-string: "-f")
  ;; Compile .o1 module for the simulator
  (fusion#ios-run-compiler arch: 'i386
                           platform-type: 'simulator
                           compiler: 'gcc
                           arguments:
                           `(,(string-append "-I" (path-expand "~~/include/"))
                             "-bundle"
                             "-Wno-unused" "-Wno-write-strings" "-fno-math-errno" "-fno-common"
                             "-fno-trapping-math" "-fno-strict-aliasing" "-fwrapv" "-fomit-frame-pointer" "-fPIC"
                             "-flat_namespace" "-undefined" "suppress"
                             ;; "-mieee-fp" "-fschedule-insns2" ;; (not available in iOS C compiler)
                             "-D___DYNAMIC"
                             "-D___SINGLE_HOST" "-O2" ;; optimizations
                             "-o" ,(string-append (ios-directory) "syntax-case.o1")
                             ,(string-append (current-build-directory) "syntax-case.o1.c"))
                           verbose: #t)
  ;; Cleanup Android toolchain
  (sake#delete-file (string-append (android-toolchain-directory)) recursive: #t))

;;------------------------------------------------------------------------------
;; Tests

(define-task test-minimal ()
  (if (file-exists? "test/tmp")
      (sake#delete-file "test/tmp" force: #t recursive: #t))
  (shell-command "sfusion new -g minimal -s generators/ test/tmp")
  (shell-command "cd test/tmp && sake host"))

(define-task test-minimal-android ()
  (if (file-exists? "test/tmp")
      (sake#delete-file "test/tmp" force: #t recursive: #t))
  (shell-command "sfusion new -g minimal -s generators/ test/tmp")
  (shell-command "cd test/tmp && sake android:setup android"))

(define-task test-remote ()
  (if (file-exists? "test/tmp")
      (sake#delete-file "test/tmp" force: #t recursive: #t))
  (shell-command "sfusion new -g remote -s generators/ test/tmp")
  (shell-command "cd test/tmp && sake host"))

(define-task test-remote-android ()
  (if (file-exists? "test/tmp")
      (sake#delete-file "test/tmp" force: #t recursive: #t))
  (shell-command "sfusion new -g remote -s generators/ test/tmp")
  (shell-command "cd test/tmp && sake android:setup android"))

(define-task test-sdl-opengl ()
  (if (file-exists? "test/tmp")
      (sake#delete-file "test/tmp" force: #t recursive: #t))
  (shell-command "sfusion new -g sdl-opengl -s generators/ test/tmp")
  (shell-command "cd test/tmp && sake host:run"))

(define-task test-sdl-opengl-android ()
  (if (file-exists? "test/tmp")
      (sake#delete-file "test/tmp" force: #t recursive: #t))
  (shell-command "sfusion new -g sdl-opengl -s generators/ test/tmp")
  (shell-command "cd test/tmp && sake android"))

(define-task test-opengl ()
  (sake#test 'opengl2.1-2d))

(define-task test (test-sdl-opengl)
  'test)
