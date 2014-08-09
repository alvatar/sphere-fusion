;;----------------------------------------------------------------------------------------
;;!! Android tasks

(define-task android:setup ()
  ;; Set up Android project files
  (fusion#android-project-setup 'android-15)
  ;; Create symlink to SDL library from SDL2 Sphere
  (let ((SDL-link (string-append (android-jni-generator-directory) "deps/SDL")))
    (unless (file-exists? SDL-link)
            (create-symbolic-link (string-append (%sphere-path 'sdl2) "deps/SDL2-2.0.3") SDL-link))))

(define-task android:compile ()
  (if #t ;; #t to compile as a single app executable
      ;; Compile all modules within the app executable
      (fusion#android-compile-app "main" 'main
                                  target: 'debug
                                  cond-expand-features: '(debug)
                                  compiler-options: '(debug)
                                  num-threads: +inf.0
                                  verbose: #t)
      (begin
        ;; Compile the Android app with just the loader code
        (fusion#android-compile-app "my-app" 'loader
                                    target: 'debug
                                    cond-expand-features: '(debug)
                                    compiler-options: '(debug)
                                    verbose: #t)
        ;; Compile the main module and its dependencies as a loadable object for the ARM
        ;; arch.  The (load) function takes care of loading code dinamically, both compiled
        ;; and source code. This can be used during Android development in the following ways:
        ;; - Bundling the code within the APK
        ;; - Uploading the code to the SD card
        ;; - Dynamically running code within the Remote Debugger in Emacs or the terminal
        (fusion#android-compile-loadable-set "main-minimal.o1" 'main-minimal
                                             merge-modules: #f
                                             target: 'debug
                                             cond-expand-features: '(debug)
                                             compiler-options: '(debug)
                                             cc-options: `(,(string-append "-I" (android-jni-directory) "deps/SDL/include") "-w")
                                             ld-options: '("-lEGL" "-lGLESv2")
                                             verbose: #t))))

(define-task android:install ()
  (fusion#android-install 'debug))

(define-task android:run ()
  ;; Run the Activity
  (fusion#android-run "org.libsdl.app/org.libsdl.app.SDLActivity")
  ;; Log cat
  (shell-command (string-append (android-adb-path) " logcat *:S *:F SchemeSpheres SDL SDL/APP")))

(define-task android (android:compile android:install android:run)
  'android)

(define-task android:clean ()
  (fusion#android-clean))

;;----------------------------------------------------------------------------------------
;;!! iOS tasks

(define-task ios:setup ()
  ;; Create symlink to SDL include library from SDL2 Sphere
  (let ((SDL-link (string-append (ios-directory) "SDL/include")))
    (unless (file-exists? SDL-link)
            (create-symbolic-link (string-append (%sphere-path 'sdl2) "deps/SDL2-2.0.3/include")
                                  SDL-link))))

(define-task ios:compile ()
  (if #f ;; #t to compile as a single app executable
      ;; Compile all modules within the app executable
      (fusion#ios-compile-app 'app
                              arch: 'i386 ;; armv7 / armv7s
                              target: 'debug
                              cond-expand-features: '(debug static)
                              compiler-options: '(debug)
                              cc-options: (list "-w"
                                                ;;"-D___SINGLE_HOST"
                                                "-O1"
                                                "-fdiagnostics-show-note-include-stack"
                                                "-fcolor-diagnostics"
                                                (string-append "-I" (ios-directory) "SDL/include")
                                                (string-append "-I" (ios-directory) "SDL_image/include"))
                              verbose: #t)
      (let ((arch 'i386)) ;; armv7 / armv7s
        ;; Copy all the foreign dependencies of the module, so they can be automatically
        ;; loaded by the application loader
        (fusion#ios-copy-foreign-dependencies 'app)
        ;; Compile the main module and its dependencies as a loadable object, for all iOS
        ;; archs. The (load) function takes care of loading code dinamically, both compiled
        ;; and source code. This can be used during iOS development in the following ways:
        ;; - Uploading code to the Resources folder (part of the app bundle)
        ;; - Uploading code to the Documents folder (created at runtime, read/write when the app is running)
        ;; - Dynamically running within the Remote Debugger in Emacs or the terminal
        ;; Finally, take into account that loadable libraries do work only on the simulator
        #;
        (fusion#ios-compile-loadable-set "globals.o1" 'globals
                                         merge-modules: #f
                                         target: 'debug
                                         arch: arch
                                         cond-expand-features: '(ios debug)
                                         compiler-options: '(debug)
                                         cc-options: (list "-w"
                                                           (string-append "-I" (ios-directory) "SDL/include")
                                                           (string-append "-I" (ios-directory) "SDL_image/include"))
                                         verbose: #t)
        ;; Compile the iOS app with just the loader module
        ;; The loader will decide which object to load according to the runtime architecture
        (fusion#ios-compile-app 'loader
                                arch: arch
                                target: 'debug
                                cond-expand-features: '(debug)
                                compiler-options: '(debug)
                                cc-options: (list "-w"
                                                  (string-append "-I" (ios-directory) "SDL/include")
                                                  (string-append "-I" (ios-directory) "SDL_image/include"))
                                verbose: #t))))

(define-task ios:run ()
  ;; First kill any running instance of the iPhone simulator
  (shell-command "killall -9 iPhone\\ Simulator &>/dev/null")
  ;; Run asset server and iOS simulator
  (let* ((asset-server (parameterize
                        ((current-directory (current-source-directory)))
                        (open-process (list path: "python"
                                            arguments: '("-m" "SimpleHTTPServer")
                                            stdin-redirection: #t
                                            stdout-redirection: #t
                                            stderr-redirection: #t))))
         (server-pid (process-pid asset-server))
         (ios-simulator (parameterize
                         ((current-directory (ios-directory)))
                         (open-process (list path: (ios-sim-path)
                                             arguments: '("launch" "build/Debug-iphonesimulator/SchemeSpheres.app")))))
         (simulator-pid (process-pid ios-simulator)))
    ;; Wait for the iOS simulator to finish
    (process-status ios-simulator)
    ;; Kill asset server when leaving simulator
    (let ((kill-server (open-process
                        (list path: "kill"
                              arguments: `("-15" ,(number->string server-pid)))))
          (kill-simulator (open-process
                           (list path: "kill"
                                 arguments: `("-9" ,(number->string simulator-pid))))))
      (process-status kill-server)
      (process-status kill-simulator))))

(define-task ios:xcode ()
  (shell-command "open -a Xcode ios/SchemeSpheres.xcodeproj"))

(define-task ios:clean ()
  (fusion#ios-clean))

(define-task ios (ios:compile ios:run)
  'ios)

;;----------------------------------------------------------------------------------------
;;!! Host (Linux/OSX) tasks

(define-task host:run ()
  (fusion#host-run-interpreted 'loader)) 

(define-task host:compile ()
  ;; Note (merge-modules): If #t this will include all dependencies in one big file before compiling to C
  ;; Note (compile-loadable-set): this must be linked flat
  (if #t ;; #t to compile the application as a single standalone
      ;; Bundle as a single executable
      (fusion#host-compile-exe "my-application-standalone" 'app
                               cond-expand-features: (case (sake#host-platform)
                                                       ((osx) '(osx debug static))
                                                       (else '(debug static)))
                               merge-modules: #f
                               verbose: #t)
      (begin
        (error "loadable modules in the host system is not supported yet (WIP)")
        ;; Compile as a loader and a loadable library
        (fusion#host-compile-exe "my-application" 'loader
                                 cond-expand-features: '(osx debug static)
                                 compiler-options: '(debug))
        ;; Compile the main module and its dependencies as a loadable object. The (load)
        ;; function takes care of loading code dinamically, both compiled and source code.
        (fusion#host-compile-loadable-set "app.o1" 'app
                                          merge-modules: #f
                                          cond-expand-features: '(osx debug)
                                          compiler-options: '(debug)
                                          verbose: #t))))

(define-task host:clean ()
  (sake#default-clean))

(define-task host (host:run)
  'host)

;;----------------------------------------------------------------------------------------
;;!! General

(define help #<<end-of-help
  
    Tasks (run with 'ssake <task>')
    ------------------------------
  
    android:setup             Setup Android project before running other tasks
    android:compile           Compile the Android app
    android:install           Install App in current Android device (hardware or emulated)
    android:run               Run App in current Android device
    android:clean             Clean all Android generated files
    android                   Execute compile, install, run

    ios:setup                 Setup iOS project before running other tasks
    ios:compile               Compile the iOS app
    ios:run                   Launch the iOS Simulator and run the app
    ios:xcode                 Open the iOS project in Xcode
    ios:clean                 Clean all iOS generated files
    ios                       Execute compile and run
  
    host:compile              Compile the host program as standalone
    host:run                  Run the host OS (Linux/OSX) program interpreted
    host:clean                Clean the generated host program files
    host                      Defaults to host:run

    clean                     Clean all targets

end-of-help
)

(define-task clean (android:clean ios:clean host:clean)
  'clean)

(define-task all ()
  (println help))
