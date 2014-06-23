;;!! Android tasks

(define-task android:setup ()
  ;; Set up Android project files
  (fusion#android-project-set-target "android-15")
  ;; Create symlink to SDL library from sdl2
  (let ((SDL-link (string-append (android-jni-generator-directory) "deps/SDL")))
    (unless (file-exists? SDL-link)
            (create-symbolic-link (string-append (%sphere-path 'sdl2) "deps/SDL2-2.0.3") SDL-link))))

(define-task android:compile ()
  (fusion#android-compile-app "main" 'main
                              target: 'debug
                              cond-expand-features: '(debug)
                              compiler-options: '(debug)))

(define-task android:install ()
  (fusion#android-install 'debug))

(define-task android:run ()
  ;; Run the Activity
  (fusion#android-run "org.libsdl.app/org.libsdl.app.SDLActivity")
  ;; log cat
  (shell-command (string-append (android-adb-path) " logcat *:S *:F SchemeSpheres SDL SDL/APP")))

(define-task android (android:compile android:install android:run)
  'android)

(define-task android:clean ()
  (fusion#android-clean))

;;!! Host tasks

(define-task host:run ()
  (fusion#host-run-interpreted 'main)) 

(define-task host:compile ()
  (fusion#host-compile-exe "main" 'main))

(define-task host:clean ()
  (sake#default-clean))

(define-task host (host:run)
  'host)

;;!! Default task

(define help #<<end-of-help
  
  Tasks (run with 'sake <task>')
  ------------------------------
  
  android:setup             Setup Android project before running other tasks
  android:compile           Compile Android project
  android:install           Install App in current Android device (hardware or emulated)
  android:run               Run App in current Android device
  android:clean             Clean all Android generated files
  android                   Execute compile, install, run

  host:compile              Compile the host program as standalone
  host:run                  Run the host OS (Linux/OSX/Windows) program interpreted
  host:clean                Clean the generated host program files
  host                      Defaults to host:run

end-of-help
)

(define-task all ()
  (println help))
