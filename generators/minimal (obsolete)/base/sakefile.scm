;;!! Android tasks

(define-task android:setup ()
  (fusion#android-project-set-target "android-10"))

(define-task android:compile ()
  (fusion#android-compile-app "main" 'main target: 'debug))

(define-task android:install ()
  (fusion#android-install 'debug))

(define-task android:run ()
  (fusion#android-run
   "org.schemespheres.fusion/org.schemespheres.fusion.MainActivity"))

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
