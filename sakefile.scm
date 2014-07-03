(define non-compiled-modules
  '(core))

(define-task compile ()
  ;; Compile Sfusion
  (sake#compile-to-exe "sfusion" '(sfusion) cond-expand-features: '(optimize)))

(define-task install-1 ()
  ;; Install Sfusion
  (copy-file (string-append (current-build-directory) "sfusion")
             "~~bin/sfusion")
  ;; Install Sake extensions
  (copy-file (string-append (current-source-directory) "sake-extensions/fusion-host.scm")
             "~~spheres/sake-extensions/src/fusion-host.scm")
  (copy-file (string-append (current-source-directory) "sake-extensions/fusion-android.scm")
             "~~spheres/sake-extensions/src/fusion-android.scm")
  (copy-file (string-append (current-source-directory) "sake-extensions/template.scm")
             "~~spheres/sake-extensions/src/template.scm"))

(define-task install-2 ()
  ;; Install the Sphere's modules and extra directories
  (sake#install-sphere-to-system extra-directories: '("generators")))

(define-task install (install-1 install-2)
  'install)

(define-task clean ()
  (sake#default-clean)
  (if (file-exists? "test/tmp")
      (sake#delete-file "test/tmp" force: #t recursive: #t)))

(define-task all (compile)
  'all)


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
