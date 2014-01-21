(##include "src/sake-extensions/fusion.scm")

(define non-compiled-modules
  '(core))

(define-task compile ()
  ;; Compile Sfusion
  (sake#compile-to-exe "sfusion" '(sfusion) cond-expand-features: '(optimize)))

(define-task post-compile ()
  ;; Create SDL symlink in SDL-based generators
  ;; When installed, files get actually copied as directory recursion is done.
  #;(let ((SDL-link "generators/opengl2d/android/android/jni/SDL"))
    (if (file-exists? SDL-link)
        (##delete-file SDL-link))
    (create-symbolic-link (string-append (%sphere-path 'sdl2) "src/android/jni/SDL")
                          (string-append (current-directory) SDL-link)))
  
  ;; Make Fusion modules available in /lib
  'post-compile)

(define-task install-binary-and-sake-extensions ()
  ;; Install Sfusion
  (copy-file (string-append (current-build-directory) "sfusion")
             "~~bin/sfusion")
  ;; Install Sake extensions
  (copy-file (string-append (current-source-directory) "sake-extensions/fusion.scm")
             "~~spheres/sake-extensions/src/fusion.scm")
  (copy-file (string-append (current-source-directory) "sake-extensions/template.scm")
             "~~spheres/sake-extensions/src/template.scm"))

(define-task install (install-binary-and-sake-extensions)
  (sake#install-sphere-to-system extra-directories: '("generators")))

(define-task clean ()
  (sake#default-clean))

(define-task all (compile post-compile)
  'all)


;;------------------------------------------------------------------------------
;; Tests

(define-task test-minimal ()
  (if (file-exists? "test/tmp")
      (sake#delete-file "test/tmp" force: #t recursive: #t))
  (shell-command "sfusion new -g minimal -s generators/ test/tmp")
  (shell-command "cd test/tmp && sake host")
  (sake#delete-file "test/tmp" force: #t recursive: #t))

(define-task test-remote ()
  (if (file-exists? "test/tmp")
      (sake#delete-file "test/tmp" force: #t recursive: #t))
  (shell-command "sfusion new -g remote -s generators/ test/tmp")
  (shell-command "cd test/tmp && sake host")
  (sake#delete-file "test/tmp" force: #t recursive: #t))

(define-task test-opengl ()
  (sake#test 'opengl2.1-2d))

(define-task test (test-remote)
  'test)
