(##include "src/sake-fusion.scm")

(define modules
  '(core))

(define-task compile ()
  (parameterize
   ((fusion-setup-directory ""))
   (make-directory (default-lib-directory))
   ;; (make-directory (android-build-directory))
   ;; Generate internal Fusion modules
   ;; (fusion:android-generate-modules
   ;;  '((fusion: driver)
   ;;    (fusion: gl-cairo)))
   ;; (fusion:android-generate-modules
   ;;  '((fusion: driver version: (debug))
   ;;    (fusion: gl-cairo version: (debug))))

   ;; Compile Fusion modules
   (for-each (lambda (m) (sake#compile-module m compiler-options: '(debug))) modules)
   (for-each sake#compile-module modules)

   ;; Compile SFusion
   (sake#compile-to-exe "sfusion" '(sfusion))))

(define-task post-compile ()
  ;; Create SDL symlink in SDL-based generators
  ;; When installed, files get actually copied as directory recursion is done.
  #;(let ((SDL-link "generators/opengl2d/android/android/jni/SDL"))
    (if (file-exists? SDL-link)
        (##delete-file SDL-link))
    (create-symbolic-link (string-append (%sphere-path 'sdl2) "src/android/jni/SDL")
                          (string-append (current-directory) SDL-link)))
  
  ;; Make Fusion modules available in /lib
  (for-each (lambda (m) (sake#make-module-available m versions: '(() (debug)))) modules))

(define-task install-binary-and-sake-extension ()
  ;; Install Sphere and Fusion Templates
  (copy-file (string-append (current-build-directory) "sfusion")
             "~~bin/sfusion")
  ;; Install Sake extension
  (copy-file (string-append (current-source-directory) "sake-fusion.scm")
             "~~spheres/sake-extensions/src/fusion.scm"))

(define-task install (install-binary-and-sake-extension)
  (sake#install-sphere-to-system extra-directories: '("generators")))

(define-task clean ()
  ;; (parameterize
  ;;  ((fusion-setup-directory ""))
  ;;  (delete-file (android-build-directory))
  ;;  (fusion:android-clean))
  (sake#default-clean))

(define-task all (compile post-compile install-binary-and-sake-extension)
  'all)


;;------------------------------------------------------------------------------
;; Tests

(define-task test-android ()
  (if (file-exists? "test/tmp")
      (sake#delete-file "test/tmp" force: #t recursive: #t))
  (shell-command "sfusion new -t opengl2d -p android test/tmp")
  (shell-command "cd test/tmp && sake android")
  (sake#delete-file "test/tmp" force: #t recursive: #t))

(define-task test-minimal ()
  (if (file-exists? "test/tmp")
      (sake#delete-file "test/tmp" force: #t recursive: #t))
  (shell-command "sfusion new -g minimal test/tmp")
  (shell-command "cd test/tmp && sake")
  (sake#delete-file "test/tmp" force: #t recursive: #t))

(define-task test-opengl ()
  (sake#test 'opengl2.1-2d))

(define-task test (test-minimal)
  'test)
