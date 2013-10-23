(include "src/sake-fusion.scm")

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

   ;; Compile SFusion
   (sake#compile-to-exe "sfusion" '(sfusion))))

(define-task install ()
  (sake#install-sphere-to-system extra-directories: '("templates"))
  (copy-file (string-append (current-build-directory) "sfusion")
             "~~bin/sfusion"))

(define-task clean ()
  ;; (parameterize
  ;;  ((fusion-setup-directory ""))
  ;;  (delete-file (android-build-directory))
  ;;  (fusion:android-clean))
  (sake#default-clean))

(define-task android:test (install) ; Install is here so the templates get installed
  (if (file-exists? "test/tmp")
      (sake#delete-file "test/tmp" force: #t recursive: #t))
  (shell-command "sfusion new -t opengl2d -p android test/tmp")
  (shell-command "cd test/tmp && sake android"))

(define-task test (android:test)
  '(sake#test 'opengl2.1-2d))

(define-task all (compile install)
  'all)
