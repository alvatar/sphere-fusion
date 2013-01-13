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
   (sake:compile-to-exe "sfusion" '(sfusion))
   ;; Compile helpers and application drivers

   ;; Prepare Sake extensions
   ))

(define-task install ()
  (copy-file (string-append (current-build-directory) "sfusion")
             "~~bin/sfusion"))

(define-task clean ()
  (parameterize
   ((fusion-setup-directory ""))
   (delete-file (android-build-directory))
   (fusion:android-clean))
  (delete-file (current-build-directory))
  'clean)

(define-task all (compile install)
  'all)
