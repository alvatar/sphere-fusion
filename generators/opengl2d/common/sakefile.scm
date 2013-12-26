(include "/data/projects/sphere-fusion/src/sake-fusion.scm")

;-------------------------------------------------------------------------------
; Tasks
;-------------------------------------------------------------------------------

(define-task desktop:run ()
  (fusion#desktop-run-interpreted '(opengl2.1: main version: (debug))))

(define-task desktop:compile ()
  (fusion#desktop-compile-and-link '(main)))

(define-task android:compile ()
  (shell-command "cd android && ndk-build"))

(define-task android:upload ()
  'upload)

(define-task android (android:compile android:upload)
  'android)

(define-task all (desktop:compile)
  'all)
