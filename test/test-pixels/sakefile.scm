(%include fusion: fusion-sake#)

;-------------------------------------------------------------------------------
; Tasks
;-------------------------------------------------------------------------------

(define-task android ()
  (fusion:setup)
  (fusion:android-generate-manifest-and-properties
   api-level: 8
   app-name: "Example Fusion App")
  (fusion:android-compile-and-link '(main)))

(define-task all (android)
  'all)
