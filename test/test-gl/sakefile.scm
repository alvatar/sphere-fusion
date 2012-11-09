(%include fusion: fusion-sake#)

;-------------------------------------------------------------------------------
; Tasks
;-------------------------------------------------------------------------------

(define-task android ()
  (fusion:setup)
  (fusion:android-generate-manifest-and-properties
   api-level: 8
   app-name: "Example Fusion App")
  (fusion:android-import-addon '(gl))
  (fusion:android-compile-and-link '((test-gl: main version: (debug)))
                                   compiler-options: '(debug)))

(define-task all (android)
  'all)
