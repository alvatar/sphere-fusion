;;! Define function to be used for output interception in the server
(cond-expand
 (android (define server-display
            (c-lambda (char-string) void "input_from_scheme")))
 (host (define server-display pp))
 (else (void)))

;;! Main
(define (main)
  (repl-server-initialize-defaults!)
  (repl-server-start #f intercept-output: (compose server-display string))
  ;; You could actually be doing useful stuff here if you wish...
  (thread-sleep! +inf.0))

;;! Run or attach 'main'
(cond-expand
 (android (c-define (c-scheme-main) '() void "scheme_main" "" (main)))
 (host (main))
 (else (void)))
