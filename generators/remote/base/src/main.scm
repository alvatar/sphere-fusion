;;! Define function to be used for output interception in the server
(cond-expand
 (android (define server-display
            (compose (c-lambda (char-string) void "input_from_scheme") string)))
 (host (define server-display pp))
 (else (void)))

;;! Run or attach 'main'
(cond-expand
 (android (c-define (c-scheme-main) '() void "scheme_main" "" (main)))
 (host (main))
 (else (void)))
