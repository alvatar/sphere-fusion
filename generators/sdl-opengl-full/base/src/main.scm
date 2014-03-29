;;! Run or attach 'main'
(cond-expand
 (android (c-define (c-scheme-main) () void "scheme_main" "" (main)))
 (host (main))
 (else #!void))
