;;; Copyright (c) 2014 by Álvaro Castro Castilla

(cond-expand
 (android (c-define (c-scheme-main) () void "scheme_main" "" (main)))
 (host (main))
 (else #!void))
