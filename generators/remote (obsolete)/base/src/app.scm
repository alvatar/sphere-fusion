;;! Main
(define (main)
  (repl-server-initialize-defaults!)
  (repl-server-start #f intercept-output: server-display)
  ;; You could actually be doing useful stuff here if you wish...
  (thread-sleep! +inf.0))
