(cond-expand
 (android
  (c-define (c-fib x) (int32) int "fib" "" (fib x))
  (c-define (c-testports) () char-string "testports" "" (test-ports)))
 (host
  (println (fib 10))
  (println (test-ports)))
 (else
  (void)))

