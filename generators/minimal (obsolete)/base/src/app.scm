(define (fib x)
  (letrec 
      ((helper (lambda (n a b)
                 (if (= n 0)
                     a
                     (helper (- n 1) b (+ a b))))))
    (helper x 0 1)))

(define (test-ports)
  (read-line (open-process "ls") "failed"))

