(let ()
  (define (fib-cps n k)
    (if (or (= n 0) (= n 1)) (k n)
        (fib-cps (- n 1)
                 (lambda (v1)
                   (fib-cps (- n 2)
                            (lambda (v2)
                              (k (+ v1 v2))))))))

  (define (fib n)
    (fib-cps n (lambda (v) v)))

  (show (fib 5)))
