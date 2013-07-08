(let ()
  (define (fib n)
    (if (or (= n 0) (= n 1)) n
        (+ (fib (- n 1) (- n 2)))))

  (show (fib 5)))
