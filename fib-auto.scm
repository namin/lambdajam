(define fib
  (lambda (n)
    (begin
      (show n)
      (if (or (= n 0) (= n 1))
          n
          (+ (fib (- n 1)) (fib (- n 2)))))))

(show (fib 5))
