(load "test-check.scm")
(load "pmatch.scm")
(load "sugar.scm")

(define (fib-tests fib)
  (eg (fib 5) 5)
  (eg (fib 6) 8))

(let ()
  (define (fib n)
    (if (< n 2) n
        (+ (fib (- n 1)) (fib (- n 2)))))
  (fib-tests fib))

