(load "test-check.scm")
(load "pmatch.scm")
(load "sugar.scm")

(define (M-tests M inc)
  (eg ((M inc) '(1 2 3)) '(2 3 4)))

(let ()
  (define M
    (lambda (f)
      (lambda (xs)
        (if (null? xs)
            '()
            (cons (f (car xs)) ((M f) (cdr xs)))))))
  
  (M-tests M add1))

