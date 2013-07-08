(load "test-check.scm")
(load "pmatch.scm")
(load "sugar.scm")

(let ()
  (define M
    (lambda (f)
      (lambda (xs)
        (if (null? xs)
            '()
            (cons (f (car xs)) ((M f) (cdr xs)))))))

  (eg ((M add1) '(1 2 3)) '(2 3 4)))


