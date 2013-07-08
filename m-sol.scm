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

;;; CPS
(let ()
  (define M
    (lambda (f k)
      (k (lambda (xs k)
           (if (null? xs)
               (k '())
               (M f (lambda (p)
                      (p (cdr xs) (lambda (d)
                                    (f (car xs) (lambda (a)
                                                  (k (cons a d)))))))))))))
  (eg (M (lambda (x k) (k (add1 x))) (lambda (p) (p '(1 2 3) (lambda (v) v)))) '(2 3 4)))

