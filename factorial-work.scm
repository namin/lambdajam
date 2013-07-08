(load "test-check.scm")
(load "pmatch.scm")
(load "sugar.scm")

(define (factorial-tests factorial)
  (eg (factorial 6) 720))

(let ()
  (define factorial
    (lambda (n)
      (if (= n 0) 1
          (* n (factorial (- n 1))))))
  
  (factorial-tests factorial))

