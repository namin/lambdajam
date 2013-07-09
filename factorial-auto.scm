(define factorial
  (lambda (n)
    (begin
      (show n)
      (if (= n 0) 1
          (* n (factorial (- n 1)))))))

(show (factorial 5))

