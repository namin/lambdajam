(let ()
  (define factorial-cps
    (lambda (n k)
      (begin
        (show n)
        (if (= n 0) (k 1)
            (factorial-cps (- n 1) (lambda (v)
                                     (k (* n v))))))))
  (define factorial
    (lambda (n)
      (factorial-cps n (lambda (v) v))))
  
  (show (factorial 5)))
