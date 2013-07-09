(let ()
  (define factorial-cps
    (lambda (n k)
      (lambda ()
        (begin
          (show n)
          (if (= n 0) (k 1)
              (factorial-cps (- n 1) (lambda (v)
                                       (k (* n v)))))))))

  (define loop
    (lambda (r)
      (while (procedure? r)
             (set! r (r))
             r)))
  
  (define factorial
    (lambda (n)
      (loop (factorial-cps n (lambda (v) v)))))
  
  (show (factorial 5)))
