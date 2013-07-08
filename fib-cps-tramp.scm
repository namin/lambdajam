(let ()
  (define (fib-cps n k)
    (begin
      (show n)
      (if (or (= n 0) (= n 1)) (k n)
          (lambda ()
            (fib-cps (- n 1)
                     (lambda (v1)
                       (lambda ()
                         (fib-cps (- n 2)
                                  (lambda (v2)
                                    (k (+ v1 v2)))))))))))

  (define (loop r)
    (while (procedure? r)
           (set! r (r))
           r))

  (define (fib n)
    (loop (fib-cps n (lambda (v) v))))

  (show (fib 5)))
