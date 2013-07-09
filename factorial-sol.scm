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

;;; CPS
(let ()
  (define factorial-cps
    (lambda (n k)
      (if (= n 0) (k 1)
          (factorial-cps (- n 1) (lambda (v)
                                   (k (* n v)))))))
  (define factorial
    (lambda (n)
      (factorial-cps n (lambda (v) v))))
  
  (factorial-tests factorial))

;;; Trampoline
(let ()
  (define factorial-cps
    (lambda (n k)
      (lambda ()
        (if (= n 0) (k 1)
            (factorial-cps (- n 1) (lambda (v)
                                     (k (* n v))))))))

  (define loop
    (lambda (r)
      (while (procedure? r)
             (set! r (r))
             r)))
  
  (define factorial
    (lambda (n)
      (loop (factorial-cps n (lambda (v) v)))))
  
  (factorial-tests factorial))

