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

;;; RI wrt continutations
(let ()
  (define apply-k
    (lambda (k v)
      (k v)))

  (define empty-k
    (lambda ()
      (lambda (v) v)))

  (define factorial-k
    (lambda (n k)
      (lambda (v)
        (apply-k k (* n v)))))

  (define factorial-cps
    (lambda (n k)
      (if (= n 0) (apply-k k 1)
          (factorial-cps (- n 1) (factorial-k n k)))))
  (define factorial
    (lambda (n)
      (factorial-cps n (empty-k))))

  (factorial-tests factorial))

;;; First-Order Representation for Continuations
(let ()
  (define apply-k
    (lambda (k v)
      (pmatch
       k
       ((empty-k)
        v)
       ((factorial-k ,n ,k)
        (apply-k k (* n v)))
       ;(,k (procedure? k) (k v))
       )))

  (define empty-k
    (lambda ()
      `(empty-k)))

  (define factorial-k
    (lambda (n k)
      `(factorial-k ,n ,k)))

  (define factorial-cps
    (lambda (n k)
      (if (= n 0) (apply-k k 1)
          (factorial-cps (- n 1) (factorial-k n k)))))
  (define factorial
    (lambda (n)
      (factorial-cps n (empty-k))))

  (factorial-tests factorial))

;;; Registerization of factorial-cps
(let ()
  (define apply-k
    (lambda (k v)
      (pmatch
       k
       ((empty-k)
        v)
       ((factorial-k ,n ,k)
        (apply-k k (* n v))))))

  (define empty-k
    (lambda ()
      `(empty-k)))

  (define factorial-k
    (lambda (n k)
      `(factorial-k ,n ,k)))

  (define *n* 'whocares)
  (define *k* 'whocares)
  (define factorial-cps
    (lambda ()
      (if (= *n* 0) (apply-k *k* 1)
          (begin
            (set! *k* (factorial-k *n* *k*))
            (set! *n* (- *n* 1))
            (factorial-cps)))))
  (define factorial
    (lambda (n)
      (begin
        (set! *k* (empty-k))
        (set! *n* n)
        (factorial-cps))))

  (factorial-tests factorial))

;;; Full registration
(let ()
  (define *v* 'whocares)
  (define apply-k
    (lambda ()
      (pmatch
       *k*
       ((empty-k)
        *v*)
       ((factorial-k ,n ,k)
        (begin
          (set! *k* k)
          (set! *v* (* n *v*))
          (apply-k))))))

  (define empty-k
    (lambda ()
      `(empty-k)))

  (define factorial-k
    (lambda (n k)
      `(factorial-k ,n ,k)))

  (define *n* 'whocares)
  (define *k* 'whocares)
  (define factorial-cps
    (lambda ()
      (if (= *n* 0)
          (begin
            (set! *v* 1)
            (apply-k))
          (begin
            (set! *k* (factorial-k *n* *k*))
            (set! *n* (- *n* 1))
            (factorial-cps)))))
  (define factorial
    (lambda (n)
      (begin
        (set! *k* (empty-k))
        (set! *n* n)
        (factorial-cps))))

  (factorial-tests factorial))
