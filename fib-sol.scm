(load "test-check.scm")
(load "pmatch.scm")
(load "sugar.scm")

(define (fib-tests fib)
  (eg (fib 5) 5)
  (eg (fib 6) 8))

(let ()
  (define (fib n)
    (if (< n 2) n
        (+ (fib (- n 1)) (fib (- n 2)))))
  (fib-tests fib))

;;; CPS
(let ()
  (define (fib-cps n k)
    (if (< n 2) (k n)
        (fib-cps (- n 1)
                 (lambda (v1)
                   (fib-cps (- n 2)
                            (lambda (v2)
                              (k (+ v1 v2))))))))

  (define (fib n)
    (fib-cps n (lambda (v) v)))

  (fib-tests fib))

;;; Trampoline
(let ()
  (define (fib-cps n k)
    (lambda ()
      (if (< n 2) (k n)
          (fib-cps (- n 1)
                   (lambda (v1)
                     (fib-cps (- n 2)
                              (lambda (v2)
                                (k (+ v1 v2)))))))))

  (define (loop r)
    (while (procedure? r)
           (set! r (r))
           r))

  (define (fib n)
    (loop (fib-cps n (lambda (v) v))))

  (fib-tests fib))

;;; RI
(let ()
    (define apply-k
    (lambda (k v)
      (k v)))

  (define empty-k
    (lambda ()
      (lambda (v) v)))

  (define fib-inner-k
    (lambda (v1 k)
      (lambda (v2)
        (apply-k k (+ v1 v2)))))

  (define fib-outer-k
    (lambda (n k)
      (lambda (v1)
        (fib-cps (- n 2) (fib-inner-k v1 k)))))

  (define (fib-cps n k)
    (if (< n 2) (apply-k k n)
        (fib-cps (- n 1) (fib-outer-k n k))))

  (define (fib n)
    (fib-cps n (empty-k)))

  (fib-tests fib))

;;; FO
(let ()
  (define apply-k
    (lambda (k^ v)
      (pmatch
       k^
       ((empty-k)
        v)
       ((fib-inner-k ,v1 ,k)
        (apply-k k (+ v1 v)))
       ((fib-outer-k ,n ,k)
        (fib-cps (- n 2) (fib-inner-k v k)))
       ;;; Once all is first-order, no need for this fall-through case.
       (,k^ (guard (procedure? k^))
        (k^ v)))))

  (define empty-k
    (lambda ()
      `(empty-k)))

  (define fib-inner-k
    (lambda (v1 k)
      `(fib-inner-k ,v1 ,k)))

  (define fib-outer-k
    (lambda (n k)
      `(fib-outer-k ,n ,k)))

  (define (fib-cps n k)
    (if (< n 2) (apply-k k n)
        (fib-cps (- n 1) (fib-outer-k n k))))

  (define (fib n)
    (fib-cps n (empty-k)))

  (fib-tests fib))

;;; Registerization starting with CPS.
(let ()
  (define *n* 'whocares)
  (define *k* 'whocares)

  (define (fib-cps)
    (if (< *n* 2)
        (*k* *n*)
        (begin
          (set! *k*
                (let ((old-n *n*)
                      (old-k *k*))
                  (lambda (v1)
                    (begin
                      (set! *n* (- old-n 2))
                      (set! *k*
                            (lambda (v2)
                              (old-k (+ v1 v2))))
                      (fib-cps)))))
          (set! *n* (- *n* 1))
          (fib-cps))))

  (define (fib n)
    (begin
      (set! *k* (lambda (v) v))
      (set! *n* n)
      (fib-cps)))

  (fib-tests fib))

;;; Registerization with RI
(let ()
  (define apply-k
    (lambda (k v)
      (k v)))

  (define empty-k
    (lambda ()
      (lambda (v) v)))

  (define fib-inner-k
    (lambda (v1 k)
      (lambda (v2)
        (apply-k k (+ v1 v2)))))

  (define fib-outer-k
    (lambda (n k)
      (lambda (v1)
        (set! *k* (fib-inner-k v1 k))
        (set! *n* (- n 2))
        (fib-cps))))

  (define *n* 'whocares)
  (define *k* 'whocares)
  (define (fib-cps)
    (if (< *n* 2) (apply-k *k* *n*)
        (begin
          (set! *k* (fib-outer-k *n* *k*))
          (set! *n* (- *n* 1))
          (fib-cps))))

  (define (fib n)
    (set! *k*  (empty-k))
    (set! *n* n)
    (fib-cps))

  (fib-tests fib))

;;; Registerization of continuations as well
(let ()

  (define *v* 'whocares)
  (define apply-k
    (lambda ()
      (*k*)))

  (define empty-k
    (lambda ()
      (lambda () *v*)))

  (define fib-inner-k
    (lambda (v1 k)
      (lambda ()
        (set! *k* k)
        (set! *v* (+ v1 *v*))
        (apply-k))))

  (define fib-outer-k
    (lambda (n k)
      (lambda ()
        (set! *k* (fib-inner-k  *v* k))
        (set! *n* (- n 2))
        (fib-cps))))

  (define *n* 'whocares)
  (define *k* 'whocares)
  (define (fib-cps)
    (if (< *n* 2)
        (begin
          (set! *v* *n*)
          (apply-k))
        (begin
          (set! *k* (fib-outer-k *n* *k*))
          (set! *n* (- *n* 1))
          (fib-cps))))

  (define (fib n)
    (set! *k*  (empty-k))
    (set! *n* n)
    (fib-cps))

  (fib-tests fib))

;;; Explicit PC counter
(let ()
  (define *pc* 'whocares)

  (define *v* 'whocares)
  (define apply-k
    (lambda ()
      (set! *pc* *k*)))

  (define empty-k
    (lambda ()
      (lambda () (set! *pc* #f))))

  (define fib-inner-k
    (lambda (v1 k)
      (lambda ()
        (set! *k* k)
        (set! *v* (+ v1 *v*))
        (set! *pc* apply-k))))

  (define fib-outer-k
    (lambda (n k)
      (lambda ()
        (set! *k* (fib-inner-k  *v* k))
        (set! *n* (- n 2))
        (set! *pc* fib-cps))))

  (define *n* 'whocares)
  (define *k* 'whocares)
  (define (fib-cps)
    (if (< *n* 2)
        (begin
          (set! *v* *n*)
          (set! *pc* apply-k))
        (begin
          (set! *k* (fib-outer-k *n* *k*))
          (set! *n* (- *n* 1))
          (set! *pc* fib-cps))))

  (define (fib n)
    (set! *k*  (empty-k))
    (set! *n* n)
    (set! *pc* fib-cps)
    (let loop ()
      (*pc*)
      (if *pc*
          (loop)
          *v*)))

  (fib-tests fib))
