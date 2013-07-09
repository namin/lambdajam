(define foo1
  (lambda (x k)
    (let ((thunk (lambda () (modulo x 2))))
      (if (= x 0)
          (thunk)
          (foo1 (- x 1) (lambda (b)
                          (if b
                              (thunk)
                              (foo1 (- x 2)))))))))
(foo1 10 (lambda (v) v))

(define foo2
  (lambda (f k)
    (k (lambda (xs k)
         (if (null? xs)
             (k xs)
             (foo2 f (lambda (p1)
                       (foo2 f (lambda (p2)
                                 (p2 (cdr xs) (lambda (v)
                                                (p1 v k))))))))))))
(foo2 add1 (lambda (p) (p '(1 2 3) (lambda (v) v))))

(define foo3
  (lambda (f n k)
    (f n (lambda (a)
           (f a k)))))
(foo3 (lambda (x k) (k (add1 x))) 0 (lambda (v) v))

(define foo4
  (lambda (f k)
    (f (lambda (g k) (g g k))
       (lambda (g k) (g g k))
       k)))
(foo4 (lambda (a b k) (k a)) (lambda (v) v))
