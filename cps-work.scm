;;; 1. continuations as an lambda-abstraction over a hole.
(+ 1 (- 2 (+ 3 4)))
(+ 1 (- 2 HOLE))
((lambda (HOLE) (+ 1 (- 2 HOLE))) 7)

;;; 2. Tail-call:
;;;    no more work left to be done when calling a function.
(let ()
  (trace-define factorial
    (lambda (n)
      (if (= n 0) 1
          (* n (factorial (- n 1))))))
  (factorial 6))

(let ()
  (trace-define factorial-iter
    (lambda (n acc)
      (if (= n 0) acc
          (factorial-iter (- n 1) (* n acc)))))
  (define factorial
    (lambda (n)
      (factorial-iter n 0)))
  (factorial 6))

;;; 3. CPS: Continuation-Passing Style.
;;;    a style that enforces calls on tail position.
(let ()
  (trace-define factorial-cps
    (lambda (n k)
      (if (= n 0) (k 1)
          (factorial-cps (- n 1) (lambda (v) (k (* n v)))))))
  (define factorial
    (lambda (n)
      (factorial-cps n (lambda (v) v))))
  (factorial 6))

;;; 4. In general,
(f (g x)) 
(g x (lambda (v) (f v k)))

;;; Brain teasers: translate to CPS, mechanically.
;;; don't think about what the function does, follow the rules.
(define foo1
  (lambda (x)
    (if (or (= x 0) (foo1 (- x 1)))
        (modulo x 2)
        (foo1 (- x 2)))))
(foo1 10)

(define foo2
  (lambda (f)
    (lambda (xs)
      (if (null? xs)
          xs
          ((foo2 f) ((foo2 f) (cdr xs)))))))
((foo2 add1) '(1 2 3))

(define foo3
  (lambda (f n)
    (let ((a (f n)))
      (f a))))
(foo3 add1 0)

(define foo4
  (lambda (f)
    (f (lambda (g) (g g))
       (lambda (g) (g g)))))
(foo4 (lambda (a b) a))
