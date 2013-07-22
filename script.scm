;; http://github.com/namin/lambdajam
;;; in racket:
;;; #lang scheme

(load "test-check.scm")
(load "pmatch.scm")
(load "sugar.scm")

;; Scheme

(+ 1 2)
(+ (+ 1 2) (+ 3 4))
(+ 3 7)
((lambda (x) (+ x 1)) 1)
((lambda () 1))
(if (= 1 1) 2 (/ 1 0))

((if (= 1 1) - +) 2 1)
(((lambda () +)) 1 2)
((lambda () (display "hello")))
+

(define a 1)
a
(define inc (lambda (x) (+ x 1)))
(inc 1)

(define adder
  (lambda (x)
    (lambda (y)
      (+ x y))))
(adder 1) ;; #<procedure>
((adder 1) 2) ;; 3

(cons 1
      (cons 2 '())
      )
;; (1 . '(2 . '()))
;; (1 2)
(cdr  (cons 'a 'd))
(pair? (cons 1 2)) ;; #t
(list? (cons 1 2)) ;; #f
(null? '()) ;; #t
(null? (cons 1 '())) ;; #f
(cons 'a (cons 'b 'c))

;;; Proper List
;;; either:
;;; (1) empty list
;;; (2) a pair whose cdr is a proper list

(cadr '(1 2 3))
(car (cdr '(1 2 3)))

(append '(1 2 3) '(4 5 6))
(define append
  (lambda (xs ys)
    (if (null? xs)
        ys
        (cons (car xs) (append (cdr xs) ys)))))

(define append
  (lambda (xs ys)
    (cond
     ((null? xs) ys)
     (else (cons (car xs) (append (cdr xs) ys))))))

(rember 'a '(a b c))   ;; '(b c)
(rember 'b '(a b c b)) ;; '(a c)
(rember 'b '(a (b a) c b)) ;; '(a (a) c)
(rember 'b '(a (b b (b) a b) c b))
(define rember
  (lambda (x xs)
    (cond
     ((null? xs) '())
     
     ((pair? (car xs))
      (cons (rember x (car xs))
            (rember x (cdr xs))))
     
     ((eq? x (car xs))
      (rember x (cdr xs)))
     (else
      (cons (car xs)
            (rember x (cdr xs)))))))


(define foo '(hello there))
`(,foo ,foo)
`(,(+ 1 2) ,(+ 3 4)) ;; (3 7)
`(,(+ 1 2) (+ 3 4)) ;; (3 (+ 3 4))

;;;; -------------------------------------------------

;; Interpreters

;;; Call-By-Value (same as in Scheme)

;;; x, y, z (variables)
;;; e = x           (variable)
;;;     (lambda (x) e_0) (abstraction)
;;;     (e_1 e_2)   (application)

;;; env: variable name -> value
(define empty-env
  (lambda (x) (error 'env-lookup "unbound variable")))

(define eval-exp
  (lambda (exp env)
    (pmatch
     exp
     (,x (guard (symbol? x)) (env x))
     (,n (guard (number? n)) n)
     (,b (guard (boolean? b)) b)
     ((zero? ,e)
      (zero? (eval-exp e env)))
     ((sub1 ,e)
      (sub1 (eval-exp e env)))
     ((* ,e1 ,e2)
      (* (eval-exp e1 env) (eval-exp e2 env)))
     ((if ,c ,a ,b) ;; '(if (zero? 1) 1 2) c will become '(zero? 1)
      (if (eval-exp c env)
          (eval-exp a env)
          (eval-exp b env)))
     ((lambda (,x) ,body) ;; lambda syntax
      (lambda (a) ;; closure representation
        (eval-exp body
                  (lambda (y) ;; env extension
                    (if (eq? y x)
                        a
                        (env y))))))
     ((,e1 ,e2)
      ((eval-exp e1 env)
       (eval-exp e2 env))))))

(define eval-top
  (lambda (exp)
    (eval-exp exp empty-env)))

(eg (eval-exp 'x
              (lambda (y) (if (eq? y 'x) 1 (empty-env y))))
    1)

(eg (eval-top '((lambda (x) 1) 2))
    1)

(eg (eval-exp '((lambda (x) x) 2)
              empty-env)
    2)

;; (eval-exp '((lambda (x) (x x)) (lambda (x) (x x))) empty-env)

(eg (eval-exp '((lambda (y) y) x)
              (lambda (y) (if (eq? y 'x) 1 (empty-env y))))
    1)

(eval-top '(((lambda (fun)
              ((lambda (F)
                 (F F))
               (lambda (F)
                 (fun (lambda (x) ((F F) x))))))
            (lambda (factorial)
              (lambda (n)
                (if (zero? n)
                    1
                    (* n (factorial (sub1 n)))))))
           6))

;;; Program Transformations

;;; CPS: continuation-passing style

;;; continuations
;;; representation of the rest of the work to be done

(+ 1 2 (+ 3 4))
((lambda (v) v) (+ 1 2 (+ 3 4)))
((lambda (HOLE) (+ 1 2 HOLE)) (+ 3 4)) ;; 12
((lambda (v) (+ 1 v (+ 3 4))) 2)

(+ (+ 1 2) (+ 3 4))
((lambda (v) (+ v (+ 3 4))) (+ 1 2))
((lambda (v) (+ (+ 1 2) v)) (+ 3 4))

(lambda (v) v) ;; identity continuation

;;; Continuation-Passing Style

;;; direct-style
(define factorial
  (lambda (n)
    (if (= n 0)
        1
        (* n (factorial (- n 1))))))
(trace factorial)
;;(factorial -1)

;;; accumulator
(define factorial-iter
  (lambda (n acc)
    (if (= n 0)
        acc
        (factorial-iter (- n 1) (* acc n)))))
(define factorial
  (lambda (n)
    (factorial-iter n 1)))
(trace factorial-iter)
;;(factorial -1)

;;; Steps
;;; - Change the name with -cps.
;;; - Add k argument to lambda.
;;; - Return by applying k.
;;; - Serious calls need to be transformed.

(define factorial-cps
  (lambda (n k)
    (if (= n 0)
        (k 1)
        (factorial-cps (- n 1) (lambda (v)
                                 (k (* n v)))))))
(define factorial
  (lambda (n)
    (factorial-cps n (lambda (v) v))))
(factorial 5)
(trace factorial-cps)

;;; direct-style
(define fib
  (lambda (n)
    (if (< n 2) n
        (+ (fib (- n 1)) (fib (- n 2))))))

(define fib-cps
  (lambda (n k)
    (if (< n 2) (k n)
        (fib-cps (- n 2)
                 (lambda (v2)
                   (fib-cps (- n 1) (lambda (v1)
                                      (k (+ v1 v2)))))))))
(define fib
  (lambda (n)
    (fib-cps n (lambda (v) v))))
(fib 5)

;;; Trampolining

(define factorial-cps
  (lambda (n k)
    (lambda ()
      (if (= n 0)
          (k 1)
          (factorial-cps (- n 1) (lambda (v)
                                   (k (* n v))))))))

(define factorial-cps
  (lambda (n k)
    (if (= n 0)
        (lambda () (k 1))
        (lambda () (factorial-cps (- n 1) (lambda (v)
                                       (k (* n v))))))))
(define factorial
  (lambda (n)
    (let ((r (factorial-cps n (lambda (v) v))))
      (while (procedure? r)
             (set! r (r)) ;; body of the while
             r)) ;; return after the while
    ))

(define factorial
  (lambda (n)
    (let loop ((r (factorial-cps n (lambda (v) v))))
      (if (procedure? r)
          (loop (r))
          r))))

;;; CPS-ing interpreter

(define empty-env
  (lambda (x) (error 'env-lookup "unbound variable")))

(define eval-exp-cps
  (lambda (exp env k)
    (pmatch
     exp
     (,x (guard (symbol? x)) (k (env x)))
     (,n (guard (number? n)) (k n))
     (,b (guard (boolean? b)) (k b))
     ((zero? ,e)
      (eval-exp-cps e env (lambda (v)
                            (k (zero? v)))))
     ((sub1 ,e)
      (eval-exp-cps e env (lambda (v)
                            (k (sub1 v)))))
     ((* ,e1 ,e2)
      (eval-exp-cps e1 env (lambda (v1)
                             (eval-exp-cps e2 env (lambda (v2)
                                                    (k (* v1 v2)))))))
     ((if ,c ,a ,b)
      (eval-exp-cps c env (lambda (vc)
                            (if vc
                                (eval-exp-cps a env k)
                                (eval-exp-cps b env k)))))
     ((lambda (,x) ,body)
      (k (lambda (a k^)
           (eval-exp-cps body
                         (lambda (y)
                           (if (eq? y x)
                               a
                               (env y)))
                         k^))))
     ((,e1 ,e2)
      (eval-exp-cps e1 env (lambda (p)
                             (eval-exp-cps e2 env (lambda (a)
                                                    (p a k)))))))))

(define eval-top
  (lambda (exp)
    (eval-exp-cps exp empty-env (lambda (v) v))))

(trace eval-exp-cps)
(eg (eval-top '((lambda (x) 1) 2))
    1)

(eval-top '(((lambda (fun)
              ((lambda (F)
                 (F F))
               (lambda (F)
                 (fun (lambda (x) ((F F) x))))))
            (lambda (factorial)
              (lambda (n)
                (if (zero? n)
                    1
                    (* n (factorial (sub1 n)))))))
           6))
