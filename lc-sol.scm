(load "test-check.scm")
(load "pmatch.scm")
(load "sugar.scm")

(define (lc-tests lc empty-env)
  (eg (lc #f empty-env) #f)
  (eg (lc '(if #f #t #f) empty-env) #f)
  (eg (lc '(((lambda (fun)
               ((lambda (F)
                  (F F))
                (lambda (F)
                  (fun (lambda (x) ((F F) x))))))
             (lambda (factorial)
                (lambda (n)
                  (if (zero? n)
                      1
                      (* n (factorial (sub1 n)))))))
            6)
          empty-env)
      720))

(let ()
  (define empty-env (lambda (y) (error 'env "unbound variable")))

  (define lc
    (lambda (exp env)
      (pmatch
       exp
       (,x (guard (symbol? x)) (env x))
       (,b (guard (boolean? b)) b)
       (,n (guard (number? n)) n)
       ((sub1 ,e)
        (sub1 (lc e env)))
       ((zero? ,e)
        (zero? (lc e env)))
       ((* ,e1 ,e2)
        (* (lc e1 env) (lc e2 env)))
       ((if ,c ,a ,b)
        (if (lc c env)
            (lc a env)
            (lc b env)))
       ((lambda (,x) ,body)
        (lambda (a)
          (lc body
              (lambda (y) (if (eq? x y) a (env y))))))
       ((,rator ,rand)
        ((lc rator env) (lc rand env))))))

  (lc-tests lc empty-env))

;;; CPS
(let ()
  (define empty-env (lambda (y) (error 'env "unbound variable")))

  (define lc-cps
    (lambda (exp env k)
      (pmatch
       exp
       (,x (guard (symbol? x)) (k (env x)))
       (,b (guard (boolean? b)) (k b))
       (,n (guard (number? n)) (k n))
       ((sub1 ,e)
        (lc-cps e env (lambda (v) (k (sub1 v)))))
       ((zero? ,e)
        (lc-cps e env (lambda (v) (k (zero? v)))))
       ((* ,e1 ,e2)
        (lc-cps e1 env (lambda (v1)
                     (lc-cps e2 env (lambda (v2)
                                  (k (* v1 v2)))))))
       ((if ,c ,a ,b)
        (lc-cps c env (lambda (vc)
                    (if vc
                     (lc-cps a env k)
                     (lc-cps b env k)))))
       ((lambda (,x) ,body)
        (k (lambda (a k)
             (lc-cps body
                 (lambda (y) (if (eq? x y) a (env y)))
                 k))))
       ((,rator ,rand)
        (lc-cps rator env (lambda (vrator)
                        (lc-cps rand env (lambda (vrand)
                                           (vrator vrand k)))))))))

  (define lc
    (lambda (exp env)
      (lc-cps exp env (lambda (v) v))))

  (lc-tests lc empty-env))

;;; Adding call/cc to a CPSed-interpreter
(let ()
  (define empty-env (lambda (y) (error 'env "unbound variable")))

  (define lc-cps
    (lambda (exp env k)
      (pmatch
       exp
       (,x (guard (symbol? x)) (k (env x)))
       (,b (guard (boolean? b)) (k b))
       (,n (guard (number? n)) (k n))
       ((sub1 ,e)
        (lc-cps e env (lambda (v) (k (sub1 v)))))
       ((zero? ,e)
        (lc-cps e env (lambda (v) (k (zero? v)))))
       ((* ,e1 ,e2)
        (lc-cps e1 env (lambda (v1)
                     (lc-cps e2 env (lambda (v2)
                                  (k (* v1 v2)))))))
       ((call/cc ,e)
        (lc-cps e env (lambda (p)
                        (p (lambda (v k^) (k v)) k))))
       ((if ,c ,a ,b)
        (lc-cps c env (lambda (vc)
                    (if vc
                     (lc-cps a env k)
                     (lc-cps b env k)))))
       ((lambda (,x) ,body)
        (k (lambda (a k)
             (lc-cps body
                 (lambda (y) (if (eq? x y) a (env y)))
                 k))))
       ((,rator ,rand)
        (lc-cps rator env (lambda (vrator)
                        (lc-cps rand env (lambda (vrand)
                                           (vrator vrand k)))))))))

  (define lc
    (lambda (exp env)
      (lc-cps exp env (lambda (v) v))))

  (lc-tests lc empty-env)

  (eg (lc '(sub1 (call/cc (lambda (k) 2)))
          empty-env)
      1)

  (eg (lc '(sub1 (call/cc (lambda (k) (* 3 (k 2)))))
          empty-env)
      1))

