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

