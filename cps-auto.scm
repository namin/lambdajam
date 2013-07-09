(load "pmatch.scm")
(load "test-check.scm")

(define id 0)
(define sym
  (lambda (x)
    (set! id (+ id 1))
    (string->symbol (format "~a~a" x id))))

(define (T expr cont)
  (pmatch
   expr
   ((lambda (,var) ,body)
    `(,cont ,(M expr)))
   (,x (guard (symbol? x))
    `(,cont ,(M expr)))
   ((,f ,e)
    (let (($f (sym '$f))
          ($e (sym '$e)))
      (T f `(λ (,$f)
               ,(T e `(λ (,$e)
                         (,$f ,$e ,cont)))))))))

(define (M expr)
  (pmatch
   expr
   ((λ (,var) ,expr)
    (let (($k (sym '$k)))
      `(λ (,var ,$k) ,(T expr $k))))
   (,x (guard (symbol? x)) x)))
     

;; Examples:

(M '(λ (x) x))

(T '(g a) 'halt)
