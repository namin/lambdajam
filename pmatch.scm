;; It has two important new features:
;; 1.  It allows for a name to be given to pmatch if an error ensues.
;; 2.  A line from the specification has been removed. (see below).  Without
;; that line removed, it was impossible for a pattern to be (quote ,x),
;; which might be worth having especially when one writes an interpreter
;; for Scheme, which includes quote as a language form.

;;; Code written by Oleg Kiselyov
;; (http://pobox.com/~oleg/ftp/)
;;;
;;; Taken from leanTAP.scm
;;; http://kanren.cvs.sourceforge.net/kanren/kanren/mini/leanTAP.scm?view=log

; A simple linear pattern matcher
; It is efficient (generates code at macro-expansion time) and simple:
; it should work on any R5RS (and R6RS) Scheme system.

; (pmatch exp (quote name) &lt;clause&gt; ...[&lt;else-clause&gt;]) | ADDED (spring, 2013)
; (pmatch exp &lt;clause&gt; ...[&lt;else-clause&gt;]) |
; (pmatch exp name &lt;clause&gt; ...[&lt;else-clause&gt;]))          ADDED (summer, 2012)
; &lt;clause&gt; ::= (&lt;pattern&gt; &lt;guard&gt; exp ...)
; &lt;else-clause&gt; ::= (else exp ...)
; &lt;guard&gt; ::= boolean exp | ()
; &lt;pattern&gt; :: =
;        ,var  -- matches always and binds the var
;                 pattern must be linear! No check is done
;         _    -- matches always
;         ?    -- matches always                        REMOVED (August 28, 2012)
;        'exp  -- comparison with exp (using equal?)    REMOVED (August 8, 2012)
;        exp   -- comparison with exp (using equal?)
;        (&lt;pattern1&gt; &lt;pattern2&gt; ...) -- matches the list of patterns
;        (&lt;pattern1&gt; . &lt;pattern2&gt;)  -- ditto
;        ()    -- matches the empty list                REMOVED (August 28, 2012)

(define-syntax pmatch
  (syntax-rules (else guard quote)
    ((_ v (quote name) (e ...) ...)
     (pmatch-aux (quote name) v (e ...) ...))
    ((_ v (e ...) ...)
     (pmatch-aux #f v (e ...) ...))
    ((_ v name (e ...) ...)
     (pmatch-aux name v (e ...) ...))))

(define-syntax pmatch-aux
  (syntax-rules (else guard)
    ((_ name (rator rand ...) cs ...)
     (let ((v (rator rand ...)))
       (pmatch-aux name v cs ...)))
    ((_ name v)
     (begin
       (if 'name
           (printf "pmatch ~s failed\n" 'name)
           (printf "pmatch failed\n"))
       (printf "with input evaluating to ~s\n" v)
       (error 'pmatch "match failed")))
    ((_ name v (else e0 e ...)) (begin e0 e ...))
    ((_ name v (pat (guard g ...) e0 e ...) cs ...)
     (let ((fk (lambda () (pmatch-aux name v cs ...))))
       (ppat v pat (if (and g ...) (begin e0 e ...) (fk)) (fk))))
    ((_ name v (pat e0 e ...) cs ...)
     (let ((fk (lambda () (pmatch-aux name v cs ...))))
       (ppat v pat (begin e0 e ...) (fk))))))

(define-syntax ppat
  (syntax-rules (unquote)
    ((_ v (unquote var) kt kf) (let ((var v)) kt))
    ((_ v (x . y) kt kf)
     (if (pair? v)
       (let ((vx (car v)) (vy (cdr v)))
	 (ppat vx x (ppat vy y kt kf) kf))
       kf))
    ((_ v lit kt kf) (if (equal? v (quote lit)) kt kf))))

