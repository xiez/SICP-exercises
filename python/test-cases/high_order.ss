(define (f x)
  (define (g)
    x)
  g)

(f 10)

(=
 ((f 10))
 10
 )
