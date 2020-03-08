(define (compose f g)
  (lambda (x) (f (g x))))

((compose square add1) 6)
