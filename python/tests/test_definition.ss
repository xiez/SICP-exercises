(define a 42)

(define (f a)
  (+ a 1))
(f 10)

(define (g b)
  (f b))
(g 20)
