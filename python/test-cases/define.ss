(define a 42)
(= a 42)

(define (f a)
  (+ a 1))
(=
 (f 10)
 11)

(define (f2 a)
  (+ a 1)
  (+ 1 1))
(=
 (f2 20)
 2)

(define (g b)
  (f b))
(=
 (g 20)
 21)

;; internal defination
;; and capture the free variable `a`
(define (foo a)
  (define (bar x)
    (+ x a))
  (bar 42))

(=
 (foo 10)
 52
 )

