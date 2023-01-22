(define f
  (lambda (x)
    (+ x 1)))
(= (f 10) 11)

(=
 ((lambda (x)
   (+ x 1))
 10)
 11)


