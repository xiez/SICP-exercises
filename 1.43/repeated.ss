(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (compose f g)
  (lambda (x y) (f (g x) y)))

(define (repeated f n)
  (lambda (x)
    ((compose expt f) x n)))

((repeated square 5) 2)
