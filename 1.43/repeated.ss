(define (repeated f n)
  (define (compose f g)
    (lambda (x)
      (f (g x))))
  
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

((repeated square 5) 2)
