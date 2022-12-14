(define (fib n)
  (if (< n 2)
      1
      (+ (fib (- n 2))
         (fib (- n 1)))))

(=
 (fib 1)
 1)

(=
 (fib 2)
 2)

(=
 (fib 3)
 3)

(=
 (fib 4)
 5)
