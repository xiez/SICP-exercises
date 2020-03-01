(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a ) result))))
  (iter a 0))

(define (sum-integers a b)
  (define (identity a) a)
  (define (inc n) (+ n 1))
  (sum identity a inc b))

(sum-integers 1 10)

