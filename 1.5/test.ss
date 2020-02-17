(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))
;; result in infinite loop (p) since the evaluation order is applicative
