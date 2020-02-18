;; square root by Newton's method
(define (sqrt-iter guess x)
  (let ((improved-guess (improve guess x)))
    (if (close-enough? improved-guess guess)
        improved-guess
        (sqrt-iter improved-guess x))))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (close-enough? a b)
  (let ((radio (if (> a b) (/ a b) (/ b a))))
    (< radio 1.01)))

(define (square x)
  (* x x))

(define (mysqrt x)
  (sqrt-iter 1.0 x))

(mysqrt 0.0009)
(square (mysqrt 0.0009))
(mysqrt 9400000000000000000)
(square (mysqrt 9400000000000000000))
