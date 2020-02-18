(define (cube-root-iter guess x)
  (let ((improved-guess (improve guess x)))
    (if (close-enough? improved-guess guess)
        improved-guess
        (cube-root-iter improved-guess x))))

(define (improve guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

(define (average x y)
  (/ (+ x y) 2))

(define (close-enough? a b)
  (let ((radio (if (> a b) (/ a b) (/ b a))))
    (< radio 1.01)))

(define (cube-root x)
  (cube-root-iter 1.0 x))

(define (cube x)
  (* (* x x) x))

(cube-root 27)
(cube (cube-root 27))
