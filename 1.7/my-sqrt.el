;; square root by Newton's method
(defun sqrt-iter (guess x)
  (let ((improved-guess (improve guess x)))
    (if (close-enough? improved-guess guess)
        improved-guess
        (sqrt-iter improved-guess x))))

(defun improve (guess x)
  (average guess (/ x guess)))

(defun average (x y)
  (/ (+ x y) 2))

(defun close-enough? (a b)
  (let ((radio (if (> a b) (/ a b) (/ b a))))
    (< radio 1.01)))

(defun square (x)
    (* x x))

(defun mysqrt (x)
  (sqrt-iter 1.0 x))

(mysqrt 0.0009)
(square (mysqrt 0.0009))
(mysqrt 9400000000000000000)
(square (mysqrt 9400000000000000000))
