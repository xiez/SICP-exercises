(defun cube-root-iter (guess x)
  (let ((improved-guess (improve guess x)))
    (if (close-enough? improved-guess guess)
        improved-guess
        (cube-root-iter improved-guess x))))

(defun improve (guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

(defun average (x y)
  (/ (+ x y) 2))

(defun close-enough? (a b)
  (let ((radio (if (> a b) (/ a b) (/ b a))))
    (< radio 1.01)))

(defun cube-root (x)
  (cube-root-iter 1.0 x))

(defun cube (x)
  (* (* x x) x))

(cube-root 27)
(cube (cube-root 27))
