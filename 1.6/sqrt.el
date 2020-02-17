;; square root by Newton's method
(defun sqrt-iter (guess x)
  (if (good-enough? guess x)
      guess
    (sqrt-iter (improve guess x)
               x)))

(defun improve (guess x)
  (average guess (/ x guess)))

(defun average (x y)
  (/ (+ x y) 2))

(defun good-enough? (guess x)
  (< (abs (- (square guess) x)) 0.001))

(defun square (x)
    (* x x))

(defun sqrt (x)
  (sqrt-iter 1.0 x))

(sqrt 9)
(sqrt (+ 100 37))
(sqrt (+ (sqrt 2) (sqrt 3)))
(square (sqrt 1000))

;; exercise 1.6
(defun new-if (predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else-clause)))
(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)

(defun sqrt-iter (guess x)
  (new-if (good-enough? guess x)
      guess
    (sqrt-iter (improve guess x)
               x)))
(sqrt 9)

;; result in Debugger entered--Lisp error: (error "Variable binding depth exceeds max-specpdl-size")
;;  (square guess)
;;  (- (square guess) x)
;;  (abs (- (square guess) x))
;;  (< (abs (- (square guess) x)) 0.001)
;;  good-enough\?(3.0 9)
;;  (new-if (good-enough\? guess x) guess (sqrt-iter (improve guess x) x))
;;  sqrt-iter(3.0 9)
;;  (new-if (good-enough\? guess x) guess (sqrt-iter (improve guess x) x))
;;  sqrt-iter(3.0 9)
;;  (new-if (good-enough\? guess x) guess (sqrt-iter (improve guess x) x))

