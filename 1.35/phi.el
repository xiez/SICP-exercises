(setq tolerance 0.00001)

(defun fixed-point (f first-guess)
  (defun close-enough? (v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (defun try (guess)
    (let ((next (funcall f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(defun sqrt (x)
  (defun average (a b)
    (/ (+ a b) 2))
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

(sqrt 4)

;; ****************************************

(defun phi ()
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               1.0))

(phi)
